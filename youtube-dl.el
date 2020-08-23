;;; youtube-dl.el --- manages a youtube-dl queue -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/youtube-dl-emacs
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; This package manages a video download queue for the youtube-dl
;; command line program, which serves as its back end. It manages a
;; single youtube-dl subprocess to download one video at a time. New
;; videos can be queued at any time.

;; The `youtube-dl' command queues a URL for download. Failures are
;; retried up to `youtube-dl-max-failures'. Items can be paused or set
;; to be downloaded at a slower rate (`youtube-dl-slow-rate').

;; The `youtube-dl-playlist' command queues an entire playlist, just
;; as if you had individually queued each video on the playlist.

;; The `youtube-dl-list' command displays a list of all active video
;; downloads. From this list, items under point can be canceled (d),
;; paused (p), slowed (s), and have its priority adjusted ([ and ]).

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'hl-line)

(defgroup youtube-dl ()
  "Download queue for the youtube-dl command line program."
  :group 'external)

(defcustom youtube-dl-directory "~"
  "Directory in which to run youtube-dl."
  :group 'youtube-dl
  :type 'directory)

(defcustom youtube-dl-program "youtube-dl"
  "The name of the program invoked for downloading YouTube videos."
  :group 'youtube-dl
  :type 'string)

(defcustom youtube-dl-arguments
  '("--no-mtime" "--restrict-filenames")
  "Arguments to be send to youtube-dl.
Instead of --rate-limit use `youtube-dl-slow-rate'."
  :group 'youtube-dl
  :type '(repeat string))

(defcustom youtube-dl-proxy ""
  "Specify the proxy for youtube-dl command.
For example:

127.0.0.1:8118
socks5://127.0.0.1:1086"
  :type 'string
  :safe #'stringp
  :group 'youtube-dl)

(defcustom youtube-dl-proxy-url-list '()
  "A list of URL domains which should use proxy for youtube-dl."
  :type 'list
  :safe #'listp
  :group 'youtube-dl)

(defcustom youtube-dl-auto-show-list t
  "Auto show youtube-dl-list buffer."
  :type 'boolean
  :safe #'booleanp
  :group 'youtube-dl)

(defcustom youtube-dl-max-failures 8
  "Maximum number of retries for a single video."
  :group 'youtube-dl
  :type 'integer)

(defcustom youtube-dl-slow-rate "2M"
  "Download speed for \"slow\" items (argument for --rate-limit)."
  :group 'youtube-dl
  :type 'string)

(defface youtube-dl-active
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting the active download item."
  :group 'youtube-dl)

(defface youtube-dl-slow
  '((t :inherit font-lock-variable-name-face :foreground "orange"))
  "Face for highlighting the slow (S) tag."
  :group 'youtube-dl)

(defface youtube-dl-pause
  '((t :inherit font-lock-type-face :foreground "blue"))
  "Face for highlighting the pause (P) tag."
  :group 'youtube-dl)

(defface youtube-dl-priority
  '((t :inherit font-lock-keyword-face :foreground "green"))
  "Face for highlighting the priority marker."
  :group 'youtube-dl)

(defface youtube-dl-failure
  '((t :inherit font-lock-warning-face :foreground "red"))
  "Face for highlighting the failure marker."
  :group 'youtube-dl)

(defvar-local youtube-dl--log-item nil
  "Item currently being displayed in the log buffer.")

(cl-defstruct (youtube-dl-item (:constructor youtube-dl-item--create)
                               (:copier nil))
  "Represents a single video to be downloaded with youtube-dl."
  url          ; Video URL (string)
  vid          ; Video ID (integer)
  directory    ; Working directory for youtube-dl (string or nil)
  destination  ; Preferred destination file (string or nil)
  failures     ; Number of video download failures (integer)
  priority     ; Download priority (integer)
  title        ; Listing display title (string or nil)
  progress     ; Current download progress (string or nil)
  total        ; Total download size (string or nil)
  log          ; All program output (list of strings)
  log-end      ; Last log item (list of strings)
  paused-p     ; Non-nil if download is paused
  slow-p)      ; Non-nil if download should be rate limited

(defvar youtube-dl-items ()
  "List of all items still to be downloaded.")

(defvar youtube-dl-process nil
  "The currently active youtube-dl process.")

(defun youtube-dl--proxy-append (url &optional option value)
  "Decide whether append proxy option in youtube-dl command based on URL."
  (let ((domain (url-domain (url-generic-parse-url url))))
    (if (and (member domain youtube-dl-proxy-url-list) ; <---------- whether toggle proxy?
             (not (string-empty-p youtube-dl-proxy)))
        (if option                      ; <------------------------- whether has command-line option?
            (list "--proxy" youtube-dl-proxy option value)
          (list "--proxy" youtube-dl-proxy))
      (if option                        ; <------------------------- return original arguments for no proxy
          (list option value)
        nil    ; <------------------------- return nothing for url no need proxy
        ))))

(defun youtube-dl--next ()
  "Returns the next item to be downloaded."
  (let (best best-score)
    (dolist (item youtube-dl-items best)
      (let* ((failures (youtube-dl-item-failures item))
             (priority (youtube-dl-item-priority item))
             (paused-p (youtube-dl-item-paused-p item))
             (score (- priority failures)))
        (when (and (not paused-p)
                   (< failures youtube-dl-max-failures))
          (cond ((null best)
                 (setf best item
                       best-score score))
                ((> score best-score)
                 (setf best item
                       best-score score))))))))

(defun youtube-dl--remove (item)
  "Remove ITEM from the queue."
  (setf youtube-dl-items (cl-delete item youtube-dl-items)))

(defun youtube-dl--add (item)
  "Add ITEM to the queue."
  (setf youtube-dl-items (nconc youtube-dl-items (list item))))

(defun youtube-dl--sentinel (proc status)
  (let ((item (plist-get (process-plist proc) :item)))
    (setf youtube-dl-process nil)
    (if (equal status "finished\n")
        (youtube-dl--remove item)
      (cl-incf (youtube-dl-item-failures item)))
    (youtube-dl--run)))

(defun youtube-dl--progress (output)
  "Return the download progress for the given output.
Progress lines that straddle output chunks are lost. That's fine
since this is just used for display purposes."
  (let ((start 0)
        (pair nil))
    (while (string-match "\\([^ ]+%\\) +of +\\([^ ]+\\) " output start)
      (setf pair (cons (match-string 1 output) (match-string 2 output))
            start (match-end 0)))
    pair))

(defun youtube-dl--get-destination (url)
  "Return the destination filename for the given `URL' (if any).
The destination filename may potentially straddle two output
chunks, but this is incredibly unlikely. It's only used for
display purposes anyway."
  (with-temp-buffer
    (apply #'call-process
           "youtube-dl"
           nil t nil
           (youtube-dl--proxy-append url "--get-filename" url))
    (replace-regexp-in-string "\n" "" (buffer-string))))

(defun youtube-dl--filter (proc output)
  (let* ((item (plist-get (process-plist proc) :item))
         (progress (youtube-dl--progress output))
         (destination (youtube-dl-item-destination item)))
    ;; Append to program log.
    (let ((logged (list output)))
      (if (youtube-dl-item-log item)
          (setf (cdr (youtube-dl-item-log-end item)) logged
                (youtube-dl-item-log-end item) logged)
        (setf (youtube-dl-item-log item) logged
              (youtube-dl-item-log-end item) logged)))
    ;; Update progress information.
    (when progress
      (cl-destructuring-bind (percentage . total) progress
        (setf (youtube-dl-item-progress item) percentage
              (youtube-dl-item-total item) total)))
    ;; Set a title if needed.
    (when destination
      (setf (youtube-dl-item-title item) destination))
    (youtube-dl--redisplay)))

(defun youtube-dl--current ()
  "Return the item currently being downloaded."
  (when youtube-dl-process
    (plist-get (process-plist youtube-dl-process) :item)))

(defun youtube-dl--run ()
  "As necessary, start or switch to the highest priority item."
  (let ((item (youtube-dl--next))
        (current-item (youtube-dl--current)))
    (if (eq item current-item)
        (youtube-dl--redisplay) ; do nothing
      (if youtube-dl-process
          (progn
            ;; Switch to higher priority job, but offset error count first.
            (cl-decf (youtube-dl-item-failures current-item))
            (kill-process youtube-dl-process)) ; sentinel will clean up
        ;; No subprocess running, start a one.
        (let* ((item-url (youtube-dl-item-url item))
               ;; fix link is an org-mode link is a property list.
               (url (if (stringp item-url)
                        item-url (substring-no-properties item-url)))
               (directory (youtube-dl-item-directory item))
               (destination (youtube-dl-item-destination item))
               (default-directory
                 (if directory
                     (concat (directory-file-name directory) "/")
                   (concat (directory-file-name youtube-dl-directory) "/")))
               (slow-p (youtube-dl-item-slow-p item))
               (proc (progn
                       (mkdir default-directory t)
                       (apply #'start-process
                              "youtube-dl" nil youtube-dl-program "--newline"
                              (nconc (cl-copy-list youtube-dl-arguments)
                                     (youtube-dl--proxy-append url)
                                     (when slow-p
                                       `("--rate-limit" ,youtube-dl-slow-rate))
                                     (when destination
                                       `("--output" ,destination))
                                     `("--" ,url))))))
          (set-process-plist proc (list :item item))
          (set-process-sentinel proc #'youtube-dl--sentinel)
          (set-process-filter proc #'youtube-dl--filter)
          (setf youtube-dl-process proc))))
    (youtube-dl--redisplay)))

(defun youtube-dl--get-vid (url)
  "Get video `URL' video vid number with youtube-dl option `--get-id'."
  (with-temp-buffer
    (apply #'call-process
           "youtube-dl"
           nil t nil
           (youtube-dl--proxy-append url "--get-id" url))
    (replace-regexp-in-string "\n" "" (buffer-string))))

;;;###autoload
(cl-defun youtube-dl
    (url &key title (priority 0) directory destination paused slow)
  "Queues URL for download using youtube-dl, returning the new item."
  (interactive
   (list (read-from-minibuffer
          "URL: " (or (thing-at-point 'url)
                      (when interprogram-paste-function
                        (funcall interprogram-paste-function))))))
  ;; remove this ID failure only on youtube.com, use URL as ID. or use youtube-dl extracted title, or hash on URL.
  (let* ((vid (youtube-dl--get-vid url))
         (destination (youtube-dl--get-destination url))
         (full-dir (expand-file-name (or directory "") youtube-dl-directory))
         (item (youtube-dl-item--create :url url
                                        :vid vid
                                        :failures 0
                                        :priority priority
                                        :paused-p paused
                                        :slow-p slow
                                        :directory full-dir
                                        :destination destination
                                        :title title)))
    (prog1 item
      (when url
        (youtube-dl--add item)
        (youtube-dl--run)
        (when youtube-dl-auto-show-list
          (youtube-dl-list))))))

(defun youtube-dl--playlist-list (playlist)
  "For each video, return one plist with :index, :vid, and :title."
  (with-temp-buffer
    (when (zerop (call-process youtube-dl-program nil t nil
                               "--ignore-config"
                               "--dump-json"
                               "--flat-playlist"
                               playlist))
      (setf (point) (point-min))
      (cl-loop with json-object-type = 'plist
               for index upfrom 1
               for video = (ignore-errors (json-read))
               while video
               collect (list :index index
                             :vid   (plist-get video :vid)
                             :title (plist-get video :title))))))

(defun youtube-dl--playlist-reverse (list)
  "Return a copy of LIST with the indexes reversed."
  (let ((max (cl-loop for entry in list
                      maximize (plist-get entry :index))))
    (cl-loop for entry in list
             for index = (plist-get entry :index)
             for copy = (copy-sequence entry)
             collect (plist-put copy :index (- (1+ max) index)))))

(defun youtube-dl--playlist-cutoff (list n)
  "Return a sorted copy of LIST with all items except where :index < N."
  (let ((key (lambda (v) (plist-get v :index)))
        (filter (lambda (v) (< (plist-get v :index) n)))
        (copy (copy-sequence list)))
    (cl-delete-if filter (cl-stable-sort copy #'< :key key))))

;;;###autoload
(cl-defun youtube-dl-playlist
    (url &key directory (first 1) paused (priority 0) reverse slow)
  "Add entire playlist to download queue, with index prefixes.

:directory PATH -- Destination directory for all videos.

:first INDEX -- Start downloading from a given one-based index.

:paused BOOL -- Start all download entries as paused.

:priority PRIORITY -- Use this priority for all download entries.

:reverse BOOL -- Reverse the video numbering, solving the problem
of reversed playlists.

:slow BOOL -- Start all download entries in slow mode."
  (interactive
   (list (read-from-minibuffer
          "URL: "
          (when interprogram-paste-function
            (funcall interprogram-paste-function)))))
  (message "Fetching playlist ...")
  (let ((videos (youtube-dl--playlist-list url)))
    (if (null videos)
        (error "Failed to fetch playlist (%s)." url)
      (let* ((max (cl-loop for entry in videos
                           maximize (plist-get entry :index)))
             (width (1+ (floor (log max 10))))
             (prefix-format (format "%%0%dd" width)))
        (when reverse
          (setf videos (youtube-dl--playlist-reverse videos)))
        (dolist (video (youtube-dl--playlist-cutoff videos first))
          (let* ((index (plist-get video :index))
                 (prefix (format prefix-format index))
                 (title (format "%s-%s" prefix (plist-get video :title)))
                 (dest (format "%s-%s" prefix "%(title)s-%(id)s.%(ext)s")))
            (youtube-dl (plist-get video :url)
                        :title title
                        :priority priority
                        :directory directory
                        :destination dest
                        :paused paused
                        :slow slow)))))))

;; List user interface:

(defun youtube-dl-list-redisplay ()
  "Immediately redraw the queue list buffer."
  (interactive)
  (with-current-buffer (youtube-dl--buffer)
    (let ((save-point (point))
          (window (get-buffer-window (current-buffer))))
      (youtube-dl--fill-listing)
      (setf (point) save-point)
      (when window
        (set-window-point window save-point))
      (when hl-line-mode
        (hl-line-highlight)))))

(defun youtube-dl--redisplay ()
  "Redraw the queue list buffer only if visible."
  (let ((log-buffer (youtube-dl--log-buffer)))
    (when log-buffer
      (with-current-buffer log-buffer
        (let ((inhibit-read-only t)
              (window (get-buffer-window log-buffer)))
          (erase-buffer)
          (mapc #'insert (youtube-dl-item-log youtube-dl--log-item))
          (when window
            (set-window-point window (point-max)))))))
  (when (get-buffer-window (youtube-dl--buffer))
    (youtube-dl-list-redisplay)))

(defun youtube-dl-list-log ()
  "Display the log of the video under point."
  (interactive)
  (let* ((n (1- (line-number-at-pos)))
         (item (nth n youtube-dl-items)))
    (when item
      (display-buffer (youtube-dl--log-buffer item))
      (youtube-dl--redisplay))))

(defun youtube-dl-list-kill-log ()
  "Kill the youtube-dl log buffer."
  (interactive)
  (let ((buffer (youtube-dl--log-buffer)))
    (when buffer
      (kill-buffer buffer))))

(defun youtube-dl-list-yank ()
  "Copy the URL of the video under point to the clipboard."
  (interactive)
  (let* ((n (1- (line-number-at-pos)))
         (item (nth n youtube-dl-items)))
    (when item
      (let ((url (concat "https://youtu.be/" (youtube-dl-item-vid item))))
        (if (fboundp 'gui-set-selection)
            (gui-set-selection nil url)     ; >= Emacs 25
          (with-no-warnings
            (x-set-selection 'PRIMARY url))) ; <= Emacs 24
        (message "Yanked %s" url)))))

(defun youtube-dl-list-kill ()
  "Remove the selected item from the queue."
  (interactive)
  (let* ((n (1- (line-number-at-pos)))
         (item (nth n youtube-dl-items)))
    (when item
      (when (= n (1- (length youtube-dl-items)))
        (forward-line -1))
      (youtube-dl--remove item)
      (youtube-dl--run))))

(defun youtube-dl-list-priority-modify (delta)
  "Change priority of item under point by DELTA."
  (let* ((n (1- (line-number-at-pos)))
         (item (nth n youtube-dl-items)))
    (when item
      (cl-incf (youtube-dl-item-priority item) delta)
      (youtube-dl--run))))

(defun youtube-dl-list-toggle-pause ()
  "Toggle pause on item under point."
  (interactive)
  (let* ((n (1- (line-number-at-pos)))
         (item (nth n youtube-dl-items)))
    (when item
      (let ((paused-p (youtube-dl-item-paused-p item)))
        (setf (youtube-dl-item-paused-p item) (not paused-p))
        (youtube-dl--run)))))

(defun youtube-dl-list-toggle-slow (item)
  "Toggle slow mode on item under point."
  (interactive
   (let* ((n (1- (line-number-at-pos))))
     (list (nth n youtube-dl-items))))
  (when item
    (let ((slow-p (youtube-dl-item-slow-p item)))
      (setf (youtube-dl-item-slow-p item) (not slow-p))
      (if (not (eq item (youtube-dl--current)))
          (youtube-dl--redisplay)
        ;; Offset error count and restart the process.
        (cl-decf (youtube-dl-item-failures item))
        (kill-process youtube-dl-process)))))

(defun youtube-dl-list-toggle-slow-all ()
  "Toggle slow mode on all items."
  (interactive)
  (let* ((count (length  youtube-dl-items))
         (slow-count (cl-count-if #'youtube-dl-item-slow-p youtube-dl-items))
         (target (< slow-count (- count slow-count))))
    (dolist (item youtube-dl-items)
      (unless (eq target (youtube-dl-item-slow-p item))
        (youtube-dl-list-toggle-slow item)))))

(defun youtube-dl-list-priority-up ()
  "Decrease priority of item under point."
  (interactive)
  (youtube-dl-list-priority-modify 1))

(defun youtube-dl-list-priority-down ()
  "Increase priority of item under point."
  (interactive)
  (youtube-dl-list-priority-modify -1))

(defvar youtube-dl-list-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map "a" #'youtube-dl)
      (define-key map "g" #'youtube-dl-list-redisplay)
      (define-key map "l" #'youtube-dl-list-log)
      (define-key map "L" #'youtube-dl-list-kill-log)
      (define-key map "y" #'youtube-dl-list-yank)
      (define-key map "j" #'next-line)
      (define-key map "k" #'previous-line)
      (define-key map "d" #'youtube-dl-list-kill)
      (define-key map "p" #'youtube-dl-list-toggle-pause)
      (define-key map "s" #'youtube-dl-list-toggle-slow)
      (define-key map "S" #'youtube-dl-list-toggle-slow-all)
      (define-key map "]" #'youtube-dl-list-priority-up)
      (define-key map "[" #'youtube-dl-list-priority-down)))
  "Keymap for `youtube-dl-list-mode'")

(define-derived-mode youtube-dl-list-mode special-mode "youtube-dl"
  "Major mode for listing the youtube-dl download queue."
  :group 'youtube-dl
  (use-local-map youtube-dl-list-mode-map)
  (hl-line-mode)
  (setf truncate-lines t
        header-line-format
        (format "%s%-11s %-6.6s %-10.10s %s"
                (propertize " " 'display '((space :align-to 0)))
                "vid" "done" "size" "title")))

(defun youtube-dl--buffer ()
  "Returns the queue listing buffer."
  (with-current-buffer (get-buffer-create " *youtube-dl list*")
    (youtube-dl-list-mode)
    (current-buffer)))

(defun youtube-dl--log-buffer (&optional item)
  "Returns a youtube-dl log buffer for ITEM."
  (let* ((name " *youtube-dl log*")
         (buffer (if item (get-buffer-create name) (get-buffer name))))
    (when (or item (and buffer (get-buffer-window buffer)))
      (with-current-buffer buffer
        (unless (eq major-mode 'special-mode)
          (special-mode))
        (when item
          (setf youtube-dl--log-item item))
        (current-buffer)))))

(defun youtube-dl--fill-listing ()
  "Erase and redraw the queue in the queue listing buffer."
  (with-current-buffer (youtube-dl--buffer)
    (let* ((inhibit-read-only t)
           (active (youtube-dl--current))
           (string-slow (propertize "S" 'face 'youtube-dl-slow))
           (string-paused (propertize "P" 'face 'youtube-dl-pause)))
      (erase-buffer)
      (dolist (item youtube-dl-items)
        (let ((vid (youtube-dl-item-vid item))
              (failures (youtube-dl-item-failures item))
              (priority (youtube-dl-item-priority item))
              (progress (youtube-dl-item-progress item))
              (paused-p (youtube-dl-item-paused-p item))
              (slow-p (youtube-dl-item-slow-p item))
              (total (youtube-dl-item-total item))
              (title (youtube-dl-item-title item))
              (url (youtube-dl-item-url item)))
          (insert
           ;;                            failure
           ;;       vid progress   total   | priority
           ;;        |      |        |     | | slow/pause
           ;;        |      |        |     | | | Title
           ;;        v      v        v     v v v | URL
           (format "%-11s %-6.6s %-10.10s %s%s%s%s %s\n"
                   (if (eq active item)
                       (propertize vid 'face 'youtube-dl-active)
                     vid)
                   (or progress "0.0%")
                   (or total "???")
                   (if (= failures 0)
                       ""
                     (propertize (format "[%d] " failures)
                                 'face 'youtube-dl-failure))
                   (if (= priority 0)
                       ""
                     (propertize (format "%+d " priority)
                                 'face 'youtube-dl-priority))
                   (cond ((and slow-p paused-p)
                          (concat string-slow string-paused " "))
                         (slow-p
                          (concat string-slow " "))
                         (paused-p
                          (concat string-paused " "))
                         (""))
                   (or title "")
                   (or url ""))))))))

;;;###autoload
(defun youtube-dl-list ()
  "Display a list of all videos queued for download."
  (interactive)
  (youtube-dl--fill-listing)
  (display-buffer (youtube-dl--buffer)))

(provide 'youtube-dl)

;;; youtube-dl.el ends here
