(defun emcfmgr-insert-list-files (files-list)
  "Insert files names into current buffer from <files-list> list"
  (if (cdr files-list)
      (progn
        (emcfmgr-insert-hilighted-file-element
         (car files-list))
        (emcfmgr-insert-list-files (cdr files-list)))
    (emcfmgr-insert-hilighted-file-element (car files-list))))

(defun emcfmgr-insert-hilighted-file-element (file-name)
  "add file element to buffer with attributes"
  (let ((print-exp
         (lambda (file-name)
           "insert text with attributes"
           (emsfmgr-insert-element
            (emcfmgr-append-attributes file-name) file-name "\n"))))
    (if (= (or
            (string-match "\\." file-name) ;;0 == t, nil != t
            1)
           0) ;; hidden file or not hidden
        (if (or hidden-p
                (string= file-name ".")
                (string= file-name ".."))
            (funcall print-exp file-name))
      (funcall print-exp file-name))))

(defun emsfmgr-insert-element (attributes file-name line-separator)
  "isert line with file name, attibutes and line separator with length trancation"
  (let ((str (concat attributes " | ")))
    (insert (concat str
                    (substring file-name
                               (if (> (length file-name)
                                      (- right-margin (length str)))
                                   (+ left-margin margin-shift)
                                 left-margin)
                               (min (- (+ right-margin margin-shift) (length str))
                                    (length file-name)))
                    line-separator))))


(defun emcfmgr-append-attributes (text)
  "add file attributes to the begining of line"
  (concat
   ;; access rights
   (car (nthcdr 8 (file-attributes text 'integer))) " "
   (user-login-name (car (cddr (file-attributes text 'integer)))) " "
   (current-time-string (car (nthcdr 5 (file-attributes text 'integer))))))

;; (car (nthcdr 8 (file-attributes ".git" 'integer)))

(defun emcfmgr-hilight-file-exists (begin end)
  "hilight text in line with background colour according to file existance"
  (if (file-exists-p (expand-file-name (buffer-substring begin end)))
      (add-face-text-property begin end '(:foreground "blue"))
    (add-face-text-property begin end '(:foreground "red"))))


(defun emcfmgr-hilight-cur-file-element-exists ()
  "Check that file (text in current line) exists and hilight it accordingly"
  (emcfmgr-hilight-file-exists
   (line-beginning-position)
   (line-end-position)))

(defun emcfmgr-down-arrow-handler ()
  "get cursor down"
  (interactive)
  (let ((position-backup (point)))
    (forward-line 1)
    (if (not (search-forward " | " (line-end-position) t nil))
        (goto-char position-backup))
    (message "move cursor down")))

(defun emcfmgr-up-arrow-handler ()
  "get cursor down"
  (interactive)
  (progn
    (forward-line -1)
    (search-forward " | " (line-end-position) t nil)
    (message "move cursor up")))

(defun emcfmgr-enter-key-handler ()
  "press enter"
  (interactive)
  (progn
    (cd (concat default-directory (emcfmgr-get-cur-file-name)))
    ;;(message "press enter")))
    (emcfmgr-load-content)
    (message (concat default-directory (emcfmgr-get-cur-file-name)))))

(defun emcfmgr-get-cur-file-name ()
  "return file name from selected line"
  (progn
    (beginning-of-line)
    (if (search-forward " | " (line-end-position) t nil)
        (buffer-substring (point) (line-end-position))
      nil)))

(defun emcfmgr-h-key-handler ()
  "handle h key press. Toggle hide/show hidden files"
  (interactive)
  (progn
    (if hidden-p
        (setq hidden-p nil)
      (setq hidden-p t))
    (emcfmgr-load-content)
    (message
     (if hidden-p
         "show hidden"
       "hide hidden"))))

(defun emcfmgr-left-arrow-handler ()
  "handle \"left arrow\" key press"
  (interactive)
  (if (> margin-shift 0)
      (let ((point-backup (point)))
        (setq margin-shift (- margin-shift 1))
        (message "left")
        (emcfmgr-load-content)
        (goto-char point-backup)
        (beginning-of-line)
        (search-forward " | " (line-end-position) t nil))))

(defun emcfmgr-right-arrow-handler ()
  "handle \"right arrow\" key press"
  (interactive)
  (let ((point-backup (point)))
    (setq margin-shift (+ margin-shift 1))
    (message "right")
    (emcfmgr-load-content)
    (goto-char point-backup)
    (beginning-of-line)
    (search-forward " | " (line-end-position) t nil)))

(defun emcfmgr-activate-key-handlers ()
  "activate key handlers for emcfmgr buffer"
  (progn
    (local-set-key (kbd "<down>") 'emcfmgr-down-arrow-handler)
    (local-set-key (kbd "<up>") 'emcfmgr-up-arrow-handler)
    (local-set-key (kbd "<left>") 'emcfmgr-left-arrow-handler)
    (local-set-key (kbd "<right>") 'emcfmgr-right-arrow-handler)
    (local-set-key (kbd "<return>") 'emcfmgr-enter-key-handler)
    (local-set-key (kbd "RET") 'emcfmgr-enter-key-handler)
    (local-set-key (kbd "h") 'emcfmgr-h-key-handler)
    )
  )

(defun emcfmgr-make-instance ()
  "Create new buffer \"emsfmgr...\" and insert files name into this buffer"
  (let ((buffer-back (current-buffer))
        (number (emcfmgr-count-emc-instances))
        (make-emc-buffer
         (lambda (name)
           (progn
             (switch-to-buffer (get-buffer-create (concat "emcfmgr-" name "-" (number-to-string number))))
             (emcfmgr-make-variables)
             (emcfmgr-load-content)
             (emcfmgr-activate-key-handlers)
             (switch-to-buffer buffer-back)))))
    (mapc make-emc-buffer '("left" "right"))))

(defun emcfmgr-load-content ()
  (progn
    (erase-buffer)
    (emcfmgr-insert-list-files (directory-files default-directory))
    (goto-char 0)
    (search-forward " | " (line-end-position) t nil)
    (emcfmgr-update-variables)))

(defun emcfmgr-make-variables ()
  (progn
    (make-local-variable 'hidden-p)
    (setq hidden-p t)
    (make-local-variable 'left-margin)
    (make-local-variable 'right-margin)
    (make-local-variable 'margin-shift)
    (setq margin-shift 0)
    (emcfmgr-update-variables)
    ))

(defun emcfmgr-update-variables ()
  (progn
    (setq right-margin (- (window-total-width) 1))
    (setq left-margin 0)))


(defun emcfmgr-count-emc-instances ()
  "get number of working mecfmgr instances"
  (let ((buffers (buffer-list)))
    (max (emcfmgr-count-left-panels buffers)
         (emcfmgr-count-right-panels buffers))))

(defun emcfmgr-count-left-panels (buffers)
  (let ((check (lambda (buffer)
                 (if (null (string-match "emcfmgr-left-[0-9]+" (buffer-name buffer)))
                     0
                   1))))
    (if (null (cdr buffers))
        (funcall check (car buffers))
      (+ (funcall check (car buffers)) (emcfmgr-count-left-panels (cdr buffers))))))

(defun emcfmgr-count-right-panels (buffers)
  (let ((check (lambda (buffer)
                 (if (null (string-match "emcfmgr-right-[0-9]+" (buffer-name buffer)))
                     0
                   1))))
    (if (null (cdr buffers))
        (funcall check (car buffers))
      (+ (funcall check (car buffers)) (emcfmgr-count-right-panels (cdr buffers))))))


(emcfmgr-count-emc-instances)




(emcfmgr-make-instance)
