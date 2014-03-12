;;
;; Globally useful functions
;;
;; (most with keyboard shortcuts)
;;

;; C-w -- close file (buffer)
(defun kill-current-buffer ()
  "Kills active buffer and activates another user buffer, if available."
  (interactive)
  (kill-buffer (current-buffer))
  (when (string-match "^*" (buffer-name))
	(next-user-buffer)))

(global-set-key (kbd "C-w") 'kill-current-buffer)

;; C-a -- select all
(global-set-key (kbd "C-a") 'mark-whole-buffer)


;; C-x C-' to reload all open files
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer)
                 (user-buffer-p (buffer-name buffer)))
        (progn
          (set-buffer buffer)
          (revert-buffer t t t)))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshing open files"))

(global-set-key (kbd "C-x *") 'revert-all-buffers)


;;
;; Switching between buffers skipping *system-buffers*
;;
;; M-<left>  moves to previous user buffer
;; M-<right> moves to next
;;
;; For switching between system buffers, you can still use
;; C-x <left> and C-x <right>
;;
(defun next-user-buffer ()
  "Switch to the next user buffer in cyclic order."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (user-buffer-p (buffer-name)) (< i 100))
      (setq i (1+ i)) (next-buffer))))

(defun previous-user-buffer ()
  "Switch to the next user buffer in cyclic order."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (user-buffer-p (buffer-name)) (< i 100))
      (setq i (1+ i)) (previous-buffer))))

(defun user-buffer-p (buffer-name)
  "True when buffer name starts with the reserved * character."
  (string-match "^*" (buffer-name)))

(global-set-key (kbd "M-<left>") 'previous-user-buffer)
(global-set-key (kbd "M-<right>") 'next-user-buffer)


;;
;; Ctrl-P now filters selected text with perl command
;;
(defun perl-command-on-region (start end command)
  "Runs Perl command on selected text and replaces text with output.
When no region active, applies command to whole buffer."
  (interactive (let ((string (read-string (concat "Perl command on " (if mark-active "region" "buffer") ": "))))
                 (if mark-active
                     (list (region-beginning) (region-end) string)
                   (list (buffer-end -1) (buffer-end 1) string))))
  (message (concat (int-to-string start) " " (int-to-string end)))
  (shell-command-on-region
      start
	  end
	  (concat "perl -pe " (escape-shell-command-arg command)) t t))

(defun escape-shell-command-arg (arg)
  "Escapes string so, that it's safe to use it as a shell command argument."
  (concat "'" (replace-regexp-in-string "'" "'\"'\"'" arg nil 1) "'"))

(global-set-key (kbd "C-p") 'perl-command-on-region)




(defun mysql-evaluate-region (start end)
  "Runs MySQL code in selected and displays results."
  
  (interactive (progn
				 (unless (mark) (error "The mark is not set now, so there is no region"))
				 (list (region-beginning) (region-end))))
  
  ; get rid of old *mysql* buffer if it exists
  (when (buffer-live-p (get-buffer "*mysql*"))
        (kill-buffer "*mysql*"))
  (shell-command-on-region start end "mysql rent -t " "*mysql*" nil)
  (display-buffer "*mysql*"))



(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
	(fill-paragraph nil)))

(global-set-key (kbd "C-M-q") 'unfill-paragraph)


;; Enable Up- and downcasing, and assign keys for them
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(global-set-key (kbd "C-u") 'upcase-region)
(global-set-key (kbd "C-l") 'downcase-region)


(defun forward-camelcase-word ()
  "Moves forward by one word inside CamelCasedWord."
  (interactive)
  (goto-char (min
              (save-excursion (forward-word) (point))
              (save-excursion (forward-camelcase-word-dumb) (point)))))

(defun backward-camelcase-word ()
  "Moves backward by one word inside CamelCasedWord."
  (interactive)
  (goto-char (max
              (save-excursion (backward-word) (point))
              (save-excursion (backward-camelcase-word-dumb) (point)))))

(defun forward-camelcase-word-dumb ()
  (setq case-fold-search nil)
  (forward-char 1)
  (re-search-forward "[A-Z]" nil t)
  (forward-char -1)
  (setq case-fold-search t))
  
(defun backward-camelcase-word-dumb ()
  (setq case-fold-search nil)
  (forward-char -1)
  (re-search-backward "[A-Z]" nil t)
  (setq case-fold-search t))
  
(global-set-key (kbd "s-<right>") 'forward-camelcase-word)
(global-set-key (kbd "s-<left>") 'backward-camelcase-word)

;; OSX specific shortcuts

(if (eq system-type 'darwin)
  (progn
    (osx-key-mode nil)
    (setq mac-command-modifier 'hyper)
    (global-set-key [(hyper x)] 'kill-region)
    (global-set-key [(hyper c)] 'kill-ring-save)
    (global-set-key [(hyper v)] 'yank)
    (global-set-key [(hyper z)] 'undo)
    (global-set-key (kbd "C-<left>") 'left-word)
    (global-set-key (kbd "C-<right>") 'right-word)
    (define-key osx-key-mode-map (kbd "C-a") 'mark-whole-buffer)
    (define-key osx-key-mode-map (kbd "<home>") 'beginning-of-line)
    (define-key osx-key-mode-map (kbd "<end>") 'end-of-line)))
