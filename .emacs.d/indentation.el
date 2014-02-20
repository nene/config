;;
;; Set-up indentation in my way
;;

(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

(global-set-key (kbd "RET") 'newline-and-indent)

(add-hook 'c-mode-hook
  (lambda ()
	(c-set-style "java")
	(c-set-offset 'arglist-intro '+)
	(c-set-offset 'arglist-close 0)))

(defun indent-line-or-region (&optional start end)
  (interactive
   (progn
	 (if mark-active (list (region-beginning) (region-end)) nil)))
  (if start
      (indent-region start end)
    (indent-according-to-mode)))


;; Helpers to work with strings

(defun string/starts-with (s begins)
  "returns non-nil if string S starts with BEGINS.  Else nil."
  (cond ((>= (length s) (length begins))
         (string-equal (substring s 0 (length begins)) begins))
        (t nil)))

(defun string/ends-with (s ending)
  "return non-nil if string S ends with ENDING."
  (cond ((>= (length s) (length ending))
         (let ((elength (length ending)))
           (string= (substring s (- 0 elength)) ending)))
        (t nil)))


(defun file-in-directory (path)
  "True when current file is inside the specified directory."
  (string/starts-with (or (buffer-file-name) default-directory) (file-truename path)))


(add-hook 'ruby-mode-hook
  (lambda ()
	(define-key ruby-mode-map (kbd "RET") 'newline-and-indent)
	(define-key ruby-mode-map (kbd "TAB") 'indent-line-or-region)))

;; Enable TABs only in Sportlyzer source
(add-hook 'js2-mode-hook
  '(lambda ()
     (if (file-in-directory "~/work/sport")
         (set-variable 'indent-tabs-mode t))))

(add-hook 'php-mode-hook
  (lambda ()
    (setq indent-tabs-mode t)
    (setq c-basic-offset 4)
	(define-key php-mode-map (kbd "TAB") 'indent-line-or-region)))

(add-hook 'less-css-mode-hook
  '(lambda ()
     (if (file-in-directory "~/work/sport")
         (set-variable 'indent-tabs-mode t))))
