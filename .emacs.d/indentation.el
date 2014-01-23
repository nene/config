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


(add-hook 'ruby-mode-hook
  (lambda ()
	(define-key ruby-mode-map (kbd "RET") 'newline-and-indent)
	(define-key ruby-mode-map (kbd "TAB") 'indent-line-or-region)))

(add-hook 'js2-mode-hook
  '(lambda ()
    (set-variable 'indent-tabs-mode nil)))

(add-hook 'php-mode-hook
  (lambda ()
    (setq indent-tabs-mode nil)
    (setq c-basic-offset 4)
	(define-key php-mode-map (kbd "TAB") 'indent-line-or-region)))

