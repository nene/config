;;
;; FlyMake
;;

(require 'flymake)

(add-hook 'find-file-hook 'flymake-find-file-hook)

;;
;; Flymake for PHP
;;

(defconst flymake-allowed-php-file-name-masks
  '(("\\.php3$" flymake-php-init)
    ("\\.inc$" flymake-php-init)
    ("\\.php$" flymake-php-init))
  "Filename extensions that switch on flymake-php mode syntax checks")

(defconst flymake-php-err-line-pattern-re
  '("\\(Parse\\|Fatal\\) error: \\(.*\\) in \\(.*\\) on line \\([0-9]+\\)" 3 4 nil 2)
  "Regexp matching PHP error messages")

(defun flymake-php-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "php" (list "-f" local-file "-l"))))

(setq flymake-allowed-file-name-masks
      (append flymake-allowed-file-name-masks flymake-allowed-php-file-name-masks))

(setq flymake-err-line-patterns
      (cons flymake-php-err-line-pattern-re flymake-err-line-patterns))

(add-hook 'php-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c n") 'flymake-goto-next-error)
            (local-set-key (kbd "C-c p") 'flymake-goto-prev-error)
            (local-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line)))


;;
;; FlyMake for JavaScript with JSLint
;;
;; Currently disabled, as we are using js2-mode
;;

;; (defconst flymake-allowed-js-file-name-masks
;;   '(("\\.js$" flymake-js-init))
;;   "Filename extensions that switch on flymake-js mode syntax checks")

;; (defconst flymake-js-err-line-pattern-re
;;   '("Lint in \\(.*\\) line \\([0-9]+\\) character \\([0-9]+\\): \\(.*\\)$" 1 2 3 4)
;;   "Regexp matching JSLint error messages")

;; (defun flymake-js-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                      'flymake-create-temp-inplace))
;;          (local-file (file-relative-name
;;                       temp-file
;;                       (file-name-directory buffer-file-name))))
;;     (list "jslint" (list local-file))))

;; (setq flymake-allowed-file-name-masks
;;       (append flymake-allowed-file-name-masks flymake-allowed-js-file-name-masks))

;; (setq flymake-err-line-patterns
;;       (cons flymake-js-err-line-pattern-re flymake-err-line-patterns))

;; (add-hook 'javascript-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c n") 'flymake-goto-next-error)
;;             (local-set-key (kbd "C-c p") 'flymake-goto-prev-error)
;;             (local-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line)))

