;;;
;;; Generic customizations
;;;

;; Allow other programs to interact with emacs through emacsclient
(server-start)

;; Add my emacs extras dir to load path
(add-to-list 'load-path "~/.emacs.d")

;; Load settings local to this machine (window size, passwords, ...)
(load "local-settings")

;; turn off fancy toolbar
(tool-bar-mode 0)

;; Enable auto-completion
(require 'completion)
(global-set-key (kbd "M-RET") 'complete)

;; Change the way files with same name are displayed
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

;; Maintain current screen position when scrolling using Page-Up/Page-Down.
(setq scroll-preserve-screen-position 1)

;; Make Emacs UTF-8 compatible for both display and editing:
(prefer-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8)

;; Make Emacs interact with X clipboard
(setq x-select-enable-clipboard t)

;; save a list of open files in ~/.emacs.desktop
;; save the desktop file automatically if it already exists
(setq desktop-save t)
(desktop-save-mode 1)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 1000)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))

;;
;; auto-generated by Custom...
;;
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list (quote ("c:/Documents and Settings/Kersti/Desktop/")))
 '(auto-save-default nil)
 '(cua-mode t nil (cua-base))
 '(javascript-indent-level 2)
 '(js2-allow-keywords-as-property-names nil)
 '(js2-auto-indent-flag nil)
 '(js2-bounce-indent-p t)
 '(js2-cleanup-whitespace nil)
 '(js2-enter-indents-newline nil)
 '(js2-highlight-level 2)
 '(js2-include-gears-externs nil)
 '(js2-include-rhino-externs nil)
 '(js2-indent-on-enter-key nil)
 '(js2-mirror-mode nil)
 '(js2-mode-escape-quotes nil)
 '(js2-rebind-eol-bol-keys nil)
 '(make-backup-files nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (3 ((shift) . 1) ((control)))))
 '(save-abbrevs (quote silently))
 '(transient-mark-mode t)
 '(twitter-password my-twitter-password)
 '(twitter-username my-twitter-username)
 '(vc-svn-global-switches nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#ffffff" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(font-lock-comment-face ((((class color) (min-colors 88) (background light)) (:foreground "#888"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "FireBrick")))))


;;
;; Includes
;;

;; My own mode-neutral customizations
(load "global-keys")
(load "indentation")
;; (load "flymake-mods")

;; My PHP mode customizations
(load "~/.emacs.d/php")

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))

;; JavaScript mode from Steve Yegge
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; My JS2 mode customizations
(load "js2mods")

(require 'anything-config)
(setq anything-sources
      '(anything-c-source-buffers
        anything-c-source-file-name-history
        anything-c-source-files-in-current-dir
        anything-c-source-emacs-commands
        ))
(global-set-key (kbd "M-a") 'anything)
(add-hook 'c-mode-hook
  '(lambda ()
     (define-key c-mode-map (kbd "M-a") 'anything)))
   
(autoload 'twitter-get-friends-timeline "twitter" nil t)
(autoload 'twitter-status-edit "twitter" nil t)
(global-set-key "\C-xt" 'twitter-get-friends-timeline)
(add-hook 'twitter-status-edit-mode-hook 'longlines-mode)
