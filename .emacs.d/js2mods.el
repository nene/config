;;
;; My js2-mode modifications
;;

(defvar js2mods-indent-tab-width 4)
(setq js2mods-indent-tab-width 4)

(defun js2mods-indent-region (start end)
  "Indents a region in my way line-by-line."
  (save-excursion
    ;; first jump to end of selection and remember the line nr of it
    (goto-char end)
    (let ((last-line-in-region (line-number-at-pos)))
      ;; then go back to beginning and indent all lines
      ;; up to the last line in selected region
      (goto-char start)
      (while (neq (line-number-at-pos) last-line-in-region)
        (js2mods-indent)
        (forward-line)))))


(defun js2mods-indent (&optional list index)
  "My indentation, that never indents more than two spaces."
  (cond ((and (js2mods-previous-line-is-indent-line)
              (js2mods-line-is-unindent-line))
         (indent-line-to (js2mods-previous-line-indent)))

        ((js2mods-previous-line-is-doc-comment-start)
         (indent-line-to (+ (js2mods-previous-line-indent) 1)))

        ((js2mods-previous-line-is-doc-comment-end)
         (indent-line-to (- (js2mods-previous-line-indent) 1)))

        ((js2mods-previous-line-is-indent-line)
         (indent-line-to (+ (js2mods-previous-line-indent) js2mods-indent-tab-width)))

        ((js2mods-line-is-unindent-line)
         (indent-line-to (- (js2mods-previous-line-indent) js2mods-indent-tab-width)))

        (t
         (indent-line-to (js2mods-previous-line-indent))))
  t)

(defun js2mods-string-repeat (str n)
  (let ((retval ""))
    (dotimes (i n)
      (setq retval (concat retval str)))
    retval))

(defun js2mods-untabify (str)
  "Replaces tabs with spaces according to current tab width"
  (replace-regexp-in-string "\t" (js2mods-string-repeat "-" js2mods-indent-tab-width) str))

(defun js2mods-previous-line-indent ()
  "Returns indentation of previous line,
when that line is empty, looks at the line before it etc."
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (if (and (looking-at "^$") (> (point) 1))
        (js2mods-previous-line-indent)
      (looking-at "^\\(\\s-*\\)")
      (length (js2mods-untabify (match-string 1))))))

(defun js2mods-previous-line-is-indent-line ()
  "Determines if previous line ends with (, {, ["
  (if (> (line-number-at-pos) 1)
      (save-excursion
        (forward-line -1)
        (beginning-of-line)
        (looking-at "^.*[[({]\\s-*$"))
    nil))

(defun js2mods-line-is-unindent-line ()
  "Determines if current line begins with ), }, ]"
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\s-*\\(}\\|)\\|]\\)")))

(defun js2mods-previous-line-is-doc-comment-start ()
  "Determines if previous line is /**"
  (if (> (line-number-at-pos) 1)
      (save-excursion
        (forward-line -1)
        (beginning-of-line)
        (looking-at "^\\s-*/\\*\\*\\s-*$"))
    nil))

(defun js2mods-previous-line-is-doc-comment-end ()
  "Determines if previous line is */"
  (if (> (line-number-at-pos) 1)
      (save-excursion
        (forward-line -1)
        (beginning-of-line)
        (looking-at "^\\s-*\\*/\\s-*$"))
    nil))

(defun js2mods-line-is-oneline-comment ()
  "Determines if current line begins with //"
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\s-*//")))

(add-hook 'js2-indent-hook 'js2mods-indent)

(defun js2mods-indent-line-or-region (&optional start end)
  (interactive
   (progn
	 (if mark-active (list (region-beginning) (region-end)) nil)))
  (if start
      (js2mods-indent-region start end)
    (indent-according-to-mode)))

(defun js2mods-insert-line-and-indent ()
  (interactive)
  ;; modified version of js2-enter-key function
  (let ((parse-status (save-excursion
                        (parse-partial-sexp (point-min) (point)))))
    (cond
     ;; check if we're inside a string
     ((nth 3 parse-status)
      (js2-mode-split-string parse-status))
     ;; check if inside a block comment
     ((and (nth 4 parse-status) (not (js2mods-line-is-oneline-comment)))
      (js2-mode-extend-comment))
     (t
      (insert "\n")
      (js2mods-indent)))))


(add-hook 'js2-mode-hook
  (lambda ()
	(define-key js2-mode-map (kbd "TAB") 'js2mods-indent-line-or-region)
	(define-key js2-mode-map (kbd "RET") 'js2mods-insert-line-and-indent)))


;;
;; Opening file with tests
;;

(defun js2mods-test-file-name (filename)
  "Transforms '/js/MyClass.js' to '/test/MyClassTest.js'."
  (replace-regexp-in-string
    "/js/\\(.*\\)\.js"
    "/test/\\1Test.js"
    filename))

(defun js2mods-buffer-classname ()
  "Returns name of the JavaScript class defined in current buffer"
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^\\([a-zA-Z0-9_.]+\\) *= *Ext.extend(")
    (match-string 1)))

(define-skeleton js2mods-test-file-skeleton
  "Inserts javascript test template."
  "Class name: "
  "MRequires('" str "');\n"
  "\n"
  "Ext.onReady(function(){\n"
  "  new " str "({\n"
  "    renderTo: document.body" _ "\n"
  "  });\n"
  "});\n")

(defun js2mods-create-new-test-file (filename classname)
  "Creates new JS test file."
  (find-file filename)
  (js2mods-test-file-skeleton classname))

(defun js2mods-find-test-file ()
  "Opens test file, associated with currently open JS file."
  (interactive)
  (let ((filename (js2mods-test-file-name (buffer-file-name))))
    (if (file-exists-p filename)
      (find-file filename)
	  (when (y-or-n-p (concat "file " filename " not found. Create it? "))
		(js2mods-create-new-test-file filename (js2mods-buffer-classname))))))


(define-skeleton js2mods-file-skeleton
  "Inserts JavaScript file template"
  ""
  "Ext.ns('" (replace-regexp-in-string "\\.[a-zA-Z0-9_]+$" "" (setq class (skeleton-read "Class: "))) "');\n"
  "\n"
  "/**\n"
  " * @class " class "\n"
  " * @extends " (setq extends (skeleton-read "Extends: ")) "\n"
  " *\n"
  " */\n"
  class " = Ext.extend(" extends ", {\n"
  "  initComponent: function() {\n"
  "    " _ "\n"
  "    " class ".superclass.initComponent.call(this);\n"
  "  }\n"
  "});\n")

(defun js2mods-grep-find-exts (extensions)
  (mapconcat (lambda (ext) (concat "-iname '*." ext "' -print0")) extensions " -or "))

(defun js2mods-grep-find (path extensions needle)
  (grep-find
   (concat "find " path " " (js2mods-grep-find-exts extensions) " | xargs -0 grep -nH -e '" needle "'")))

(defun jsgrep (needle)
  (interactive "sFind JS: ")
  (js2mods-grep-find "." '("js") needle))

(defun rbgrep (needle)
  (interactive "sFind Ruby: ")
  (js2mods-grep-find "." '("rb") needle))

(defun cssgrep (needle)
  (interactive "sFind SCSS: ")
  (js2mods-grep-find "." '("scss") needle))

(defun phpgrep (needle)
  (interactive "sFind PHP: ")
  (js2mods-grep-find "." '("php") needle))

(defun sdk-grep (needle)
  (interactive "sFind SDK JS: ")
  (js2mods-grep-find "~/work/SDK/extjs/src ~/work/SDK/platform/src ~/work/SDK/platform/core/src" '("js") needle))

(defun touch-grep (needle)
  (interactive "sFind Touch JS: ")
  (js2mods-grep-find "~/work/SDK/touch/src" '("js") needle))

(defun rrsoft-grep (needle)
  (interactive "sFind JS&PHP&CSS in parim/: ")
  (js2mods-grep-find "~/rrsoft/parim" '("php" "js" "css") needle))


(defun jshint-to-string ()
  (let ((fname (file-relative-name (buffer-file-name))))
    (shell-command-to-string (concat "jshint " fname " | sed 's/: line /:/; s/, col /:/; s/, /: /'"))))

(defun jshint-switch-to-clean-buffer (name)
  "Kills old buffer (if present) and opens a new one."
  (if (get-buffer name)
      (kill-buffer name))
  (switch-to-buffer name))

(defun jshint ()
  (interactive)
  (let ((warnings (jshint-to-string)))
    (split-window-vertically)
    (other-window 1)
    (jshint-switch-to-clean-buffer "*jshint warnings*")
    (insert warnings)
    (grep-mode)))
