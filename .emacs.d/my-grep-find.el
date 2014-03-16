;;
;; Quick grepping helpers
;;

(defun my-grep-find-exclude (base paths)
  (if paths
      (concat "\\( "
              (mapconcat (lambda (p) (concat "-path " base "/" p)) paths " -o ")
              " \\) -prune -o")
    ""))

(defun my-grep-find-exts (extensions)
  (concat "\\( "
          (mapconcat (lambda (ext) (concat "-iname '*." ext "'")) extensions " -o ")
          " \\)"))

(defun my-grep-find (path extensions needle &optional exclude)
  (grep-find
   (concat "find " path " "
           (my-grep-find-exclude path exclude)
           " "
           (my-grep-find-exts extensions)
           " -type f -print0"
           " | xargs -0 grep -nH -e '" needle "'")))

(defun jsgrep (needle)
  (interactive "sFind JS: ")
  (my-grep-find "." '("js") needle))

(defun rbgrep (needle)
  (interactive "sFind Ruby: ")
  (my-grep-find "." '("rb") needle))

(defun scssgrep (needle)
  (interactive "sFind SCSS: ")
  (my-grep-find "." '("scss") needle))

(defun lessgrep (needle)
  (interactive "sFind LESS: ")
  (my-grep-find "." '("less") needle))

(defun phpgrep (needle)
  (interactive "sFind PHP: ")
  (my-grep-find "." '("php") needle))

(defun htmgrep (needle)
  (interactive "sFind HTM templates: ")
  (my-grep-find "." '("htm") needle))

(defun sdk-grep (needle)
  (interactive "sFind SDK JS: ")
  (my-grep-find "~/work/SDK/extjs/src ~/work/SDK/platform/src ~/work/SDK/platform/core/src" '("js") needle))

(defun touch-grep (needle)
  (interactive "sFind Touch JS: ")
  (my-grep-find "~/work/SDK/touch/src" '("js") needle))

(defun spl-grep (needle)
  (interactive "sFind Sportlyzer sources/: ")
  (my-grep-find "~/work/sport" '("php" "js" "json" "css" "less" "htm" "html") needle '("c_tpl" ".git" "build" "cache" "node_modules")))
