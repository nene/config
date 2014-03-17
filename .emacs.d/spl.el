(require 'thingatpt)

(defun spl-goto-class ()
  "Opens the Sportlyzer PHP class at point."
  (interactive)
  (when (thing-at-point 'symbol)
    (let ((filename (spl-php-class-to-filename (thing-at-point 'symbol))))
      (if (file-exists-p filename)
          (find-file filename)
        (message (concat "File not found: " filename))))))


(defun spl-php-class-to-filename (classname)
  "Transforms PHP class name into relative file name."
  (concat "~/work/sport/AddonClasses/" (replace-regexp-in-string "_" "/" classname) ".php"))


(defun spl-find-uses ()
  "Greps for uses of function at point."
  (interactive)
  (when (thing-at-point 'symbol)
    (spl-grep (concat "[:-][:>]" (thing-at-point 'symbol) "(") t)))
