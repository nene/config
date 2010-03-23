;;
;; Display help for ExtJs classes and methods
;;


(defvar ext-classes nil)

(defconst ext-classes-dir "~/ext-2.2/source")

(defun ext-doc ()
  "Completes ExtJs class and method/option/event/property names in minibuffer.

When ExtJs data is not jet loaded, load it from the directory
specified by ext-classes-dir."
  (interactive)
  (when (null ext-classes)
    (setq ext-classes (ext-parse-dir ext-classes-dir)))
  (ext-complete-member (completing-read "ExtJs class: " ext-classes nil t nil)))


(defun ext-complete-member (class-name)
  "Complete option/property/method/event of ExtJs class."
  (let ((class-members (ext-all-class-members (ext-get-class class-name))))
    (ext-display-doc
     (assoc-string (completing-read class-name class-members nil t)
                        class-members))))

(defun ext-all-class-members (class)
  "Returns all members of the class (including inherited ones)."
  (let ((parent-class (ext-class-parent class))
        (local-class-members (ext-class-members class)))
    (if (null parent-class)
        local-class-members
      (append (ext-all-class-members parent-class) local-class-members))))

(defun ext-display-doc (class-member)
  "Display documentation for class Ext class member"
  (message (concat (symbol-name (ext-member-type class-member)) " "
                   (ext-member-name class-member)
                   (ext-member-signature class-member))))
  


(defun ext-class-parent (class)
  (ext-get-class (car (cdr class))))

(defun ext-class-members (class)
  (car (cdr (cdr class))))

(defun ext-get-class (class-name)
  (assoc class-name ext-classes))


(defun ext-member-name (member)
  (replace-regexp-in-string "^." "" (car member)))

(defun ext-member-type (member)
  (car (cdr member)))

(defun ext-member-3 (member)
  (car (cdr (cdr member))))

(defun ext-member-4 (member)
  (car (cdr (cdr (cdr member)))))

(defun ext-member-signature (member)
  (let ((type (ext-member-type member)))
    (cond ((or (eq type 'option) (eq type 'property))
           (concat " : " (ext-member-3 member)))
          ((eq type 'method)
           (concat (ext-sprint-method-params (ext-member-3 member))
                   " : "
                   (ext-member-4 member)))
          ((eq type 'event)
           (concat " : " (ext-sprint-method-params (ext-member-3 member))))
          (t "???"))))

(defun ext-sprint-method-params (params)
  "Pretty-prints method parameters in a format like:
 (TypeForA a, TypeForB b, ...)"
  (concat "("
          (mapconcat (lambda (x) (concat (car (cdr x)) " " (car x))) params ", ")
          ")"))



;;
;; Parsing of ExtJs source files
;;

(defconst ext-dirs
  '("core" "data" "dd" "state" "util" "widgets"
    "widgets/form" "widgets/grid" "widgets/layout"
    "widgets/menu" "widgets/tips" "widgets/tree"))

(defun ext-parse-dir (ext-source-dir)
  (let ((classes nil))
    (dolist (filename (ext-make-file-list ext-source-dir) classes)
      (message (concat "Parsing " filename " ..."))
      (setq result (ext-parse-file filename))
      (when result
        (add-to-list 'classes result)))))

(defun ext-make-file-list (ext-source-dir)
  "Creates list of files to be parsed by ext-parse-file"
  (apply 'append
   (mapcar
    (lambda (dir) (directory-files (concat ext-source-dir "/" dir) t "\\.js$"))
    ext-dirs)))


(defun ext-parse-file (filename)
  "Extracts information from ExtJs source code file.
It returns extracted data as a list:
'(class-name extended-class-name class-members)
When no class declaration is found, it returns nil."
  (save-current-buffer
    (set-buffer (find-file-noselect filename t t))
    (if (ext-parse-class-name)
        (let ((result (list
                       (ext-parse-class-name)
                       (ext-parse-extended-class-name)
                       (append
                        ;(ext-prefix ":" (ext-parse-options))
                        ;(ext-prefix "=" (ext-parse-properties))
                        ;(ext-prefix "." (ext-parse-methods))
                        (ext-prefix "@" (ext-parse-events))))))
          (kill-buffer (current-buffer))
          result)
      (progn
        (kill-buffer (current-buffer))
        nil))))

(defun ext-prefix (prefix members)
  (mapcar (lambda (m) (cons (concat prefix (car m)) (cdr m))) members))

(defun ext-parse-class-name ()
  (goto-char (point-min))
  (if (re-search-forward "@class +\\([A-Za-z0-9.]+\\)" nil t)
      (match-string 1)
    nil))
  
(defun ext-parse-extended-class-name ()
  (goto-char (point-min))
  (if (re-search-forward "@extends +\\([A-Za-z0-9.]+\\)" nil t)
      (match-string 1)
    nil))

(defun ext-parse-options ()
  (let ((options nil))
    (goto-char (point-min))
    (while (re-search-forward "@cfg +{\\([A-Za-z0-9./]+\\)} +\\([A-Za-z0-9]+\\)" nil t)
      (add-to-list 'options (list (match-string 2) 'option (match-string 1))))
    options))

(defun ext-parse-events ()
  (let ((events nil))
    (goto-char (point-min))
    (while (re-search-forward "@event +\\([A-Za-z0-9]+\\)" nil t)
      (add-to-list 'events (list (match-string 1) 'event (ext-parse-event-params))))
    events))

(defun ext-parse-event-params ()
  "Returns list of @param names from event documentation block."
  (forward-line)
  (let ((params nil))
    (while (ext-doc-comment-p)
      (when (looking-at ".*@param +{\\([A-Za-z0-9./]+\\)} +\\([A-Za-z0-9]+\\)")
        (add-to-list 'params (list (match-string 2) (match-string 1))))
      (forward-line))
    (reverse params)))

(defun ext-parse-methods ()
  (let ((methods nil))
    (goto-char (point-min))
    ;; find each method
    (while (re-search-forward "^ *\\([A-Za-z0-9]+\\) *: *function *(" nil t)
      (let ((method-name (match-string 1))
            (method-params nil)
            (return-type "void"))
        
        ;; Only add the method, when previous line contains doc-comment
        (when (ext-preceded-by-doc-comment-p)
          ;; from inside doc-comment search for params and return value
          (save-excursion
            (ext-goto-prev-doc-comment)
            (while (ext-doc-comment-p)
              (when (looking-at ".*@return +{\\([A-Za-z0-9./]+\\)}")
                (setq return-type (match-string 1)))
              (when (looking-at ".*@param +{\\([A-Za-z0-9./]+\\)} +\\([A-Za-z0-9]+\\)")
                (add-to-list 'method-params (list (match-string 2) (match-string 1))))
              (forward-line)))
          (add-to-list 'methods (list method-name
                                      'method
                                      (reverse method-params)
                                      return-type)))))
    methods))

(defun ext-parse-properties ()
  (let ((properties nil))
    (goto-char (point-min))
    (while (re-search-forward "@type +\\([A-Za-z0-9./]+\\)" nil t)
      (let ((prop-type (match-string 1))
            (prop-name nil))
        
        ;; is the prop-name on next line with @property ?
        (forward-line 1)
        (when (looking-at ".*@property +\\([A-Za-z0-9]+\\)")
          (setq prop-name (match-string 1)))

        ;; skip empty property definition
        (when (looking-at ".*@property *$")
          (forward-line))
          
        ;; or is it on even next line in code like "foo: bar," ?
        (forward-line 1)
        (when (looking-at " *\\([A-Za-z0-9]+\\) *: *.* *,? *$")
          (setq prop-name (match-string 1)))
        
        (add-to-list 'properties (list prop-name 'property prop-type))))
    properties))


;;
;; Functions for dealing with doc-comments
;;

(defun ext-goto-prev-doc-comment ()
  "Takes point to the beginning of previous doc-comment."
  (forward-line -1)
  (while (not (ext-beginning-of-doc-comment-p))
    (forward-line -1)))

(defun ext-preceded-by-doc-comment-p ()
  "True when previous line contains doc-comment."
  (save-excursion
    (forward-line -1)
    (ext-end-of-doc-comment-p)))

(defun ext-doc-comment-p ()
  "True when point is on line that is at the beginning, end or
inside of doc comment."
  (or (ext-inside-doc-comment-p)
      (ext-beginning-of-doc-comment-p)
      (ext-end-of-doc-comment-p)))

(defun ext-inside-doc-comment-p ()
  "True when point is on line that is inside doc-comment block."
  (looking-at "[[:space:]]*\\*"))

(defun ext-beginning-of-doc-comment-p ()
  "True when point is on line at the beginning of doc comment."
  (looking-at "[[:space:]]*/\\*\\*"))

(defun ext-end-of-doc-comment-p ()
  "True when point is on line at the end of doc comment."
  (looking-at "[[:space:]]*\\*/"))
 


