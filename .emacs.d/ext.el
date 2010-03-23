;;
;; Display help for ExtJs classes and methods
;;


(defvar ext-classes nil)

(defconst ext-classes-dir "~/ext-2.2/source")

(defconst ext-dirs
  '("core" "data" "dd" "state" "util" "widgets"
    "widgets/form" "widgets/grid" "widgets/layout"
    "widgets/menu" "widgets/tips" "widgets/tree"))


(defun ext-parse-file (filename)
  "Extracts information from ExtJs source code file."
  (save-current-buffer
    (set-buffer (find-file-noselect filename t t))
    (let ((result (ext-parse-buffer)))
      (kill-buffer (current-buffer))
      result)))

(defun ext-parse-buffer ()
  "Parses buffer and returns list of parsed doc-comments."
  (goto-char (point-min))
  (let ((docs nil))
    (while (not (eq (point) (point-max)))
      (when (ext-beginning-of-doc-comment-p)
        (add-to-list 'docs (ext-parse-doc-comment)))
      (forward-line))
    (reverse docs)))

(defun ext-parse-doc-comment ()
  (let ((doc nil))
    (while (not (ext-doc-comment-end-p))
      (ext-at-rule-cond
       (" ?/\\*\\*" nil)
       (" * @class <type>"
        (add-to-list 'doc `(class . ,type)))
;;        (" * @extends <type>"
;;         (add-to-list 'doc `(extends . ,type)))
;;        (" * @constructor"
;;         (add-to-list 'doc `(constructor . t)))
;;        (" * @cfg .*@hide" nil)  ; ignore hidden config options
;;        (" * @cfg {<type>} <ident> <optional> <comment>"
;;         (add-to-list 'doc `(cfg . ((type . ,type)
;;                                    (name . ,ident)
;;                                    (optional . ,optional)
;;                                    (comment . ,comment)))))
;;        (" * @property {<type>} <ident> <comment>"
;;         (add-to-list 'doc `(property . ((type . ,type)
;;                                         (name . ,ident)
;;                                         (comment . ,comment)))))
;;        (" * @param {<type>} <ident> <optional> <comment>"
;;         (add-to-list 'doc `(param . ((type . ,type)
;;                                      (name ident)
;;                                      (optional . ,optional)
;;                                      (comment . ,comment)))))
;;        (" * @renturns? {<type>} <comment>"
;;         (add-to-list 'doc `(return . ((type . ,type)
;;                                       (comment . ,comment)))))
;;        (" * @type <type>"
;;         (add-to-list 'doc `(type . ,type)))
;;        (" * @event <ident>"
;;         (add-to-list 'doc `(event . ,ident)))
;;        (" * <comment>"
;;         (ext-append-to-prev-comment-if-possible))
;;        ("<comment>"
;;         (ext-append-to-prev-comment-if-possible))
       )
      (forward-line))
    ;; check if line after doc-comment contains function
    (save-excursion
      (forward-line)
      (ext-at-rule-cond (" <ident> ?: ?function.*"
                        (add-to-list 'doc `(method . ,ident)))))
    (reverse doc)))


(defmacro ext-append-to-prev-comment-if-possible ()
  "When previous entry in `doc' is comment, then append this
comment to it.  When previous entry is not comment, then check if
it contains key 'comment in its value alist - if it does, then
append the current comment to it.  Otherwise add new comment to
`doc'."
  `(cond ((eq 'comment (caar doc))
          (setf (cdar doc)
                (concat (cdar doc) "\n" comment)))
         ((and (listp (cdar doc))
               (assoc 'comment (cdar doc)))
          (setf (cdar doc)
                (cons (cons 'comment (concat (cdr (assoc 'comment (cdar doc))) "\n" comment))
                      (assq-delete-all 'comment (copy-alist (cdar doc))))))
         (t (add-to-list 'doc (cons 'comment comment)))))
  



(defmacro ext-at-rule-cond (&rest at-rule-body-pairs)
  "Conditional execution based on matching of @rules.
@rule is a string, with following special rules:

<ident>    = Ext identifier (e.g. 'myVariable_1')
<type>     = Ext class name (e.g. 'Ext.form.Field')
<comment>  = free-form text with whitespace stripped at the beginning and end
<optional> = matches optional
' * '      = '*' at the beginning of doc-comment line
' ?'       = optional whitespace
' '        = mandatory whitespace

Each @rule it followed by rule body, that can contain any
number of expressions, where you can use the variables `ident',
`type', `comment' and `optional'.

For example when we are on the following line:

    * @cfg {Array} parent  Parent class name

And we have the following @rules:

    (ext-at-rule-cond
       (\"@class <type>\"
        (list \"@class\" type))
       (\"@cfg {<type>} <ident> <comment>\"
        (list \"@cfg\" type ident comment)))


Then the second @rule will match and the following list will be
returned:

    (\"@cfg\" \"Array\" \"parent\" \"Parent class name\")"
  `(let ((tag-alist nil))
     (cond ,@(mapcar (lambda (rule)
                       `((setq tag-alist (ext-looking-at-rule ,(car rule)))
                         (let ((type (cdr (assoc 'type tag-alist)))
                               (ident (cdr (assoc 'ident tag-alist)))
                               (comment (cdr (assoc 'comment tag-alist)))
                               (optional (cdr (assoc 'optional tag-alist)))
                               (everything (cdr (assoc 'everything tag-alist))))
                           ,@(cdr rule))))
                       at-rule-body-pairs))))


(defun ext-looking-at-rule (at-rule)
  "Tests if we are looking at `at-rule'.  If we do, then return
alist of symbol-value pairs."
  (let ((symbols (ext-at-rule-symbols at-rule))
        (result nil)
        (cnt 0))
    (when (looking-at (ext-at-rule-regexp at-rule))
      (setq result
            (mapcar (lambda (sym)
                      (setq cnt (1+ cnt))
                      (if (and (eq sym 'optional) (match-string cnt))
                          (cons 'optional t)
                        (cons sym (match-string cnt))))
                    symbols))
      (add-to-list 'result (cons 'everything (match-string 0))))
    result))

        
(defun ext-at-rule-symbols (at-rule)
  "Returns list of symbols in the given `at-rule'.
For example:

    (ext-at-rule-symbols \"@foo <ident> {<type>} <comment>\")

Will return the following list of symbols:

    (ident type comment)"
  ;; find the position of each <tag> by creating alist of tag-position
  ;; pairs
  (let ((tags (list (cons 'type (string-match "<type>" at-rule))
                    (cons 'ident (string-match "<ident>" at-rule))
                    (cons 'comment (string-match "<comment>" at-rule))
                    (cons 'optional (string-match "<optional>" at-rule)))))
    ;; filter out tags, that were not found (nil positions)
    (setq tags (remove-if (lambda (x) (null (cdr x))) tags))
    ;; sort tags by position
    (setq tags (sort tags (lambda (x y) (< (cdr x) (cdr y)))))
    ;; forget about positions and just return a list of tags
    (mapcar 'car tags)))

  
(defun ext-at-rule-regexp (at-rule)
  "Converts `at-rule' to regular expression."
  (let ((regexp at-rule))
    (setq regexp (replace-regexp-in-string "<type>" "\\\\([A-Za-z0-9_.]+\\\\)" regexp))
    (setq regexp (replace-regexp-in-string "<ident>" "\\\\([A-Za-z0-9_.]+\\\\)" regexp))
    (setq regexp (replace-regexp-in-string "<comment>" "\\\\(.*?\\\\)" regexp))
    (setq regexp (replace-regexp-in-string "<optional> *" "\\\\((Optional) *\\\\)?" regexp))
    (setq regexp (replace-regexp-in-string "^ \\* " "[[:space:]]*\\\\*[[:space:]]*" regexp))
    (setq regexp (replace-regexp-in-string " \\?" "[[:space:]]*" regexp))
    (setq regexp (replace-regexp-in-string " " "[[:space:]]+" regexp))
    (setq regexp (concat regexp "[[:space:]]*$"))
    regexp))

(ext-at-rule-regexp " * @class <type>")

(defun ext-doc-comment-begin-p ()
  "True when point is on line at the beginning of doc comment."
  (looking-at "[[:space:]]*/\\*\\*"))


(defun ext-doc-comment-end-p ()
  "True when point is on line at the end of doc comment."
  (looking-at "[[:space:]]*\\*/"))
 














(ext-parse-file "~/ext-2.2/source/data/Connection.js")

