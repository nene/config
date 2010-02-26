(defun bbcode-make-block (start end name &optional value)
  "Creates BBCode block around region specified by `start' and
`end'.  The `name' parameter defines the name of the block.
Optional `value' parameter defines the value.  Example:

  (bbcode-make-block 10 17 \"url\" \"http://example.com\")

This might create the following BBCode block:

  [url=\"http://example.com\"]example[/url]
"
  (let ((region (buffer-substring start end)))
    (delete-region start end)
    ;; when value is defined then append ="value" to type
    (setq value (if value (concat "=\"" value "\"") ""))
    (insert (concat "[" name value "]" region "[/" name "]"))))

(defmacro bbcode-block-function (function-name tag-name)
  `(defun ,function-name (start end)
     (interactive "r")
     (bbcode-make-block start end ,tag-name)))

(bbcode-block-function bbcode-b "b")
(bbcode-block-function bbcode-i "i")
(bbcode-block-function bbcode-u "u")
(bbcode-block-function bbcode-c "c")
(bbcode-block-function bbcode-code "code")
(bbcode-block-function bbcode-ot "ot")
(bbcode-block-function bbcode-user "user")
(bbcode-block-function bbcode-spoiler "spoiler")
(bbcode-block-function bbcode-img "img")

(defun bbcode-url (start end)
  "Places [url=\"http://example.com\"] and [/url] around selection.
User is prompted to enter the URL."
  (interactive "r")
  (bbcode-make-block start end "url" (read-string "URL: ")))

(defun bbcode-syntax (start end)
  "Places [syntax=\"lang\"] and [/syntax] around selection.
User is prompted to select the value for `lang' from predefined
list."
  (interactive "r")
  (let* ((langs '("asp" "bash" "c" "c++" "c#" "css" "html" "java"
                "javascript" "oracle8" "pascal" "perl" "php-brief"
                "php" "python" "qbasic" "smarty" "sql" "vbnet" "vb"
                "xml"))
         (lang (completing-read "Language: " langs nil t nil)))
    (bbcode-make-block start end "syntax" lang)))



