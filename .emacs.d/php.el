;;;
;;; PHP mode customization
;;;


;;
;; PHP function name completions
;;
(add-hook
 'php-mode-user-hook
 (lambda ()
   (setq php-completion-file "~/.emacs.d/lisp/php-completions.txt")
   (define-key php-mode-map (kbd "C-+") 'php-complete-function)))



;;
;; Shortuct for typing "array()"
;;
(defun php-insert-array (&optional count)
    "Inserts PHP array() and places cursor inside braces."
    (interactive "p")
	(unless count (setq count 1))
    (dotimes (i count)
      (insert "array()")
      (backward-char 1)))

(add-hook 'php-mode-user-hook
  '(lambda ()
     (define-key php-mode-map (kbd "C-c a") 'php-insert-array)))


;;
;; Loading PHP file with unit tests
;;
(defun phpunit-test-file-name (php-filename)
  "Transforms '/rent/app/MyClass.php' to '/rent/test/unit/MyClassTest.php'."
  (replace-regexp-in-string
    "/rent/app/\\(.*\\)\.php"
    "/rent/test/unit/\\1Test.php"
    php-filename))

(defun phpunit-create-new-unit-test-file (filename)
  "Creates new PHP unit test file."
  (find-file filename)
  (insert-file-contents "~/.emacs.d/lisp/phpunit-template.php")
  (goto-char (point-min))
  ; fill in class name
  (while (re-search-forward "%test-name%" nil t)
	(replace-match (phpunit-test-name filename)))
  ; position cursor inside setUp()
  (re-search-forward "setUp() {" nil t)
  ; ignore all those automatic modifications
  (set-buffer-modified-p nil))

(defun phpunit-find-test-file ()
  "Opens unit-test file, associated with currently open PHP file."
  (interactive)
  (let ((filename (phpunit-test-file-name (buffer-file-name))))
    (if (file-exists-p filename)
      (find-file filename)
	  (when (y-or-n-p (concat "file " filename " not found. Create it? "))
		(phpunit-create-new-unit-test-file filename)))))



(defun phpunit-test-name (filename)
  "Transforms phpunit test filename into phpunit test name."
  (replace-regexp-in-string
    "/"
    "_"
    (replace-regexp-in-string
      "^.*/rent/test/[a-z]+/\\(.*\\)Test\.php$"
      "\\1Test"
      filename)))

(defun phpunit-test-type (filename)
  "Transforms phpunit test filename into test type: unit, integration or functional."
  (replace-regexp-in-string "^.*/rent/test/\\([a-z]+\\)/.*$" "\\1" filename))


(defun phpunit-process-command (command)
  "Runs command and interprets the results as phpunit output."
  ; get rid of old *phpunit* buffer if it exists
  (when (buffer-live-p (get-buffer "*phpunit*"))
        (kill-buffer "*phpunit*"))
  ; run phpunit and collect output to buffer
  (call-process-shell-command command nil "*phpunit*")
  ; search for "OK " in phpunit output
  (save-current-buffer
    (set-buffer "*phpunit*")
      (goto-char (point-min))
      (if (re-search-forward "^\\(OK .*\\)$" nil t)
         ; when test went okay, display just status message
         (message (match-string 1))
         ; when test failed, display full output of phpunit
         (display-buffer "*phpunit*"))))
  
;;
;; Running unit tests associated with given PHP class file
;; or when the unit tests file itself is open -- running tests
;; in that file.
;;
(defun phpunit ()
  "Runs unit tests associated with active file with PHPUnit.
   When the test name is selected, then runs only the selected test.
   Otherwise runs all tests in suite."
  (interactive)
  
  (let (test-file-name test-name test-type options)
	
	;; determine unit test filename
	(setq test-file-name
		  (if (string-match "Test\.php$" (buffer-file-name))
			  (buffer-file-name)
			  (phpunit-test-file-name (buffer-file-name))))

	;; determine name and type of the test
	(setq test-name (phpunit-test-name test-file-name))
	(setq test-type (phpunit-test-type test-file-name))

	;; when we are inside test method, add a filter option to command-line
	(let ((test-function-name (phpunit-current-test-function-name)))
	  (cond
	   (test-function-name
		(setq options (concat "--filter '/" test-function-name "/'"))
		(message (concat "Running testcase: " test-function-name)))
	   (t
		(setq options "")
		(message (concat "Running testsuite: " test-name)))))
	
	(if test-file-name
		(phpunit-process-command
		 (concat "cd ~/rent/test/" test-type "/; php ~/bin/phpunit " options " " test-name))
	  ;; when no unit test was available
	  (message "No unit tests available"))))


(defun php-current-function-name ()
  "Returns the name of the PHP function where the point is currently in.
When not inside a function, returns nil."
  (save-excursion
	(when (and
		   (re-search-backward "^\t[^\t]" nil t)
		   (looking-at "\t\\(public +\\|static +\\)*function"))
	  (re-search-forward "function +[a-zA-Z0-9]")
	  (word-at-point))))
  
(defun phpunit-current-test-function-name ()
  "Returns the name of the PHPUnit test method the point is currently in.
Or nil otherwise."
  (let ((function-name (php-current-function-name)))
	(when (and function-name (string-match "^test" function-name))
	  function-name)))

  


;;;
;;; PHP find
;;;
(defun smarty-find-template-file ()
  "Opens Smarty template file, associated with currently open PHP file."
  (interactive)
  (let ((filename (smarty-template-file-name (buffer-file-name))))
    (if (file-exists-p filename)
      (find-file filename)
	  (when (y-or-n-p (concat "file " filename " not found. Create it? "))
		(find-file filename)))))

(defun smarty-template-file-name (php-filename)
  "Transforms '/rent/app/HtmlComponent/EkspressRent/MyClass.php'
           to '/rent/templates/templates/HtmlComponent/EkspressRent/MyClass.tpl'."
  (replace-regexp-in-string
    "/rent/app/HtmlComponent/EkspressRent/\\(.*\\)\.php"
    "/rent/templates/templates/HtmlComponent/EkspressRent/\\1.tpl"
    php-filename))



;;
;; Running all unit tests
;;
(defun phpunit-all-unit-tests ()
  "Runs all unit tests."
  (interactive)
  (message "Running all unit tests...")
  (phpunit-process-command "cd ~/rent/test/unit/; php AllTests.php"))



(defun php-load-test-data ()
  "Updates database with test-data and resizes test-images."
  (interactive)
  (shell-command
   (concat
	"cd ~/rent/db/test-data/; "
	"php load-test-data.php; "
	"cd ~/rent/web/ekspressrent.ee/car-img/; "
	"php load-test-data.php; ")))


(defun php-evaluate-region (start end)
  "Evaluates PHP code in selected region."
  
  (interactive (progn
				 (unless (mark) (error "The mark is not set now, so there is no region"))
				 (list (region-beginning) (region-end))))
  
  ; get rid of old *php* buffer if it exists
  (when (buffer-live-p (get-buffer "*php*"))
        (kill-buffer "*php*"))
  
  (call-process-shell-command
     (concat "php -r " (escape-shell-command-arg (buffer-substring start end)))
	 nil
	 "*php*")
  
  (display-buffer "*php*"))




(define-skeleton php-if
  "insert PHP if statement."
  nil
  \n >
  > "if ( " - " ) {" \n
  > _ \n
  > "}" >
  )

(define-skeleton php-array
  "insert PHP array()"
  nil
  \n >
  > "array(" \n
  > _ \n
  > ")" >
  )


;;
;; Fix array indentation of PHP mode
;;

(defun ywb-php-lineup-arglist-intro (langelem)
  (save-excursion
    (goto-char (cdr langelem))
    (vector (+ (current-column) c-basic-offset))))

(defun ywb-php-lineup-arglist-close (langelem)
  (save-excursion
    (goto-char (cdr langelem))
    (vector (current-column))))

(add-hook 'php-mode-hook (lambda ()
    (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
    (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close)))