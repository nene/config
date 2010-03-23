
(defun etmk-l10n ()
  "Refreshes ETMK translations file and opens it for editing."
  (interactive)
  (shell-command "cd c:\\nene\\etmk\\locale & php refresh.php")
  (find-file "c:/nene/etmk/locale/et_EE.po")
  (add-hook 'after-save-hook 'etmk-l10n-compile nil t))

(defun etmk-l10n-compile ()
  "Runs compile.php."
  (shell-command "cd c:\\nene\\etmk\\locale & php compile.php"))


