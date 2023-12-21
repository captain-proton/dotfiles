;; this should point to the directory where config.org is located
(setq proton/config-directory (getenv "VANILLAMACSDIR"))
(when (string-empty-p proton/config-directory)
  (setq proton/config-directory user-emacs-directory))

(org-babel-load-file
 (expand-file-name
  "config.org"
  proton/config-directory))
