;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Nils Verheyen
;;
;; Author: Nils Verheyen <nils@ungerichtet.de>
;; Maintainer: Nils Verheyen <nils@ungerichtet.de>
;; Created: Dezember 22, 2023
;; Modified: Dezember 22, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/nils/init
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; this should point to the directory where config.org is located
(setq proton/config-directory (getenv "VANILLAMACSDIR"))
(when (string-empty-p proton/config-directory)
  (setq proton/config-directory user-emacs-directory))

(org-babel-load-file
 (expand-file-name
  "config.org"
  proton/config-directory))


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(transient seq)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1)))))
