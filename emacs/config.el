(setq user-full-name "Nils Verheyen"
      user-mail-address "nils@ungerichtet.de")

(setq doom-font (font-spec :family "MesloLGS NF" :size 13 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "MesloLGS NF" :size 13))

(setq doom-theme 'doom-nord)

(setq display-line-numbers-type 'relative)

(setq org-directory "~/Dokumente/Org/")

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(after! neotree
    (setq neo-window-fixed-size nil)
    (setq neo-window-width 40))

(setq-default line-spacing 4)

(with-temp-buffer
  (insert-file-contents "~/dotfiles/ansible.cfg")
  (keep-lines "vault_password_file" (point-min) (point-max))
  (setq ansible-vault-password-file (when (string-match "vault_password_file\s+=\s+\\(.*\\)" (buffer-string))
                        (match-string 1 (buffer-string)))))

(def-project-mode! +ansible-yaml-mode
  :modes '(yaml-mode)
  :add-hooks '(ansible ansible-auto-decrypt-encrypt ansible-doc-mode)
  :files (or "playbooks/" "roles/" "tasks/" "handlers/"))

(setq local-settings-file (format "%s/.doom.d/local.el" (getenv "HOME")))
(when (file-exists-p local-settings-file)
  (load local-settings-file))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))
