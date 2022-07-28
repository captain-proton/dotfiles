(doom-load-envvars-file "~/.emacs.d/.local/env")

(beacon-mode 1)

(setq dired-kill-when-opening-new-dired-buffer t)

(setq whitespace-style '(face tabs tab-mark spaces space-mark trailing
                              lines-tail)
      whitespace-line-column 140)
(setq whitespace-global-modes '(yaml-mode python-mode go-mode java-mode prog-mode))
(global-whitespace-mode +1)

(setq user-full-name "Nils Verheyen"
      user-mail-address "nils@ungerichtet.de")

(setq doom-font (font-spec :family "MesloLGS NF" :size 13 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "MesloLGS NF" :size 13))

(setq doom-theme 'doom-nord)

(setq display-line-numbers-type 'relative)

(setq org-directory "~/Dokumente/Org/")

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq-default line-spacing 4)

(with-temp-buffer
  (insert-file-contents "~/dotfiles/ansible.cfg")
  (keep-lines "vault_password_file" (point-min) (point-max))
  (setq ansible-vault-password-file
        (when (string-match "vault_password_file\s+=\s+\\(.*\\)"
                            (buffer-string))
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

(after! org
  (setq org-log-done 'time
        org-todo-keywords
        '((sequence
           "TODO(t)"            ; Backlog items in kanban that should be executed
           "DOING(d)"           ; Things that are currently in work (work in progress)
           "WAIT(w)"            ; A task that can not be set as DOING
           "|"                  ; Separate active and inactive items
           "DONE(e)"            ; Finished work ... yeah
           "CANCELLED(c)"))     ; Cancelled things :(
        )
  )

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(after! company
  (add-to-list 'company-backends #'company-tabnine)

  (set-company-backend! '(emacs-lisp-mode
                          conf-mode
                          lisp-mode
                          sh-mode
                          python-mode
                          go-mode
                          json-mode
                          yaml-mode
                          )
    '(company-tabnine
      :separate company-capf
      )
    )

  (setq +lsp-company-backends '(company-tabnine :separate company-capf))
  (setq company-show-numbers t)
  (setq company-idle-delay 0)
  )

(setq dap-auto-configure-mode t)
(require 'dap-python)

(after! dap-mode
  (setq dap-python-debugger 'debugpy))
