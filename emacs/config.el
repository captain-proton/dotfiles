(doom-load-envvars-file "~/.emacs.d/.local/env")

(setq frame-title-format
    '(""
      "%b"
      (:eval
       (let ((project-name (projectile-project-name)))
         (unless (string= "-" project-name)
           (format " in [%s]" project-name))))))

(blink-cursor-mode 1)

(setq dired-kill-when-opening-new-dired-buffer t)

(setq doom-themes-neotree-file-icons t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(map! :nvi "C-+" #'doom/increase-font-size
      :nvi "C--" #'doom/decrease-font-size
      :nvi "C-=" #'doom/reset-font-size
      )

(setq whitespace-style '(face tabs tab-mark spaces space-mark trailing
                              lines-tail)
      whitespace-line-column 140)
(setq whitespace-global-modes '(yaml-mode python-mode go-mode java-mode rustic-mode prog-mode))
(global-whitespace-mode +1)

(defun proton/set-highlight-thing-colors ()
  (set-face-background 'highlight-thing (doom-darken (doom-color 'highlight) 0.4))
  (set-face-foreground 'highlight-thing (doom-lighten (doom-color 'fg) 0.4)))
(add-hook! 'highlight-thing-mode-hook #'proton/set-highlight-thing-colors)

(global-highlight-thing-mode)
(setq highlight-thing-what-thing 'sexp)

(defun proton/fringe-on-zen ()
  (if (bound-and-true-p writeroom-mode)
      (fringe-mode 0)
    (fringe-mode '(nil . nil))))
(add-hook 'writeroom-mode-hook 'proton/fringe-on-zen)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(evil-define-key nil evil-visual-state-map
  (kbd "A") 'evil-mc-make-cursor-in-visual-selection-end
  (kbd "I") 'evil-mc-make-cursor-in-visual-selection-beg)

(defun proton/close-project ()
  "Close the current frame and delete all buffers associated to the project"
  (interactive)
  (if (> (length (visible-frame-list)) 1)
      (progn (projectile-kill-buffers)
             (delete-frame nil t))
    (evil-quit)))

(map! :leader
      :desc "Quit project" "p q" #'proton/close-project)

 (use-package! centaur-tabs
   :init
   (centaur-tabs-group-by-projectile-project)
   :config
   (centaur-tabs-headline-match)
   (centaur-tabs-mode t)
   (setq uniquify-separator "/")
   (setq uniquify-buffer-name-style 'forward)
   (defun centaur-tabs-buffer-groups ()
     "`centaur-tabs-buffer-groups' control buffers' group rules.

 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
     (list
      (cond
       ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
       ;; "Remote")
       ((or (string-equal "*" (substring (buffer-name) 0 1))
            (memq major-mode '(magit-process-mode
                               magit-status-mode
                               magit-diff-mode
                               magit-log-mode
                               magit-file-mode
                               magit-blob-mode
                               magit-blame-mode
                               )))
        "Emacs")
       ((derived-mode-p 'prog-mode)
        "Editing")
       ((derived-mode-p 'dired-mode)
        "Dired")
       ((memq major-mode '(helpful-mode
                           help-mode))
        "Help")
       ((memq major-mode '(org-mode
                           org-agenda-clockreport-mode
                           org-src-mode
                           org-agenda-mode
                           org-present-mode
                           org-indent-mode
                           org-bullets-mode))
        "OrgMode")
       (t (centaur-tabs-get-group-name (current-buffer))))))
   :hook
   (dashboard-mode . centaur-tabs-local-mode)
   (term-mode . centaur-tabs-local-mode)
   (calendar-mode . centaur-tabs-local-mode)
   (org-agenda-mode . centaur-tabs-local-mode)
   (helpful-mode . centaur-tabs-local-mode)
   :bind
   ("C-<prior>" . centaur-tabs-backward)
   ("C-<next>" . centaur-tabs-forward)
   ("C-c t s" . centaur-tabs-counsel-switch-group)
   ("C-c t p" . centaur-tabs-group-by-projectile-project)
   ("C-c t g" . centaur-tabs-group-buffer-groups)
   (:map evil-normal-state-map
    ("g t" . centaur-tabs-forward)
    ("g T" . centaur-tabs-backward))
   )

(setq user-full-name "Nils Verheyen"
      user-mail-address "nils@ungerichtet.de")

;; Set reusable font name variables
(defvar proton/fixed-width-font "JetBrains Mono Nerd Font"
  "The font to use for monospaced (fixed width) text.")

(defvar proton/variable-width-font "Noto Sans"
  "The font to use for variable-pitch (document) text.")

(setq doom-font (font-spec :family proton/fixed-width-font :size 15)
      doom-variable-pitch-font (font-spec :family proton/variable-width-font :size 15)
      doom-big-font (font-spec :family proton/variable-width-font :size 24)
      doom-font-increment 1)

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq doom-theme 'doom-nord)

(setq display-line-numbers-type 'relative)

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

(elfeed-org)

(after! elfeed
  (setq elfeed-search-filter "@2-weeks-ago +unread"
        elfeed-search-title-min-width 80
        visual-fill-column-mode 1
        )
  )
(add-hook! 'elfeed-show-mode-hook (hide-mode-line-mode 1))
(add-hook! 'elfeed-search-update-hook #'hide-mode-line-mode)

(setq rmh-elfeed-org-files (list "~/Org/elfeed.org"))

(map! :leader
      :desc "Activate lsp-org" "m L" #'lsp-org
      :desc "deactivate lsp-org" "m D" #'lsp-virtual-buffer-disconnect)

(defun proton/org-colors-nord ()
  "Enable Nord colors for Org headers."
  (interactive)
  (dolist
      (face
       '((org-level-1 1.7 "#81a1c1" bold)
         (org-level-2 1.6 "#b48ead" bold)
         (org-level-3 1.5 "#a3be8c" semi-bold)
         (org-level-4 1.4 "#ebcb8b" normal)
         (org-level-5 1.3 "#bf616a" light)
         (org-level-6 1.2 "#88c0d0" light)
         (org-level-7 1.1 "#81a1c1" light)
         (org-level-8 1.0 "#b48ead" light)))
    (set-face-attribute (nth 0 face) nil
                        :font doom-variable-pitch-font
                        :height (nth 1 face)
                        :foreground (nth 2 face)
                        :weight (nth 3 face)))
  (set-face-attribute 'org-table nil
                      :font doom-font
                      :weight 'normal
                      :height 1.0
                      :foreground "#bfafdf"))

(proton/org-colors-nord)

(require 'org-faces)

;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
(set-face-attribute 'org-block nil :foreground nil :font proton/fixed-width-font :height 1.0 :weight 'light)

(setq org-directory (file-truename "~/Org/notes"))

(after! org
  (setq org-log-done 'time
        org-todo-keywords
        '((sequence
           "TODO(t)"            ; Backlog items in kanban that should be executed
           "DOING(o)"           ; Things that are currently in work (work in progress)
           "WAIT(w)"            ; A task that can not be set as DOING
           "|"                  ; Separate active and inactive items
           "DONE(d)"            ; Finished work ... yeah
           "CANCELLED(c@)"))    ; Cancelled things :(
        org-todo-repeat-to-state "TODO"
        org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿"))
  )

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default nil))

(setq visual-fill-column-width 110
      visual-fill-column-center-text t)

(defun proton/org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-show-entry)

  ;; Show only direct subheadings of the slide but don't expand them
  (org-show-children))

(defun proton/org-present-start ()
  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
  (doom-big-font-mode 1)
  (display-line-numbers-mode 0)
  (hl-line-mode 0)
  (highlight-thing-mode 0)
  (centaur-tabs-mode 0)
  (variable-pitch-mode 1)

  ;; Tweak font sizes
  (setq-local face-remapping-alist '((default (:height 1.4) variable-pitch)
                                     (header-line (:height 2.2) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.0) org-code)
                                     (org-verbatim (:height 1.0) org-verbatim)
                                     (org-block (:height 0.9) org-block)
                                     (org-block-begin-line (:height 0.4) org-block)))

  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " ")

  ;; Display inline images automatically
  (org-display-inline-images)
  )

(defun proton/org-present-end ()
  ;; Stop centering the document
  (visual-fill-column-mode 0)
  (visual-line-mode 0)
  (doom-big-font-mode 0)
  (display-line-numbers-mode 1)
  (hl-line-mode 1)
  (highlight-thing-mode 1)
  (centaur-tabs-mode 1)
  (variable-pitch-mode 0)

  ;; Reset font customizations, default was nil
  (setq-local face-remapping-alist nil)

  ;; Clear the header line string so that it isn't displayed
  (setq header-line-format nil)

  ;; Stop displaying inline images
  (org-remove-inline-images)
  )

(add-hook 'org-present-mode-hook 'proton/org-present-start)
(add-hook 'org-present-mode-quit-hook 'proton/org-present-end)
(add-hook 'org-present-after-navigate-functions 'proton/org-present-prepare-slide)

(setq proton/org-roam-home (format "%s/Org/roam" (getenv "HOME")))
(when (not (file-directory-p proton/org-roam-home))
  (make-directory proton/org-roam-home 'parents))

(setq org-roam-directory (file-truename proton/org-roam-home))
(org-roam-db-autosync-mode)

;; Load ob-ess-julia and dependencies
(use-package! ob-ess-julia
  :ensure t
  :config
  ;; Add ess-julia into supported languages:
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((ess-julia . t))))
  ;; Link this language to ess-julia-mode (although it should be done by default):
  (setq org-src-lang-modes
        (append org-src-lang-modes '(("ess-julia" . ess-julia)))))

(setq dap-auto-configure-mode t)

;; Displaying DAP visuals.
(dap-ui-mode t)

;; enables mouse hover support
(dap-tooltip-mode t)

;; use tooltips for mouse hover
;; if it is not enabled `dap-mode' will use the minibuffer.
(tooltip-mode t)

;; displays floating panel with debug buttons
;; requies emacs 26+
(dap-ui-controls-mode t)

(use-package! dap-mode
  :config
  ;; call dap-hydra after a breakpoint has been hit
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  )

(use-package! dap-mode
  :after lsp-mode
  :commands dap-debug
  :hook ((python-mode . dap-ui-mode) (python-mode . dap-mode))
  :config
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (defun dap-python--pyenv-executable-find (command)
    (with-venv (executable-find "python"))))

(dap-register-debug-template "Rust::GDB Run Configuration"
                             (list :type "gdb"
                                   :request "launch"
                                   :name "GDB::Run"
                                   :gdbpath "rust-gdb"
                                   :target nil
                                   :cwd nil))
