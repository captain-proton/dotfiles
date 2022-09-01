(doom-load-envvars-file "~/.emacs.d/.local/env")

(blink-cursor-mode 1)

(setq dired-kill-when-opening-new-dired-buffer t)

(setq doom-themes-neotree-file-icons t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(setq whitespace-style '(face tabs tab-mark spaces space-mark trailing
                              lines-tail)
      whitespace-line-column 140)
(setq whitespace-global-modes '(yaml-mode python-mode go-mode java-mode prog-mode))
(global-whitespace-mode +1)

(global-highlight-thing-mode)
(setq highlight-thing-what-thing 'word)

(defun proton/fringe-on-zen ()
  (if (bound-and-true-p writeroom-mode)
      (fringe-mode 0)
    (fringe-mode '(nil . nil))))
(add-hook 'writeroom-mode-hook 'proton/fringe-on-zen)

(evil-define-key nil evil-visual-state-map
  (kbd "A") 'evil-mc-make-cursor-in-visual-selection-end
  (kbd "I") 'evil-mc-make-cursor-in-visual-selection-beg)

(defun proton/close-project ()
  "Close the current frame and delete all buffers associated to the project"
  (interactive)
  (projectile-kill-buffers)
  (delete-frame nil t))

(map! :leader
      :desc "Quit project" "p q" #'proton/close-project)

(setq user-full-name "Nils Verheyen"
      user-mail-address "nils@ungerichtet.de")

;; Set reusable font name variables
(defvar proton/fixed-width-font "JetBrains Mono Nerd Font"
  "The font to use for monospaced (fixed width) text.")

(defvar proton/variable-width-font "Noto Sans"
  "The font to use for variable-pitch (document) text.")

(defvar proton/unicode-font "JuliaMono"
  "The font to use for displaying unicode characters.")

(setq doom-font (font-spec :family proton/fixed-width-font :size 15 :weight 'light)
      doom-variable-pitch-font (font-spec :family proton/variable-width-font :size 15)
      doom-big-font (font-spec :family proton/variable-width-font :size 24)
      doom-unicode-font (font-spec :family proton/unicode-font)
      doom-font-increment 1)

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq doom-theme 'doom-tomorrow-night)

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
  (setq org-auto-tangle-default t))

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
  (variable-pitch-mode 1)

  ;; Tweak font sizes
  (setq-local face-remapping-alist '((default (:height 1.2) variable-pitch)
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

(setq dap-auto-configure-mode t)
(require 'dap-python)

(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(97 . 100) '(90 . 90)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)
