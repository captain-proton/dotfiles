(doom-load-envvars-file "~/.config/emacs.d/doom/.local/env")

(setq frame-title-format
    '(""
      "%b"
      (:eval
       (let ((project-name (projectile-project-name)))
         (unless (string= "-" project-name)
           (format " in [%s]" project-name))))))

(setq doom-modeline-persp-name t)

(setq! pixel-scroll-precision-mode t
       pixel-scroll-precision-large-scroll-height 40.0)

(map! :nvi "C-+" #'doom/increase-font-size
      :nvi "C--" #'doom/decrease-font-size
      :nvi "C-=" #'doom/reset-font-size
      "C-c s" #'org-edit-special
      "C-x c" #'evilnc-comment-or-uncomment-lines
      "C-x e" #'embark-export
      )

(after! evil
  (map! :i "C-V" #'evil-paste-before-cursor-after
        :i "C-v" #'evil-paste-after
        )
  (setq evil-kill-on-visual-paste nil)
  )

(blink-cursor-mode 1)

(setq dired-kill-when-opening-new-dired-buffer t)

(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "Z") 'dired-do-compress
  (kbd "+") 'dired-create-directory
  (kbd "% l") 'dired-downcase
  (kbd "% m") 'dired-mark-files-regexp
  (kbd "% u") 'dired-upcase
  (kbd "* %") 'dired-mark-files-regexp
  (kbd "* .") 'dired-mark-extension
  (kbd "* /") 'dired-mark-directories)
;; With dired-open plugin, you can launch external programs for certain extensions
;; As is I am/was also a kde user, use gwenview for images and vlc for videos
(setq dired-open-extensions '(("gif" . "gwenview")
                              ("jpg" . "gwenview")
                              ("png" . "gwenview")
                              ("mkv" . "vlc")
                              ("mp4" . "vlc")))

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (when (buffer-file-name)
              (call-interactively #'save-buffer))))

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

(add-hook! 'start-mode-hook
  (fringe-mode '(nil . nil)))

(defun proton/fringe-on-zen ()
  (if (bound-and-true-p writeroom-mode)
      (fringe-mode 0)
    (fringe-mode '(nil . nil))))
(add-hook 'writeroom-mode-hook 'proton/fringe-on-zen)

(setq +tree-sitter-hl-enabled-modes '(python-mode java-mode rustic-mode yaml-mode))

(setq-default line-spacing 4)

(evil-define-key nil evil-visual-state-map
  (kbd "A") 'evil-mc-make-cursor-in-visual-selection-end
  (kbd "I") 'evil-mc-make-cursor-in-visual-selection-beg
  (kbd "M-n") 'evil-mc-make-and-goto-next-match
  (kbd "M-p") 'evil-mc-make-and-goto-prev-match
  )

(defun proton/close-project ()
  "Close the current frame and delete all buffers associated to the project"
  (interactive)
  (if (> (length (+workspace-list-names)) 1)
      (progn (mapc 'kill-buffer (+workspace-buffer-list))
             (+workspace/delete (+workspace-current-name)))
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
(defvar proton/fixed-width-font "JetBrainsMono NF"
  "The font to use for monospaced (fixed width) text.")

(defvar proton/variable-width-font "Fira Sans"
  "The font to use for variable-pitch (document) text.")

(setq doom-font (font-spec :family proton/fixed-width-font :size 15)
      doom-variable-pitch-font (font-spec :family proton/variable-width-font :size 15)
      doom-unicode-font (font-spec :family proton/fixed-width-font :size 15)
      doom-big-font (font-spec :family proton/variable-width-font :size 24)
      doom-font-increment 1)

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

;; Try to fix treemacs icons
;; provided by https://github.com/emacs-lsp/lsp-treemacs/issues/89#issuecomment-779976219
(use-package! doom-themes
  :custom
  (doom-themes-treemacs-theme "doom-colors")
  :config
  ;; Enable customized theme
  ;; FIXME https://github.com/emacs-lsp/lsp-treemacs/issues/89
  (with-eval-after-load 'lsp-treemacs
    (doom-themes-treemacs-config)))

(setq doom-theme 'doom-nord)

(setq display-line-numbers-type 'relative)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (setq fancy-splash-image (expand-file-name "splash/doom-emacs-splash.svg" doom-private-dir))
;; (setq local-settings-file (format "%s/local.el" (getenv "DOOMDIR")))
(setq initial-buffer-choice (format "%s/start.org" (getenv "DOOMDIR")))

(define-minor-mode start-mode
  "Provide functions for custom start page."
  :lighter " start"
  :keymap (let ((map (make-sparse-keymap)))
            ;;(define-key map (kbd "M-z") 'eshell)
            (evil-define-key 'normal start-mode-map
              (kbd "1") '(lambda () (interactive) (find-file (format "%s/config.org" (getenv "DOOMDIR"))))
              (kbd "2") '(lambda () (interactive) (find-file (format "%s/init.el" (getenv "DOOMDIR"))))
              (kbd "3") '(lambda () (interactive) (find-file (format "%s/packages.el" (getenv "DOOMDIR")))))
            map)
  (+zen/toggle)
  (display-line-numbers-mode -1))

(add-hook 'start-mode-hook 'read-only-mode) ;; make start.org read-only; use 'SPC t r' to toggle off read-only.
(provide 'start-mode)

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
  :files (or "playbooks/" "roles/" (and "tasks/main.yml" "defaults/")))

(setq local-settings-file (format "%s/local.el" (getenv "DOOMDIR")))
(when (file-exists-p local-settings-file)
  (load local-settings-file))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(after! elfeed
  (elfeed-org)
  (defadvice! cp/elfeed-in-own-workspace (&rest _)
  "Open Elfeed in its own workspace."
  :before #'elfeed
  (when (modulep! :ui workspaces)
    (+workspace-switch "Elfeed" t)))
  )
(custom-set-faces!
  '(elfeed-search-unread-title-face
    :weight normal)
  '(elfeed-search-title-face
    :family "Vollkorn"
    :height 1.4)
  )
(add-hook! 'elfeed-show-mode-hook (hide-mode-line-mode 1))
(add-hook! 'elfeed-search-update-hook #'hide-mode-line-mode)

(use-package! elfeed
  :config
  (defun cp/elfeed-entry-line-draw (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (concat "[" (mapconcat 'identity tags ",") "]"))
           (title-width (- (window-width) elfeed-goodies/feed-source-column-width
                           elfeed-goodies/tag-column-width 4))
           (title-column (elfeed-format-column
                          title (elfeed-clamp
                                 elfeed-search-title-min-width
                                 title-width
                                 elfeed-search-title-max-width)
                          :left))
           (tag-column (elfeed-format-column
                        tags-str (elfeed-clamp (length tags-str)
                                               elfeed-goodies/tag-column-width
                                               elfeed-goodies/tag-column-width)
                        :left))
           (feed-column (elfeed-format-column
                         feed-title (elfeed-clamp elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width)
                         :left))
           )
      (if (>= (window-width) (* (frame-width) elfeed-goodies/wide-threshold))
          (progn
            (insert (propertize date 'face 'elfeed-search-date-face) " ")
            (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
            (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
            (insert (propertize title 'face title-faces 'kbd-help title))
            )
        (insert (propertize title 'face title-faces 'kbd-help title)))))

  (defun search-header/draw-wide (separator-left separator-right search-filter stats db-time)
    (let* ((update (format-time-string "%Y-%m-%d %H:%M:%S %z" db-time))
           (lhs (list
                 (powerline-raw (-pad-string-to "Date" (- 9 4)) 'powerline-active2 'l)
                 (funcall separator-left 'powerline-active2 'powerline-active1)
                 (powerline-raw (-pad-string-to "Feed" (- elfeed-goodies/feed-source-column-width 4)) 'powerline-active1 'l)
                 (funcall separator-left 'powerline-active1 'powerline-active2)
                 (powerline-raw (-pad-string-to "Tags" (- elfeed-goodies/tag-column-width 6)) 'powerline-active2 'l)
                 (funcall separator-left 'powerline-active2 'mode-line)
                 (powerline-raw "Subject" 'mode-line 'l)))
           (rhs (search-header/rhs separator-left separator-right search-filter stats update)))
      (concat (powerline-render lhs)
              (powerline-fill 'mode-line (powerline-width rhs))
              (powerline-render rhs))))

  (setq rmh-elfeed-org-files (list "~/Org/elfeed.org")
        elfeed-search-print-entry-function 'cp/elfeed-entry-line-draw
        elfeed-search-filter "@2-weeks-ago +unread"
        elfeed-search-title-min-width 80
        elfeed-goodies/tag-column-width 20
        +rss-enable-sliced-images nil
        visual-fill-column-mode 1)
  )

(map! :leader
      :prefix ("o" . "open")
      :desc "Elfeed" "e" #'elfeed)

(after! plantuml
  (setq plantuml-indent-level 2)
  )

(setq nov-unzip-program (executable-find "bsdtar")
      nov-unzip-args '("-xC" directory "-f" filename))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(defun proton/nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Vollkorn"
                                           :height 1.4))
(add-hook 'nov-mode-hook 'proton/nov-font-setup)

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (map! :map nov-mode-map
        :n "RET" #'nov-scroll-up)

  (advice-add 'nov-render-title :override #'ignore)

  (defun +nov-mode-setup ()
    "Tweak nov-mode to our liking."
    (face-remap-add-relative 'variable-pitch
                             :family "Vollkorn"
                             :height 1.4)
    (face-remap-add-relative 'default :height 1.3)
    (require 'visual-fill-column nil t)
    (setq-local visual-fill-column-center-text t
                visual-fill-column-width 101
                nov-text-width 100
                )
    (visual-fill-column-mode 1)
    (highlight-thing-mode 0)
    (hl-line-mode -1)
    ;; Re-render with new display settings
    (nov-render-document))

  (add-hook 'nov-mode-hook #'+nov-mode-setup)
  )

(require 'hideshow)

;; optional key bindings, easier than hs defaults
(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle hiding of block"
       "h" #'hs-toggle-hiding))

(require 'nxml-mode)
(require 'sgml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))

(add-to-list 'hs-special-modes-alist
             '(rustic-mode "{" "}" "/[*/]" nil nil))

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

(defvar proton/org-notes-dir (file-truename "~/Org/notes")
  "Directory containing all my org notes files")
(setq org-directory proton/org-notes-dir
      org-agenda-files (list proton/org-notes-dir))

(after! org
  (setq org-log-done 'time
        org-todo-keywords
        '((sequence
           "DOING(o)"           ; Things that are currently in work (work in progress)
           "TODO(t)"            ; Backlog items in kanban that should be executed
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
  (setq org-auto-tangle-default t)
  )

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

(after! (org-roam)
  (defadvice! yeet/org-roam-in-own-workspace-a (&rest _)
  "Open all roam buffers in there own workspace."
  :before #'org-roam-node-find
  :before #'org-roam-node-random
  :before #'org-roam-buffer-display-dedicated
  :before #'org-roam-buffer-toggle
  :before #'org-roam-dailies-goto-today
  (when (modulep! :ui workspaces)
    (+workspace-switch "Org-roam" t))))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking org-roam-ui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(require 'asdf-vm)
(asdf-vm-init)
(use-package! lsp-mode
  :init
  (setq! lsp-inlay-hint-enable t)
  :config
  (setq! lsp-ui-doc-show-with-mouse t
         lsp-ui-doc-max-width 96
         lsp-ui-doc-max-height 13)
  )

(map!
 :map lsp-ui-mode-map
 [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
 [remap xref-find-references] #'lsp-ui-peek-find-references
 )

(defun proton/toggle-comment ()
  (interactive)
  (evilnc-comment-or-uncomment-lines 1)
  (evil-next-line 1))
(map!
 :desc "toggle line comment" :ne "C-/" #'proton/toggle-comment
 )

(add-hook! python-mode #'display-fill-column-indicator-mode)

(add-hook! rust-mode #'display-fill-column-indicator-mode)

(use-package! rustic
  :config
  (setq! lsp-rust-analyzer-cargo-watch-enable t
         lsp-rust-analyzer-cargo-watch-command "clippy"
         lsp-rust-analyzer-proc-macro-enable t
         lsp-rust-analyzer-cargo-load-out-dirs-from-check t
         lsp-rust-analyzer-inlay-hints-mode t
         lsp-rust-analyzer-server-display-inlay-hints t
         lsp-rust-analyzer-display-chaining-hints t
         lsp-rust-analyzer-display-parameter-hints t))

(setq lsp-java-configuration-runtimes '[(:name "JavaSE-1.8"
                                         :path (format "%s/.asdf/installs/java/adoptopenjdk-8.0.372+7" (getenv "HOME"))
                                         (:name "JavaSE-17"
                                          :path (format "%s/.asdf/installs/java/adoptopenjdk-17.0.7+7" (getenv "HOME"))
                                          :default t))])

(use-package! lsp-java
  :after lsp
  :init (when (boundp local/lsp-java-configuration-runtimes)
          (setq lsp-java-configuration-runtimes local/lsp-java-configuration-runtimes))
  )

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
  (setq dap-default-terminal-kind "integrated") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
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

(use-package! rustic
  :config
  (require 'dap-gdb-lldb)
  (require 'dap-cpptools))
(dap-register-debug-template "Rust::GDB Run Configuration"
                             (list :type "gdb"
                                   :request "launch"
                                   :name "GDB::Run"
                                   :gdbpath "rust-gdb"
                                   :program "${workspaceFolder}/target/debug/hello / replace with binary"
                                   :cwd "${workspaceFolder}"
                                   :console "external"
                                   :dap-compilation "cargo build"
                                   :dap-compilation-dir "${workspaceFolder}"
                                   ))
