(defvar local-settings-file (expand-file-name "local.el" proton/config-directory))
(when (file-exists-p local-settings-file)
  (load local-settings-file))

(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                       :ref nil
                       :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                       :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;;When installing a package which modifies a form used at the top-level
;;(e.g. a package which adds a use-package key word),
;;use `elpaca-wait' to block until that package has been installed/configured.
;;For example:

(use-package general
  :demand t
  :config
  (general-evil-setup)
  ;; set 'SPC' as global leader key
  (general-create-definer proton/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC") ;; access leader key in insert mode

  (defun proton/edit-config ()
    (interactive)
    (find-file (expand-file-name "config.org" proton/config-directory))
  )

  (proton/leader-keys
   "." '(find-file :wk "Find file")
   "f c" '(proton/edit-config :wk "Edit config.org")
   )

  (proton/leader-keys
   "b" '(:ignore t :wk "Buffer") ;; just a prefix, no real key binding
   "b b" '(switch-to-buffer :wk "Switch buffer")
   "b i" '(ibuffer :wk "IBuffer")
   "b k" '(kill-this-buffer :wk "Kill buffer")
   "b n" '(next-buffer :wk "Next buffer")
   "b p" '(previous-buffer :wk "Previous buffer")
   "b r" '(revert-buffer :wk "Reload buffer")
   "b s" '(save-buffer :wk "Save buffer")
   )

  (proton/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d n" '(neotree-dir :wk "Open directory in neotree")
    "d p" '(peep-dired :wk "Peep-dired"))

  (proton/leader-keys
   "h" '(:ignore t :wk "Help") ;; just a prefix, no real key binding
   "h f" '(describe-function :wk "Describe function")
   "h k" '(describe-key :wk "Describe key")
   "h m" '(describe-keymap :wk "Describe keymap")
   "h p" '(describe-package :wk "Describe package")
   "h r r" '((lambda () (interactive)
	       (load-file (expand-file-name "init.el" user-emacs-directory))
	       (ignore (elpaca-process-queues)))
	     :wk "Reload emacs config")
   "h v" '(describe-variable :wk "Describe variable")
   )

  (proton/leader-keys
   "m" '(:ignore t :wk "Org")
   "m l" '(org-insert-link :wk "Insert link")
   )

  (proton/leader-keys
   "e" '(:ignore t :wk "Evaluate/Eshell")
   "e b" '(eval-buffer :wk "Eval buffer")
   "e e" '(eval-expression :wk "Evaluate and elisp expression")
   "e r" '(eval-region :wk "Eval region")
   "e s" '(eshell :wk "Open Eshell")
   )

  )
(elpaca-wait)

(use-package emacs
  :elpaca nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Expands to: (elpaca evil (use-package evil :demand t))
;;(use-package evil :demand t)
(use-package evil
  :init  ;; tweak evil before loading it
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)  ;; do not load default evil keybindings
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode)
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  :config
  (proton/leader-keys
   "b N" '(evil-buffer-new :wk "Open a new empty buffer")
   )
)

(use-package evil-collection
  :after evil
  :config
  ;; Do not uncomment this unless you want to specify each and every mode
  ;; that evil-collection should works with.  The following line is here 
  ;; for documentation purposes in case you need it.  
  ;; (setq evil-collection-mode-list '(calendar dashboard dired ediff info magit ibuffer))
  (add-to-list 'evil-collection-mode-list '(help dashboard dired ibuffer)) ;; evilify help mode
  (evil-collection-init))

(use-package evil-tutor)

;; Using RETURN to follow links in Org/Evil 
;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

(use-package evil-nerd-commenter
    :after evil
    :config
    (evilnc-default-hotkeys))

(use-package company
  :diminish
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (global-company-mode t)
  )

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package dired-open
  :config
  (setq dired-open-extensions '(("gif" . "sxiv")
                                ("jpg" . "sxiv")
                                ("png" . "sxiv")
                                ("mkv" . "vlc")
                                ("mp4" . "vlc"))))

(use-package peep-dired
  :after dired
  :hook (evil-normalize-keymaps . peep-dired-hook)
  :config
    (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
    (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
    (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
    (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
)

(defvar proton/fixed-width-font "JetBrainsMono NF"
  "The font to use for monospaced (fixed width) text.")

(defvar proton/variable-width-font "Fira Sans"
  "The font to use for variable-pitch (document) text.")

(set-face-attribute 'default nil
                    :font proton/fixed-width-font
                    :height 110
                    :weight 'medium)
(set-face-attribute 'variable-pitch nil
                    :font proton/variable-width-font
                    :height 120
                    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
                    :font proton/fixed-width-font
                    :height 110
                    :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(setq text-scale-mode-step 1.05)
(defun proton/text-scale-reset ()
  (interactive)
  (text-scale-adjust 0))
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'proton/text-scale-reset)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'custom-theme-load-path (expand-file-name (concat user-emacs-directory "themes/")))
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled, t by default
        doom-themes-enable-italic t) ; if nil, italics is universally disabled, t by default
  ;; This is the default theme
  (load-theme 'doom-nord t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 24      ;; sets modeline height
        doom-modeline-bar-width 5    ;; sets right bar width
        doom-modeline-persp-name t   ;; adds perspective name to modeline
        doom-modeline-persp-icon t   ;; adds folder icon next to persp name
        doom-modeline-minor-modes t  ;; show minor modes
	)
  )

(use-package minions
  :config (minions-mode 1))

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish
  :hook
  ((org-mode prog-mode) . rainbow-mode))

(global-set-key [escape] 'keyboard-escape-quit)

(use-package dashboard
  :ensure t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  ;; (setq dashboard-startup-banner "/home/dt/.config/emacs/images/emacs-dash.png")  ;; use custom image as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 3)
                          (projects . 5)
                          (registers . 3)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

(use-package diminish)

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :init (global-flycheck-mode))

(use-package projectile
  :ensure t
  :diminish
  :config
  (projectile-mode +1)
  (proton/leader-keys
    "p" '(:ignore t :wk "Project")
    "p d" '(projectile-discover-projects-in-search-path :wk "Discover projects")
    "p e" '(projectile-edit-dir-locals :wk "Edit project .dir-locals.el")
    "p i" '(projectile-invalidate-cache :wk "Invalidate project cache")
    "p p" '(projectile-switch-project :wk "Switch project")
    "SPC" '(projectile-find-file :wk "Find file in project")
  )
)

(use-package vertico
  :ensure t
  :diminish
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-f" . vertico-exit)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :elpaca nil  ;; built-in to emacs, no package manager required
  :init
  (savehist-mode))

(use-package consult
  :diminish
  :config
  (proton/leader-keys
    "<" '(consult-buffer :wk "Consult buffer")
    "RET" '(consult-bookmark :wk "Consult bookmark")
    "f r" '(consult-recent-file :wk "Consult recent file")
    "m h" '(consult-org-heading :wk "Consult org heading")
    "s" '(:ignore t :wk "Search")
    "s r" '(consult-ripgrep :wk "Consult rg")
    "s g" '(consult-grep :wk "Consult grep")
    "s G" '(consult-git-grep :wk "Consult git grep")
    "s f" '(consult-find :wk "Consult find")
    "s F" '(consult-fd :wk "Consult fd")
    "s b" '(consult-line :wk "Consult line")
    "S" '(:ignore t :wk "Additional Search")
    "S y" '(consult-yank-from-kill-ring :wk "Consult yank from kill ring")
    "i" '(consult-imenu :wk "Consult imenu"))
  )

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(defun +elpaca-unload-seq (e)
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

;; You could embed this code directly in the reicpe, I just abstracted it into a function.
(defun +elpaca-seq-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-seq 'elpaca--activate-package)))

(use-package seq
  :elpaca `(seq :build ,(+elpaca-seq-build-steps)))

(use-package magit
  :init
  ;; Do not call on :config as this block
  ;; is executed after opening magit
  (proton/leader-keys
    "g" '(:ignore t :wk "Git")
    "g g" '(magit :wk "Open magit buffer")
  )
  :commands
  (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )

(use-package git-timemachine
  :init
  (proton/leader-keys
    "g t" '(git-timemachine-toggle :wk "Toggle git timemachine")
  )
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
  (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
  (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision)
)

(use-package neotree
  :config
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 45
        neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action) 
  ;; truncate long file names in neotree
  (add-hook 'neo-after-create-hook
      #'(lambda (_)
          (with-current-buffer (get-buffer neo-buffer-name)
              (setq truncate-lines t)
              (setq word-wrap nil)
              (make-local-variable 'auto-hscroll-mode)
              (setq auto-hscroll-mode nil))))

  (proton/leader-keys
    "n" '(:ignore t :wk "Neotree")
    "n f" '(neotree-find :wk "Neotree find")
    "n t" '(neotree-toggle :wk "Toggle neotree")
  )
)

(setq org-return-follows-link t)
(setq org-hide-emphasis-markers t)

(use-package org
  :elpaca nil
  :init
  (proton/leader-keys
    "m e" '(org-edit-special :wk "Org edit special")
    )
  )

(defvar proton/org-notes-dir (file-truename "~/Org/notes")
  "Directory containing all my org notes files")
(setq org-directory proton/org-notes-dir
      org-agenda-files (list proton/org-notes-dir))

(with-eval-after-load 'org
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
   (define-key org-src-mode-map (kbd "C-c C-c") 'org-edit-src-exit)
  )

(use-package toc-org
  :commands toc-org-enable
  :init
  (add-hook 'org-mode-hook 'toc-org-enable)
  ;; enable in markdown, too
  (add-hook 'markdown-mode-hook 'toc-org-mode)
  )

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-faces)

;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
(set-face-attribute 'org-block nil
		    :foreground nil
		    :font proton/fixed-width-font
		    :height 1.0
		    :weight 'light)

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
    (let ((face-name (car face))
          (height (nth 1 face))
          (foreground (nth 2 face))
          (weight (nth 3 face)))
      
      (set-face-attribute (car face) nil
                          :family proton/variable-width-font
                          :height height
                          :foreground foreground
                          :weight weight)
    )
  )
  (set-face-attribute 'org-table nil
                      :family proton/fixed-width-font
                      :weight 'normal
                      :height 1.0
                      :foreground "#88c0d0")
  )
(with-eval-after-load 'org
  (add-hook 'org-mode-hook 'proton/org-colors-nord))

(setq org-src-preserve-indentation t)

(use-package perspective
  :ensure t
  :custom
  (persp-mode-prefix-key (kbd "C-<tab>"))
  (persp-sort 'created)
  (doom-modeline-display-default-persp-name t)
  :init
  (persp-mode)
  :config
  (proton/leader-keys
    "TAB" '(:ignore t :wk "Perspective")
    "<" '(persp-switch-to-buffer* :wk "Switch buffer")
    "TAB r" '(persp-rename :wk "Rename perspective")
    "TAB s" '(persp-switch :wk "Create/Switch perspective")
    "TAB n" '(persp-next :wk "Next perspective")
    "TAB p" '(persp-previous :wk "Previous perspective")
    "TAB q" '(persp-kill :wk "Kill perspective")
    "TAB k" '(persp-remove-buffer :wk "Remove buffer from perspective")
    "TAB a" '(persp-add-buffer :wk "Add buffer to perspective")
    "TAB A" '(persp-set-buffer :wk "Set buffer to perspective")
    )
  )

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1)
  )
(setq eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "btm" "htop" "ssh" "top" "zsh"))

;; Configure Tempel
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")
  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'org-mode-hook 'tempel-setup-capf)
  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  :config
  
  (proton/leader-keys
   "t" '(:ignore t :wk "Templates")
   "t c" '(tempel-complete :wk "Complete")
   "t i" '(tempel-complete :wk "Insert")
   )
)

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection
  :after tempel)

(add-to-list 'default-frame-alist '(alpha-background . 95))

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (org "https://github.com/milisims/tree-sitter-org")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (sql "https://github.com/m-novikov/tree-sitter-sql")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     ))

(dolist (lang treesit-language-source-alist)
  (unless (treesit-language-available-p (car lang))
    (treesit-install-language-grammar (car lang))))

(setq major-mode-remap-alist
 '(
   (bash-mode . bash-ts-mode)
   (css-mode . css-ts-mode)
   (html-mode . html-ts-mode)
   (json-mode . json-ts-mode)
   (makefile-mode . makefile-ts-mode)
   (python-mode . python-ts-mode)
   (yaml-mode . yaml-ts-mode)
   ))

(use-package sudo-edit
  :config
  (proton/leader-keys
    "f" '(:ignore t :wk "Files")
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
    "f U" '(sudo-edit :wk "Sudo edit file")
  )
)

(use-package which-key
  :init
  (which-key-mode)
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
	which-key-sort-order #'which-key-key-order-alpha
	which-key-sort-uppercase-first nil
	which-key-min-display-lines 6
	which-key-side-window-max-height 0.25
	which-key-idle-delay 0.3
	which-key-separator "  " ))
