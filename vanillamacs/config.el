(defvar local-settings-file (format "%slocal.el" user-emacs-directory))
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

;; Expands to: (elpaca evil (use-package evil :demand t))
;;(use-package evil :demand t)
(use-package evil
  :init  ;; tweak evil before loading it
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)  ;; do not load default evil keybindings
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
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

  (proton/leader-keys
   "." '(find-file :wk "Find file")
   "f c" '(lambda () (interactive) (find-file (concat user-emacs-directory "config.org")) :wk "Edit config.org")
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
   "h" '(:ignore t :wk "Help") ;; just a prefix, no real key binding
   "h f" '(describe-function :wk "Describe function")
   "h k" '(describe-key :wk "Describe key")
   "h m" '(describe-keymap :wk "Describe keymap")
   "h p" '(describe-package :wk "Describe package")
   "h r r" '((lambda () (interactive)
               (load-file (concat user-emacs-directory "init.el"))
               (ignore (elpaca-process-queues)))
             :wk "Reload emacs config")
   "h v" '(describe-variable :wk "Describe variable")
   )

  (proton/leader-keys
   "e" '(:ignore t :wk "Evaluate")
   "e b" '(eval-buffer :wk "Eval buffer")
   "e e" '(eval-expression :wk "Evaluate and elisp expression")
   "e r" '(eval-region :wk "Eval region")
   )

  )

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
  (load-theme 'doom-nord t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 24      ;; sets modeline height
        doom-modeline-bar-width 5    ;; sets right bar width
        doom-modeline-persp-name t   ;; adds perspective name to modeline
        doom-modeline-persp-icon t)) ;; adds folder icon next to persp name

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

(use-package projectile
  :config
  (projectile-mode 1)
  (proton/leader-keys
    "p" '(:ignore t :wk "Project")
    "p d" '(projectile-discover-projects-in-search-path :wk "Discover projects")
    "p e" '(projectile-edit-dir-locals :wk "Edit project .dir-locals.el")
    "p i" '(projectile-invalidate-cache :wk "Invalidate project cache")
    "p p" '(project-switch-project :wk "Switch project")
    "SPC" '(projectile-find-file :wk "Find file in project")
  )
)

(use-package vertico
  :ensure t
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
  :config
  (proton/leader-keys
    "<" '(consult-buffer :wk "consult buffer")
    "RET" '(consult-bookmark :wk "consult bookmark")
    "f r" '(consult-recent-file :wk "consult recent file")
    "m h" '(consult-org-heading :wk "consult org heading")
    "s" '(:ignore t :wk "Search")
    "s r" '(consult-ripgrep :wk "consult rg")
    "s g" '(consult-grep :wk "consult grep")
    "s G" '(consult-git-grep :wk "consult git grep")
    "s f" '(consult-find :wk "consult find")
    "s b" '(consult-line :wk "consult line")
    "S y" '(consult-yank-from-kill-ring :wk "consult yank from kill ring")
    "i" '(consult-imenu :wk "consult imenu"))
  )

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(setq org-return-follows-link t)

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

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1))))
 )

(setq org-src-preserve-indentation t)

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
  :config
  (setq which-key-side-window-location 'bottom
	which-key-sort-order #'which-key-key-order-alpha
	which-key-sort-uppercase-first nil
	which-key-min-display-lines 6
	which-key-side-window-max-height 0.25
	which-key-idle-delay 0.3
	which-key-separator "  " ))
