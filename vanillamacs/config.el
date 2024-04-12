(setq make-backup-files nil  ;; do not create backup files at all
      delete-old-versions t  ;; delete backup files (filename~) automatically
      create-lockfiles nil   ;; no need for .# lock files
      vc-follow-symlinks t   ;; don't ask every time to follow sym links to vc repos
      )

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

(defvar local-settings-file (expand-file-name "local.el" proton/config-directory))
(when (file-exists-p local-settings-file)
  (load local-settings-file))

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
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
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
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
  ;;Enable Elpaca's use-package support
  (elpaca-use-package-mode)
  ;; Assume :ensure t unless otherwise specified.
  (setq use-package-always-ensure t))

;; Block until current queue processed.
(elpaca-wait)

;;When installing a package which modifies a form used at the top-level
;;(e.g. a package which adds a use-package key word),
;;use `elpaca-wait' to block until that package has been installed/configured.
;;For example:

(use-package general
  :ensure t
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
  (defun show-message-log ()
    (interactive)
    (switch-to-buffer "*Messages*")
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
   "b m" '(show-message-log :wk "*Messages*")
   "b n" '(next-buffer :wk "Next buffer")
   "b p" '(previous-buffer :wk "Previous buffer")
   "b r" '(revert-buffer :wk "Reload buffer")
   "b s" '(save-buffer :wk "Save buffer")
   )

  (proton/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d p" '(peep-dired :wk "Peep-dired"))

  (proton/leader-keys
    "f" '(:ignore t :wk "Files/Fonts")
    )

  (proton/leader-keys
    "v" '(:ignore t :wk "Vanillamacs")
    "v r" '((lambda () (interactive)
            (load-file (expand-file-name "init.el" user-emacs-directory))
            (ignore (elpaca-process-queues)))
          :wk "Reload emacs config")
    "v R" '(restart-emacs :wk "Restart Emacs")
    "v q" '(kill-emacs :wk "Save and quit emacs"))

  (proton/leader-keys
   "h" '(:ignore t :wk "Help") ;; just a prefix, no real key binding
   "h f" '(describe-function :wk "Describe function")
   "h k" '(describe-key :wk "Describe key")
   "h K" '(describe-keymap :wk "Describe keymap")
   "h m" '(describe-mode :wk "Describe mode")
   "h p" '(describe-package :wk "Describe package")
   "h v" '(describe-variable :wk "Describe variable")
   )

  (proton/leader-keys
   "m" '(:ignore t :wk "Org")
   "m l" '(org-insert-link :wk "Insert link")
   )

  (proton/leader-keys
   "e" '(:ignore t :wk "Evaluate")
   "e b" '(eval-buffer :wk "Eval buffer")
   "e e" '(eval-expression :wk "Evaluate and elisp expression")
   "e r" '(eval-region :wk "Eval region")
   )

  (proton/leader-keys
   "o" '(:ignore t :wk "Open")
   "o d" '(dashboard-open :wk "Dashboard")
   "o e" '(eshell :wk "Open Eshell")
   )

  )
(elpaca-wait)

(defvar proton/fixed-width-font "JetBrainsMono NF"
  "The font to use for monospaced (fixed width) text.")

(defvar proton/variable-width-font "Fira Sans"
  "The font to use for variable-pitch (document) text.")

(defun proton/load-default-fontaine-preset ()
  (interactive)
  (fontaine-set-preset 'regular))

(use-package fontaine
  :ensure t
  :after evil
  :general
  (proton/leader-keys
    "f d" '(proton/load-default-fontaine-preset :wk "Set default font preset")
    "f f" '(fontaine-set-preset :wk "Set font preset")
    )
  :config
  (setq fontaine-presets
        '((regular
           :default-height 110
           :line-spacing 0.16)
          (feedreader
           :default-family "JetBrainsMono Nerd Font"
           :default-height 140
           :default-weight regular
           :line-spacing 0.12)
          (presentation
           :default-height 180
           :line-spacing 0.16)
          (t
           :default-family "JetBrainsMono Nerd Font"
           :default-height 100
           :default-weight regular
           :fixed-pitch-family "JetBrainsMono Nerd Font"
           :variable-pitch-family "Fira Sans"
           :variable-pitch-height 120
           :variable-pitch-weight regular
           :line-spacing nil)))
    )

;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
            :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
            :slant 'italic)
(elpaca-wait)

(require 'fontaine)
(setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))

;; The other side of `fontaine-restore-latest-preset'.
(add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

;; Recover last preset or fall back to desired style from
;; `fontaine-presets'.
(with-eval-after-load 'doom-themes
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  )

(use-package emacs
  :ensure nil
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
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package elfeed
  :ensure t
  :after (general perspective)
  :bind
  (:map elfeed-show-mode-map
        ([remap elfeed-kill-buffer] . evil-delete-buffer))
  (:map elfeed-search-mode-map
        ([remap proton/persp-kill-current] . proton/quit-elfeed))
  :general
  (proton/leader-keys
    "o f" '(elfeed :wk "elfeed"))
  :config
  (setq elfeed-search-filter "@2-weeks-ago +unread")
  )

(with-eval-after-load 'elfeed
  (custom-set-faces
   '(elfeed-search-unread-title-face ((t :weight medium)))
   '(elfeed-search-title-face ((t :family "Vollkorn" :height 1.4)))
   )
  )

(defun proton/on-entering-elfeed()
  (fontaine-set-preset 'feedreader)
  (display-line-numbers-mode 0)
  )

(add-hook 'elfeed-search-mode-hook 'proton/on-entering-elfeed)

(defun proton/quit-elfeed()
  (interactive)
  (proton/load-default-fontaine-preset)
  (display-line-numbers-mode 1)
  (elfeed-search-quit-window)
  (persp-kill "elfeed")
  )

(general-advice-add 'elfeed
                    :before (lambda (&rest r) (persp-switch "elfeed")))

(use-package elfeed-org
  :ensure t
  :after elfeed
  :init
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Org/elfeed.org")))

(use-package elfeed-goodies
  :ensure t
  :after elfeed
  :config
  (elfeed-goodies/setup)
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
            ;; (insert (propertize entry-score 'face 'elfeed-search-feed-face) " ")
            (insert (propertize date 'face 'elfeed-search-date-face) " ")
            (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
            (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
            ;; (insert (propertize authors-column 'face 'elfeed-search-tag-face) " ")
            (insert (propertize title 'face title-faces 'kbd-help title))
            )
        (insert (propertize title 'face title-faces 'kbd-help title)))))
  (setq elfeed-search-print-entry-function 'cp/elfeed-entry-line-draw)
  )

;; Expands to: (elpaca evil (use-package evil :demand t))
;;(use-package evil :demand t)
(use-package evil
  :ensure t
  :init  ;; tweak evil before loading it
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)  ;; do not load default evil keybindings
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode)
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  :config
  (evil-set-undo-system 'undo-redo)
  (proton/leader-keys
   "b N" '(evil-buffer-new :wk "Open a new empty buffer")
   "b k" '(evil-delete-buffer :wk "Evil delete buffer")
   )
)

(use-package evil-collection
  :ensure t
  :after evil
  :config
  ;; Do not uncomment this unless you want to specify each and every mode
  ;; that evil-collection should works with.  The following line is here 
  ;; for documentation purposes in case you need it.  
  ;; (setq evil-collection-mode-list '(calendar dashboard dired ediff info magit ibuffer))
  (add-to-list 'evil-collection-mode-list '(help dashboard dired ibuffer)) ;; evilify help mode
  (evil-collection-init))

(use-package evil-tutor
  :ensure t
  :after evil
  )

;; Using RETURN to follow links in Org/Evil 
;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

(global-set-key [remap evil-quit] 'evil-delete-buffer)

(use-package evil-snipe
  :ensure t
  :after evil
  :config
  (evil-snipe-override-mode 1))

(with-eval-after-load 'evil-maps
  (evil-define-key 'normal 'global (kbd "f") 'evil-snipe-f)
  (evil-define-key 'normal 'global (kbd "s") 'evil-snipe-s)
  (evil-define-key 'normal 'global (kbd "F") 'evil-snipe-F)
  (evil-define-key 'normal 'global (kbd "S") 'evil-snipe-S))

(use-package evil-nerd-commenter
  :ensure t
  :after evil
  :bind
  ("C-/" . evilnc-comment-operator)
  :config
  (evilnc-default-hotkeys))

(use-package company
  :ensure t
  :diminish
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1)
  (company-global-modes '(not eshell-mode shell-mode))
  ;; Search other buffers with the same modes for completion instead of
  ;; searching all other buffers.
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  ;; M-<num> to select an option according to its number.
  (company-show-numbers t)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1)
  ;; Use company with text and programming modes.
  :hook ((text-mode . company-mode)
         (prog-mode . company-mode))
  )

(use-package company-box
  :ensure t
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package dired-open
  :ensure t
  :after dired
  :config
  (setq dired-open-extensions '(("gif" . "sxiv")
                                ("jpg" . "sxiv")
                                ("png" . "sxiv")
                                ("mkv" . "vlc")
                                ("mp4" . "vlc"))))

(use-package peep-dired
  :ensure t
  :after dired
  :hook (evil-normalize-keymaps . peep-dired-hook)
  :config
    (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
    (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
    (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
    (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
)

(use-package vscode-icon
  :ensure t
  :commands (vscode-icon-for-file)
  )

(use-package dired-sidebar
  :ensure t
  :after dired
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (display-line-numbers-mode 0)
              (unless (file-remote-p default-directory)
                (auto-revert-mode))
              ))
  (proton/leader-keys
    "d s" '(dired-sidebar-toggle-sidebar :wk "Dired sidebar"))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "  ")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-width 45)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  )

(setq text-scale-mode-step 1.05)
(defun proton/text-scale-reset ()
  (interactive)
  (text-scale-adjust 0))
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'proton/text-scale-reset)

(setq use-short-answers t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(defun proton/set-highlight-thing-colors ()
  (interactive)
  (set-face-background 'highlight-thing (doom-darken (doom-color 'highlight) 0.5))
  (set-face-foreground 'highlight-thing (doom-lighten (doom-color 'fg) 0.5)))

(use-package highlight-thing
  :ensure t
  :init
  (global-highlight-thing-mode)
  :hook (highlight-thing-mode . proton/set-highlight-thing-colors)
  :config
  (setq highlight-thing-what-thing 'sexp) ;; sexp = symbol expression (https://en.wikipedia.org/wiki/S-expression)
  )

(add-to-list 'custom-theme-load-path (expand-file-name (concat user-emacs-directory "themes/")))
(use-package doom-themes
  :ensure t
  :init
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled, t by default
        doom-themes-enable-italic t) ; if nil, italics is universally disabled, t by default

  ;; This is the default theme
  (load-theme 'doom-nord t)

  ;; Add "padding" around tabs, the colour must be added to correct the colouring
  (set-face-attribute 'tab-line-tab-current nil :box '(:line-width 8 :color "#2E3440"))
  (set-face-attribute 'tab-line-tab-inactive nil :box '(:line-width 8 :color "#272C36"))
)

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
  :ensure t
  :config (minions-mode 1)
  )

(use-package ligature
  :ensure t
  :config
  ;; Enable all JetBrains Mono ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
                                      "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
                                      "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
                                      "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
                                      "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
                                      "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
                                      ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
                                      "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
                                      "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
                                      "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
                                      "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                dashboard-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package rainbow-mode
  :ensure t
  :diminish
  :hook
  ((org-mode prog-mode) . rainbow-mode))

(global-set-key [escape] 'keyboard-escape-quit)

(setq help-window-select t)

;; Set default indentation to use spaces instead of tabs
(setq-default indent-tabs-mode nil)

(use-package indent-bars
  :ensure (:host github :repo "jdtsmith/indent-bars")
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string nil)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
                                      list list_comprehension
                                      dictionary dictionary_comprehension
                                      parenthesized_expression subscript)))
  :hook ((prog-mode yaml-mode) . indent-bars-mode)
  :config
  (setq
    indent-bars-color '(highlight :face-bg t :blend 0.2)
    indent-bars-pattern "."
    indent-bars-width-frac 0.1
    indent-bars-pad-frac 0.1
    indent-bars-zigzag nil
    indent-bars-color-by-depth nil
    indent-bars-highlight-current-depth nil
    indent-bars-display-on-blank-lines nil)
  )

(use-package whitespace
  :ensure nil
  :init
  (global-whitespace-mode)
  :config
  ;; Don't enable whitespace for.
  (setq-default whitespace-global-modes
                '(not shell-mode
                      help-mode
                      text-mode
                      magit-mode
                      magit-diff-mode
                      ibuffer-mode
                      dired-mode
                      occur-mode))
  (setq
    whitespace-style '(face tabs tab-mark spaces space-mark trailing))
  (custom-set-faces
   '(whitespace-space ((t (:foreground "#4c566a" :background unspecified)))))
  )

(use-package tab-line
  :ensure nil
  :init
  (global-tab-line-mode t)
  :config
  (setq tab-line-new-button-show nil  ;; do not show add-new button
        tab-line-close-button-show nil  ;; do not show close button
        )
  ;; do not use :bind C-<next> ... they are bound in global.el
  (define-key (current-global-map) [remap scroll-right] 'previous-buffer)
  (define-key (current-global-map) [remap scroll-left] 'next-buffer)
  )
(require 'tab-line)

(with-eval-after-load 'evil
  (proton/leader-keys
    "w" '(:ignore t :wk "Windows")
    "w c" '(evil-window-delete :wk "Close current window")
    "w v" '(evil-window-vsplit :wk "Split |")
    "w h" '(evil-window-split :wk "Split -")
    "w w" '(evil-window-next :wk "Next window")
    "w W" '(evil-window-prev :wk "Previous window")
    )
  )

(use-package golden-ratio
  :ensure t
  :init
  (golden-ratio-mode 1)
  )

(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  ;; (setq dashboard-startup-banner "/home/dt/.config/emacs/images/emacs-dash.png")  ;; use custom image as banner
  (setq dashboard-center-content t) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 3)
                          (projects . 5)
                          (registers . 3)))
  (setq dashboard-projects-backend 'projectile)
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook)
  (display-line-numbers-mode 0)
  )

(use-package diminish
  :ensure t
  )

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :config (global-flycheck-mode))

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
    "p r" '(projectile-recentf :wk "Recent project files")
    "SPC" '(projectile-find-file :wk "Find file in project")
  )
)

(use-package vertico
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :diminish
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-f" . vertico-exit)
         ("?" . minibuffer-completion-help)
         ("M-RET" . minibuffer-force-complete-and-exit)
         ("M-TAB" . minibuffer-complete)
         :map minibuffer-local-map
         ("C-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :ensure nil  ;; built-in to emacs, no package manager required
  :init
  (savehist-mode))

(use-package consult
  :ensure t
  :diminish
  :config
  (proton/leader-keys
    "<" '(consult-project-buffer :wk "Consult buffer")
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
  :ensure t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package embark
  :after evil
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-," . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  (:map vertico-map
        ("C-x e" . embark-export))

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(defun +elpaca-unload-seq (e)
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

;; You could embed this code directly in the reicpe, I just abstracted it into a function.
(defun +elpaca-seq-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-seq 'elpaca--activate-package)))

(use-package seq
  :ensure `(seq :build ,(+elpaca-seq-build-steps)))

(use-package transient
  :ensure (:host github :repo "magit/transient")
  )

(use-package magit
  :ensure t
  :after (seq transient)
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
  :ensure t
  :init
  (proton/leader-keys
    "g t" '(git-timemachine-toggle :wk "Toggle git timemachine")
  )
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
  (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
  (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision)
)

(setq org-return-follows-link t)
(setq org-hide-emphasis-markers t)

(use-package org
  :ensure nil
  :init
  (proton/leader-keys
    "m" '(:ignore t :wk "Org")
    "m e" '(org-edit-special :wk "Org edit special")
    )
  :config
  ;; This is considered highly unsafe!
  ;; But confirm again and again does lead to the same issue
  (setq org-confirm-babel-evaluate nil)
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

(use-package org-roam
  :ensure t
  :after org
  :general
  (proton/leader-keys
    "m r" '(:ignore t :wk "Roam")
    "m r f" '(org-roam-node-find :wk "Find node")
    "m r i" '(org-roam-node-insert :wk "Insert node")
    )
  :config
  (setq proton/org-roam-home (format "%s/Org/roam" (getenv "HOME")))
  (when (not (file-directory-p proton/org-roam-home))
    (make-directory proton/org-roam-home 'parents))

  (setq org-roam-directory (file-truename proton/org-roam-home))
  (org-roam-db-autosync-mode)
  )

(defun proton/open-org-roam-perspective ()
    (interactive)
    (persp-switch "org-roam")
  )
(dolist (f '(org-roam-node-find org-roam-node-insert))
  (general-advice-add f :before #'proton/open-org-roam-perspective))

(use-package toc-org
  :ensure t
  :commands toc-org-enable
  :init
  (add-hook 'org-mode-hook 'toc-org-enable)
  ;; enable in markdown, too
  (add-hook 'markdown-mode-hook 'toc-org-mode)
  )

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (makefile . t)
     (plantuml . t)
     (js . t)
     (sql . t)
     (sqlite . t)
     ;; Add more languages as needed
     )))

  (setq org-src-fontify-natively t) ; Enable syntax highlighting in source blocks

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets
  :ensure t
  )
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-faces)

;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
(set-face-attribute 'org-block nil
            :foreground 'unspecified
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

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (bash-ts-mode . lsp))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-enable-snippet nil)
  :general
  (proton/leader-keys
    "c" '(:ignore t :wk "Code")
    "c c" '(evilnc-comment-operator :wk "Recompile")
    "c r" '(recompile :wk "Recompile")
    "c f" '(lsp-format-region :wk "Format region")
    "c F" '(lsp-format-buffer :wk "Format buffer")
    )
  )

;; force lsp-mode to forget the workspace folders for multi root servers so the workspace folders are added on demand
(advice-add 'lsp
            :before (lambda (&rest _args)
                      (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht))))
            )
;; The path to lsp-mode needs to be added to load-path as well as the
;; path to the `clients' subdirectory.
(add-to-list 'load-path (expand-file-name "lib/lsp-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib/lsp-mode/clients" user-emacs-directory))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.venv\\'"))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ("C-c d" . lsp-ui-doc-toggle))
  :general
  (proton/leader-keys
    "c d" '(lsp-ui-doc-show :wk "Document that")
    "c D" '(lsp-ui-doc-show :wk "Close doc")
    )
  :config
  (setq lsp-ui-doc-position 'at-point)
  )

(use-package ansible
  :ensure t
  :hook ((yaml-ts-mode . ansible)
         (ansible . ansible-auto-decrypt-encrypt))
  :config
  (setq ansible-section-face 'font-lock-variable-name-face
        ansible-task-label-face 'font-lock-doc-face
        ansible-vault-password-file nil)
  )
(use-package ansible-doc
  :ensure t
  )
(use-package jinja2-mode
  :ensure t
  :mode "\\.j2$"
  )

(use-package yaml-mode
  :ensure t
  :hook (
         (yaml-ts-mode . lsp-deferred)
         (yaml-ts-mode . company-mode)
         (yaml-ts-mode . whitespace-mode)
         )
  )

(use-package python
  :ensure nil
  :hook ((python-ts-mode . lsp-deferred))
  )

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
    "TAB r" '(persp-rename :wk "Rename perspective")
    "TAB s" '(persp-switch :wk "Create/Switch perspective")
    "TAB n" '(persp-next :wk "Next perspective")
    "TAB p" '(persp-prev :wk "Previous perspective")
    "TAB q" '(proton/persp-kill-current :wk "Kill perspective")
    "TAB k" '(persp-remove-buffer :wk "Remove buffer from perspective")
    "TAB a" '(persp-add-buffer :wk "Add buffer to perspective")
    "TAB A" '(persp-set-buffer :wk "Set buffer to perspective")

    "TAB 1" '((lambda () (interactive) (persp-switch-by-number 1)) :wk "Switch to perspective 1")
    "TAB 2" '((lambda () (interactive) (persp-switch-by-number 2)) :wk "Switch to perspective 2")
    "TAB 3" '((lambda () (interactive) (persp-switch-by-number 3)) :wk "Switch to perspective 3")
    "TAB 4" '((lambda () (interactive) (persp-switch-by-number 4)) :wk "Switch to perspective 4")
    "TAB 5" '((lambda () (interactive) (persp-switch-by-number 5)) :wk "Switch to perspective 5")
    "TAB 6" '((lambda () (interactive) (persp-switch-by-number 6)) :wk "Switch to perspective 6")
    "TAB 7" '((lambda () (interactive) (persp-switch-by-number 7)) :wk "Switch to perspective 7")
    "TAB 8" '((lambda () (interactive) (persp-switch-by-number 8)) :wk "Switch to perspective 8")
    "TAB 9" '((lambda () (interactive) (persp-switch-by-number 9)) :wk "Switch to perspective 9")
    "TAB 0" '((lambda () (interactive) (persp-switch-by-number 10)) :wk "Switch to perspective 10")
    )
  )

(defun proton/persp-kill-current()
  "Kill the current active perspective"
  (interactive)
  (persp-kill (persp-current-name))
  )

(use-package persp-projectile
  :ensure t
  :init
  (proton/leader-keys
    "p p" '(projectile-persp-switch-project :wk "Switch project"))
  )

(use-package eshell-syntax-highlighting
  :ensure t
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
  :ensure t
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
   "t i" '(tempel-insert :wk "Insert")
   )
)

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection
  :ensure t
  :after tempel
  )

(use-package tldr
  :ensure t
  :config
  (proton/leader-keys
    "s t" '(tldr :wk "Lookup tldr for command help"))
  )

;; (add-to-list 'default-frame-alist '(alpha-background . 95))

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
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

(dolist (mapping
         '((bash-mode . bash-ts-mode)
           (css-mode . css-ts-mode)
           (html-mode . html-ts-mode)
           (json-mode . json-ts-mode)
           (makefile-mode . makefile-ts-mode)
           (python-mode . python-ts-mode)
           (yaml-mode . yaml-ts-mode)))
  (add-to-list 'major-mode-remap-alist mapping))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; (use-package ts-fold
;;   :ensure (:host github :repo "emacs-tree-sitter/ts-fold")
;;   :config
;;   (global-ts-fold-mode)
;;   )

;; (use-package ts-fold-indicators
;;   :ensure (:host github :repo "emacs-tree-sitter/ts-fold")
;;   :config
;;   (global-ts-fold-indicators-mode)
;;   )

(use-package hideshow
  :ensure nil
  :commands (hs-toggle-hiding
             hs-hide-block
             hs-show-block
             hs-hide-level
             hs-show-all
             hs-hide-all)
  :config
  (defun proton/ensure-hideshow (&rest _)
    ;; Enable hideshow if it is not already active
    (unless (bound-and-true-p hs-minor-mode)
      (hs-minor-mode +1)))

  (defun proton/nxml-forward-element ()
    (let ((nxml-sexp-element-flag))
      (setq nxml-sexp-element-flag (not (looking-at "<!--")))
      (unless (looking-at outline-regexp)
        (condition-case nil
            (nxml-forward-balanced-item 1)
          (error nil)))))

  (add-to-list 'hs-special-modes-alist '(yaml-ts-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>"
                         ""
                         "#"
                         nil nil))
  (add-to-list 'hs-special-modes-alist '(json-ts-mode "[[{]" "[]}]"))
  (add-to-list 'hs-special-modes-alist
        '(nxml-mode
          "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
          ""
          "<!--" ;; won't work on its own; uses syntax table
          (lambda (arg) (proton/nxml-forward-element))
          nil))

  (dolist (cmd '(hs-toggle-hiding
                 hs-hide-block
                 hs-show-block
                 hs-hide-level
                 hs-show-all
                 hs-hide-all))
    (advice-add cmd :before #'proton/ensure-hideshow))
  )

(use-package sudo-edit
  :ensure t
  :config
  (proton/leader-keys
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
    "f U" '(sudo-edit :wk "Sudo edit file")
  )
)

(use-package which-key
  :ensure t
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
    which-key-separator "  ")
  )

(use-package inheritenv
  :ensure t
  )

(use-package envrc
  :ensure t
  :config
  (envrc-global-mode)
  )
