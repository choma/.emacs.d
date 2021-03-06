;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;; GNU Emacs configuration file, by Matías Croce

;;; Code:

;; Temporary workaround for Tramp bug.
;; See: http://emacs.stackexchange.com/a/14828/7093
(setq tramp-ssh-controlmaster-options "") ;; reduces init time

;; start emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; PERSONAL DATA
(setq user-full-name "Matías Croce"
      user-mail-address "mati@nelumboweb.com.ar")


;; PACKAGES

;; Install signed code only, see:
;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(setq tls-checktrust t)
(setq gnutls-log-level '2)
(let ((trustfile
       (replace-regexp-in-string
	"\\\\" "/"
	(replace-regexp-in-string
	 "\n" ""
	 (shell-command-to-string "python -m certifi")))))
  (setq tls-program
	(list
	 (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
		 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

;; Add package sources
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ))
(package-initialize)
(package-refresh-contents)

(setq load-prefer-newer t)
(add-to-list 'load-path "~/elisp")
(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))
(setq use-package-verbose t)
(setq use-package-always-ensure t) ;; use :ensure t for all packages

(eval-when-compile
  (require 'use-package))
(require 'bind-key)                ;; if you use any :bind variant

(use-package auto-compile)
(auto-compile-on-load-mode 1)

(use-package diminish :ensure t)

;; Elisp Bug Hunter!!
;; (use-package bug-hunter)


;; ENVIRONMENT DATA

;; Make exec-path === PATH, to make sure Emacs finds all executables
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(require 'iso-transl) ;; Make <dead-acute> work again
(setq current-language-environment "Spanish")
(setq default-input-method "spanish-prefix")

;; Set locale to UTF8
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; move Emacs "Custom" settings to a different file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)


;; ORG-MODE
;; Indent different levels
(add-hook 'org-mode-hook
	  (lambda ()
	    (org-indent-mode t))
	  t)
(setq org-modules '(org-protocol))

;; EMACS USAGE

;; remap previous-buffer, next-buffer (default keys, <left> and <right>, are too far)
(global-set-key (kbd "C->") 'next-buffer)
(global-set-key (kbd "C-<") 'previous-buffer)

;; tramp
(setq tramp-default-method "ssh")

;; eshell
;; Copied from: https://github.com/bodil/ohai-emacs/
;; Define a keybinding to get to your eshell quickly.
(global-set-key (kbd "C-c e") 'eshell)
;; Define a pretty prompt.
(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

;; Disable hl-line-mode in eshell
(add-hook 'eshell-mode-hook '(lambda() (set (make-local-variable 'global-hl-line-mode) nil)))

(use-package dired
  :ensure nil ;; don't try to install it
  :defer t
  :config
  (put 'dired-find-alternate-file 'disabled nil))
;; ;; dired
;; (use-package dired+)
;; ;; Do not open new buffers when visiting new directories
;; (diredp-toggle-find-file-reuse-dir 1)
;; ;; Move files between split panes
;; (setq dired-dwim-target 1)

(use-package dired-k)
(add-hook 'dired-initial-position-hook 'dired-k)
(add-hook 'dired-after-readin-hook #'dired-k-no-revert)

(use-package dired-hacks-utils)
(use-package dired-subtree
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

;; narrow dired to match filter
(use-package dired-narrow
  :after dired-k
  :bind (:map dired-mode-map
	      ("/" . dired-narrow)))

;; Ask "y" or "n" instead of "yes" or "no"
(fset 'yes-or-no-p 'y-or-n-p)

(use-package atomic-chrome)
(atomic-chrome-start-server)

;; search matches counter
(use-package anzu
  :diminish)
(global-anzu-mode +1)

;; Magit!! :D
(use-package magit)
(global-set-key (kbd "C-x g") 'magit-status)

(use-package magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;; git-timemachine! :D
(use-package git-timemachine)

;; git-gutter
(use-package fringe-helper)
(use-package git-gutter-fringe)
(global-git-gutter-mode +1)
(require 'git-gutter-fringe)
;; (setq git-gutter-fr:side 'right-fringe)

;; Helm
;; Copied from Sacha Chua:
;; http://pages.sachachua.com/.emacs.d/Sacha.html#unnumbered-14
(use-package helm
  :diminish
  :init (progn
	  (require 'helm-config)
	  (setq helm-candidate-number-limit 100)
	  ;; From https://gist.github.com/antifuchs/9238468
	  (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
		helm-input-idle-delay 0.01  ; this actually updates things
					; reeeelatively quickly.
		helm-quick-update t
		helm-M-x-requires-pattern nil
		helm-M-x-fuzzy-match t ;; optional fuzzy matching for helm-M-x
		helm-ff-skip-boring-files t
		helm-ff-auto-update-initial-value t
		helm-buffers-fuzzy-matching t
		helm-recentf-fuzzy-match t
		;; helm-autoresize-mode 1
		helm-ff-file-name-history-use-recentf t)
	  (helm-mode))
  :bind (("C-c h" . helm-mini)
	 ("C-h a" . helm-apropos)
	 ("C-x C-b" . helm-buffers-list)
	 ("C-x b" . helm-buffers-list)
	 ("C-x r l" .  helm-bookmarks)
	 ("C-x C-f" . helm-find-files)
	 ("M-y" . helm-show-kill-ring)
	 ("M-x" . helm-M-x)
	 ("C-x c o" . helm-occur) ;; search in current buffer
	 ("C-x c s" . helm-swoop) ;; search in current buffer
	 ("C-x c m" . helm-multi-swoop-all) ;; search in all buffers
	 ("C-x c SPC" . helm-all-mark-rings)))
(helm-autoresize-mode t)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p t) ; open helm buffer inside current window, not occupy whole other window
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(use-package helm-ag)
(defun projectile-helm-ag ()
  "Searching with silversearcher-ag inside projects with projectile."
  (interactive)
  (helm-ag (projectile-project-root)))

(use-package helm-swoop)

;; For describing bindings
(use-package helm-descbinds
  :defer t
  :bind (("C-h b" . helm-descbinds)
	 ("C-h w" . helm-descbinds)))

;; Projectile
(use-package projectile
  :diminish)
;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; FIX hange issue with tramp
;; source: https://github.com/bbatsov/prelude/issues/594#issuecomment-220951394
;; (projectile-global-mode)
(add-hook 'text-mode-hook 'projectile-mode)
(add-hook 'prog-mode-hook 'projectile-mode)

;; (projectile-enable-caching t)

;; Make projectile search locally only for SVN repos. See:
;; https://github.com/bbatsov/projectile/issues/520
(setq projectile-svn-command "find . -type f -not -iwholename '*.svn/*' -print0")

;; Helm-Projectile integration
(use-package helm-projectile)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(use-package sudo-edit)
(global-set-key (kbd "C-c C-r") 'sudo-edit-current-file)


;; DOCUMENTATION

;; eldoc
(use-package eldoc
  :diminish
  :init (progn
	  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
	  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)))

;; php documentation
(use-package php-eldoc
  :config (add-hook 'php-mode-hook 'php-eldoc-enable))

(use-package company-php)

(add-hook 'php-mode-hook
	  (lambda ()
	    (set (make-local-variable 'company-backends)
		 ;; '((php-extras-company company-dabbrev-code) company-capf company-files))))
		 '((company-dabbrev-code) company-capf company-files))))


;; css documentation
(use-package css-eldoc
  :init(progn
	 (eldoc-mode 1)))
(add-hook 'css-mode-hook 'turn-on-css-eldoc)


;; CODE EDITING

;; Delete selected region when start typing
(delete-selection-mode t)

(use-package expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(use-package skewer-mode
  :defer t
  ;; :diminish (skewer-mode skewer-html-mode skewer-css-mode)
  :diminish
  :init (skewer-setup))
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;; syntax highlighting for all buffers
(global-font-lock-mode t)

;; Visual line (soft-word-wrap)
(global-visual-line-mode t)

;; Highlight corresponding parentheses when cursor is on one
(show-paren-mode t)
(setq show-paren-style 'expression)

;; Use cua-selection-mode
(cua-selection-mode t)



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package lsp-mode
  :config
  (setq lsp-completion-enable-additional-text-edit nil)
  (setq lsp-java-format-enabled nil)
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c C-l")
  ;; Performance
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  ;; (setq lsp-idle-delay 1)
  (setq lsp-log-io nil) ; if set to true can cause a performance hit
  ;;
  (setq lsp-java-vmargs '("-XX:+UseParallelGC"
			  "-XX:GCTimeRatio=4"
			  "-XX:AdaptiveSizePolicyWeight=90"
			  "-Dsun.zip.disableMemoryMapping=true"
			  "-Xmx1G" ;;"-Xmx2G"
			  "-Xms100m"
			  ;; Enable lombok support
			  "-javaagent:/home/choma/.m2/repository/org/projectlombok/lombok/1.16.22/lombok-1.16.22.jar"
			  "-Xbootclasspath/a:/home/choma/.m2/repository/org/projectlombok/lombok/1.16.22/lombok-1.16.22.jar")
	)

  ;; (setq lsp-java-configuration-runtimes '[
  ;;					  ;; NOPE: https://github.com/microsoft/vscode-java-test/issues/1048#issuecomment-676318039
  ;;					  ;; (
  ;;					  ;;  :name "JavaSE-1.8"
  ;;					  ;;  :path "/usr/lib/jvm/adoptopenjdk-8-openj9-amd64/"
  ;;					  ;;  :default t
  ;;					  ;;  )
  ;;					  (
  ;;					   :name "JavaSE-11"
  ;;					   :path "/usr/lib/jvm/java-11-openjdk-amd64/"
  ;;					   )
  ;;					  ])

  :config
  ;; https://github.com/emacs-lsp/lsp-mode/issues/1672#issuecomment-626277665
  (define-key lsp-mode-map (kbd "C-c C-l") lsp-command-map)

  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	 ;; (XXX-mode . lsp-deferred)
	 (java-mode . lsp-deferred)
	 ;; if you want which-key integration
	 ;; (lsp-mode . lsp-enable-which-key-integration)
	 )

  :commands lsp lsp-deferred)

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)
;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))
(use-package dap-java
  :ensure nil)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
;; (use-package which-key
;;   :config
;;   (which-key-mode))

(use-package helm-lsp)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; editorconfig (editorconfig.org)
(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))
(add-hook 'editorconfig-custom-hooks
	  (lambda (hash) (setq web-mode-block-padding 0)))

;; Undo tree
(use-package undo-tree
  :diminish
  :config
  (setq undo-tree-visualizer-diff t))
(global-undo-tree-mode)

;; Highlight nested parens, brackets, braces a different color at each depth.
(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; agressive indent! :D
(use-package aggressive-indent
  :diminish)
(global-aggressive-indent-mode)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

(add-to-list
 'aggressive-indent-dont-indent-if
 '(and (derived-mode-p 'c++-mode)
       (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
			   (thing-at-point 'line)))))

;; multiple-cursors
(use-package multiple-cursors)
;; (require 'multiple-cursors)

;; Hydra
(use-package hydra)

(global-set-key
 (kbd "C-c m")
 ;; Hydra for multiple-cursors
 ;; taken from: https://github.com/abo-abo/hydra/wiki/multiple-cursors
 (defhydra hydra-multiple-cursors (:hint nil)
   "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
 [Click] Cursor at point       [_q_] Quit"
   ("l" mc/edit-lines :exit t)
   ("a" mc/mark-all-like-this :exit t)
   ("n" mc/mark-next-like-this)
   ("N" mc/skip-to-next-like-this)
   ("M-n" mc/unmark-next-like-this)
   ("p" mc/mark-previous-like-this)
   ("P" mc/skip-to-previous-like-this)
   ("M-p" mc/unmark-previous-like-this)
   ("s" mc/mark-all-in-region-regexp :exit t)
   ("0" mc/insert-numbers :exit t)
   ("A" mc/insert-letters :exit t)
   ("<mouse-1>" mc/add-cursor-on-click)
   ;; Help with click recognition in this hydra
   ("<down-mouse-1>" ignore)
   ("<drag-mouse-1>" ignore)
   ("q" nil))
 )

;; ace-jump
(use-package ace-jump-mode)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-c ESC") 'ace-jump-mode-pop-mark)

;; snippets
(use-package yasnippet
  :diminish yas-minor-mode)
;; (yas-global-mode t)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; FILES

;; backups (source: http://www.emacswiki.org/emacs/BackupDirectory )
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/backups"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

;; desktop (remember open files from last session)
(desktop-save-mode t)       ;; save/restore opened files
;;(setq-default desktop-dirname "~/.emacs.d/desktop")
;;(setq desktop-directory '(("." . "~/.emacs.d/desktop")))

;; company (autocomplete)
(use-package company
  :diminish
  :init (progn
	  (setq company-auto-complete nil)
	  (global-company-mode t)
	  (add-to-list 'company-backends 'company-ispell t)
	  (add-to-list 'company-backends 'company-css t)
	  ;; (add-to-list 'company-backends 'php-extras-company t)
	  ;; (add-to-list 'company-backends 'company-dabbrev t) ;; dabrev must be after other backends to work correctly (?)
	  ;; (add-to-list 'company-backends 'company-dabbrev-code t)
	  (add-to-list 'company-backends 'company-capf t)
	  (add-to-list 'company-backends 'company-files t)))
(add-hook 'after-init-hook 'global-company-mode)
;; 0.1 second delay before the pop-up appears
(setq company-idle-delay 0.1)
;; only one character before auto-completion starts
(setq company-minimum-prefix-length 1)
(setq company-dabbrev-downcase nil)

(global-set-key (kbd "C-c y") 'company-yasnippet)

(use-package pos-tip)
(require 'pos-tip)
(use-package company-quickhelp)
(company-quickhelp-mode t)

;; autopair
(use-package autopair
  :diminish
  :init (progn
	  (autopair-global-mode t)))

(use-package toggle-quotes)
(global-set-key (kbd "C-'") 'toggle-quotes)

;; flycheck (syntax checking)
(use-package flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)  ;; globally enable flycheck
(use-package flycheck-pos-tip) ;; show error messages with popup.el
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))
(use-package helm-flycheck
  :bind("C-c ! h" . helm-flycheck))

;; idle-highlight-mode (highlight current word ocurrences)
(use-package idle-highlight-mode
  :init (progn
	  (add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))))

;; LANGUAGES

;; .env files
(use-package dotenv-mode)

;; markdown-mode
(use-package markdown-mode)

;; yaml-mode
(use-package yaml-mode)

;; web-mode (php,js,html,css)
(use-package web-mode)
;; NOTE: flycheck doesn't support multi-language buffers, so I won't be using web-mode that much :-/
;; see: https://github.com/flycheck/flycheck/issues/349

;; (with-eval-after-load 'flycheck
;;   ;; TODO: enable checkers by file extension
;;   (flycheck-add-mode 'html-tidy 'web-mode)
;;   (flycheck-add-mode 'css-csslint 'web-mode)
;;   )


(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))  ;; html
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode)) ;; cake views
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode)) ;; sass
(add-hook 'sgml-mode-hook 'web-mode) ;; and markup modes
(add-hook 'html-mode-hook 'web-mode)
(add-hook 'css-mode-hook 'web-mode)
(setq web-mode-enable-current-element-highlight t) ;; highlight current element
(setq web-mode-enable-current-column-highlight t) ;; highlight current col

;; JS
;; js2 (javascript editing)
(use-package js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; Source: https://stackoverflow.com/questions/35162106/how-to-disable-js2-mode-syntax-checking-globally-in-spacemacs
;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
;; Increase highlight-level
(setq js2-highlight-level 3)

;; tern
;; TODO see: https://truongtx.me/2014/04/20/emacs-javascript-completion-and-refactoring
(use-package tern
  :diminish
  :config
  (tern-mode t))

(use-package company-tern)
(add-to-list 'company-backends 'company-tern)

(setq tern-command (append tern-command '("--no-port-file"))) ;; don't create .tern-port files
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-hook 'web-mode-hook (lambda () (tern-mode t)))

;; Vue-mode
(use-package vue-mode)

;; jquery documentation
(use-package jquery-doc)
(add-hook 'javascript-mode-hook 'jquery-doc-setup)

;; emmet (zen coding)
(use-package emmet-mode
  :diminish)
(add-hook 'web-mode-hook 'emmet-mode)
(use-package helm-emmet)

;; lorem ipsum text filler
(use-package lorem-ipsum)
;; use default bindings
;; C-c l p	lorem-ipsum-insert-paragraphs
;; C-c l s	lorem-ipsum-insert-sentences
;; C-c l l	lorem-ipsum-insert-list
(lorem-ipsum-use-default-bindings)

;; CSS
(add-to-list 'company-backends 'company-css t)

;; rainbow (displays strings representing colors with the color they represent)
(use-package rainbow-mode
  :init (progn
	  (add-hook 'css-mode-hook (lambda () (rainbow-mode 1)))))

;; python documentation
;;(add-hook 'python-mode-hook 'turn-on-eldoc-mode)
;; (use-package anaconda-mode
;;   :init(progn
;;	 (eldoc-mode 1)))
;; (use-package company-anaconda)
;; (add-to-list 'company-backends 'company-anaconda)

;; PHP
;; php-mode
;; TODO: make doc comments to not indent. Make indentation to always use tabs
(use-package php-mode
  :init(progn
	 ;; Avoid HTML compatibility. See: https://github.com/emacs-php/php-mode#avoid-html-template-compatibility
	 (setq php-template-compatibility nil)
	 ;; Configure per-project phpcs if available
	 (setq-default php-manual-path "~/www/utilidades/docs/php5/php-manual/") ;; php docs local copy
	 ))
;; set psr-2 coding style
(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)
(setq flycheck-phpcs-standard "PSR2")

;; php-auto-yasnippet
(use-package php-auto-yasnippets)
(require 'php-auto-yasnippets)
(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)

;; ;; php-extras
;; (use-package php-extras
;;   :defer t)

;; I'm not using it (find a way to use it correctly with cakephp)
;; geben (to debug php with xdebug)
;; (use-package geben)
;; (autoload 'geben "geben" "DBGp protocol frontend, a script debugger" t)

;; INI, SERVICE and CONF files
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.service$" . conf-mode))

;; 80 column indicator
;; (use-package fill-column-indicator)
;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode t)))
;; (setq-default fci-rule-column 80) ;; this line breaks global-visual-line-mode
;; (global-fci-mode t)

;;Remove useless whitespaces and add a new line at the end of the file
(setq-default show-trailing-whitespace t)
(setq-default highlight-tabs t)
(setq require-final-newline t)
(setq next-line-add-newlines nil)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

(setq
 ;; No backups
 make-backup-files nil
 ;; backup-inhibited t
 auto-save-default nil
 ;; Startup buffer
 inhibit-startup-message t
 inhibit-splash-screen t
 initial-scratch-message ";; Scratch buffer\n\n(setq debug-on-error t)\n\n"
 ;; Don't keep message buffers around
 message-kill-buffer-on-exit t
 ;; Kill the whole line
 kill-whole-line t
 ;; Special files localtions
 ;;bookmark-default-file (concat user-emacs-directory "bookmarks")
 )


;; LAYOUT

;; winner
(winner-mode 1)

;; remove useless GUI elements
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; visible error notification
(setq visible-bell t)

(tooltip-mode t)

(use-package popup)

;; ;; change window size automatically on focus
;; (use-package golden-ratio
;;   :diminish)
;; (golden-ratio-mode 1)
;; ;; Fix golden-ratio conflict with helm-autoresize
;; ;; source: https://tuhdo.github.io/helm-intro.html
;; (defun pl/helm-alive-p ()
;;   "Fix golden-ratio conflict with helm-autoresize."
;;   (if (boundp 'helm-alive-p)
;;       (symbol-value 'helm-alive-p)))
;; (add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

(set-frame-font "Source Code Pro for Powerline Medium 10" nil t)

;; line number on left fringe
(if (version<= "26.0.50" emacs-version )
    (global-display-line-numbers-mode)
  (global-linum-mode))

(line-number-mode t)        ;; line number on mode line
(column-number-mode t)      ;; column number on mode line
(global-hl-line-mode 1)     ;; highlight current line

;; transparency
(set-frame-parameter (selected-frame) 'alpha 95)

;; smart-mode-line
(use-package smart-mode-line
  :config (progn
	    (setq sml/theme nil)
	    (sml/setup)))

;; moe
(use-package moe-theme)
(moe-dark) ;; moe isn't just a theme, so it could be called this way
(load-theme 'moe-dark t)

;; powerline
(use-package powerline)
(add-hook 'after-change-major-mode-hook 'powerline-moe-theme)
(powerline-moe-theme)

;; (moe-flycheck-mode-line)
(add-to-list 'load-path "~/.emacs.d/git/moe-flycheck-mode-line")
(require 'moe-flycheck-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'moe-flycheck-mode-line-mode))
;; Override moe-theme to show keywords in bold
(set-face-attribute 'font-lock-keyword-face nil :bold t)


;; smart-mode-line directory prefixes
;; Cakephp 2 folders
(add-to-list 'sml/replacer-regexp-list '("^~/www/.*/webroot/app/Config/" ":Cnfg:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/www/.*/webroot/app/Controller/" ":Ctlr:") t)
(add-to-list 'sml/replacer-regexp-list '("^:Ctlr:Component/" ":Cmpt:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/www/.*/webroot/app/Model/" ":Mdl:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/www/.*/webroot/app/Model/Behavior/" ":Bhvr:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/www/.*/webroot/app/Plugin/" ":Plgn:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/www/.*/webroot/app/Vendor/" ":Vndr:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/www/.*/webroot/app/View/" ":View:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/www/.*/webroot/app/View/Helper/" ":Hlpr:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/www/.*/webroot/app/View/Elements/" ":Lmts:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/www/.*/webroot/app/webroot/" ":wr:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/www/.*/webroot/app/webroot/css/" ":css:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/www/.*/webroot/app/webroot/js/" ":js:") t)

;; Hide modes in mode line
(diminish 'visual-line-mode)
(diminish 'abbrev-mode)
(global-hi-lock-mode t)
(diminish 'hi-lock-mode)
(diminish 'isearch-mode)
(diminish 'auto-revert-mode)
(diminish 'git-gutter-mode)


;; DOCS
;; TODO: move all docs packages together
(use-package zeal-at-point
  :bind ("C-c z" . zeal-at-point))

(use-package tldr)

;; SERVICES
(use-package docker)
(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package docker-tramp
  :config
  (setq docker-tramp-use-names t))

(use-package restclient)

(use-package wttrin)
(setq wttrin-default-cities '("Neuquén?lang=es" "Mendoza?lang=es"))


;; OTHER TOOLS

;; Plantuml
(use-package plantuml-mode
  :init
  (setq plantuml-jar-path (concat user-emacs-directory "plantuml.jar"))
  (setq org-plantuml-jar-path plantuml-jar-path)
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))
(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages
			       (append org-babel-load-languages
				       '((plantuml . t)
					 (octave . t)))))
(if (not (file-exists-p plantuml-jar-path))
    (plantuml-download-jar))

;; ORG-MODE
(setq org-log-done t) ;; Add timestamp when done
;; key bindings
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;; config
(setq org-directory "~/org")
(setq org-agenda-files (directory-files-recursively org-directory ""))

(setq org-default-notes-file (concat org-directory "/refile.org"))
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))

;; Enable narrow-to-region
;; (narrow: C-x n n )
;; (widen: C-x n w)
(put 'narrow-to-region 'disabled nil)
;;; init.el ends here
