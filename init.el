;;; package --- Summary
;;; Commentary:
;;; GNU Emacs configuration file, by Matías Croce

;;; Code:

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
;; Emacs built-in TLS isn't working? see:
;; https://www.reddit.com/r/emacs/comments/3sjdyi/your_text_editor_is_malware/cxfol83
(if (fboundp 'gnutls-available-p)
	(fmakunbound 'gnutls-available-p))

;; Add package sources
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
						 ("marmalade" . "https://marmalade-repo.org/packages/")
						 ("melpa" . "https://melpa.org/packages/")
						 ))
(package-initialize)

(setq load-prefer-newer t)
(add-to-list 'load-path "~/elisp")
(unless (package-installed-p 'use-package)
  (progn
	(package-refresh-contents)
	(package-install 'use-package)))
(setq use-package-verbose t)

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(use-package auto-compile
  :ensure t)
(auto-compile-on-load-mode 1)


;; ENVIRONMENT DATA
(setq current-language-environment "Spanish")

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


;; EMACS USAGE

;; dired
(use-package dired+
  :ensure dired+)
;; Do not open new buffers when visiting new directories
(diredp-toggle-find-file-reuse-dir 1)

;; editorconfig (editorconfig.org)
(use-package editorconfig
  :ensure editorconfig)

;; Ask "y" or "n" instead of "yes" or "no"
(fset 'yes-or-no-p 'y-or-n-p)

;; search matches counter
(use-package anzu
  :ensure anzu
  :diminish anzu-mode)
(global-anzu-mode +1)

;; Magit!! :D
(use-package magit
  :ensure magit)
(global-set-key (kbd "C-x g") 'magit-status)
;; git-timemachine! :D
(use-package git-timemachine
  :ensure git-timemachine)

;; Helm
;; Copied from Sacha Chua:
;; http://pages.sachachua.com/.emacs.d/Sacha.html#unnumbered-14
(use-package helm
  :ensure helm
  :diminish helm-mode
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
		helm-buffers-fuzzy-matching t
		helm-recentf-fuzzy-match t
		;; helm-autoresize-mode 1
		helm-ff-file-name-history-use-recentf t)
	  (helm-mode))
  :bind (("C-c h" . helm-mini)
	 ("C-h a" . helm-apropos)
	 ("C-x C-b" . helm-buffers-list)
	 ("C-x b" . helm-buffers-list)
	 ("M-y" . helm-show-kill-ring)
	 ("M-x" . helm-M-x)
	 ("C-x c o" . helm-occur) ;; search in current buffer
	 ("C-x c s" . helm-swoop) ;; search in current buffer
	 ("C-x c m" . helm-multi-swoop-all) ;; search in all buffers
	 ("C-x c SPC" . helm-all-mark-rings)))
(helm-autoresize-mode t)

(use-package helm-ag
  :ensure helm-ag)
(defun projectile-helm-ag ()
  "Searching with silversearcher-ag inside projects with projectile."
  (interactive)
  (helm-ag (projectile-project-root)))

(use-package helm-swoop
  :ensure helm-swoop)

;; For describing bindings
(use-package helm-descbinds
  :ensure helm-descbinds
  :defer t
  :bind (("C-h b" . helm-descbinds)
	 ("C-h w" . helm-descbinds)))

;; Projectile
(use-package projectile
  :ensure projectile)
(projectile-global-mode)
;; (projectile-enable-caching t)

;; Make projectile search locally only for SVN repos. See:
;; https://github.com/bbatsov/projectile/issues/520
(setq projectile-svn-command "find . -type f -not -iwholename '*.svn/*' -print0")

;; Helm-Projectile integration
(use-package helm-projectile
  :ensure helm-projectile)
(setq projectile-completion-system 'helm)
(helm-projectile-on)


;; CODE EDITING

;; syntax highlighting for all buffers
(global-font-lock-mode t)

;; Visual line (soft-word-wrap)
(global-visual-line-mode t)

;; Highlight corresponding parentheses when cursor is on one
(show-paren-mode t)
(setq show-paren-style 'expression)

;; Undo tree
(use-package undo-tree
  :ensure undo-tree)
(global-undo-tree-mode)

;; Highlight nested parens, brackets, braces a different color at each depth.
(use-package rainbow-delimiters
  :ensure rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; agressive indent! :D
(use-package aggressive-indent
  :ensure aggressive-indent)
(global-aggressive-indent-mode)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

;; multiple-cursors
(use-package multiple-cursors
  :ensure multiple-cursors)
(require 'multiple-cursors)

;; ace-jump
(use-package ace-jump-mode
  :ensure ace-jump-mode)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
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
  :ensure yasnippet
  :diminish yas-minor-mode)
;; (yas-global-mode t)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; TODO: use yasnippet to do this
(defun insert-doc-comment ()
  "To insert doc comments on php functions."
  (interactive)
  (insert "/**\n * TODO add description. \n * \n * @param \n * @return \n */"))
(define-key global-map [(S-f1)] 'insert-doc-comment) ;; shift + F1


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
  :ensure company
  :diminish company-mode
  :init (progn
	  (add-hook 'after-init-hook 'global-company-mode)
	  (setq company-auto-complete nil)
	  (global-company-mode t)
	  (add-to-list 'company-backends 'company-dabbrev t)
	  (add-to-list 'company-backends 'company-ispell t)
	  ;;(add-to-list 'company-backends 'company-css t)
	  (add-to-list 'company-backends 'company-files t)))
;; 0.1 second delay before the pop-up appears
(setq company-idle-delay 0.1)
;; only one character before auto-completion starts
(setq company-minimum-prefix-length 1)
(setq company-dabbrev-downcase nil)

(global-set-key (kbd "C-c y") 'company-yasnippet)

;; autopair
(use-package autopair
  :ensure autopair
  :diminish autopair-mode
  :init (progn
	  (autopair-global-mode t)))

;; flycheck (syntax checking)
(use-package flycheck
  :ensure flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)  ;; globally enable flycheck
(use-package flycheck-pos-tip ;; show error messages with popup.el
  :ensure flycheck-pos-tip)
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))
(use-package helm-flycheck
  :ensure helm-flycheck)


;;; Highlights poor English writing.
;;(use-package writegood-mode
;;  :load-path "lisp/writegood-mode/"
;;  :bind ("C-c n g" . writegood-mode))


;; idle-highlight-mode (highlight current word ocurrences)
(use-package idle-highlight-mode
  :ensure idle-highlight-mode
  :init (progn
	  (add-hook 'emacs-lisp-mode-hook (lambda () (idle-highlight-mode t)))
	  (add-hook 'js3-mode-hook (lambda () (idle-highlight-mode t)))
	  (add-hook 'php-mode-hook (lambda () (idle-highlight-mode t))))
  )

;; LANGUAGES

;; markdown-mode
(use-package markdown-mode
  :ensure markdown-mode)

;; web-mode (php,js,html,css)
(use-package web-mode
  :ensure web-mode)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq-default tab-width 4)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))  ;; html
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode)) ;; cake views
(add-hook 'sgml-mode-hook 'web-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'web-mode)
(add-hook 'css-mode-hook 'web-mode)
(add-hook 'js-mode-hook 'web-mode)
(setq web-mode-enable-current-element-highlight t) ;; highlight current element
(setq web-mode-enable-current-column-highlight t) ;; highlight current col

;; JS
;; js3 (javascript editing)
(use-package js3-mode
  :ensure js3-mode)
(add-to-list 'load-path "~/www/utilidades/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(use-package company-tern
  :ensure company-tern)
(add-to-list 'company-backends 'company-tern)
(tern-mode t)
(add-hook 'js3-mode-hook (lambda () (tern-mode t)))
(add-hook 'web-mode-hook (lambda () (tern-mode t)))

;; jquery documentation
(use-package jquery-doc
  :ensure jquery-doc)
(add-hook 'javascript-mode-hook 'jquery-doc-setup)

;; emmet (zen coding)
(use-package emmet-mode
  :ensure emmet-mode
  :diminish emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(use-package helm-emmet
  :ensure helm-emmet)

;; lorem ipsum text filler
(use-package lorem-ipsum
  :ensure t)
;; use default bindings
;; C-c l p	lorem-ipsum-insert-paragraphs
;; C-c l s	lorem-ipsum-insert-sentences
;; C-c l l	lorem-ipsum-insert-list
(lorem-ipsum-use-default-bindings)

;; CSS
;; css documentation
(use-package css-eldoc
  :ensure css-eldoc
  :init(progn
		 (eldoc-mode 1)))
(add-hook 'css-mode-hook 'turn-on-css-eldoc)
(add-to-list 'company-backends 'company-css t)

;; rainbow (displays strings representing colors with the color they represent)
(use-package rainbow-mode
  :ensure rainbow-mode
  :init (progn
	  (add-hook 'css-mode-hook (lambda () (rainbow-mode 1)))))

;; Stylus
;; (use-package stylus-mode
;;   :ensure stylus-mode)
;; (add-to-list 'auto-mode-alist '("\\.styl\\'" . stylus-mode))  ;; styl

;; python documentation
;;(add-hook 'python-mode-hook 'turn-on-eldoc-mode)
;; (use-package anaconda-mode
;;   :ensure anaconda-mode
;;   :init(progn
;;	 (eldoc-mode 1)))
;; (use-package company-anaconda
;;   :ensure company-anaconda)
;; (add-to-list 'company-backends 'company-anaconda)

;; PHP
;; php-mode
;; TODO: make doc comments to not indent. Make indentation to always use tabs
(use-package php-mode
  :ensure php-mode
  :init(progn
	 (setq-default flycheck-phpcs-standard "CakePHP")
	 (setq-default php-manual-path "~/www/utilidades/docs/php5/php-manual/") ;; php docs local copy
	 ;;(eldoc-mode 1)
	 ;;(php-eldoc-enable t)
	 ))
;; set psr-2 coding style
(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)

;; php-auto-yasnippet
(use-package php-auto-yasnippets
  :ensure php-auto-yasnippets)
(require 'php-auto-yasnippets)
(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)

;; php-eldoc
(use-package php-eldoc
  :ensure php-eldoc
  :init(progn
	 (eldoc-mode 1)))

;; php-extras
(use-package php-extras
  :ensure php-extras)

;; I'm not using it (find a way to use it correctly with cakephp)
;; geben (to debug php with xdebug)
;; (use-package geben
;;   :ensure geben)
;; (autoload 'geben "geben" "DBGp protocol frontend, a script debugger" t)


;; 80 column indicator
(use-package fill-column-indicator
  :ensure fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode t)))
(setq-default fci-rule-column 80)
(global-fci-mode t)

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

;; remove useless GUI elements
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; visible error notification
(setq visible-bell t)

(tooltip-mode t)

(use-package popup
  :ensure popup)

;; change window size automatically on focus
(use-package golden-ratio
  :ensure golden-ratio
  :diminish golden-ratio-mode
  )
(golden-ratio-mode 1)
;; Fix golden-ratio conflict with helm-autoresize
;; source: https://tuhdo.github.io/helm-intro.html
(defun pl/helm-alive-p ()
  "Fix golden-ratio conflict with helm-autoresize."
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))
(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

;; Font
;; Fira Mono
(add-to-list 'default-frame-alist '(font . "Fira Mono 11" ))
(set-face-attribute 'default t :font "Fira Mono 11" )

(global-linum-mode 1)       ;; line number on left fringe
(line-number-mode t)        ;; line number on mode line
(column-number-mode t)      ;; column number on mode line
(global-hl-line-mode 1)     ;; highlight current line

;; transparency
(set-frame-parameter (selected-frame) 'alpha 95)

;; smart-mode-line
(use-package smart-mode-line
  :ensure smart-mode-line
  :config (progn
	    (setq sml/theme nil)
	    (sml/setup)))

;; moe
(use-package moe-theme
  :ensure moe-theme)
(moe-dark) ;; moe isn't just a theme, so it could be called this way
(load-theme 'moe-dark t)

;; powerline
(use-package powerline
  :ensure powerline)
(add-hook 'after-change-major-mode-hook 'powerline-moe-theme)
(powerline-moe-theme)

;; (moe-flycheck-mode-line)
(add-to-list 'load-path "~/.emacs.d/git/moe-flycheck-mode-line")
(require 'moe-flycheck-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'moe-flycheck-mode-line-mode))


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

;; DOCS
;; TODO: move all docs packages together
(use-package zeal-at-point
  :ensure t
  :bind ("C-c z" . zeal-at-point))

(use-package tldr
  :ensure tldr)

;; SERVICES
(use-package docker
  :ensure docker)

;;; init.el ends here
