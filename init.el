;; This is Jmacs - Jerry's interpretation of GNU Emacs!
;; Author:   Jerry Starke 
;; Created:  January 09, 2024
;; Modified: January 12, 2024
;; Version:  0.1
;; Build:    1A96
;; Homepage: https://github.com/JerrySM64

;; Install MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;; Make Emacs stop putting stuff into this file
(setq custom-file (make-temp-file "emacs-custom"))
(load custom-file 'noerror)


;; Evil Mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-vsplit-window-right t
	evil-split-window-below t)
  (evil-mode))
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list '(dashbaord dired ibuffer))
  (evil-collection-init))
(use-package evil-tutor
  :ensure t)


;; Configuration of the general package
(use-package general
  :ensure t
  :config
  (general-evil-setup)

  ;;; Keybinding Definers
  (general-create-definer jmacs/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  ;;; Keybindings per definer
  (jmacs/leader-keys
    "b" '(:ignore t :wk "Buffer")
    "b i" '(ibuffer :wk "Open ibuffer")
    "b k" '(kill-this-buffer :wk "Kill this buffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b s" '(switch-to-buffer :wk "Switch to buffer"))

  (jmacs/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t t" '(visual-line-mode :wk "Toggle truncated lines")
    "t v" '(vterm-toggle :wk "Toggle vterm"))

  (jmacs/leader-keys
    "E" '(:ignore t :wk "Evaluate")
    "E b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "E d" '(eval-defun :wk "Evaluate defun containing or after point")
    "E e" '(eval-expression :wk "Evaluate an elisp expression")
    "E l" '(eval-last-expression :wk "Evaluate elisp expression before point")
    "E r" '(eval-region :wk "Evaluate elisp in region"))

  (jmacs/leader-keys
    "." '(find-file :wk "Open file")
    "TAB TAB" '(comment-line :wk "Comment lines"))

  (jmacs/leader-keys
    "h" '(:ignore t :wk "Help")
    "h f" '(describe-function :wk "Describe function")
    "h v" '(describe-variable :wk "Describe variable")
    "h r r" '(reload-jmacs :wk "Reload Jmacs"))

  (jmacs/leader-keys
    "T" '(:ignore t :wk "Tabs")
    "T d" '(tab-detach :wk "Detach this tab")
    "T n" '(tab-new :wk "Create a new tab")
    "T w" '(tab-close :wk "Close this tab"))

  (jmacs/leader-keys
    "o" '(:ignore t :wk "Open")
    "o c" '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :wk "Edit Jmacs")
    "o f" '(find-file :wk "Open file")
    "o r" '(counsel-recentf :wk "Open recent file"))

  (jmacs/leader-keys
    "i" '(:ignore t :wk "Ivy")
    "i b s" '(ivy-switch-buffer-other-window :wk "Switch Ivy buffer to other window")
    "i r" '(ivy-resume :wk "Ivy Resume"))

  (jmacs/leader-keys
    "w" '(:ignore t :wk "Virtual windows")
    "w c" '(evil-window-delete :wk "Close window")
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    "w w" '(evil-window-next :wk "Goto next window"))

  (jmacs/leader-keys
    "e" '(:ignore t :wk "Eshell")
    "e s" '(eshell :wk "Open Eshell")
    "e h" '(counsel-esh-history :wk "Eshell history")))


;; Create reload-jmacs function
  (defun reload-jmacs ()
    (interactive)
    (load-file user-init-file)
    (load-file user-init-file))


;; Which Key
(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :config
  (setq which-key-side-window 'bottom
	which-key-sort-order 'which-key-key-order-alpha
	which-key-sort-uppercase-first nil
	which-key-add-column-padding 1
	which-key-max-display-columns nil
	which-key-min-display-lines 6
	which-key-side-window-slot -10
	which-key-side-window-max-height 0.25
	which-key-idle-delay 0.6
	which-key-max-description-length 25
	which-key-allow-imprecise-window-fit t
	which-key-allow-evil-operators t
        which-key-separator "  " ))


;; Ivy and Counsel
(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode))

(use-package ivy
  :ensure t
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode)
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<tab>") 'ivy-alt-done))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1)
  :custom
  (ivy-virtualabbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
			       'ivy-rich-switch-buffer-transformer))


;; sudo-edit
(use-package sudo-edit
  :ensure t
  :config
  (jmacs/leader-keys
    "f" '(:ignore t :wk "Invoke sudo (root)")
    "f ." '(sudo-edit-find-file :wk "Open file (root)")
    "f e" '(sudo-edit :wk "Edit file (root)")))


;; Shell and Terminal Support
(use-package eshell-syntax-highlighting
  :ensure t
  :after eshell-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
      eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "btop" "fish" "htop" "ssh" "top" "zsh"))

(use-package vterm
  :ensure t
  :config
  (setq vterm-max-scrollback 5000))

(use-package vterm-toggle
  :ensure t
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
	       '((lambda (buffer-or-name _)
		   (let ((buffer (get-buffer buffer-or-name)))
		     (with-current-buffer buffer
		       (or (equal major-mode 'vterm-mode)
			   (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
		 (display-buffer-reuse-window display-buffer-at-bottom)
		 (reusable-frames - visible)
		 (window-height . 0.3))))


;; Rainbow Mode
(use-package rainbow-mode
  :ensure
  :hook org-mode prog-mode)


;; Aditional language support
;;; Go
(use-package go-mode
  :ensure t
  :hook
  (go-mode . (lambda ()
               ;; Customize according to your preferences
               (setq tab-width 4)
               (setq indent-tabs-mode t))))

(use-package go-eldoc
  :ensure t
  :hook (go-mode . go-eldoc-setup))

(use-package go-rename
  :ensure t)

(use-package go-guru
  :ensure t
  :config
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

;;; Haskell
(use-package haskell-mode
  :ensure t
  :hook
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . haskell-indentation-mode)
  (haskell-mode . haskell-doc-mode)
  (haskell-mode . turn-on-haskell-indentation))

(use-package flycheck-haskell
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(use-package hindent
  :ensure t
  :hook (haskell-mode . hindent-mode))

;;; HTML & CSS
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-current-element-highlight t)
  (add-hook 'web-mode-hook 'emmet-mode))

(use-package company-web
  :ensure t
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-web-html))
              (company-mode))))

;;; JavaScript
(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode))
  :config
  (setq js-indent-level 2)
  (setq js2-basic-offset 2)
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-strict-trailing-comma-warning nil)
  (setq js2-missing-semi-one-line-override nil))

;;; Julia
(use-package julia-mode
  :ensure t)

(use-package julia-repl
  :ensure t
  :hook (julia-mode . julia-repl-mode))

(use-package flycheck-julia
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-julia-setup))

;;; Lua
(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-mode)
  :config
  (setq lua-indent-level 2)
  (setq lua-indent-string-contents t))

;;; Nix
(use-package nix-mode
  :ensure t
  :defer t
  :mode ("\\.nix\\'" "\\.nix.in\\'"))
(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")
(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))
(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl))

;;; Node
(use-package nodejs-repl
  :ensure t)

;;; OCaml
(use-package tuareg
  :ensure t
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode))
  :config
  (setq tuareg-indent-align-with-first-arg t)
  (setq tuareg-italic-comments t)
  (setq tuareg-prettify-symbols-full t)
  (setq tuareg-font-lock-symbols nil)
  (setq merlin-command 'opam)
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (add-hook 'caml-mode-hook 'merlin-mode))

(use-package utop
  :ensure t
  :hook (tuareg-mode . utop-minor-mode))

(use-package merlin
  :ensure t)

(use-package flycheck-ocaml
  :ensure t)

;;; PHP
(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-mode)
  :config
  (setq php-mode-coding-style 'psr2)
  (setq php-template-compatibility nil))

(use-package phpunit
  :ensure t)

(use-package composer
  :ensure t)

;;; Scala
(use-package scala-mode
  :ensure t
  :interpreter
  ("scala" . scala-mode))

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command)

;;; TOML
(use-package toml-mode
  :ensure t
  :mode ("\\.toml\\'" . toml-mode))

;;; Rust
(use-package racer
  :ensure t
  :hook (rust-mode . racer-mode))

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (setq rust-indent-offset 2)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;;Zig
(use-package zig-mode
  :ensure t
  :mode ("\\.zig\\'" . zig-mode))

;;; Flycheck for syntax checking
(use-package flycheck
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'flycheck-mode)
  (add-hook 'lua-mode-hook #'flycheck-mode)
  (add-hook 'tuareg-mode-hook #'flycheck-mode)
  (add-hook 'php-mode-hook #'flycheck-mode)
  (add-hook 'scala-mode-hook #'flycheck-mode)
  (add-hook 'zig-mode-hook #'flycheck-mode))


;; Set default font
(set-face-attribute 'default nil
  :font "JetBrains Mono"
  :height 100
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "JetBrains Mono"
  :height 110
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "JetBrains Mono"
  :height 100
  :weight 'medium)
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-10"))
(setq-default line-spacing 0.1)


;; Get Icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))


;; Default GUI Theme
(use-package doom-themes
  :ensure t
  :config
  (setq custom-safe-themes t)
  (load-theme 'doom-challenger-deep t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))


;;Default GUI Dashboard
(use-package savehist
  :init
  (setq savehist-additional-variables '(projectile-project-type projectile-project-root))
  (savehist-mode 1))

(use-package projectile
  :ensure t)

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-display-icons-p t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Welcome to Jmacs!")
  (setq dashboard-startup-banner (concat user-emacs-directory "jmacs-logo.png"))
  (setq dashboard-center-content t)
  (setq dashboard-show-items t)
  (setq dashboard-items '((recents . 7)
			  (projects . 7)))
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))


;; Default GUI settings
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)
(tab-bar-mode 1)
(add-to-list 'default-frame-alist '(alpha-background . 80))
