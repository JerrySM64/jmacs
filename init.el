;; This is Jmacs - Jerry's interpretation of GNU Emacs!
;; Author:   Jerry Starke 
;; Created:  January 09, 2024
;; Modified: January 10, 2024
;; Version:  0.0.15
;; Build:    1A34
;; Homepage: https://github.com/JerrySM64

;;; Install Elpaca
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

;;; Evil Mode

;; Install a package via the elpaca macro
;; See the "recipes" section of the manual for more details.

;; (elpaca example-package)

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
;;(use-package general :demand t)
;;(elpaca-wait)

;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (evil-mode))
  (use-package evil-collection
    :after evil
    :config
    (setq evil-collection-mode-list '(dashbaord dired ibuffer))
    (evil-collection-init))
  (use-package evil-tutor)

;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause the declaration to be interpreted immediately (not deferred).
;;Useful for configuring built-in emacs features.
(use-package emacs :elpaca nil :config (setq ring-bell-function #'ignore))

;; Don't install anything. Defer execution of BODY
(elpaca nil (message "deferred"))

;;; Configuration of the general package
(use-package general
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
    "t" '(:ignore t :wk "Tabs")
    "t n" '(tab-new :wk "Create a new tab")
    "t d" '(tab-detach :wk "Detach this tab")
    "t w" '(tab-close :wk "Close this tab"))

  (jmacs/leader-keys
    "e" '(:ignore t :wk "Evaluate")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate an elisp expression")
    "e l" '(eval-last-expression :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region"))

  (jmacs/leader-keys
    "." '(find-file :wk "Open file")
    "TAB TAB" '(comment-line :wk "Comment lines"))

  (jmacs/leader-keys
    "h" '(:ignore t :wk "Help")
    "h f" '(describe-function :wk "Describe function")
    "h v" '(describe-variable :wk "Describe variable")
    "h r r" '(reload-jmacs :wk "Reload Jmacs"))

  (jmacs/leader-keys
    "T" '(:ignore t :wk "Toggle")
    "T l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "T t" '(visual-line-mode :wk "Toggle truncated lines"))

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
    "w" '(:ignore t :wk "Window"))
)

;;; Create reload-jmacs function
  (defun reload-jmacs ()
    (interactive)
    (load-file user-init-file)
    (load-file user-init-file))
    
;;; Which Key
(use-package which-key
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
        which-key-separator "  " ))

;;; Ivy and Counsel
(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package ivy
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

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

;;; sudo-edit
(use-package sudo-edit
  :config
  (jmacs/leader-keys
    "f" '(:ignore t :wk "Invoke sudo (force)")
    "f ." '(sudo-edit-find-file :wk "Open file (root)")
    "f e" '(sudo-edit :wk "Edit file (root)")))

;;; Set font
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

;;; Get Icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

;;; GUI settings
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
		
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)
(tab-bar-mode 1)
