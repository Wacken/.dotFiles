(defconst emacs-config-path "~/.config/emacs/")

(setq byte-compile-warnings '(cl-functions)) ; ignore Package cl is depcrecated warning
(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . ,(concat emacs-config-path "backups"))) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms `((".*" ,(concat emacs-config-path "auto-save-list/") t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(global-auto-revert-mode)
(setq auto-revert-verbose nil)
(setq save-interprogram-paste-before-kill t)
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-scratch-message "Welcome in Emacs") ; print a default message in the empty scratch buffer opened at startup 
(setq inhibit-startup-message t)
(setq menu-bar-mode nil)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(global-hl-line-mode)
(setq blink-cursor-mode nil)
(setq scroll-bar-mode nil)
(setq tool-bar-mode nil)
(fset 'yes-or-no-p 'y-or-n-p) ; instead af yes or no type onl y or p
(defalias 'list-buffers 'ibuffer)

(use-package general :ensure t)

(defconst my-leader "SPC")
(general-create-definer my-leader-def
  :prefix my-leader
  :non-normal-prefix (concat "C-" my-leader))

(my-leader-def
  :states '(normal visual insert emacs)
  "." 'counsel-find-file
  "SPC" 'counsel-M-x
  "RET" 'counsel-bookmark)
(general-define-key
 :states '(normal visual insert emacs)
 "<f5>" 'revert-buffer
 "M-y" 'counsel-yank-pop)
(general-define-key
 :states '(normal visual)
 "gs" 'avy-goto-char-timer
 "/" 'swiper
 "C-+" 'er/expand-region
 "C--" 'er/contract-region)
;; (general-define-key
;;  :states '(insert emacs)
;;  :keymaps 'yas-minor-mode-map
;;  "SPC" 'yas-maybe-expand)

(my-leader-def
   :states '(normal visual insert emacs)
   "f" '(:ignore t :which-key "files")
   "ff" 'counsel-find-file
   "fs" 'save-buffer
   "fr" 'counsel-recentf
   )

(my-leader-def
   :states '(normal visual insert emacs)
   "b" '(:ignore t :which-key "buffers")
   "bl" 'list-buffers
   "bs" 'switch-to-buffer
   "bc" 'kill-current-buffer
   )

(my-leader-def
  :states '(normal visual insert emacs)
  "w" '(:ignore t :which-key "windows")
  "wv" 'evil-window-vsplit
  "wa" 'ace-window
  "wo" 'delete-other-windows
  "wc" 'ace-delete-window
  "ws" 'ace-swap-window)

(my-leader-def
   :states '(normal visual insert emacs)
   "l" '(:ignore t :which-key "lisp")
   "ll" 'eval-last-sexp
   "lb" 'eval-buffer
   )

(use-package hydra
  :ensure t)

(use-package diminish
  :ensure t)

(use-package try
  :commands (try)
  :ensure t)

(use-package which-key
  :ensure t
  :diminish which-key-mode 
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1
	which-key-show-operator-state-maps t)
  )

(use-package evil
  :ensure t
  :config
  (evil-mode))

(setq evil-emacs-state-modes nil)
;; don't put into normal mode in REPL (setq evil-insert-state-modes nil)
(setq evil-motion-state-modes nil)

(use-package counsel
  :diminish (ivy-mode counsel-mode)
  :ensure t
  :config
  (ivy-mode)
  (counsel-mode)
  (setq ivy-height 20)
  (setq ivy-count-format "(%d/%d)")
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq search-default-mode #'char-fold-to-regexp))
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(use-package ivy-prescient
  :after counsel
  :ensure t
  :config
  (setq ivy-prescient-retain-classic-highlighting t)
  (prescient-persist-mode)
  (ivy-prescient-mode))

(use-package ivy-rich
  :ensure t)

(use-package ace-window
  :ensure t
  :commands (ace-window ace-delete-window ace-swap-window)
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))))
(setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))

(use-package avy
  :commands avy-goto-char-timer
  :ensure t
  )

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package persp-mode
  :ensure t
  :config
  (persp-mode))

;; (use-package tabbar
;;   :ensure t
;;   :config
;;   (tabbar-mode 1))

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode))

(use-package expand-region
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package iedit
  :ensure t)

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (beacon-mode))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))
(use-package flycheck-pos-tip
  :ensure t
  :after flycheck)

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-require-match nil)
  (add-to-list 'company-backends 'company-omnisharp)
  ;;(add-to-list 'company-backends #'company-tabnine)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-quickhelp
  :ensure t
  :config 
  (company-quickhelp-mode))

(use-package company-box
  :diminish company-box-mode
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package company-statistics
  :ensure t
  :config
  (add-hook 'after-init-hook 'company-statistics-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'csharp-mode-hook #'aggressive-indent-mode)
  (add-hook 'python-mode-hook #'aggressive-indent-mode))
;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
	  '(defaults       ; should be included.
	     pretty-parens  ; different paren styles for different modes.
	     evil           ; If you use Evil.
	     ;;lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
	     ;;paredit        ; Introduce some paredit commands.
	     smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
	     smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

(use-package omnisharp
  :ensure t
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook #'company-mode)
  (add-hook 'csharp-mode-hook #'flycheck-mode))

(defun my-csharp-repl ()
    "Switch to the CSharpRepl buffer, creating it if necessary."
    (interactive)
    (if-let ((buf (get-buffer "*CSharpRepl*")))
	(pop-to-buffer buf)
      (when-let ((b (make-comint "CSharpRepl" "csharp")))
	(switch-to-buffer-other-window b))))
;; (define-key csharp-mode-map (kbd "C-c C-z") 'my-csharp-repl)

(use-package cider
  :ensure t
  :config
  (setq
   cider-repl-history-file ".cider-repl-history"  ;; not squiggly-related, but I like it
   nrepl-log-messages t))                          ;; not necessary, but useful for trouble-shooting

(use-package flycheck-clj-kondo
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(use-package cider-hydra
  :ensure t
  :config
  (add-hook 'clojure-mode #'cider-hydra-mode))

(use-package jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup))

(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list
	'("▶" "✚" "●" "◆" "◇"))
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package haskell-mode
  :ensure t)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-palenight t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode)
  :config
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-buffer-encoding nil)
  (column-number-mode))

;; (use-package powerline
;;   :ensure t
;;   :config
;;   (powerline-default-theme))

(use-package fira-code-mode
  :ensure t
  :config (global-fira-code-mode))

;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(use-package elfeed
  :ensure t)

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org))
