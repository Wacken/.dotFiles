(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
       '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defconst init-org-path "~/.config/emacs-custom/myInit.org")

;; (defun tangle-init ()
;;   "If the current buffer is 'init.org' the code-blocks are
;;  tangled, and the tangled file is compiled."
;;   (when (equal (buffer-file-name)
;; 	       (expand-file-name (concat user-emacs-directory "init.org")))
;;     ;; Avoid running hooks when tangling.
;;     (let ((prog-mode-hook nil))
;;       (org-babel-tangle)
;;       (byte-compile-file (concat user-emacs-directory "init.el")))))

;; (add-hook 'after-save-hook 'tangle-init)

(org-babel-load-file (expand-file-name init-org-path))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8d7684de9abb5a770fbfd72a14506d6b4add9a7d30942c6285f020d41d76e0fa" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" default))
 '(org-directory "~/Files/Org")
 '(org-startup-folded 'overview)
 '(org-startup-indented t)
 '(package-selected-packages
   '(lispyville lispy yasnippet-snippets which-key use-package try smartparens rainbow-delimiters persp-mode peep-dired parinfer org-bullets omnisharp jedi ivy-rich ivy-prescient iedit hungry-delete haskell-mode ggtags general flycheck-pos-tip flycheck-clj-kondo fira-code-mode expand-region evil-org evil-collection elfeed-org elfeed-goodies dumb-jump doom-themes doom-modeline diredfl dired-rsync diminish dashboard counsel-projectile company-statistics company-quickhelp company-box clj-refactor cider-hydra beacon auto-package-update all-the-icons-ivy all-the-icons-dired aggressive-indent ace-window)))
   ;; '(smartparens smartparens-config dired-x all-the-icons-dired dired-rsync diredfl all-the-icons-ivy company-tng evil-org clj-refactor peep-dired dumb-jump elfeed-goodies paredit evil-collection auto-package-update haskell-mode elfeed-org elfeed persp-mode company-fuzzy company-statistics projectile projetile cider-hydra flycheck-clj-kondo flycheck-clojure aggressive-indent agressive-indent cider clojure-mode beacon iedit parinfer rainbow-delimiters rainbow-delimiter expand-region hungry-delete fira-code-mode omnisharp yasnippet-snippets yasnippet company-quickhelp flycheck org-tempo ivy-prescient doom-modeline hydra diminish general company company-mode ace-window counsel doom-themes evil org-bullets try use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "DAMA" :family "Fira Code"))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

(put 'dired-find-alternate-file 'disabled nil)
