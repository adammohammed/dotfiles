;;; Init.el --- Emacs config
;;; Commentary:

;;; Code:
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-auto-revert-mode t)
(setq indent-tabs-mode nil)

(setq custom-file-dir "~/.emacs.d/")
(setq custom-file (concat custom-file-dir "custom.el"))
(load custom-file 'noerror)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package use-package
  :config
  (setq use-package-always-ensure t)
  (setq use-package-compute-statistics t)
  (setq use-package-verbose t))

(add-to-list 'load-path
	     (expand-file-name "private" user-emacs-directory))

(use-package ivy
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers 'bookmarks)
  (defun adam-ivy-format-function-prefix (cands)
    "Transform CANDS into a string with prefix on default candidate"
    (ivy--format-function-generic
     (lambda (str)
       (concat " " (ivy--add-face str 'ivy-current-match)))
     (lambda (str)
       (concat "   " str))
     cands
     "\n"))
  (setcdr (assq 't ivy-format-functions-alist) #'adam-ivy-format-function-prefix)
  :bind (("C-c C-r" . ivy-resume)))

(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)))

(use-package projectile
  :init
  (projectile-mode)
  :config
  (setq projectile-enable-caching t)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :init
  (counsel-projectile-mode 1))

;; Buffer navigation
(use-package ace-window
  :bind
  ("M-o" . 'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Magit VC
(use-package magit)

;; Better defaults
(setq save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      visible-bell t
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))
(show-paren-mode)
(global-display-fill-column-indicator-mode t)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (load-theme 'doom-nord t)
  (doom-themes-org-config))

(use-package all-the-icons)


(use-package doom-modeline
  :init
  (doom-modeline-mode))


(use-package markdown-mode)

(use-package flycheck
  :init (global-flycheck-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Complete Anything
(use-package company
  :ensure t
  :config
  (global-company-mode t)
  :hook ((python-mode) . company-mode))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode))

(use-package yasnippet
  :init (yas-global-mode 1))

(use-package yasnippet-snippets)

;; IDE features
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"))

(use-package dap-mode
  :config
  (dap-auto-configure-mode)
  (require 'dap-python))

;; Python
(use-package blacken
  :hook
  (python-mode . blacken-mode))

(use-package python-pytest
  :bind (("M-t" . python-pytest-function-dwim)
         ("M-T" . python-pytest-file-dwim)))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package lsp-pyright
  :hook
  (python-mode . (lambda ()
		   (require 'lsp-pyright)
		   (lsp))))


;; Golang
(use-package go-mode
  :hook
  ((go-mode . lsp-deferred)))

;;  Terraform
(use-package terraform-mode)

;; OpenAPI
(require 'swagger-mode)

;; Yaml
(use-package yaml-mode)


(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))


;; Lua
(use-package lua-mode
  :defer t
  :ensure t
  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))
(defun lua-busted-indent-fix ()
  (save-excursion
    (lua-forward-line-skip-blanks 'back)
    (let* ((current-indentation (current-indentation))
           (line (thing-at-point 'line t))
           (busted-p (s-matches?
                      (rx (+ bol (* space)
                             (or "context" "describe" "it" "setup" "teardown")
                             "("))
                      line)))
          (when busted-p
            (+ current-indentation lua-indent-level)))))

(defun rgc-lua-calculate-indentation-override (old-function &rest arguments)
  (or (lua-busted-indent-fix)
      (apply old-function arguments)))

(advice-add #'lua-calculate-indentation-override
            :around #'rgc-lua-calculate-indentation-override)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
