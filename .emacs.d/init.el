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

(defun xah-fill-or-unfill ()
  "Reformat current paragraph or region to `fill-column', like `fill-paragraph' or “unfill”.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.
URL `http://ergoemacs.org/emacs/modernization_fill-paragraph.html'
Version 2017-01-08"
  (interactive)
  ;; This command symbol has a property “'compact-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let ( ($compact-p
          (if (eq last-command this-command)
              (get this-command 'compact-p)
            (> (- (line-end-position) (line-beginning-position)) fill-column)))
         (deactivate-mark nil)
         ($blanks-regex "\n[ \t]*\n")
         $p1 $p2
         )
    (if (use-region-p)
        (progn (setq $p1 (region-beginning))
               (setq $p2 (region-end)))
      (save-excursion
        (if (re-search-backward $blanks-regex nil "NOERROR")
            (progn (re-search-forward $blanks-regex)
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (if (re-search-forward $blanks-regex nil "NOERROR")
            (progn (re-search-backward $blanks-regex)
                   (setq $p2 (point)))
          (setq $p2 (point)))))
    (if $compact-p
        (fill-region $p1 $p2)
      (let ((fill-column most-positive-fixnum ))
        (fill-region $p1 $p2)))
    (put this-command 'compact-p (not $compact-p))))

(global-set-key (kbd "M-Q") 'xah-fill-or-unfill)

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
  (setq lsp-keymap-prefix "C-c l")
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\venv\\'")
  (setq lsp-rust-server 'rust-analyzer))



(use-package lsp-ui :commands lsp-ui-mode)

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

;; Make sure to get SSH_AUTH_SOCK from shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (dolist (var '("SSH_AUTH_SOCK"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))


;; DB Configs
(use-package sql
  :config
  (sql-set-product-feature 'mysql :prompt-regexp "^\\(MariaDB\\|MySQL\\) \\[[_a-zA-Z]*\\]> ")
  (setq sql-connection-alist
	'((preprod-db
	   (sql-product 'mysql)
	   (sql-server "127.0.0.1")
	   (sql-port 43306)
	   (sql-user "<SOME_USER>")
	   (sql-password "<SOME_PASSWORD>")
	   (sql-database "hosting"))))
  (defun connect-alpha-db ()
    (interactive)
    (sql-connect 'alpha-db))
  (defun connect-dev-db ()
    (interactive)
    (setq sql-product 'mysql)
    (sql-connect 'dev-db))

  :hook
  ((sql-interactive-mode . (lambda () (toggle-truncate-lines t)))))


(defun generate-bapi-changelog (tag)
  (interactive "stag:")
  (progn
    (message (format "Trying to build changelog for %s in " tag default-directory))
    (with-output-to-temp-buffer "*bapi-changelog*"
      (shell-command (format "~/devenv/venv/bin/gitchangelog ...%s" tag) "*bapi-changelog*"))))
;; Rust

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
	      ("M-j" . lsp-ui-imenu)
	      ("M-?" . lsp-find-references)
	      ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-format-on-save t)
  :hook
  (rustic-mode . lsp-mode))
