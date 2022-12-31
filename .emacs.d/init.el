;;; init.el --- Initialization file for Emacs
;;; Commentary:

;;; Code:
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-auto-revert-mode t)
(setq indent-tabs-mode nil)

(defvar custom-file-dir "~/.emacs.d/" "Default directory to store custom.el file.")
(setq custom-file (concat custom-file-dir "custom.el"))
(load custom-file 'noerror)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(add-to-list 'load-path
	     (expand-file-name "private" user-emacs-directory))

(straight-use-package 'use-package)

(defun xah-fill-or-unfill ()
  "Reformat current paragraph or region to `fill-column' stuff."
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
  :straight t
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
  :straight t
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)))

(use-package projectile
  :straight t
  :init
  (projectile-mode)
  :config
  (setq projectile-enable-caching t)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :straight t
  :init
  (counsel-projectile-mode 1))

;; Buffer navigation
(use-package ace-window
  :straight t
  :bind
  ("M-o" . 'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Magit VC
(use-package magit
  :straight t)

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
  :straight t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (load-theme 'doom-nord t)
  (doom-themes-org-config))

(use-package all-the-icons
  :straight t)


(use-package doom-modeline
  :straight t
  :init
  (doom-modeline-mode))

;; Easy access to configs
(defun adam/find-init-file ()
  "This function opens up the init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun adam/reload-init-file ()
  "This function reloads the init.el file anywhere."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-c m i") 'adam/find-init-file)
(global-set-key (kbd "C-c m r") 'adam/reload-init-file)

(use-package markdown-mode
  :straight t)

(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Complete Anything
(use-package company
  :straight t
  :config
  (global-company-mode t)
  :hook ((python-mode) . company-mode))

(use-package company-quickhelp
  :straight t
  :config
  (company-quickhelp-mode))

(use-package yasnippet
  :straight t
  :init (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t)

;; IDE features
(use-package eglot
  :straight t
  :hook
  (go-mode . eglot-ensure))

;; Python
(use-package blacken
  :straight t
  :hook
  (python-mode . blacken-mode))

(use-package python-pytest
  :straight t
  :bind (("M-t" . python-pytest-function-dwim)
         ("M-T" . python-pytest-file-dwim)))

(use-package pyvenv
  :straight t
  :config
  (pyvenv-mode 1))

(use-package lsp-pyright
  :straight t
  :hook
  (python-mode . (lambda ()
		   (require 'lsp-pyright)
		   (lsp))))


;; Golang
(use-package go-mode
  :straight t)

;;  Terraform
(use-package terraform-mode
  :straight t)

;; Yaml
(use-package yaml-mode
  :straight t)


(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))


;; Lua
(use-package lua-mode
  :straight t
  :defer t
  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))
(defun lua-busted-indent-fix ()
  "Fix issue with lua indenting."
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
  "Lua hook to fix indentation before passing to OLD-FUNCTION with ARGUMENTS."
  (or (lua-busted-indent-fix)
      (apply old-function arguments)))

(advice-add #'lua-calculate-indentation-override
            :around #'rgc-lua-calculate-indentation-override)

;; Make sure to get SSH_AUTH_SOCK from shell
(use-package exec-path-from-shell
  :straight t
  :config
  (dolist (var '("SSH_AUTH_SOCK"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))


;; DB Configs
(use-package sql
  :straight t
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


;; Rust
(use-package rustic
  :straight t
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

;; Dired hacks
(defun cert-info (filename bufname)
  "Show the details of a certificate given a FILENAME output to the buffer BUFNAME."
  (with-output-to-temp-buffer bufname
    (shell-command (format "openssl x509 -text -noout -in %s" filename) bufname)
    (pop-to-buffer bufname)
    (local-set-key (kbd "q") (quit-window t))))
(defun adam/dired-x509-info ()
  "Retrieve certificate information for file under point in Dired."
  (interactive)
  (let ((bufname (get-buffer-create "*cert-info*")))
    (cert-info (dired-get-file-for-visit) bufname)))

(define-key dired-mode-map (kbd "C-c t") 'adam/dired-x509-info)

;; Tramp configuration
(setq tramp-default-method "ssh")

;; Common-Lisp
(use-package slime
  :straight t
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package tree-sitter :straight t)
(use-package tree-sitter-langs :straight t)

(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold"))

(use-package docker-tramp
  :straight t)
