;;; Init.el --- Emacs config
;;; Commentary:

;;; Code:
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Use-package configuration
(use-package use-package
  :config
  (setq use-package-always-ensure t)
  (setq use-package-compute-statistics t)
  (setq use-package-verbose t))

;; Buffer configuration
;; If buffers changed on disk reload automatically
(global-auto-revert-mode t)

;; Basic global config
(setq-default indent-tabs-mode nil)

(defun xah-unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

(global-set-key (kbd "M-Q") 'xah-unfill-paragraph)

;; Folder navigation

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
  :bind
  (("C-c C-r" . ivy-resume)))



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
  :ensure t
  :bind
  ("M-o" . 'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Magit VC
(use-package magit
  :ensure t)

(use-package fill-column-indicator
  :ensure t)

(global-set-key (kbd "M-/") 'company-complete)
(setq save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      visible-bell t
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))
(show-paren-mode)

;; Theme Configs

(defun change-minimal-theme (light-theme)
  "Set the theme and comment colors.  LIGHT-THEME."
  (interactive)
  (cond ((and light-theme) (progn
                             (load-theme 'minimal-light t)
                             (set-face-foreground 'font-lock-comment-face "SkyBlue3")
                             (set-face-foreground 'font-lock-comment-delimiter-face "SkyBlue3")))
        ((not light-theme) (progn
                             (load-theme 'minimal t)
                             (set-face-foreground 'font-lock-comment-face "light green")
                             (set-face-foreground 'font-lock-comment-delimiter-face "light green")))))

(defvar adam/use-theme "doom-nord")
(defvar adam/enable-light-theme t)

(use-package minimal-theme
  :ensure t)

(use-package atom-dark-theme
  :ensure t)

;; Enable theme based on setting
(cond ((string= adam/use-theme "minimal")   (change-minimal-theme adam/enable-light-theme))
      ((string= adam/use-theme "atom-dark") (load-theme 'atom-dark t))
      ((string= adam/use-theme "doom-nord") (load-theme 'doom-nord t))
      (t (load-theme 'doom-nord t)))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-nord t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package all-the-icons
  :ensure t)

;; Window sizing
(defun set-window-size-by-resolution ()
  "Change it so it isn't so damn tiny."
  (interactive)
  (if window-system
      (progn
	(if (> (x-display-pixel-width) 1280)
	    (add-to-list 'default-frame-alist (cons 'width 180))
	  (add-to-list 'default-frame-alist (cons 'width 120)))
	(add-to-list 'default-frame-alist
		     (cons 'height (/ (- (x-display-pixel-height) 120) (frame-char-height)))))))

(set-window-size-by-resolution)

;; Easy access to configs
(defun adam/find-init-file ()
  "This function opens up the init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun adam/reload-init-file ()
  "This function reloads the init.el file anywhere."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-c i") 'adam/find-init-file)
(global-set-key (kbd "C-c r") 'adam/reload-init-file)


;; Syntax checking
(use-package exec-path-from-shell
  :ensure t)
(exec-path-from-shell-initialize)
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Complete Anything
(use-package company
  :ensure t
  :hook ((python-mode) . company-mode))


(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

;; (use-package company-jedi
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (setq jedi:complete-on-dot t)
;;   (setq jedi:use-shortcuts t)
;;   (defun config/enable-company-jedi ()
;;     (venv-workon "venv")
;;     (add-to-list 'company-backends 'company-jedi))
;;   (add-hook 'python-mode-hook 'config/enable-company-jedi))

;; Python config
(use-package blacken
  :ensure t)

(use-package pyvenv
   :ensure t
   :config
   (pyvenv-mode 1))

(use-package python-pytest
  :ensure t
  :bind (("M-t" . python-pytest-function-dwim)
         ("M-T" . python-pytest-file-dwim)))

(use-package anaconda-mode
  :ensure t)

(add-hook 'python-mode-hook
	  (lambda ()
            (anaconda-mode)
	    (fci-mode)
	    (blacken-mode)
	    (setq fill-column 80)))

(defun adam/nose-find-func-at-point ()
  "Detect and run the current function"
  (save-excursion
    (let ((name (python-info-current-defun)))
      (unless name
        (user-error "No class/function found"))
      name)))
(defun adam/nose-run-test-at-point ()
  "this is the docstring"
  (interactive)
  (let ((file (file-relative-name buffer-file-name (projectile-project-root)))
        (func (adam/nose-find-func-at-point)))
    (let* ((buffer (get-buffer-create "nostest-bapi"))
           (process (get-buffer-process buffer))
           (nosetest-executable "docker-compose exec bapi-backend python setup.py nosetests")
           (nosetest-args "--tests")
           (command (format "%s %s %s:%s" nosetest-executable nosetest-args file func)))
      (with-current-buffer buffer
        (when (comint-check-proc buffer)
          (unless (or compilation-always-kill
                      (yes-or-no-p "Kill running nosetest process?"))
            (user-error "Aborting; nosetest still running")))
        (when process
          (delete-process process))
        (erase-buffer)
        (unless (eq major-mode 'python-pytest-mode)
          (python-pytest-mode))
        (compilation-forget-errors)
        (insert (format "cwd: %s\ncmd: %s\n\n" default-directory command))
        (make-comint-in-buffer "nosetests" buffer "bash" nil "-c" command)
        (display-buffer buffer)))))


(setenv "PYTHONPATH" (shell-command-to-string "$SHELL --login -c 'echo -n $PYTHONPATH'"))

;; Favorited files

(defun adam/day-to-day-notes ()
  "Opens your day to day notes"
  (interactive)
  (find-file "~/Notes/DayToDay2020.org"))
(global-set-key (kbd "C-c f n") 'adam/day-to-day-notes)

;; Groovy
(use-package groovy-mode
  :ensure t)

;; JS config
 (defface extra-whitespace-face
   '((t (:background "pale green")))
   "Used for tabs and such.")
(defvar my-extra-keywords
   '(("\t" . 'extra-whitespace-face)))
(add-hook 'js-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil my-extra-keywords)
	    (setq indent-tabs-mode nil)
	    (setq js-indent-level 2)))

;; ;; ---------- Fix TTY Escape sequences in M-x compile
(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)

;; Stolen from (https://oleksandrmanzyuk.wordpress.com/2011/11/05/better-emacs-shell-part-i/)
(defun regexp-alternatives (regexps)
  "Return the alternation of a list of regexps."
  (mapconcat (lambda (regexp)
               (concat "\\(?:" regexp "\\)"))
             regexps "\\|"))

(defvar non-sgr-control-sequence-regexp nil
  "Regexp that matches non-SGR control sequences.")
(setq non-sgr-control-sequence-regexp
      (regexp-alternatives
       '(;; icon name escape sequences
         "\033\\][0-2];.*?\007"
         ;; non-SGR CSI escape sequences
         "\033\\[\\??[0-9;]*[^0-9;m]"
         ;; noop
         "\012\033\\[2K\033\\[1F"
         )))

(defun filter-non-sgr-control-sequences-in-region (begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward
            non-sgr-control-sequence-regexp end t)
      (replace-match ""))))

(defun filter-non-sgr-control-sequences-in-output (ignored)
  (let ((start-marker
         (or comint-last-output-start
             (point-min-marker)))
        (end-marker
         (process-mark
          (get-buffer-process (current-buffer)))))
    (filter-non-sgr-control-sequences-in-region
     start-marker
     end-marker)))

(add-hook 'comint-output-filter-functions
          'filter-non-sgr-control-sequences-in-output)

;; Markdown - github flavor
(use-package markdown-mode
  :ensure t
  :mode
  (("\\.md\\'" . gfm-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-header-scaling t)
  (setq markdown-header-scaling-values '(1.5 1.4 1.2 1.1 1.0 1.0))
  (set-face-attribute 'markdown-header-face nil :foreground "SkyBlue4"))

;; Yaml
(use-package flycheck-yamllint
  :ensure t
  :defer t
  :init
  (progn
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))

;; Enable docker through tramp/helm
(use-package docker-tramp
  :ensure t)

(use-package helm-tramp
  :ensure t
  :bind
  (("C-x t" . 'helm-tramp)))

;; Enable emoji support
(use-package emojify
  :ensure t)
(use-package company-emoji
  :ensure t)

;; Editor config
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Golang mode
(use-package go-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

;; Org mode settings
(use-package org
  :config
  (setq org-image-actual-width nil)
  (setq org-todo-keywords '((sequence "TODO" "DOING" "REVIEW" "DONE"))))

;; Salt-mode
(use-package salt-mode
  :ensure t
  :mode "\\.sls\\'")


(use-package ob-http
  :ensure t)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil)
   (shell . t)
   (http . t)))

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

;; Line breaks
(use-package page-break-lines
  :config
  (global-page-break-lines-mode))


(setq custom-file-dir "~/.emacs.d/")
(setq custom-file (concat custom-file-dir "custom.el"))
(load custom-file 'noerror)
