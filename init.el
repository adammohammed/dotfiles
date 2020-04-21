;;; Init.el --- Emacs config
;;; Commentary:

;;; Code:
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;--------Use-package configuration--
(setq use-package-always-ensure t)

;;------- Buffer configuration ------
;; If buffers changed on disk reload automatically
(global-auto-revert-mode t)

;;------- Basic global config -------
(setq-default indent-tabs-mode nil)

;;------- Folder navigation ---------
(use-package helm
  :ensure t
  :config (setq helm-split-window-inside-p t)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x f" . helm-recentf)
	 ("C-x b" . helm-buffers-list)))

(use-package helm-projectile
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (projectile-mode)
  :config
  (setq projectile-enable-caching t)
  (setq projectile-switch-project-action 'helm-projectile)
  (define-key projectile-command-map (kbd "p") 'helm-projectile-switch-project)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; ------ Buffer navigation ---------
(use-package ace-window
  :ensure t
  :bind
  ("M-o" . 'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; ------ Magit VC ------------------
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

;; ------ Theme Configs -------------

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

(defvar adam/use-theme "minimal")
(defvar adam/enable-light-theme t)

(use-package minimal-theme
  :ensure t)

(use-package atom-dark-theme
  :ensure t)

;; Enable theme based on setting
(cond ((string= adam/use-theme "minimal")   (change-minimal-theme adam/enable-light-theme))
      ((string= adam/use-theme "atom-dark") (load-theme 'atom-dark t)))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; ------- Window sizing -----------
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

;; ------ Easy access to configs ----
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


;; ------- Syntax checking ---------
(use-package exec-path-from-shell
  :ensure t)
(exec-path-from-shell-initialize)
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ------- Complete Anything -------
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

;; ------- Replace Isearch with helm-
(global-set-key (kbd "C-s") 'helm-occur)

;; ------- Python config -----------
(use-package blacken
  :ensure t)

(use-package python-pytest
  :ensure t
  :bind (("M-t" . python-pytest-function-dwim)
         ("M-T" . python-pytest-file-dwim)))

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

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))

(setenv "PYTHONPATH" (shell-command-to-string "$SHELL --login -c 'echo -n $PYTHONPATH'"))

;; --------- Favorited files -----

(defun adam/day-to-day-notes ()
  "Opens your day to day notes"
  (interactive)
  (find-file "~/Notes/DayToDay2020.org"))
(global-set-key (kbd "C-c f n") 'adam/day-to-day-notes)

;; --------- Groovy ---------------
(use-package groovy-mode
  :ensure t)

;; --------- JS config -------------
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
  :mode (("README\\.md'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
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
(setq org-image-actual-width nil)

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



(setq custom-file-dir "~/.emacs.d/")
(setq custom-file (concat custom-file-dir "custom.el"))
(load custom-file 'noerror)
