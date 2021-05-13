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

;; os-specific key-bindings
(defvar *is-a-mac* (string= system-type "darwin"))
(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))


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
  (setcdr (assq 't ivy-format-functions-alist) #'adam-ivy-format-function-prefix)
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

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-nord t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package all-the-icons
  :ensure t)

;; Enable theme based on setting
(cond ((string= adam/use-theme "minimal")   (change-minimal-theme adam/enable-light-theme))
      ((string= adam/use-theme "atom-dark") (load-theme 'atom-dark t))
      ((string= adam/use-theme "doom-nord") (load-theme 'doom-nord t))
      (t (load-theme 'doom-nord t)))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))


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
  :config
  (global-company-mode t)
  :hook ((python-mode) . company-mode))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode))

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

;; Enable docker through tramp
(use-package docker-tramp
  :ensure t)

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


;; Perl configuration
(setq lsp-keymap-prefix "s-l")
(use-package lsp-mode
  :hook ((cperl-mode . lsp))
  :config
  (add-to-list 'cperl-style-alist '("LinodePerl"
                          (cperl-indent-level . 4)
                          (cperl-brace-offset . 0)
                          (cperl-continued-brace-offset . 0)
                          (cperl-label-offset . -4)
                          (cperl-continued-statement-offset . 4)
                          (cperl-close-paren-offset . -4)
                          (cperl-indent-parens-as-block . t)
                          (cperl-merge-trailing-else . t)))
  :commands lsp)
(use-package lsp-ui :commands lsp-ui-mode)




(add-to-list 'auto-mode-alist '("\\.\\([pP]\\([Llm]\\|erl\\|od\\)\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

;; Line breaks
(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

;; Dired hacks
(defun cert-info (filename bufname)
  "Shows the details of a certificate given a filename output to the buffer"
  (with-output-to-temp-buffer bufname
    (shell-command (format "openssl x509 -text -noout -in %s" filename) bufname)
    (pop-to-buffer bufname)
    (local-set-key (kbd "q") (quit-window t))))
(defun adam/dired-x509-info ()
  "Retrieve certificate information for file under point in Dired"
  (interactive)
  (cert-info (dired-get-file-for-visit) "*cert-info*"))

(define-key dired-mode-map (kbd "C-c t") 'adam/dired-x509-info)

(setq custom-file-dir "~/.emacs.d/")
(setq custom-file (concat custom-file-dir "custom.el"))
(load custom-file 'noerror)
