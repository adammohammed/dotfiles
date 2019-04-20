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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;------- Folder navigation ---------
(use-package helm
  :ensure t
  :config (setq helm-split-window-inside-p t)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x f" . helm-recentf)
	 ("C-x b" . helm-buffers-list)))

(use-package projectile
  :ensure t
  :init
  (projectile-mode)
  :config
  (setq projectile-enable-caching t)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; ------ Magit VC ------------------
(use-package magit
  :ensure t)

(use-package fill-column-indicator
  :ensure t)

(global-set-key (kbd "M-/") 'hippie-expand)
(setq save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      visible-bell t
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))
(show-paren-mode)

;; ------ Theme Configs -------------
(use-package minimal-theme
  :ensure t
  :config
  (defvar enable-light-theme nil)
  (load-theme (if enable-light-theme 'minimal-light 'minimal) t)
  (set-face-foreground 'font-lock-comment-face "light green"))


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

;; ------- Replace Iserch with helm-
(global-set-key (kbd "C-s") 'helm-occur)

;; ------- Python config -----------
(load-file (concat user-emacs-directory "python.el"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes)
 '(package-selected-packages
   (quote
    (exec-path-from-shell flycheck minimal-theme projectile fill-column-indicator helm use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
