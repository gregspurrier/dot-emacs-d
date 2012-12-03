;; Packages managed by package.el
;; ------------------------------
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; The packages checked in to git under ~/.emacs.d/elpa are not checked
;; in with their .elc files. To compile them, execute this command:
;;   (byte-recompile-directory (expand-file-name "~/.emacs.d/elpa") 0)

;; Packages installed locally
;; --------------------------
(defvar local-package-root "~/.emacs.d/pkgs/")
(defvar local-packages '(slime clojure-mode nrepl))

(dolist (pkg local-packages)
  (let ((pkg-root (format "%s/%s" local-package-root pkg)))
    (when (file-exists-p pkg-root)
      (add-to-list 'load-path pkg-root)
      (require pkg))))

;; General Customizations
;; ----------------------
(setq make-backup-files nil)
(setq auto-save-default nil)
(global-auto-revert-mode t)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)

;; Customize the way that Emacs looks
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)

(if (window-system)
    (progn
      (load-theme 'zenburn t)
      (set-default-font "Inconsolata-14")
      (set-frame-height (selected-frame) 48)))

;; Meta key bindings for Mac OS X
(setq mac-option-modifier 'hyper)
(setq mac-command-modifier 'meta)

;; Programming modes
;; -----------------

;; Lisps in General
(defun lispy-mode-setup ()
  (paredit-mode t)
  (highlight-parentheses-mode t)
  (if (window-system)
      (progn
        (setq fci-rule-column 80)
        (setq fci-rule-color "orange")
        (fci-mode))))

(add-hook 'emacs-lisp-mode-hook 'lispy-mode-setup)
(add-hook 'scheme-mode-hook 'lispy-mode-setup)

;; Common Lisp
(add-hook 'lisp-mode-hook 'lispy-mode-setup)
(setq inferior-lisp-program "/usr/local/bin/sbcl --noinform")
(slime-setup '(slime-fancy))

;; Clojure
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-hook 'clojure-mode-hook 'lispy-mode-setup)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'highlight-parentheses-mode)
