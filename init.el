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
(defvar local-packages '(slime midje-mode shen-mode))

(dolist (pkg local-packages)
  (let ((pkg-root (format "%s/%s" local-package-root pkg)))
    (when (file-exists-p pkg-root)
      (add-to-list 'load-path pkg-root)
      (require pkg))))

(let ((org-lisp (format "%s/%s" local-package-root "org/lisp")))
  (when (file-exists-p org-lisp)
    (add-to-list 'load-path org-lisp)
    (require 'org)))


;; General Customizations
;; ----------------------
(setq make-backup-files nil)
(setq auto-save-default nil)
(global-auto-revert-mode t)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(setq inhibit-startup-message t)
(setq ispell-program-name "/usr/local/bin/aspell")

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

;; Key bindings
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c =") 'er/expand-region)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; Programming modes
;; -----------------
(defun prog-mode-setup ()
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'prog-mode-setup)

;; Lisps in General
(defun lispy-mode-setup ()
  (paredit-mode t)
  (highlight-parentheses-mode t)
  (if (window-system)
      (progn
        (setq fci-rule-column 80)
        (setq fci-rule-color "orange")
        (fci-mode))))

(defun repl-mode-setup ()
  (paredit-mode t)
  (highlight-parentheses-mode t))

(add-hook 'emacs-lisp-mode-hook 'lispy-mode-setup)
(add-hook 'scheme-mode-hook 'lispy-mode-setup)

;; Common Lisp
(add-hook 'lisp-mode-hook 'lispy-mode-setup)
(setq inferior-lisp-program "/usr/local/bin/sbcl --noinform")
(slime-setup '(slime-fancy))
(add-hook 'slime-repl-mode-hook 'repl-mode-setup)

;; Clojure
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-hook 'clojure-mode-hook 'lispy-mode-setup)
(add-hook 'nrepl-mode-hook 'repl-mode-setup)

;; Shen
(require 'inf-shen)
(setq inferior-shen-program "~/bin/shen-repl")
(add-to-list 'auto-mode-alist '("\.shen$" . shen-mode))
(add-hook 'shen-mode-hook 'lispy-mode-setup)
(add-hook 'inferior-shen-mode-hook 'repl-mode-setup)

;; JavaScript
(setq js-indent-level 2)
