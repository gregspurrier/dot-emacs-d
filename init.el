;; Packages managed by package.el
;; ------------------------------
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
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
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq inhibit-startup-message t)
(setq ispell-program-name "aspell")

;; Paths
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path (concat (getenv "HOME") "/bin"))
(add-to-list 'exec-path (concat (getenv "HOME") "/.rbenv/shims"))

;; Customize the way that Emacs looks
(menu-bar-mode -1)
(when (window-system) (scroll-bar-mode -1))
(tool-bar-mode -1)
(column-number-mode t)

;; Fonts and Themes
(defun set-first-available-font (choices)
  (let* ((font-list (font-family-list))
         (choice (find-if (lambda (f) (member (car f) font-list))
                          choices)))
    (when choice
      (set-frame-font (format "%s-%d" (car choice) (cdr choice))))))

(if (window-system)
    (progn
      (load-theme 'solarized-light t)
      (set-first-available-font '(("Source Code Pro" . 12)
                                  ("Inconsolata" . 14)))
      (set-frame-height (selected-frame) 40)))

;; Key bindings
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c =") 'er/expand-region)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; Programming modes
;; -----------------
(require 'yasnippet)
(yas-reload-all)
(defun prog-mode-setup ()
  (setq show-trailing-whitespace t)
  (yas-minor-mode 1))

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
(add-hook 'shen-mode-hook 'lispy-mode-setup)
(add-hook 'inferior-shen-mode-hook 'repl-mode-setup)

;; Bypass paredit's backslash handling. It gets confused by Shen's
;; comments.
(defun disable-paredit-backslash ()
  (local-set-key [remap paredit-backslash]
                 (lambda ()
                   (interactive)
                   (insert "\\"))))

(add-hook 'shen-mode-hook 'disable-paredit-backslash t)
(add-hook 'inferior-shen-mode-hook 'disable-paredit-backslash t)

;; JavaScript
(setq js-indent-level 2)

;; mmm-mode
(unless (eq system-type 'darwin)
  (require 'mmm-mode)
  (mmm-add-classes
   '((asciidoc-ruby
      :submode ruby-mode
      :front "\\[source,ruby\\]\n----\n"
      :back "^----\n"
      :insert ((?r embruby nil
                   @ "[source,ruby]\n----\n" @ _ @ "\n----\n" @)))
     (asciidoc-shen
      :submode shen-mode
      :front "\\[source,lisp\\]\n----\n"
      :back "^----\n"
      :insert ((?s embshen nil
                   @ "[source,lisp]\n----\n" @ _ @ "\n----\n" @)))))
  (setq mmm-global-mode 'maybe)
  (mmm-add-mode-ext-class 'text-mode nil 'asciidoc-ruby)
  (mmm-add-mode-ext-class 'text-mode nil 'asciidoc-shen))
