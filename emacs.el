;; basic appearance bits

(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq-default c-basic-offset 4)
(menu-bar-mode 0)
(setq column-number-mode t)
(setq default-tab-width 4)
(setq indent-tabs-mode nil)

(setq scheme-program-name "guile")

(if (and (display-graphic-p)
         (not (getenv "BIG")))
    (progn
      (scroll-bar-mode 0)
      (tool-bar-mode 0)))



;; Any machine-specific setup can go in an external
;; file. If it exists, we should load it:

(if (file-exists-p "~/.local.el")
    (load "~/.local.el"))



;; *elpa setup
(require 'package)
(package-initialize)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
  '("gelpa" . "http://gelpa.gdritter.com/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(if (getenv "FORCE_REFRESH")
    (package-refresh-contents))
(require 'use-package)



;; org-mode

(use-package org
  :ensure t)



;; web-mode
(use-package web-mode
  :ensure t
  :init (progn
          (mapcar (lambda (extension)
                    (add-to-list 'auto-mode-alist
                                 `(,extension . web-mode)))
                  '("\\.html?\\'"
                    "\\.tpl\\.php\\'"
                    "\\.[agj]sp\\'"
                    "\\.as[cp]x\\'"
                    "\\.erb\\'"
                    "\\.mustache\\'"
                    "\\.djhtml\\'"))
          (setq web-mode-markup-indent-offset 2)
          (setq web-mode-css-indent-offset 2)
          (setq web-mode-code-indent-offset 2)
          (setq web-mode-attr-indent-offset 2)))



;; unicode stuff
(use-package unicode-fonts
  :ensure t
  :defer t
  :init (unicode-fonts-setup))



;; misc. package setup

(use-package undo-tree
  :ensure t)

(use-package magit
  :ensure t
  :defer t
  :init (progn
          (setq magit-last-seen-setup-instructions "1.4.0")))

(use-package twittering-mode
  :ensure t
  :defer t
  :init (progn
          (setq twittering-use-master-password t)
          (setq twittering-cert-file "/etc/ssl/certs/ca-certificates.crt")))

;; for redo syntax highlighting
(add-to-list 'auto-mode-alist '("\\.do\\'" . sh-mode))
;; for void linux template file highlighting
(add-to-list 'auto-mode-alist '("\\template\\'" . sh-mode))


(use-package vagrant
  :ensure t)
(add-to-list 'auto-mode-alist '("\\Vagrantfile\\'" . ruby-mode))

(use-package lua-mode
  :ensure t)

(use-package scala-mode
  :ensure t)

(use-package ensime
  :ensure t
  :init (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))



;; tuareg-mode

(use-package tuareg
  :ensure t
  :defer t)



;; dockerfile mode

(use-package dockerfile-mode
  :ensure t)



;; rust-mode

(use-package rust-mode
  :ensure t)



;; various markup/text file modes

(use-package toml-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
      (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
      (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))))

(use-package pandoc-mode
  :ensure t
  :config (add-hook 'markdown-mode-hook 'pandoc-mode))

(use-package fountain-mode
  :ensure t)



;; cryptol-mode

(use-package cryptol-mode
  :defer t
  :ensure t)



(use-package helm
  :ensure t
  :diminish helm-mode
  :defines (helm-apropos-fuzzy-match
            helm-completion-mode-string
            helm-ff-file-name-history-use-recentf)
  :commands (helm-mode))



;; color things!

(when (display-graphic-p)
  (use-package color-theme
    :ensure t
    :init
      (progn
        (use-package zenburn-theme :ensure t)
        (use-package solarized-theme :ensure t)
        (use-package color-theme-sanityinc-tomorrow :ensure t)
        (color-theme-initialize)
        (let ((theme (getenv "THEME")))
          (cond
           ((string= theme "solarized-dark")
            (load-theme 'solarized-dark t))
           ((string= theme "solarized-light")
            (load-theme 'solarized-light t))
           ((string= theme "tomorrow")
            (load-theme 'tomorrow-night t))
           (t (load-theme 'zenburn t))))))
  (custom-set-faces
   '(default
      ((t (:family "Inconsolata"
           :foundry "unknown"
           :slant normal
           :weight normal
           :height 140
           :width normal))))
   '(tex-verbatim
     ((t (:family "consolas"))))))




(use-package whitespace
  :ensure t
  :init
    (progn
      (setq whitespace-style '(face empty tabs trailing))
      (global-whitespace-mode t)))



;; haskell-mode

(use-package haskell-mode
  :ensure t
  :init
    (progn
      (setq haskell-mode-hook 'turn-on-haskell-simple-indent)
      (add-to-list 'Info-default-directory-list "/usr/lib/emacs/haskell-mode/")
      (setq haskell-mode-hook '(turn-on-haskell-indentation))))

(use-package ghc
  :ensure t
  :init
  (progn
    (autoload 'ghc-init "ghc" nil t)
    (autoload 'ghc-debug "ghc" nil t)
    (add-hook 'haskell-mode-hook (lambda () (ghc-init)))))



;; evil-mode! Just in case.

(use-package evil
  :ensure t)



;; idris-mode?

(use-package idris-mode
  :ensure t)
;(use-package helm-idris
;  :ensure t)

<<<<<<< HEAD
=======



;; python stuff

(use-package elpy
  :ensure t
  :init
    (elpy-enable))

(use-package py-autopep8
  :ensure t
  :init
    (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))



;; some custom modes

(use-package adnot-mode
  :ensure t)
(use-package gidl-mode
  :ensure t)
(use-package ndbl-mode
  :ensure t)
(use-package pico-ml-mode
  :ensure t)
(use-package telml-mode
  :ensure t)
(use-package yue-mode
  :ensure t)



;; spacing fixes!

(defun fix-spacing ()
  (let ((here (point)))
    (interactive)
    (untabify (point-min) (point-max))
    (replace-regexp "\s*$" "")
    (goto-char here)
    (princ "Fixing all spacing...")))

(setq gdritter/spacing-modes
      '(;c-mode
        sh-mode
        scala-mode
        c++-mode
        asm-mode
        haskell-mode
        haskell-cabal-mode
        emacs-lisp-mode
        lisp-mode
        scheme-mode
        d-mode
        erlang-mode
        tuareg-mode))

(defun fix-spacing-hook ()
  (when (member major-mode gdritter/spacing-modes)
    (fix-spacing)))

(defun gdritter/change-whitespace-mode-hook ()
  (if (member major-mode '(c-mode c++-mode go-mode))
      (progn (setq whitespace-style '(face))
             (global-whitespace-mode t))))

(add-hook 'before-save-hook 'fix-spacing-hook)

(add-hook 'after-change-major-mode-hook
          'gdritter/change-whitespace-mode-hook)



;; text-fringe-mode (for editing single prose files)

(define-minor-mode bzg-big-fringe-mode
  "Minor mode to center text using large fringe"
  :init-value nil
  :global t
  :variable bzg-big-fringe-mode
  :group 'editing-basics
  (if (not bzg-big-fringe-mode)
      (set-fringe-style nil)
    (progn
      (set-fringe-mode
       (/ (- (frame-pixel-width)
             (* 300 (frame-char-width)))
          2))
      (mapcar (lambda (fb) (set-fringe-bitmap-face fb 'org-hide))
              fringe-bitmaps))))



;; env variable directives

(if (getenv "TWIT")
    (twit))
(if (getenv "EVIL")
    (progn
      (evil-mode)))
(if (getenv "NARROW")
    (bzg-big-fringe-mode 1))
(put 'narrow-to-region 'disabled nil)
