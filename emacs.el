;; basic appearance bits

(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(setq column-number-mode t)
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)

(setq c-default-style "stroustrup")
(c-set-offset 'substatement-open 0)
(add-to-list 'auto-mode-alist '("\\.edl\\'" . c-mode))

(setq scheme-program-name "guile")
(setq vc-follow-symlinks t)

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
  '("melpa" . "https://melpa.org/packages/") t)
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
  :ensure t
  :init (add-hook 'org-mode-hook 'visual-line-mode))

(defun gdritter/add-meta-bindings ()
  (let ((map outline-minor-mode-map))
    (define-key map (kbd "M-<up>") 'outline-previous-visible-heading)
    (define-key map (kbd "M-<down>") 'outline-next-visible-heading)))

(add-hook 'outline-minor-mode-hook 'gdritter/add-meta-bindings)



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

(use-package rjsx-mode :ensure t)

(setq js2-basic-offset 2)



;;
(use-package persistent-soft
  :ensure t)

;; display Github-like emoji if we're in a text-editing mode of some
;; kind
(defun gdritter/enable-emoji-hook ()
  (if (member major-mode '(markdown-mode pandoc-mode text-mode telml-mode))
      (emojify-mode)))

(use-package emojify
  :ensure t
  :init (add-hook 'after-change-major-mode-hook
                  'gdritter/enable-emoji-hook))



;; some C/C__ setup

(defun gdritter/line-width-hook()
  (if (member major-mode '(c-mode c++-mode))
      (set-fill-column 120)))

(add-hook 'after-change-major-mode-hook 'gdritter/line-width-hook)

(use-package dumb-jump
  :bind (("M-g j" . dump-jump-go)
         ("M-g i" . dump-jump-go-prompt))
  :config (setq dump-jump-selector 'helm)
  :ensure t)

(defconst gdritter/cc-style
  '("cc-mode" (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "gdritter/cc-style" gdritter/cc-style)
(setq c-default-style "gdritter/cc-style" c-basic-offset 4)



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

(use-package meson-mode
  :ensure t)

(use-package vagrant
  :ensure t)
(add-to-list 'auto-mode-alist '("\\Vagrantfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rbi\\'" . ruby-mode))

(setq ruby-deep-indent-param nil)
(setq ruby-insert-encoding-magic-comment nil)

(use-package lua-mode
  :ensure t
  :init (setq lua-indent-level 2))

(use-package fennel-mode
  :ensure t)

(use-package scala-mode
  :ensure t)

(use-package ensime
  :ensure t
  :init (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

(use-package glsl-mode
  :ensure t)

(use-package ponylang-mode
  :ensure t)

(use-package bazel-mode
  :ensure t)



;; tuareg-mode

(use-package tuareg
  :ensure t
  :defer t)

(use-package dash :ensure t)
(use-package dash-functional :ensure t)

(use-package fsharp-mode
  :ensure t
  :defer t)



;; dockerfile mode

(use-package dockerfile-mode
  :ensure t)



;; rust-mode

(use-package rust-mode
  :ensure t)

(use-package company
  :ensure t)

(use-package racer
  :ensure t
  :init
  (progn
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)

    (require 'rust-mode)
    (define-key rust-mode-map
      (kbd "TAB") #'company-indent-or-complete-common)
    (setq company-tooltip-align-annotations t)))

(use-package cargo
  :ensure t
  :init (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))



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
      (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
      (add-hook 'markdown-mode-hook 'visual-line-mode)))

(add-hook 'rst-mode-hook 'visual-line-mode)

(use-package pandoc-mode
  :ensure t
  :config (add-hook 'markdown-mode-hook 'pandoc-mode))

(use-package fountain-mode
  :ensure t)

(defun gdritter/rst-csv-to-latex ()
  (interactive)
  (let ((here (point)))
    (while (re-search-forward "^ *\"" nil t)
      (replace-match ""))
    (goto-char here)
    (while (re-search-forward "\", *\"" nil t)
      (replace-match " & "))
    (goto-char here)
    (while (re-search-forward "\" *$" nil t)
      (replace-match " \\\\\\\\"))
    (goto-char here)))

;; Pandoc can begin with YAML metadata, and sometimes a pandoc file
;; will indicate which format it should be using. I'm not going to
;; write a full YAML parser in elisp! But usually, parsing simple
;; key-value pairs should be sufficient.
(defun gdritter/pandoc-parse-basic-meta-block ()
  (let ((here (point)))
    (goto-char 0)
    (let ((rs
           (if (re-search-forward "^---\n" nil t)
               (let ((block-start (point)))
                 (if (re-search-forward "^\\(---\\|...\\)\n" nil t)
                     (progn (goto-char block-start)
                            (gdritter/pandoc-parse-kv-section))
                   (progn (message "no metadata block found")
                          nil))))))
      (goto-char here)
      rs)))

;; This tries to parse all the simple key: value forms it can
(defun gdritter/pandoc-parse-kv-section ()
  (if (looking-at "^\\(---\\|...\\)\n")
      '()
    (let ((rs (re-search-forward "^\\([A-Za-z0-9_]+\\):[ ]*\\(.*\\)\n" nil t)))
      (if rs
          (cons (cons (match-string 1)
                      (match-string 2))
                (gdritter/pandoc-parse-kv-section))
        (forward-line 1)))))

;; This will try to dispatch which mode to open based on the
;; metadata block. If the metadata block doesn't contain a format
;; key, then it'll default to Markdown with Pandoc extensions.
(defun gdritter/pandoc-choose-mode ()
  (interactive)
  (let* ((meta (gdritter/pandoc-parse-basic-meta-block))
         (format (assoc-string "format" meta))
         (fmt (if format (cdr format) nil)))
    (cond ((equal fmt "org") (org-mode))
          ((equal fmt "latex") (latex-mode))
          ((equal fmt "rst") (rst-mode))
          ((equal fmt "html") (web-mode))
          (t (markdown-mode)))))

(add-to-list 'auto-mode-alist '("\\.page\\'" . gdritter/pandoc-choose-mode))
(add-to-list 'auto-mode-alist '("\\.pandoc\\'" . gdritter/pandoc-choose-mode))



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
  :commands (helm-mode)
  :init (progn
          (global-set-key (kbd "C-x C-f") #'helm-find-files)
          (global-set-key (kbd "C-x C-b") #'helm-mini)))



;; color things!

(when (display-graphic-p)
  (use-package color-theme-modern
    :ensure t
    :init
      (progn
        (use-package zenburn-theme :ensure t)
        (use-package solarized-theme :ensure t)
        (use-package color-theme-sanityinc-tomorrow :ensure t)
        (let ((theme (getenv "THEME")))
          (cond
           ((string= theme "solarized-dark")
            (load-theme 'solarized-dark t))
           ((string= theme "solarized-light")
            (load-theme 'solarized-light t))
           ((string= theme "tomorrow")
            (load-theme 'sanityinc-tomorrow-night t))
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
      (setq haskell-mode-hook '(turn-on-haskell-indentation))
      (add-hook 'haskell-mode-hook 'outline-minor-mode)
      (setq outline-regexp "-- \\*+")))

(use-package dante
  :ensure t
  :commands 'dante-mode
  :init
    (add-hook 'haskell-mode-hook 'dante-mode)
    (add-hook 'haskell-mode-hook 'flycheck-mode))

;; an editor macro for creating obvious trivial lens impls
(defun mk-lens ()
 (interactive)
 (let ((rstart (region-beginning))
       (rend (region-end)))
   (goto-char rstart)
   (if (re-search-forward
        "^@lens \\([A-Za-z0-9_]+\\) *\\([A-Za-z0-9_]+\\) *\\([A-Za-z0-9._]+\\)"
        nil
        t)
       (let ((val (match-string 1))
             (typ (match-string 2))
             (fil (match-string 3)))
         (goto-char (match-beginning 0))
         (delete-region (match-beginning 0) (match-end 0))
         (insert val "L :: Lens' " typ " " fil "\n")
         (insert val "L = makeLens " val " (\\ t s -> s { " val " = t })\n")
         (mk-lens)))))


;; (use-package ghc
;;   :ensure t
;;   :init
;;   (progn
;;     (autoload 'ghc-init "ghc" nil t)
;;     (autoload 'ghc-debug "ghc" nil t)
;;     (add-hook 'haskell-mode-hook (lambda () (ghc-init)))))



;; evil-mode! Just in case.

(use-package evil
  :ensure t)



;; idris-mode?

(use-package idris-mode
  :ensure t)

(use-package pony-mode
  :ensure t
  :init
  (add-hook 'ponylang-mode-hook (lambda () (set-variable 'tab-width 2))))



;; python stuff

;; (use-package elpy
;;   :ensure t
;;   :init
;;     (elpy-enable))

;; (use-package py-autopep8
;;   :ensure t)
;; ;  :init
;; ;    (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))



;; some custom modes

(use-package adnot-mode
  :ensure t)
(use-package electric-boogaloo-mode
  :ensure t)
(use-package gidl-mode
  :ensure t)
(use-package ndbl-mode
  :ensure t)
(use-package pico-ml-mode
  :ensure t)
(use-package suppl-mode
  :ensure t)
(use-package telml-mode
  :ensure t
  :init
  (add-hook 'telml-mode-hook 'visual-line-mode))
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
      '(scala-mode
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
  (if (member major-mode '(c-mode c++-mode go-mode org-mode))
      (progn (setq whitespace-style '(face))
             (global-whitespace-mode t))))

(add-hook 'before-save-hook 'fix-spacing-hook)

(add-hook 'after-change-major-mode-hook
          'gdritter/change-whitespace-mode-hook)



;; text-fringe-mode (for editing single prose files)
(use-package olivetti
  :ensure t)

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
             (* 240 (frame-char-width)))
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

(use-package lsp-mode
  :ensure t
  :config
  (add-hook 'c++-mode-hook #'lsp)
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error")))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(typescript-mode olivetti olivetti-mode quack racket-mode flymake-racket lsp-mode dumb-jump bazel-mode ponylang-mode rjsx-mode fennel-mode go-mode auctex dante zenburn-theme yue-mode yaml-mode web-mode vagrant use-package unicode-fonts twittering-mode tuareg toml-mode telml-mode suppl-mode solarized-theme scala-mode rust-mode py-autopep8 pico-ml-mode pandoc-mode ndbl-mode markdown-mode magit lua-mode io-mode idris-mode helm glsl-mode gidl-mode ghc fsharp-mode fountain-mode evil erlang ensime elpy electric-boogaloo-mode dockerfile-mode dash-functional cryptol-mode color-theme-sanityinc-tomorrow color-theme adnot-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 140 :width normal))))
 '(lsp-ui-doc-background ((t (:background "black"))))
 '(lsp-ui-sideline-current-symbol ((t (:foreground "white" :weight ultra-bold :height 0.99))))
 '(tex-verbatim ((t (:family "consolas")))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
