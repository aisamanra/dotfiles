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



;; *elpa setup
(require 'package)
(package-initialize)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)



;; misc. package setup

(use-package magit
  :ensure t)

(use-package twittering-mode
  :ensure t
  :init (setq twittering-use-master-password t))

;; for redo syntax highlighting
(add-to-list 'auto-mode-alist '("\\.do\\'" . sh-mode))



;; tuareg-mode

(use-package tuareg
  :ensure t
  :init
    (progn
      (autoload 'tuareg-mode "tuareg-mode"
        "Major mode for editing Caml or whatever." t)
      (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
      (autoload 'tuareg-imenu-set-imenu "tuareg-imenu"
        "Configuration of imenu for tuareg" t)
      (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
      (add-to-list 'auto-mode-alist '("\\.ml[ilyp]?\\'" . tuareg-mode))))



;; dockerfile mode

(use-package dockerfile-mode
  :ensure t
  :init
    (progn
      (add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-mode))))



;; rust-mode

(use-package rust-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))



;; various markup/text file modes

(use-package toml-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode)))

(use-package yaml-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\.ya?ml\'" . yaml-mode)))

(use-package markdown-mode
  :ensure t
  :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
      (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
      (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))))



;; cryptol-mode

(use-package cryptol-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.cry\\'" . cryptol-mode)))



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
           :height 98
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



;; spacing fixes!

(defun fix-spacing ()
  (let ((here (point)))
    (interactive)
    (untabify (point-min) (point-max))
    (replace-regexp "\s*$" "")
    (goto-char here)
    (princ "Fixing all spacing...")))

(setq gdritter/spacing-modes
      '(c-mode
        c++-mode
        asm-mode
        haskell-mode
        haskell-cabal-mode
        emacs-lisp-mode
        lisp-mode
        d-mode
        erlang-mode
        tuareg-mode))

(defun fix-spacing-hook ()
  (when (member major-mode gdritter/spacing-modes)
    (fix-spacing)))

(add-hook 'before-save-hook 'fix-spacing-hook)



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
             (* 100 (frame-char-width)))
          2))
      (mapcar (lambda (fb) (set-fringe-bitmap-face fb 'org-hide))
              fringe-bitmaps))))



;; env variable directives

(if (getenv "TWIT")
    (twit))
(if (getenv "EVIL")
    (progn
      (require 'evil)
      (evil-mode)))
(if (getenv "NARROW")
    (bzg-big-fringe-mode 1))
