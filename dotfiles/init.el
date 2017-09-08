; Package management

(setq evil-want-C-d-scroll t)
(setq evil-want-C-u-scroll t)
(setq tab-width 4)

(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(require 'diminish)
(require 'bind-key)

(use-package evil-leader
  :diminish evil-leader-mode
  :init (add-hook 'evil-mode-hook #'global-evil-leader-mode)
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key "f" #'evil-avy-goto-char)
  (evil-leader/set-key "%" #'evil-avy-goto-line)
  (evil-leader/set-key "n" #'next-error))

(use-package evil
  :ensure
  :init
  
  (use-package evil-search-highlight-persist
    :config (global-evil-search-highlight-persist t))
  (use-package evil-numbers
    :config
    (evil-leader/set-key (kbd "+") #'evil-numbers/inc-at-pt)
    (evil-leader/set-key (kbd "-") #'evil-numbers/dec-at-pt))
  (use-package evil-mc
    :diminish evil-mc-mode
    :init (add-hook 'evil-mode-hook #'global-evil-mc-mode))
  (use-package evil-tabs
    :diminish evil-tabs-mode
    :init (add-hook 'evil-mode-hook #'global-evil-tabs-mode))
  (use-package evil-surround
    :init (add-hook 'evil-mode-hook #'global-evil-surround-mode))
  (use-package evil-smartparens
	:diminish evil-smartparens-mode
	:init (add-hook 'evil-mode #'evil-smartparens-mode))
  
  :config
  
  (define-key evil-normal-state-map (kbd "j") #'evil-next-visual-line)
  (define-key evil-visual-state-map (kbd "j") #'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") #'evil-previous-visual-line)
  (define-key evil-visual-state-map (kbd "k") #'evil-previous-visual-line))

(use-package avy
  :ensure
  :config (use-package evil-avy))

(use-package helm
  :init (add-hook 'evil-mode-hook
		  (lambda ()
		    (add-to-list 'evil-ex-commands '("ls" . helm-mini))))
  :config
  (helm-mode t))

(use-package dired+)

(use-package flyspell
  :ensure
  :commands (flyspell-prog-mode
			 flyspell-mode
			 flyspell-buffer)
  :diminish (flyspell-mode
			 flyspell-prog-mode)
  :init (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :config
  (setq flyspell-issue-message-flag nil
		flyspell-issue-welcome-flag nil)
  (cond
   ((executable-find "hunspell")
	(setq ispell-program-name "hunspell")
	(setq ispell-really-hunspell t
		  ispell-extra-args '("-i" "utf-8")
		  ispell-local-dictionary-alist
		  '("english"
			"[A-Za-z]" "[^A-Za-z]" "[']" nil
			("-d" "en_GB")
			nil utf-8)
		  ispell-dictionary "english"))
   (t (setq ispell-program-name nil))))

(use-package flycheck
  :ensure
  :commands global-flycheck-mode
  :init (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package magit
  :ensure
  :commands magit-status
  :init
  (use-package magit-gh-pulls
	:init (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))
  (use-package evil-magit)
  (defun magit-ignored-files ()
	(magit-git-items "ls-files" "--others" "--ignored" "--exclude-standard" "-z" "--directory"))
  (defun magit-insert-ignored-files ()
  (-when-let (files (magit-ignored-files))
    (magit-insert-section (ignored)
      (magit-insert-heading "Ignored files:")
      (magit-insert-un/tracked-files-1 files nil)
      (insert ?\n))))
  (add-hook 'magit-status-sections-hook #'magit-insert-ignored-files))

(use-package org
  :commands org-mode
  :mode ("\\.org\\'" . org-mode)
  :config
  (use-package evil-org)
  (evil-leader/set-key-for-mode 'org-mode "a" #'org-insert-heading-after-current)
  (evil-leader/set-key-for-mode 'org-mode "A" #'org-insert-todo-subheading)
  (evil-leader/set-key-for-mode 'org-mode "h" #'org-metaleft)
  (evil-leader/set-key-for-mode 'org-mode "l" #'org-metaright)
  (evil-leader/set-key-for-mode 'org-mode "t" #'org-todo)
  (evil-leader/set-key-for-mode 'org-mode "p" #'org-priority-up)
  (evil-leader/set-key-for-mode 'org-mode "P" #'org-priority-down)
  (setq org-src-tab-acts-natively t)
  (setq org-src-fontify-natively t))

(use-package company
  :config
  (global-company-mode)
  (define-key company-active-map (kbd "j") 'company-select-next)
  (define-key company-active-map (kbd "k") 'company-select-previous)
  (define-key company-active-map (kbd "<esc>") 'company-abort)
  (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
  (global-set-key (kbd "C-<tab>") #'company-complete-common-or-cycle)
  (setq company-tooltip-align-annotations t))

(use-package doc-view
  :config (setq doc-view-continuous t))

(use-package fiplr)
(use-package adaptive-wrap)

;(use-package lsp-mode
;  :init (add-hook 'prog-mode-hook #'lsp-mode)
;  :config
;  (use-package lsp-flycheck
;	:ensure f)
;  (use-package lsp-rust))

; individual language modes

(use-package idris-mode
  :mode ("\\.idris\\'" . idris-mode)
  :config
  (evil-leader/set-key-for-mode 'idris-mode (kbd "e") #'idris-make-lemma)
  (evil-leader/set-key-for-mode 'idris-mode (kbd "j") #'idris-load-forward-line)
  (evil-leader/set-key-for-mode 'idris-mode (kbd "k") #'idris-load-backward-line)
  (evil-leader/set-key-for-mode 'idris-mode (kbd "L") #'idris-load-to))


(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :init
  (add-hook 'rust-mode-hook
	    (lambda ()
	      (if (file-exists-p "Cargo.toml")
		  (set (make-local-variable 'compile-command) "cargo check --lib")
		(if (file-exists-p "../Cargo.toml")
		    (set (make-local-variable 'compile-command) "cargo check --manifest-path ../Cargo.toml --lib"))
		(set (make-local-variable 'compile-command) (concat "rustc " buffer-file-name)))))
  (setq rust-format-on-save t)
  (use-package flycheck-rust
	:commands flycheck-rust-setup
	:init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package racer
	:commands racer-mode
	:diminish racer-mode
	:init
	(add-hook 'rust-mode-hook #'racer-mode)
	:config
	(racer-turn-on-eldoc)
	(use-package company-racer
	  :config
	  (add-to-list 'company-backends 'company-racer)
	  (setq company-tooltip-align-annotations t)
	  (evil-leader/set-key-for-mode 'rust-mode "d" #'racer-find-definition)))

(use-package gnu-apl-mode
  :mode ("\\.apl\\'" . gnu-apl-mode)
  :config
  (defun gnu-apl-eval-line ()
	"Send the current line to the inferior APL process"
	(interactive)
	(gnu-apl-interactive-send-region (point-at-bol) (point-at-eol)))
  (evil-leader/set-key-for-mode 'gnu-apl-mode "r" #'gnu-apl-interactive-send-buffer)
  (evil-leader/set-key-for-mode 'gnu-apl-mode "e" #'gnu-apl-eval-line))

(byte-compile-file "~/.emacs.d/q-mode.el")
(load-file "~/.emacs.d/q-mode.elc")

(use-package q-mode
  :mode ("\\.[qk]\\'" . q-mode)
  :config
  (evil-leader/set-key-for-mode 'q-mode "r" #'q-load-buffer)
  (evil-leader/set-key-for-mode 'q-mode "l" #'q-load)
  (evil-leader/set-key-for-mode 'q-mode "e" #'q-eval-line)
  (evil-leader/set-key-for-mode 'q-mode "E" #'q-eval-region)
  (evil-leader/set-key-for-mode 'q-mode "t" #'q-type-symbol)
  (evil-leader/set-key-for-mode 'q-mode "s" #'q-show-symbol))

(use-package groovy-mode
  :mode ("\\.gradle\\'" . groovy-mode))

; themes and default settings

(setq default-directory "~")
(setq backup-directory-alist `((".*" . ,"~/.emacs.d/backups")))
(setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "/autosave/") t)))
(setq inhibit-splash-screen t)
(set-frame-font "inconsolata:size=12" t t)

(use-package color-theme-sanityinc-solarized
  :config
  (load-theme 'sanityinc-solarized-dark t))

(show-paren-mode)
(global-set-key (kbd "<f5>") #'compile)
(global-set-key (kbd "<f4>") #'flycheck-buffer)
(setq compilation-scroll-output 'first-error)
(desktop-save-mode 1)
(tool-bar-mode t)
(evil-mode)
(electric-pair-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-dabbrev-ignore-case nil)
 '(custom-safe-themes
   (quote
	("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(package-selected-packages
   (quote
	(fiplr company-racer evil-magit magit-gh-pulls q-mode evil-org helm evil-smartparens use-package racer popup magit idris-mode helm-core groovy-mode gnu-apl-mode flycheck-rust evil-tabs evil-surround evil-search-highlight-persist evil-numbers evil-mc evil-leader evil-avy dired+ company color-theme-sanityinc-solarized cargo)))
 '(q-qsm-path "qsm")
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
