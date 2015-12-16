(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(setq inhibit-splash-screen t)

(setq racer-cmd (expand-file-name "/home/tylercapital.ads/cquilley/git/racer/target/release/racer"))
(setq racer-rust-src-path (expand-file-name "~/rustc-nightly/src/"))

(add-hook 'rust-mode-hook #'racer-mode)

(global-company-mode)
(define-key company-active-map (kbd "j") 'company-select-next)
(define-key company-active-map (kbd "k") 'company-select-previous)
(define-key company-active-map (kbd "<esc>") 'company-abort)

(require 'company-racer)
(add-to-list 'company-backends 'company-racer)
(require 'company-jedi)
(add-to-list 'company-backends 'company-jedi)

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(global-set-key (kbd "C-<tab>") #'company-complete-common-or-cycle)
(setq company-tooltip-align-annotations t)

(load-file "~/.emacs.d/q-mode/q-mode.el")
(require 'q-mode)

(require 'idris-mode)
(setq evil-want-C-u-scroll t)
(require 'evil)
(require 'evil-leader)
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)
(global-evil-leader-mode)
(global-evil-tabs-mode t)
(idris-define-evil-keys)
(evil-leader/set-leader "<SPC>")
(evil-mode 1)

;; Evil leader keys
 
;; python mode
(evil-leader/set-key-for-mode 'python-mode "r" 'python-shell-send-buffer)
(evil-leader/set-key-for-mode 'python-mode "l" 'python-eldoc-at-point)

;; rust mode
(evil-leader/set-key-for-mode 'rust-mode "f" 'racer-find-definition)
(evil-leader/set-key-for-mode 'rust-mode "d" 'racer-eldoc)

;; q mode
(evil-leader/set-key-for-mode 'q-mode "r" 'q-load-buffer)
(evil-leader/set-key-for-mode 'q-mode "l" 'q-load)
(evil-leader/set-key-for-mode 'q-mode "f" 'q-eval-function)
(evil-leader/set-key-for-mode 'q-mode "e" 'q-eval-line)
(evil-leader/set-key-for-mode 'q-mode "E" 'q-eval-region)
(evil-leader/set-key-for-mode 'q-mode "t" 'q-type-symbol)
(evil-leader/set-key-for-mode 'q-mode "s" 'q-show-symbol)

;; org mode
(evil-leader/set-key-for-mode 'org-mode "a" 'org-insert-heading-after-current)
(evil-leader/set-key-for-mode 'org-mode "A" 'org-insert-todo-heading-respect-content)
(evil-leader/set-key-for-mode 'org-mode "h" 'org-metaleft)
(evil-leader/set-key-for-mode 'org-mode "l" 'org-metaright)
(evil-leader/set-key-for-mode 'org-mode "t" 'org-todo)

(require 'rustfmt)
(add-hook 'rust-mode-hook #'rustfmt-enable-on-save)

(setq default-directory "~")
(setq backup-directory-alist `((".*" . ,"~/.emacs.d/backups")))
(setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "autosave/") t)))

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

(load-theme 'sanityinc-solarized-dark t)

(show-paren-mode 1)
(global-linum-mode)

;; Org-mode
(setq org-log-done 'time)

;; M-x compile hooks
(setq compilation-scroll-output 'first-error)
(add-hook 'rust-mode-hook
	  (lambda ()
	    (if (file-exists-p "Cargo.toml")
		(set (make-local-variable 'compile-command) "cargo build")
	      (if (file-exists-p "../Cargo.toml")
		  (set (make-local-variable 'compile-command) "cargo build --manifest-path ..")
		(set (make-local-variable 'compile-command) (concat "rustc " buffer-file-name))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(company-racer-executable "~/git/racer/target/release/racer")
 '(company-racer-rust-src nil)
 '(company-selection-wrap-around t)
 '(company-tooltip-align-annotations t)
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(global-company-mode t)
 '(racer-cmd
   "/home/tylercapital.ads/cquilley/git/racer/target/release/racer" t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
