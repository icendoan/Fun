;; get package repositories
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; currently installed packages:
(mapc (lambda (p) (or (package-installed-p p) (package-install p)))
      '(zone-sl window-purpose imenu-list vlf tss log4e yaxception smart-compile pytest pdf-tools tablist paredit-everywhere multiple-cursors meghanada maven-test-mode js2-mode javap-mode javaimp javadoc-lookup java-imports j-mode helm-j-cheatsheet groovy-mode gradle-mode gnuplot-mode gnuplot foggy-night-theme firebelly-theme fiplr grizzl evil-smartparens evil-paredit paredit evil-org evil-numbers evil-multiedit iedit evil-mu4e evil-magit darktooth-theme danneskjold-theme clipmon cargo browse-at-remote birds-of-paradise-plus-theme apropospriate-theme anaconda-mode pythonic svg-clock svg mu4e-maildirs-extension gnu-apl-mode csharp-mode helm-mode-manager helm-ispell helm-idris helm-helm-commands helm-ghc helm-fuzzy-find helm-flycheck helm-etags-plus helm-dired-recent-dirs helm-dired-history helm-dictionary helm-anything anything helm helm-core fstar-mode evil-mc dired+ autothemer package-utils package+ linum-relative magit-gh-pulls gh logito marshal ht pcache adaptive-wrap auto-virtualenv color-theme color-theme-sanityinc-solarized company-coq company-jedi company-math company-racer dedicated egg ein elpy company evil-leader evil-search-highlight-persist evil-surround evil-tabs elscreen evil find-file-in-project flycheck-rust flycheck flymake-json flymake-easy ghc gnugo ascii-art-to-unicode goto-chg haskell-mode highlight highlight-indentation hindent idris-mode ivy jedi auto-complete jedi-core epc ctable concurrent json-mode json-reformat json-snatcher latex-extra auctex latex-pretty-symbols latex-preview-pane magic-latex-buffer magit git-commit magit-popup math-symbol-lists origami pkg-info epl popup prop-menu python-environment deferred pyvenv racer f request rust-mode rustfmt s seq smartparens undo-tree virtualenv websocket with-editor dash async xpm yasnippet))

(setq inhibit-splash-screen t)
(set-default-font "Inconsolata:size=11")

;; auto-mode-alist
(require 'groovy-mode)
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))

;; Company mode and associated plugins
(global-company-mode)
(define-key company-active-map (kbd "j") 'company-select-next)
(define-key company-active-map (kbd "k") 'company-select-previous)
(define-key company-active-map (kbd "<esc>") 'company-abort)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(global-set-key (kbd "C-<tab>") #'company-complete-common-or-cycle)
(setq company-tooltip-align-annotations t)

(setq racer-cmd (expand-file-name "/home/caleb/git/racer/target/release/racer"))
(setq racer-rust-src-path (expand-file-name "~/src/rust/src/"))
(add-hook 'rust-mode-hook #'racer-mode)
(require 'company-racer)
(add-to-list 'company-backends 'company-racer)

(require 'company-jedi)
(add-to-list 'company-backends 'company-jedi)

;; load q-mode file
(byte-compile-file "~/.emacs.d/q-mode/q-mode.el")
(load-file "~/.emacs.d/q-mode/q-mode.elc")
(require 'q-mode)
;; autoselect js2-mode instead of the older Javascript-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; require idris-mode before defining evil keys
(require 'idris-mode)

(require 'helm)
(helm-mode 1)

(require 'gnu-apl-mode)
(require 'linum-relative)
(linum-relative-global-mode t)

(require 'smartparens)
(smartparens-global-mode)

(require 'evil)
(require 'evil-leader)
(require 'evil-search-highlight-persist)
(require 'evil-numbers)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-d-scroll t)
(global-evil-search-highlight-persist t)
(global-evil-leader-mode)
(global-evil-tabs-mode t)
(global-evil-surround-mode t)
(global-evil-mc-mode 1)
(evil-smartparens-mode)
(idris-define-evil-keys)
(evil-leader/set-leader "<SPC>")
(evil-mode 1)

(evil-leader/set-key "+" 'evil-numbers/inc-at-pt)
(evil-leader/set-key "-" 'evil-numbers/dec-at-pt)

;; keys
 
(evil-leader/set-key "n" 'next-error)

(evil-leader/set-key-for-mode 'emacs-lisp-mode "r" 'eval-buffer)

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
(evil-leader/set-key-for-mode 'org-mode "A" 'org-insert-todo-subheading)
(evil-leader/set-key-for-mode 'org-mode "h" 'org-metaleft)
(evil-leader/set-key-for-mode 'org-mode "l" 'org-metaright)
(evil-leader/set-key-for-mode 'org-mode "t" 'org-todo)
(evil-leader/set-key-for-mode 'org-mode "p" 'org-priority-up)
(evil-leader/set-key-for-mode 'org-mode "P" 'org-priority-down)


;; idris-mode
(evil-leader/set-key-for-mode 'idris-mode "e" 'idris-make-lemma)
(evil-leader/set-key-for-mode 'idris-mode "j" 'idris-load-forward-line)
(evil-leader/set-key-for-mode 'idris-mode "k" 'idris-load-backward-line)
(evil-leader/set-key-for-mode 'idris-mode "L" 'idris-load-to-here)

;; apl mode

(defun gnu-apl-eval-line ()
  "Send the current line to the inferior APL process"

  (interactive)
  (gnu-apl-interactive-send-region (point-at-bol) (point-at-eol)))

(evil-leader/set-key-for-mode 'gnu-apl-mode "r" 'gnu-apl-interactive-send-buffer)
(evil-leader/set-key-for-mode 'gnu-apl-mode "e" 'gnu-apl-eval-line)

;; eshell
;; (define-key eshell-mode-map (kbd "<up>") 'eshell-previous-input)
;; (define-key eshell-mode-map (kbd "<down>") 'eshell-next-input)

(require 'rustfmt)
(add-hook 'rust-mode-hook #'rustfmt-enable-on-save)

(setq default-directory "~")
(setq backup-directory-alist `((".*" . ,"~/.emacs.d/backups")))
(setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "autosave/") t)))

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

(require 'eshell)
(define-key eshell-mode-map (kbd "C-k") 'eshell-previous-input)
(define-key eshell-mode-map (kdb "C-j") 'eshell-next-input)
(define-key eshell-mode-map (kbd "<up>") 'eshell-previous-input)
(define-key eshell-mode-map (kdb "<down>") 'eshell-next-input)
(define-key eshell-mode-map (kbd "C-K") 'eshell-previous-matching-input-from-input)
(define-key eshell-mode-map (kdb "C-J") 'eshell-next-matching-input-from-input)

(load-theme 'apropospriate-dark t)

(show-paren-mode 1)
(global-linum-mode)
(linum-relative-global-mode)

(require 'magit)
(require 'magit-gh-pulls)
(turn-on-magit-gh-pulls)

;; Org-mode
(setq org-log-done 'time)
(setq org-todo-keywords '((sequence "TODO" "VERIFY" "|" "DONE" "BLOCKED" "DELEGATED" "PENDING")))

;; M-x compile hooks
(global-set-key (kbd "<f5>") 'compile)

(setq compilation-scroll-output 'first-error)
(add-hook 'rust-mode-hook
	  (lambda ()
	    (if (file-exists-p "Cargo.toml")
		(set (make-local-variable 'compile-command) "cargo check --lib") 
	      (if (file-exists-p "../Cargo.toml")
		  (set (make-local-variable 'compile-command) "cargo check --manifest-path ../Cargo.toml --lib")
		(set (make-local-variable 'compile-command) (concat "rustc " buffer-file-name))))))

(add-hook 'java-mode-hook
	  (lambda()
	    (set (make-local-variable 'compile-command) (concat "javac " buffer-file-name))))


(require 'magit)
(require 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;; desktop mode
(desktop-save-mode 1)
(setq desktop-load-locked-desktop t) ; gets the emacs server to autoload files


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(browse-url-browser-function (quote browse-url-firefox))
 '(column-number-mode t)
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-racer-executable "~/git/racer/target/release/racer")
 '(company-racer-rust-src nil)
 '(company-selection-wrap-around t)
 '(company-tooltip-align-annotations t)
 '(compilation-skip-threshold 2)
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("d606ac41cdd7054841941455c0151c54f8bff7e4e050255dbd4ae4d60ab640c1" "ba9be9caf9aa91eb34cf11ad9e8c61e54db68d2d474f99a52ba7e87097fa27f5" "47aa6e82734866b2915781c6e1d9517bd897d45fe8aec360dd4b6294fec73068" "f01e589752ca7edbda53ff23f28f58ce313d3716edb39cbc1e9093a67d41a5b7" "44eec3c3e6e673c0d41b523a67b64c43b6e38f8879a7969f306604dcf908832c" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(electric-pair-mode t)
 '(evil-surround-pairs-alist
   (quote
    (quote
     ((40 "( " . " )")
      (91 "[ " . " ]")
      (123 "{ " . " }")
      (41 "(" . ")")
      (93 "[" . "]")
      (125 "{" . "}")
      (35 "#{" . "}")
      (98 "(" . ")")
      (66 "{" . "}")
      (116 . evil-surround-read-tag)
      (60 . evil-surround-read-tag)
      (102 . evil-surround-function)))))
 '(evil-want-C-d-scroll t)
 '(evil-want-C-u-scroll t)
 '(fci-rule-color "#073642")
 '(global-company-mode t)
 '(global-origami-mode t)
 '(idris-interpreter-flags (quote ("-p contrib" "-p pruviloj")))
 '(j-console-cmd "j")
 '(jdee-java-environment-variables (quote ("JAVA_VERSION" "/usr/lib/jvm/java-openjdk")))
 '(magit-popup-show-common-commands t)
 '(magit-popup-use-prefix-argument (quote default))
 '(package-selected-packages
   (quote
    (magit-gh-pulls package+ package-utils helm-anything helm-dictionary helm-dired-history helm-dired-recent-dirs helm-etags-plus helm-flycheck helm-fuzzy-find helm-ghc helm-helm-commands helm-idris helm-ispell helm-mode-manager svg-clock gnuplot gnuplot-mode browse-at-remote meghanada firebelly-theme foggy-night-theme darktooth-theme danneskjold-theme birds-of-paradise-plus-theme apropospriate-theme adaptive-wrap zone-sl fiplr pytest anaconda-mode groovy-mode helm-j-cheatsheet j-mode clipmon linum-relative evil-numbers fstar-mode latex-math-preview javadoc-lookup javaimp javap-mode maven-test-mode tss ein gnu-apl-mode mu4e-maildirs-extension evil-mu4e dired+ window-purpose vlf smart-compile rustfmt racer pdf-tools paredit-everywhere origami multiple-cursors magic-latex-buffer latex-preview-pane latex-pretty-symbols latex-extra js2-mode jedi java-imports idris-mode haskell-mode gradle-mode gnugo flycheck-rust f evil-tabs evil-surround evil-smartparens evil-search-highlight-persist evil-paredit evil-org evil-multiedit evil-mc evil-magit elpy dedicated csharp-mode company-racer company-jedi color-theme-sanityinc-solarized cargo)))
 '(purpose-use-default-configuration nil)
 '(purpose-user-name-purposes
   (quote
    (("*idris-holes*" . notes)
     ("*idris-notes*" . notes)
     ("*compilation*" . notes)
     ("*idris-repl*" . repl)
     ("*q*" . repl))))
 '(racer-cmd
   "/home/tylercapital.ads/cquilley/git/racer/target/release/racer")
 '(rustfmt-popup-errors t)
 '(show-paren-mode t)
 '(smartparens-global-mode nil)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-current-diff-A ((t (:background "firebrick" :foreground "#E0E0E0"))))
 '(ediff-current-diff-B ((t (:background "pale green" :foreground "dark slate gray"))))
 '(ediff-fine-diff-B ((t (:background "light goldenrod" :foreground "DarkGoldenrod4"))))
 '(mouse ((t (:background "black")))))
