(package-initialize)

(global-font-lock-mode t)

;; Inhibit all tabs.  Use C-q TAB to insert a tab.
(setq-default indent-tabs-mode nil)
;(setq default-tab-width 2)
;(setq-default tab-width 4)

;; No startup message.
(setq inhibit-startup-message t)

;; Add newline to the end of file.
(setq require-final-newline t)
;(setq next-line-add-newlines t)

;; Disable backup.
(setq backup-inhibited t)
(setq make-backup-files nil)

;; Disable autosave.
(setq auto-save-default nil)

;; Show trailing whitespace.
;; M-x set-variable RET show-t TAB RET nil RET
(setq-default show-trailing-whitespace t)

(global-auto-revert-mode 1)

;; No beep.
(setq visible-bell t)
;; No bell at all (to work around corrupt bell visuals).
(setq ring-bell-function 'ignore)

;; Highlight matching brackets.
(show-paren-mode 1)

;; No icon-ladden tool bar at the top.
(tool-bar-mode -1)
;; (menu-bar-showhide-tool-bar-menu-customize-disable)

;; Keep all opened files in the same frame.
(setq ns-pop-up-frames nil)

;; Disable scroll bars (appear on the right of each frame).
(scroll-bar-mode -1)

;; To toggle a soft-wrap mode: M-x visual-line-mode

;; (set-face-attribute 'default nil :height 150)

;; Auto-close bracket insertion, including double-quotes.
(electric-pair-mode 1)

;; (set-face-attribute 'default nil :font "M+ 1m" :height 200)

;; (set-frame-font "M+ 1m-16")
;; (set-frame-font "M+ 1m-18")
;; (set-frame-font "M+ 1mn-14")
(set-frame-font "M+ 1mn-16")
;; (set-frame-font "M+ 1mn-18")
;; (set-frame-font "M+ 1mn-20")

;; (set-default-font "M+ 1m-20")
;; (set-default-font "M+ 1m-24")
;; (set-default-font "m+ 1m-22")
;; (set-default-font "m+ 1m-28")
;; (add-to-list 'default-frame-alist '(height . 35))
;; (add-to-list 'default-frame-alist '(width . 181))
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 120))

;; Auto-close bracket insertion, including double-quotes.
(electric-pair-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(my-long-line-face ((((class color)) (:background "color-52" :foreground "brightred"))) t)
 '(my-tab-face ((t (:background "gray8"))) t)
 '(my-trailing-space-face ((((class color)) (:background "grey8"))) t))

(add-hook 'font-lock-mode-hook
          (function
           (lambda ()
             (setq font-lock-keywords
                   (append font-lock-keywords
                           '(("\t+" (0 'my-tab-face t))
                             ;; ("^.\\{80\\}\\(.\\).*$" (1 'my-long-line-face t))
                             ("[ \t]+$"      (0 'my-trailing-space-face t))))))))

;; Inhibit the menu bar.
(menu-bar-mode -1)

;; Enable display of column number for point.
(setq-default column-number-mode t)

;; Have cursor line always highlighted.
(global-hl-line-mode 1)

;; Set cursor to I-beam.  (Ignored in the terminal.)
(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))

;; (add-to-list 'load-path "~/.emacs.d/elisp")

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(global-set-key [C-tab] 'other-window)
(global-set-key [M-tab] 'switch-to-buffer)

(setq shell-file-name "/usr/local/bin/dash")

;; (toggle-frame-maximized)
;; (toggle-frame-fullscreen)


(add-hook 'dired-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))
(add-hook 'shell-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))


;;;;;;;;;;;;;
;; Server
;;;;;;;;;;;;;
(load "server")
(set-default 'server-socket-dir "~/.emacs.d/server")
(unless (server-running-p)
  (server-start))
;; (if (functionp 'window-system)
;;   (when (and (window-system)
;;              (>= emacs-major-version 24))
;;     (server-start)))


;;;;;;;;;;;;;
;; Package
;;;;;;;;;;;;;
(require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;; (package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; https://scalameta.org/metals/docs/editors/emacs.html
(setq use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*") . ,temporary-file-directory)
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;;
;; Entries below this line will require installation of mode files.
;;


;;;;;;;;;;;;;
;; Lisp
;;;;;;;;;;;;;
(load (expand-file-name "~/lib/quicklisp/slime-helper.el"))
(require 'slime-autoloads)
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))


;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;
;; (require 'color-theme)
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;; ;     (color-theme-emacs-nw)
;; ;     (color-theme-tty-dark)
;; ;     (color-theme-hober)
;; ;     (color-theme-taming-mr-arneson)
;; ;     (color-theme-midnight)
;; ;     (color-theme-renegade)
;;      (color-theme-tomorrow-night-bright)
;; ;     (color-theme-tomorrow-night)
;; ;     (color-theme-blackboard)
;;     ))

(load-theme 'sanityinc-tomorrow-night t)
;; (load-theme 'material t)


;;;;;;;;;;;;;
;; Paredit
;;;;;;;;;;;;;
;;(require 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook       #'enable-paredit-mode)

(electric-pair-mode 1)

(set-face-background 'hl-line "#223344")
(set-face-foreground 'highlight nil)


;; For GUI emacs, adds $PATH to exec-path.
;; Part of package exec-path-from-shell.
(exec-path-from-shell-initialize)

;; (require 'rainbow-delimiters)
;; (global-rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

(setq auto-mode-alist (cons '("\\.adoc$" . adoc-mode) auto-mode-alist))

(global-set-key [C-tab] 'other-window)
(global-set-key [M-tab] 'switch-to-buffer)

(counsel-mode 1)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-c l") 'counsel-locate)

(require 'company)


;;;;;;;;;;;;
;; Go
;;;;;;;;;;;;
(require 'go-mode)
(require 'company-go)
(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width 2 indent-tabs-mode 1)
            (setq gofmt-command "goimports")
            ;; eldoc shows the signature of the function at point in the status bar.
            (go-eldoc-setup)
            (local-set-key (kbd "M-.") #'godef-jump)
            (add-hook 'before-save-hook 'gofmt-before-save)
            ;; (evil-mode 1)
            (flycheck-mode)

            ;; extra keybindings from https://github.com/bbatsov/prelude/blob/master/modules/prelude-go.el
            (let ((map go-mode-map))
              (define-key map (kbd "C-c a") 'go-test-current-project) ;; current package, really
              (define-key map (kbd "C-c m") 'go-test-current-file)
              (define-key map (kbd "C-c .") 'go-test-current-test)
              (define-key map (kbd "C-c b") 'go-run))

            (company-mode-on)
            (set (make-local-variable 'company-backends) '(company-go))))


;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;
(add-hook 'clojure-mode-hook
          (lambda ()
            (company-mode-on)))

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-l") #'cider-repl-clear-buffer)
            (setq cider-repl-display-help-banner nil)
            (company-mode-on)
            (setq show-trailing-whitespace nil)
            ;; (setq cider-default-cljs-repl 'nodejs)
            ))

;; (add-hook 'cider-mode-hook
;;           (lambda ()
;;             (set-variable cider-lein-parameters "with-profile +test repl")))


;;;;;;;;;;;;
;; Kotlin
;;;;;;;;;;;;
(add-hook 'kotlin-mode-hook
          (lambda ()
            (setq kotlin-tab-width 4)
            (kotlin-eldoc-setup)
            (flycheck-mode)))


;;;;;;;;;;;;
;; Groovy
;;;;;;;;;;;;
(add-hook 'groovy-mode-hook
          (lambda ()
            ;; (c-set-offset 'label 2)
            (setq groovy-indent-offset 2)))


;;;;;;;;;;;;
;; Rust
;;;;;;;;;;;;
(setq racer-rust-src-path "/Users/rnorton/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'cargo-minor-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
(require 'company-racer)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-racer))
;; https://www.reddit.com/r/rust/comments/a3da5g/my_entire_emacs_config_for_rust_in_fewer_than_20/
(setq company-minimum-prefix-length 1)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)


;;;;;;;;;;;;
;; Scala
;;;;;;;;;;;;
;; Enable scala-mode for highlighting, indentation, and motion commands.
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

;; ;; Enable sbt mode for executing sbt commands.
;; (use-package sbt-mode
;;   :commands sbt-start sbt-command
;;   :config
;;   ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
;;   ;; Allows using SPACE when in the minibuffer
;;   (substitute-key-definition
;;    'minibuffer-complete-word
;;    'self-insert-command
;;    minibuffer-local-completion-map)
;;   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
;;   (setq sbt:program-options '("-Dsbt.supershell=false")))

;; ;; Enable nice rendering of diagnostics like compile errors.
;; (use-package flycheck
;;   :init (global-flycheck-mode))

;; (use-package lsp-mode
;;   ;; Optional - enable lsp-mode automatically in scala files
;;   :hook
;;   (scala-mode . lsp)
;;   (lsp-mode . lsp-lens-mode)

;;   :config
;;   (setq lsp-prefer-flymake nil))

;; ;; Enable nice rendering of documentation on hover
;; (use-package lsp-ui)

;; ;; lsp-mode supports snippets, but in order for them to work you need to use yasnippet.
;; ;; If you don't want to use snippets, set lsp-enable-snippet to nil in your lsp-mode settings
;; ;;   to avoid odd behavior with snippets and indentation.
;; (use-package yasnippet)

;; ;; Add company-lsp backend for metals
;; (use-package company-lsp)

;; ;; Use the Debug Adapter Protocol for running tests and debugging.
;; (use-package posframe
;;   ;; Posframe is a pop-up tool that must be manually installed for dap-mode.
;;   )
;; (use-package dap-mode
;;   :hook
;;   (lsp-mode . dap-mode)
;;   (lsp-mode . dap-ui-mode))

;; ;; Use the Tree View Protocol for viewing the project structure and triggering compilation.
;; (use-package lsp-treemacs
;;   :config
;;   (lsp-metals-treeview-enable t)
;;   (setq lsp-metals-treeview-show-when-views-received t))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(use-package lsp-ui company-lsp lsp-java lsp-mode lsp-scala company-racer ac-racer ameba crystal-playground yarn-mode poly-erb color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow clj-refactor clojure-mode-extra-font-locking flycheck-joker cljsbuild-mode ac-cider flycheck-kotlin kotlin-mode intero flycheck-haskell flycheck-clojure rust-playground elm-mode paredit-everywhere bazel-mode emmet-mode which-key csharp-mode smartparens alchemist js2-mode coffee-mode yaml-mode vimrc-mode utop tuareg tt-mode toml-mode toml swift-mode sql-indent sml-mode slime slim-mode slamhound scala-mode sass-mode rust-mode requirejs-mode readline-complete rainbow-delimiters rainbow-blocks racket-mode python-mode py-isort py-import-check py-autopep8 pretty-mode pretty-lambdada pod-mode perlcritic paredit org nodejs-repl nginx-mode muttrc-mode mustache-mode mmm-mode merlin matlab-mode markdown-mode magit json-mode jedi javap-mode jade-mode inf-ruby inf-groovy hl-todo helm haskell-mode hackernews groovy-mode go-mode gist ghc fiplr exercism exec-path-from-shell evil ess-view ess-R-object-popup ess-R-data-view eshell-manual erlang epresent emacs-cl elpy elixir-mode doctags dockerfile-mode django-mode cmake-mode cinspect cedit brainfuck-mode bison-mode bash-completion async applescript-mode apache-mode ansible adoc-mode)))
