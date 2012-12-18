;; Inhibit all tabs.  Use C-q TAB to insert a tab.
(setq-default indent-tabs-mode nil)
;(setq default-tab-width 2)

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

;; No beep.
(setq visible-bell t)

;; Highlight matching brackets.
(show-paren-mode 1)
; (setq show-paren-style 'mixed)

;; To toggle a soft-wrap mode: M-x visual-line-mode

;; M-x whitespace-mode RET
;(require 'whitespace)
;(setq whitespace-trailing-regexp
;      "\\b\\(\\(\t\\| \\|\xA0\\|\x8A0\\|\x920\\|\xE20\\|\xF20\\)+\\)$")
;; Turns on whitespace-mode for entire session.
;(global-whitespace-mode)
;(setq-default whitespace-style '(face trailing tabs tab-mark lines))

;; Inhibit the menu bar.
(menu-bar-mode -1)

;; Enable display of column number for point.
(setq-default column-number-mode t)

(add-to-list 'load-path "~/.emacs.d")

;;
;; Entries below this line will require installation of mode files.
;;

(load "haml-mode")
(load "sass-mode")
(load "scss-mode")
(load "coffee-mode")
(load "yaml-mode")
(load "markdown-mode")
;(load "nxhtml/autostart.el") ; has funky blue bg and deprecation warnings.
;(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(add-to-list 'load-path "~/.emacs.d/elisp/feature-mode")
(load "~/.emacs.d/elisp/feature-mode")

(require 'color-theme)
(add-to-list 'load-path "~/.emacs.d/themes")
(eval-after-load "color-theme"
'(progn
 (color-theme-initialize)
 (color-theme-tty-dark)))
;    (color-theme-tomorrow-night-bright)))
;    (color-theme-hober)))
(put 'upcase-region 'disabled nil)

(setq auto-mode-alist
  (append '(("\\.rjs$" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist
  (append '(("\\.rake$" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist
  (append '(("Rakefile$" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist
  (append '(("Gemfile$" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist
  (append '(("\\.md$" . ruby-mode)) auto-mode-alist))
;; Avoid adding # -*- coding: utf-8 -*-
(setq ruby-insert-encoding-magic-comment nil)

(load "php-mode")

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(load "magit")
(put 'downcase-region 'disabled nil)

(add-hook 'coffee-mode-hook
      '(lambda()
        (setq tab-width 2)))

; (add-hook 'c-mode-hook
;           '(lambda()
;             (setq tab-width 4)))

;; Hooks defined by Linux coding style guide.
(defun c-lineup-arglist-tabs-only (ignored)
"Line up argument lists by tabs, not spaces"
(let* ((anchor (c-langelem-pos c-syntactic-element))
     (column (c-langelem-2nd-pos c-syntactic-element))
     (offset (- (1+ column) anchor))
     (steps (floor offset c-basic-offset)))
(* (max steps 1)
   c-basic-offset)))

(add-hook 'c-mode-common-hook
      (lambda ()
        ;; Add kernel style.
        (c-add-style
         "linux-tabs-only"
         '("linux" (c-offsets-alist
                    (arglist-cont-nonempty
                     c-lineup-gcc-asm-reg
                     c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
      (lambda ()
        (let ((filename (buffer-file-name)))
          ;; Enable kernel mode for the appropriate files
          ; (when (and filename
          ;            (string-match (expand-file-name "~/src/linux-trees")
          ;                          filename))
        (setq indent-tabs-mode t)
        (c-set-style "linux-tabs-only"))))

(add-to-list 'load-path "~/.emacs.d/elisp/jump")
(add-to-list 'load-path "~/.emacs.d/elisp/rinari")
(require 'rinari)

;; MuMaMo-Mode for rhtml files.
(add-to-list 'load-path "~/.emacs.d/nxhtml/util")
(require 'mumamo-fun)
(setq mumamo-chunk-coloring 'submode-colored)
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . eruby-html-mumamo))
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-html-mumamo))
;; Ditch the garish blue background.
(setq mumamo-background-colors nil)