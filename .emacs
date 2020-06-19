;;;; Universal settings.

(setq ring-bell-function 'ignore ; No more stupid beeping.
			initial-scratch-message ""
			initial-major-mode 'org-mode ; Make Org mode the default.
			require-final-newline nil)
(setq-default tab-width 2)
(setq-default require-final-newline nil)
(setq-default cursor-type 'bar) ; Thin cursor.

;; Remove the stuff. Not that it's useless just that it's ugly.
(when (fboundp 'scroll-bar-mode)
	(scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
	(menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
	(tool-bar-mode -1))

(fset 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no.

;; Combo to scroll 1 line.
(defun scroll-down-1 ()
	(interactive)
	(scroll-up 2))

(defun scroll-up-1 ()
	(interactive)
	(scroll-down 2))

(global-set-key (kbd "<C-down>") 'scroll-down-1)
(global-set-key (kbd "<C-up>") 'scroll-up-1)

;; Make new buffers.
(setq new-buffer-number 1)
(defun new-buffer ()
	"Make new buffer and select it."
	(interactive)
	(switch-to-buffer (concat "*scratch<" (number-to-string new-buffer-number) ">*"))
	(setq new-buffer-number (1+ new-buffer-number)))

(global-set-key (kbd "C-c t") 'new-buffer)

(setq make-backup-files nil) ; Stop creating backup~ files.
(setq auto-save-default nil) ; Stop creating #autosave# files.
(setq create-lockfiles nil) ; Stop creating dumb links.

;; Reopen killed buffers.
(defvar killed-file-list nil
  "List of recently killed files.")

(defun add-file-to-killed-file-list ()
  "If buffer is associated with a file name, add that file to the
`killed-file-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name killed-file-list)))

(add-hook 'kill-buffer-hook 'add-file-to-killed-file-list)

(defun reopen-killed-file ()
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (when killed-file-list
    (find-file (pop killed-file-list))))

(global-set-key (kbd "C-c T") 'reopen-killed-file)

;; Set default directory.
(setq inhibit-startup-message t)
(cond
 ((string-equal system-type "windows-nt")
	(progn
		(let ((Ben (concat (getenv "HOME") "/../../Documents/Ben/")))
			(setq default-directory Ben)
			(setenv "Ben" Ben))))) ; cd $Ben to move to the default Windows Ben folder.

(defun id (x) x)

(put 'upcase-region 'disabled nil) ; C-x C-uppercase
(put 'downcase-region 'disabled nil) ; C-x C-lowercase
(put 'narrow-to-region 'disabled nil) ; C-x n n Narrow to region.
(put 'dired-find-alternate-file 'disabled nil) ; a to open file in the same buffer.

;; org-mode settings.
(setq org-catch-invisible-edits t)
(setq org-return-follows-link t)
(font-lock-add-keywords
 'org-mode
 '(("^ *\\([-]\\) "
		(0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Remember settings.
(global-set-key (kbd "C-x r r") 'remember) ; Take quick notes.
(setq remember-data-file "~/.emacs.d/notes.org")

;; No more *Messages* buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Kill *Completions* buffer after use.
(defun kill-completions-buffer ()
	(let ((buffer "*Completions*"))
		(and (get-buffer buffer)
				 (kill-buffer buffer))))
(add-hook 'minibuffer-exit-hook 'kill-completions-buffer)

(setq set-mark-command-repeat-pop t) ; C-SPC after C-u C-SPC to keep popping.

(set-face-attribute 'default nil :font "Fantasque Sans Mono-14") ;; Dang does Comic Sans Mono look good.

(desktop-save-mode 1)

(global-set-key (kbd "<mouse-5>") 'next-buffer)
(global-set-key (kbd "<mouse-4>") 'previous-buffer)

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun toggle-window ()
	"Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

;;;; MELPA

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 (quote
		("5d75f9080a171ccf5508ce033e31dbf5cc8aa19292a7e0ce8071f024c6bcad2a" default)))
 '(package-selected-packages
	 (quote
		(ace-window dash haskell-mode popup-kill-ring popup-kill-ringup aggressive-indent paredit undo-tree esup ace-jump-mode company-lua company lua-mode avk-emacs-themes)))
 '(undo-tree-visualizer-diff nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(popup-face ((t (:inherit default :background "dark slate blue" :foreground "peach puff"))))
 '(popup-isearch-match ((t (:inherit default :background "medium slate blue"))))
 '(popup-menu-mouse-face ((t (:background "royal blue" :foreground "peach puff"))))
 '(popup-menu-selection-face ((t (:inherit default :background "slate blue" :foreground "peach puff"))))
 '(popup-scroll-bar-background-face ((t (:background "dark slate blue"))))
 '(popup-scroll-bar-foreground-face ((t (:background "gray8")))))

;;;; Package dependent settings.

(load-theme 'avk-darkblue-yellow)
(add-hook 'after-init-hook 'global-company-mode) ; Autocomplete mode.
(global-set-key (kbd "M-y") 'popup-kill-ring) ; Show little menu for killed things.

;; Ace Jump Mode settings.
(global-set-key (kbd "C-x C-SPC") 'ace-jump-char-mode) ; Jump really fast.
(global-set-key (kbd "C-x C-S-SPC") 'ace-jump-line-mode) ; Jump to line.

(global-set-key (kbd "C-x o") 'ace-window) ; Jump to window.

;; C-/ undo
;; C-? redo
;; C-x u show tree.
(global-undo-tree-mode)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)

(defun lua-settings ()
	(setq indent-tabs-mode t
				tab-width 3
				lua-indent-level tab-width
				require-final-newline nil)
	(aggressive-indent-mode 1))
(add-hook 'lua-mode-hook 'lua-settings)

(defun js-settings ()
	(setq indent-tabs-mode t
				tab-width 2
				js-indent-level tab-width
				js-expr-indent-offset (- js-indent-level)
				js-switch-indent-offset tab-width
				require-final-newline nil)
	(aggressive-indent-mode 1))
(add-hook 'js-mode-hook 'js-settings)

(defun css-settings ()
	(setq indent-tabs-mode t
				tab-width 2
				css-indent-offset tab-width
				require-final-newline nil)
	(aggressive-indent-mode 1))
(add-hook 'css-mode-hook 'css-settings)

(defun elisp-settings ()
	(aggressive-indent-mode 1))
(add-hook 'emacs-lisp-mode-hook 'elisp-settings)

(defun scheme-settings ()
	(aggresive-indent-mode 1))
(add-hook 'scheme-mode-hook 'scheme-settings)

(defun agda-setup ()
	"Load Agda and the DejaVu font to see special symbols."
	(interactive)
	(load-file (let ((coding-system-for-read 'utf-8))
							 (shell-command-to-string "agda-mode locate")))
	(setq agda2-highlight-face-groups 'default-faces)
	(set-face-attribute 'default nil
											:font "DejaBVu Sans Mono-14"
											:box nil))

(load "~/.emacs.d/eshell-utilities.el") ; Load the utilites.

;; Eshell settings.
(defun eshell-setup ()
	(require 's)
	(require 'dash)
	;; Aliases.
	(eshell/alias "cls" "clear 1") ; Delete Eshell contents instead of just scrolling.
	(eshell/alias "dh" "dired .")

	(eshell/setq "nl" "\n") ; For some reason "\n" doesn't work for me. This a nice workaround.
	;; Commands that require some level of interactivity.
	;; (add-to-list 'eshell-visual-commands "lua")
	(local-set-key (kbd "C-c e") 'insert-eshell-prefix))

(add-hook 'eshell-mode-hook 'eshell-setup)

(setq eshell-buffer-shorthand t)

(defun eshell-prompt ()
	(concat
	 (if (= (user-uid) 0) "root" (or (getenv "USER") "Camto"))
	 "@"
	 (abbreviate-file-name (eshell/pwd))
	 "> "))
(setq eshell-prompt-function 'eshell-prompt)
(setq eshell-prompt-regexp "^[^\n@]*@[^\n>]*> ")

(eval-after-load "em-ls"
  '(progn
     (defun ted-eshell-ls-find-file-at-point (point)
       "RET on Eshell's `ls' output to open files."
       (interactive "d")
       (find-file (buffer-substring-no-properties
                   (previous-single-property-change point 'help-echo)
                   (next-single-property-change point 'help-echo))))

     (defun pat-eshell-ls-find-file-at-mouse-click (event)
       "Middle click on Eshell's `ls' output to open files.
 From Patrick Anderson via the wiki."
       (interactive "e")
       (ted-eshell-ls-find-file-at-point (posn-point (event-end event))))

     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "RET") 'ted-eshell-ls-find-file-at-point)
       (define-key map (kbd "<return>") 'ted-eshell-ls-find-file-at-point)
       (define-key map (kbd "<mouse-2>") 'pat-eshell-ls-find-file-at-mouse-click)
       (defvar ted-eshell-ls-keymap map))

     (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
       "Eshell's `ls' now lets you click or RET on file names to open them."
       (add-text-properties 0 (length ad-return-value)
                            (list 'help-echo "RET, mouse-2: visit this file"
                                  'mouse-face 'highlight
                                  'keymap ted-eshell-ls-keymap)
                            ad-return-value)
       ad-return-value)))

(defun ted-eshell-ls-find-file ()
  (interactive)
	(let ((fname (buffer-substring-no-properties
								(previous-single-property-change (point) 'help-echo)
								(next-single-property-change (point) 'help-echo))))
    ;; Remove any leading whitespace, including newline that might
    ;; be fetched by buffer-substring-no-properties
	  (setq fname (replace-regexp-in-string "^[ \t\n]*" "" fname))
    ;; Same for trailing whitespace and newline
	  (setq fname (replace-regexp-in-string "[ \t\n]*$" "" fname))
	  (cond
	   ((equal "" fname)
	    (message "No file name found at point"))
	   (fname
	    (find-file fname)))))
