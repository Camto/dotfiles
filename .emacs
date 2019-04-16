; Universal settings.

(setq ring-bell-function 'ignore ;;; No more stupid beeping.
			initial-scratch-message ""
			require-final-newline nil)
(setq-default tab-width 2)
(setq-default require-final-newline nil)
(setq-default cursor-type 'bar) ;;; Thin cursor.


;; Set default directory.
(setq inhibit-startup-message t)
(cond
 ((string-equal system-type "windows-nt")
	(progn
		(setq default-directory (concat (getenv "HOME") "/../../Documents/Ben/")))))

;; No more *Messages* buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Kill *Completions* buffer after use.
(defun kill-completions-buffer ()
	(let ((buffer "*Completions*"))
		(and (get-buffer buffer)
				 (kill-buffer buffer))))
(add-hook 'minibuffer-exit-hook 'kill-completions-buffer)

(setq set-mark-command-repeat-pop t) ;;; C-SPC after C-u C-SPC to keep popping.
(set-face-attribute 'default nil :font "Fira Code-12")

(if (version<= "26.0.50" emacs-version)
		(global-display-line-numbers-mode)
	(global-linum-mode 1))

;; Quickly zoom in and out of the buffer.
(defun zoom-out ()
	(interactive)
  (if (version<= "26.0.50" emacs-version)
			(display-line-numbers-mode -1)
		(linum-mode -1))
	(text-scale-set -10))

(defun zoom-reset ()
	(interactive)
  (if (version<= "26.0.50" emacs-version)
			(display-line-numbers-mode 1)
		(linum-mode 1))
	(text-scale-set 0))

(global-set-key (kbd "C-x C-_") 'zoom-out)
(global-set-key (kbd "C-x C-+") 'zoom-reset)

(desktop-save-mode 1)

(global-set-key (kbd "<mouse-5>") 'next-buffer)
(global-set-key (kbd "<mouse-4>") 'previous-buffer)

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

; MELPA

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
 '(neo-theme (quote ascii))
 '(package-selected-packages
	 (quote
		(buffer-move ace-jump-mode company-lua company undo-tree helm-descbinds neotree lua-mode smooth-scrolling avk-emacs-themes)))
 '(undo-tree-visualizer-diff t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; Package dependent settings.

(load-theme 'avk-darkblue-yellow)
(neotree)
(global-set-key (kbd "C-x n t") 'neotree) ;;; Open NeoTree.
(add-hook 'after-init-hook 'global-company-mode) ;;; Autocomplete mode.

(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-S-y") 'helm-show-kill-ring)

;; Ace Jump Mode settings.
(global-set-key (kbd "C-c C-SPC") 'ace-jump-char-mode) ;;; Jump really fast.
(global-set-key (kbd "C-c C-c C-SPC") 'ace-jump-line-mode) ;;; Jump to line.

;; C-/ undo
;; C-? redo
;; C-x u show tree.
(global-undo-tree-mode)

(defun lua-settings ()
	(setq indent-tabs-mode t
				tab-width 3
				lua-indent-level 3
				require-final-newline nil))
(add-hook 'lua-mode-hook 'lua-settings)

;; Don't jank scroll when curson moves.
(smooth-scrolling-mode 1)

(put 'upcase-region 'disabled nil) ;;; C-x C-uppercase
(put 'downcase-region 'disabled nil) ;;; C-x C-lowercase

;; org-mode settings.
(setq org-catch-invisible-edits t
			org-support-shift-select t)

(helm-descbinds-mode) ;;; C-h b for current key combos.
