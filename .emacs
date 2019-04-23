;;;; Universal settings.

(setq ring-bell-function 'ignore ; No more stupid beeping.
			initial-scratch-message ""
			require-final-newline nil)
(setq-default tab-width 2)
(setq-default require-final-newline nil)
(setq-default cursor-type 'bar) ; Thin cursor.

;; Remove the stuff. Not that it's useless just that it's ugly.
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Combo to scroll 1 line.
(defun scroll-down-1 ()
	(interactive)
	(scroll-up 1))

(defun scroll-up-1 ()
	(interactive)
	(scroll-down 1))

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

;; org-mode settings.
(setq org-catch-invisible-edits t)

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
(set-face-attribute 'default nil :font "Fira Code-12")

;; Quickly zoom in and out of the buffer.
(defun zoom-out ()
	(interactive)
	(text-scale-set -10))

(defun zoom-reset ()
	(interactive)
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

(defun window-split-toggle ()
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

;; Eshell settings.
(defun eshell-setup ()
	(eshell/alias "cls" "clear 1")) ; Delete Eshell contents instead of just scrolling.
	;; Commands that require some level of interactivity.
	;; (add-to-list 'eshell-visual-commands "lua")

(add-hook 'eshell-mode-hook 'eshell-setup)

(defun eshell/-buffer-as-args (buffer separator command)
  "Takes the contents of BUFFER, and splits it on SEPARATOR, and
runs the COMMAND with the contents as arguments. Use an argument
`%' to substitute the contents at a particular point, otherwise,
they are appended."
  (let* ((lines (with-current-buffer buffer
                  (split-string
                   (buffer-substring-no-properties (point-min) (point-max))
                   separator)))
         (subcmd (if (-contains? command "%")
                     (-flatten (-replace "%" lines command))
                   (-concat command lines)))
         (cmd-str  (string-join subcmd " ")))
    (message cmd-str)
    (eshell-command-result cmd-str)))

(defun eshell/bargs (buffer &rest command)
  "Passes the lines from BUFFER as arguments to COMMAND."
  (eshell/-buffer-as-args buffer "\n" command))

(defun eshell/sargs (buffer &rest command)
  "Passes the words from BUFFER as arguments to COMMAND."
  (eshell/-buffer-as-args buffer nil command))

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
		(undo-tree esup ace-jump-mode company-lua company helm-descbinds lua-mode smooth-scrolling avk-emacs-themes)))
 '(undo-tree-visualizer-diff nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;; Package dependent settings.

(load-theme 'avk-darkblue-yellow)
(add-hook 'after-init-hook 'global-company-mode) ; Autocomplete mode.

(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-S-y") 'helm-show-kill-ring)

;; Ace Jump Mode settings.
(global-set-key (kbd "C-x C-SPC") 'ace-jump-char-mode) ; Jump really fast.
(global-set-key (kbd "C-x C-S-SPC") 'ace-jump-line-mode) ; Jump to line.

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

(put 'upcase-region 'disabled nil) ; C-x C-uppercase
(put 'downcase-region 'disabled nil) ; C-x C-lowercase

(helm-descbinds-mode) ; C-h b for current key combos.
