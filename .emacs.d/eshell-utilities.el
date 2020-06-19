;;;; General Eshell utilities.

(require 'cl)
(require 'cl-lib)
(require 'dash)

(defun insert-eshell-prefix ()
	"Insert prefix for Eshell commands."
	(interactive)
	(insert "eshell/"))

;;; Eshell functions.

(defun eshell/to-func (func)
	"Turn `func' into a callable object."
	(if (stringp func)
			(intern func)
		func))

(defun eshell/call (func &rest args)
	"Call `func' with `args' without needing to use apply and
listify."
	(apply (eshell/to-func func) args))

(defun eshell/map (func &rest ls)
	"Map over `ls' with `func' without need for listify."
	(mapcar (eshell/to-func func) ls))

(defun eshell/filter (pred &rest ls)
	"Filter `ls' by the function `pred' without need for listify."
	(seq-filter (eshell/to-func pred) ls))

(defun eshell/fold (func init &rest ls)
	"Fold `ls' by `func' starting with `init'"
	(seq-reduce (eshell/to-func func) ls init))

(defun eshell/make-prepend (str1)
	"Make a function that prepends `str1' to `str2'."
	(lexical-let ((tmp str1))
		#'(lambda (str2) (concat tmp str2))))

(defun eshell/make-append (str1)
	"Make a function that appends `str1' to `str2'."
	(lexical-let ((tmp str1))
		#'(lambda (str2) (concat str2 tmp))))

(defun eshell/make-concat (&rest format)
	"Make a function that replaces `str' in `format'."
	(lexical-let ((tmp format))
		#'(lambda (str) (mapconcat 'identity (-replace "%" str tmp) ""))))

(defun eshell/-buffer-as-args (buffer separator command)
  "Takes the contents of `buffer', and splits it on `separator', and
runs the `command' with the contents as arguments. Use an argument
`%' to substitute the contents at a particular point, otherwise,
they are appended."
  (let* ((lines (with-current-buffer buffer
									(split-string
									 (buffer-substring-no-properties (point-min) (point-max))
									 separator)))
         (subcmd (if (-contains? command "%")
                     (-flatten (-replace "%" lines command))
                   (-concat command lines)))
         (cmd-str (mapconcat 'identity subcmd " ")))
    (eshell-command-result cmd-str)))

(defun eshell/bargs (buffer &rest command)
  "Passes the lines from `buffer' as arguments to `command'."
  (eshell/-buffer-as-args buffer "\n" command))

(defun eshell/sargs (buffer &rest command)
  "Passes the words from `buffer' as arguments to `command'."
  (eshell/-buffer-as-args buffer nil command))

(defun eshell/-pass-buffer (buffer command)
  "Takes the contents of `buffer' and runs the `command' with the
contents as arguments. Use an argument `%' to substitute the
contents at a particular point, otherwise, they are appended."
  (let* ((buf (with-current-buffer buffer
								(prin1-to-string (buffer-substring-no-properties (point-min) (point-max)))))
         (subcmd (if (-contains? command "%")
                     (-replace "%" buf command)
									 (-concat command (list buf))))
         (cmd-str (mapconcat 'identity subcmd " ")))
		;; (message (prin1-to-string subcmd))
		;; (message "%s" cmd-str)
    (eshell-command-result cmd-str)))

(defun eshell/wargs (buffer &rest command)
	"Passes the whole `buffer' as argument to `command'"
	(eshell/-pass-buffer buffer command))
