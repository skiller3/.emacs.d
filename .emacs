(setq lexical-binding t) ; use lexical closures

(require 'cl) ; use cl functions

(defmacro disable-y-or-n (unquoted-symbol)
  "Convenience macro for silencing  y/n prompts associated with a command"
  `(defadvice ,unquoted-symbol (around stfu compile activate)
     (flet ((yes-or-no-p (&rest args) t)
	    (y-or-n-p (&rest args) t))
       ad-do-it)))

(defmacro bind-key (key-str unquoted-symbol)
  "Shortcut macro for globally binding a command to a key sequence"
  `(global-set-key (kbd ,key-str) (quote ,unquoted-symbol)))

(defun string-ends-with (str suffix)
  "Determines if a string ends with a particular suffix"
  (string= suffix (substring str (- (length str) (length suffix)))))

(defun from-load-path (suffix)
  "Returns the first item on the emacs load-path with the specified suffix"
  (car (remove-if-not 
	(lambda (file) (string-ends-with file suffix))
	load-path)))

(defun open-load-file (name) (interactive "MEnter elisp file name: ") 
  "Opens .emacs or an elisp file that is on the editor's load-path"
  (if (or (string= name "emacs") (string= name ".emacs") (string= name ""))
    (find-file (from-load-path ".emacs"))
    (let ((fname (if (string-ends-with name ".el") name (concat name ".el"))))
      (find-file (from-load-path fname)))))

(defun load-recursive (file)
  "Function that recursively adds the contents of a particular directory 
onto the editor's load path"
  (progn
    (print (concat "Adding to load path: " file))
    (add-to-list 'load-path file)
    (if (and (file-directory-p file) 
	     (not (string-ends-with file ".")))
	(let ((sub-files (directory-files file t)))
	  (mapcar (symbol-function 'load-recursive) sub-files)))
    load-path))

; Add everything in ~/.emacs.d to the load path
(load-recursive (expand-file-name "~/.emacs.d"))

(require 'appearance)
(setup-appearance)

(require 'python)
(setup-python)

(require 'lisp)
(setup-lisp)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (from-load-path "ac-dict"))
(ac-config-default)
(ac-quick-help t)
(setq ac-quick-help-delay 0.1)

(require 'highlight-current-line)
(highlight-current-line-on t)

(bind-key "C-x C-o" open-load-file)
(bind-key "C-x C-n" make-frame)
(bind-key "C-x C-k" delete-frame)
(bind-key "C-x k" kill-this-buffer)
(bind-key "C-t" ansi-term)

(disable-y-or-n kill-this-buffer)
