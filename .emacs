(setq lexical-binding t) ; use lexical closures

(require 'cl) ; use cl functions

(defmacro fn-1 (body) 
  "Convenience macro for defining anonymous unary functions"
  `(lambda (_) ,body))

(defun string-ends-with (str suffix)
  "Determines if a string ends with a particular suffix"
  (string= suffix (substring str (- (length str) (length suffix)))))

(defun from-load-path (suffix)
  "Returns the first item on the emacs load-path with the specified suffix"
  (car (remove-if-not 
	(fn-1 (string-ends-with _ suffix))
	load-path)))

(defun open-load-file (name) (interactive "MEnter elisp file name: ") 
  "Opens .emacs or an elisp file that is on the editor's load-path"
  (if (or (string= name "emacs") (string= name ".emacs") (string= name ""))
    (find-file (from-load-path ".emacs"))
    (let ((fname (if (string-ends-with name ".el") name (concat name ".el"))))
      (find-file (from-load-path fname)))))

(defun kbind-global (key-str symbol)
  "Shortcut function for globally binding a command to a key sequence"
  (global-set-key (kbd key-str) symbol))


(kbind-global "C-x C-o" 'open-load-file)

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

(kbind-global "C-x C-n" 'make-frame)
(kbind-global "C-x C-k" 'delete-frame)
