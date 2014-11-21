;; -*- lexical-binding: t; -*-

(require 'cl)

(defmacro disable-y-or-n (unquoted-fn-symbol)
  "Convenience macro for silencing  y/n prompts associated with a command"
  `(defadvice ,unquoted-fn-symbol (around stfu compile activate)
     (flet ((yes-or-no-p (&rest args) t)
	    (y-or-n-p (&rest args) t))
       ad-do-it)))

(defmacro bind-key (key-str unquoted-fn-symbol)
  "Shortcut macro for globally binding a command to a key sequence"
  `(global-set-key (kbd ,key-str) (quote ,unquoted-fn-symbol)))

(defmacro lambda-0 (body)
  `(lambda () ,body))

(defmacro lambda-1 (body)
  `(lambda (_) ,body))

(defun with-split (split-fn action-fn)
  "Wraps an interactive function so its work is done in another buffer"
   (lambda () (interactive)
     (progn
       (funcall split-fn)
       (other-window 1)
       (funcall action-fn))))

(fset 'with-split-right (lambda-1 (with-split #'split-window-right _)))
(fset 'with-split-below (lambda-1 (with-split #'split-window-below _)))

(defun elisp-doc () (interactive)
  "Show table of contents for the Elisp manual."
  (info "elisp"))

(defun string-ends-with (str suffix)
  "Determines if a string ends with a particular suffix"
  (string= suffix (substring str (- (length str) (length suffix)))))

(defun from-load-path (suffix)
  "Returns the first item on the emacs load-path with the specified suffix"
  (car (remove-if-not 
	(lambda-1 (string-ends-with _ suffix))
	load-path)))

(defun open-load-file (name) (interactive "MEnter elisp file name: ") 
  "Opens .emacs or an elisp file that is on the editor's load-path"
  (if (or (string= name "emacs") (string= name ".emacs") (string= name ""))
    (find-file (from-load-path ".emacs"))
    (let ((fname (if (string-ends-with name ".el") name (concat name ".el"))))
      (find-file (from-load-path fname)))))

(defun load-recursive (file)
  "Function that recursively adds the contents of a particular directory onto the editor's load path"
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

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (from-load-path "ac-dict"))
(ac-config-default)
(ac-quick-help t)
(setq ac-quick-help-delay 0.1)

(require 'highlight-current-line)
(highlight-current-line-on t)

(bind-key "C-o" open-load-file)
(fset 'open-load-file-split-right (with-split-right (lambda-0 (open-load-file))))
(fset 'open-load-file-split-below (with-split-below (lambda-0 (open-load-file))))
(bind-key "C-x C-3 C-o" open-load-file-split-right)
(bind-key "C-x C-2 C-o" open-load-file-split-below)
(bind-key "C-d" elisp-doc)
(fset 'elisp-doc-split-right (with-split-right (lambda-0 (elisp-doc))))
(fset 'elisp-doc-split-below (with-split-below (lambda-0 (elisp-doc))))
(bind-key "C-x C-3 C-d" elisp-doc-split-right)
(bind-key "C-x C-2 C-d" elisp-doc-split-below)
(bind-key "C-x C-n" make-frame)
(bind-key "C-x C-k" delete-frame)
(bind-key "C-x k" kill-this-buffer)
(bind-key "C-t" ansi-term)
(fset 'ansi-term-split-right (with-split-right (lambda-0 (call-interactively 'ansi-term))))
(fset 'ansi-term-split-below (with-split-below (lambda-0 (call-interactively 'ansi-term))))
(bind-key "C-x C-3 C-t" ansi-term-split-right)
(bind-key "C-x C-2 C-t" ansi-term-split-below)
(bind-key "C-l" ielm)
(fset 'ielm-split-right (with-split-right (lambda-0 (ielm))))
(fset 'ielm-split-below (with-split-below (lambda-0 (ielm))))
(bind-key "C-x C-3 C-l" ielm-split-right)
(bind-key "C-x C-2 C-l" ielm-split-below)

(disable-y-or-n kill-this-buffer)

