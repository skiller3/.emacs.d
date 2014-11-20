(provide 'python)

(defun setup-python ()
  "Configures python-specific functionality"
  (progn
    ; The python-oriented material all came out of Jessica Hamrick's page/blog:
    ; http://www.jesshamrick.com/2012/09/18/emacs-as-a-python-ide/
    (require 'ipython)

    ; python-mode
    (require 'python-mode)
    (setq py-install-directory (from-load-path "python-mode.el-6.1.3"))
    (add-to-list 'load-path py-install-directory)

    ; use IPython
    (setq-default py-shell-name "ipython")
    (setq-default py-which-bufname "IPython")

    ; set reasonable key-bindings for python 
    ; NOTE: C-c C-c would usually execute buffer
    (define-key python-mode-map (kbd "C-c C-a") 'py-execute-buffer)
    (define-key python-mode-map (kbd "C-c C-c") 'py-execute-line) 
    (define-key python-mode-map (kbd "C-c C-v") 'py-execute-region) 

    (symbol-function 'py-execute-buffer)

    ; use the wx backend, for both mayavi and matplotlib
    (setq py-python-command-args
	  '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
    (setq py-force-py-shell-name-p t)

    ; switch to the interpreter after executing code
    (setq py-shell-switch-buffers-on-execute-p nil)
    (setq py-switch-buffers-on-execute-p nil)

    ; don't split windows
    (setq py-split-windows-on-execute-p nil)

    ; try to automagically figure out indentation
    (setq py-smart-indentation t)))