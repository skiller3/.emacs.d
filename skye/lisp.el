(provide 'lisp)

(defun elisp-doc () (interactive)
  "Show table of contents for the Elisp manual."
  (progn
    (split-window-right)
    (other-window 1)
    (info "elisp")))

(defun setup-lisp ()
  "Configures general lisp functionality"
  (progn
    ; Decided that paredit is too intense, so the following has been commented out:
    ;; (autoload 'enable-paredit-mode "paredit" 
    ;;   "Turn on pseudo-structural editing of Lisp code." t)
    ;; (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    ;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    ;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    ;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    ;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    ;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
    (global-set-key (kbd "C-S-d") 'elisp-doc)))
