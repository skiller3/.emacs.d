;;;;;;;;;;;;;;;;
;; Base config ;
;;;;;;;;;;;;;;;;

;; Turn of the annoying bell
(setq visible-bell 1)

;; Create key-binding that makes opening *THIS* file easy
(defun find-init-file ()
    (interactive)
    (find-file "c:/Users/skye/AppData/Roaming/.emacs.d/init.el"))
(global-set-key (kbd "C-x C-i") 'find-init-file)

;; Initialize package manager and add extra repositories
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa.org/packages/")
	     '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)

;; Apply a pleasant theme
(load-theme 'zenburn t)

;; Sicker command completion than normal using helm
(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)

;; Sick cursor movement commands
(require 'ace-jump-mode)
(global-set-key (kbd "C-c C-c") 'ace-jump-char-mode)
(global-set-key (kbd "C-c C-w") 'ace-jump-word-mode)
(global-set-key (kbd "C-c C-l") 'ace-jump-line-mode)

;; Sick semantic selection expansion
(require 'expand-region)
(global-set-key (kbd "C-c a") 'er/expand-region)
(global-set-key (kbd "C-c C-a") 'er/expand-region)

;; Set up eshell so we don't have to use Command Prompt
(setq eshell-login-script "c:/Users/skye/AppData/Roaming/.emacs.d/eshell/login")



;;;;;;;;;;;;;;;;;;;;;
;; JavaScript stuff ;
;;;;;;;;;;;;;;;;;;;;;

(add-hook 'js-mode-hook 'js2-minor-mode)
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)

;; Turn on paredit for JS
(defun my-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))
(add-hook 'js-mode-hook 'my-paredit-nopnlisp)

;; Formatting command for JS
(add-hook 'js-mode-hook 
	  (lambda () (local-set-key (kbd "C-c C-f") 'web-beautify-js)))

;; JS file-type assignments
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js-mode))



;;;;;;;;;;;;;;;;;;;;;
;; HTML & CSS stuff ;
;;;;;;;;;;;;;;;;;;;;;

;; Auto opening, auto completion, auto expanders, code folding, navigation, 
;; snippets - HTML/Django, context aware processing.
(require 'web-mode)

;; Establish formatting key-bindings for HTML & CSS
(add-hook 'html-mode-hook 
	  (lambda () (local-set-key (kbd "C-c C-f") 'web-beautify-html)))
(add-hook 'css-mode-hook 
	  (lambda () (local-set-key (kbd "C-c C-f") 'web-beautify-css)))

;; HTML & CSS file-type assignments
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.j2$" . jinja2-mode))

;; Define web-mode variables

; Don't really know what this does:
; (setq web-mode-engines-alist '(("django" . "\\.html\\'")))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-expanding t)
(setq web-mode-enable-css-colorization t)

;;;;;;;;;;;;;;;;;
;; Python stuff ;
;;;;;;;;;;;;;;;;;

;; Most of this config was taken from this presentation:
;; http://chillaranand.github.io/emacs-py-ide/
(require 'elpy)
(elpy-enable)
(define-key elpy-mode-map (kbd "C-c r") 'elpy-shell-send-region-or-buffer)
(define-key elpy-mode-map (kbd "C-c C-c") 'ace-jump-char-mode)


(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;
;; API Blueprint stuff ;
;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'apib-mode "apib-mode"
  "Major mode for editing API Blueprint files" t)
(add-to-list 'auto-mode-alist '("\\.apib$" . apib-mode))

;;;;;;;;;;;;;;;;;;;;;;;
;; Custom face config ;
;;;;;;;;;;;;;;;;;;;;;;;

;; Apply any custom look-and-feel adjustments that have been made through
;; customize-faces menu
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
