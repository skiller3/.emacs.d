(provide 'appearance)

(defun setup-appearance () 
  "Sets up a decent look-and-feel for emacs"
  (progn
    (custom-set-variables
       ;; custom-set-variables was added by Custom.
       ;; If you edit it by hand, you could mess it up, so be careful.
       ;; Your init file should contain only one such instance.
       ;; If there is more than one, they won't work right.
     )

    ;(custom-set-faces
    ;; custom-set-faces was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.
    
    ; '(default ((t (:inherit nil :stipple nil :background "white" 
    ; :foreground "black" :inverse-video nil :box nil :strike-through 
    ; nil :overline nil :underline nil :slant normal :weight normal 
    ; :height 96 :width normal :foundry "unknown" :family "Ubuntu Mono"
    ; 
    
    ; use the "Subtle Hacker" color theme as a base for the custom scheme
    (require 'color-theme)
    (color-theme-initialize)
    (setq color-theme-is-global t)
    (color-theme-subtle-hacker)

    ; Color matching parenthesis in all programming-related modes
    (require 'rainbow-delimiters)
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

    (custom-set-faces
     '(default ((t (:overline nil :inherit nil :stipple nil :background "gray2"
			      :foreground "#FFF991" :inverse-video nil :box nil
			      :strike-through nil :underline nil
			      :slant normal :weight normal :height 100 :width normal
			      :foundry "unknown" :family "DejaVu Sans Mono"))))
     '(border ((t nil)))
     '(cursor ((t (:background "firebrick1" :foreground "black"))))
     '(font-lock-comment-delimiter-face
       ((default (:inherit font-lock-comment-face :weight ultra-bold))
	(((class color) (min-colors 16)) nil)))
     '(font-lock-comment-face ((t (:foreground "lime green"))))
     '(font-lock-doc-face ((t (:foreground "tomato" :slant italic))))
     '(font-lock-function-name-face
       ((t (:foreground "deep sky blue" :underline t :weight bold))))
     '(font-lock-keyword-face ((t (:foreground "gold" :weight bold))))
     '(font-lock-string-face ((t (:foreground "tomato" :slant italic))))
     '(fringe ((nil (:background "black"))))
     '(highlight ((t (:background "black" :box (:line-width -1 :color "firebrick1")))))
     '(highlight-current-line-face ((t (:inherit highlight))))
     '(lazy-highlight ((t (:background "paleturquoise" :foreground "black"))))
     '(link ((t (:foreground "DodgerBlue3" :underline t))))
     '(menu ((t (:background "gray2" :foreground "#FFF991"))))
     '(minibuffer-prompt ((t (:foreground "royal blue"))))
     '(mode-line ((t (:background "dark olive green"
				  :foreground "dark blue"
				  :box (:line-width -1 :color "gray75")
				  :weight bold))))
     '(mode-line-buffer-id ((t (:background "dark olive green" :foreground "beige"))))
     '(mode-line-highlight ((((class color) (min-colors 88)) nil)))
     '(mode-line-inactive ((t (:background "dark olive green"
					   :foreground "dark khaki" :weight light))))
     '(mouse ((t (:background "Grey" :foreground "black"))))
     '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
     '(rainbow-delimiters-depth-2-face ((t (:foreground "lawn green"))))
     '(rainbow-delimiters-depth-3-face ((t (:foreground "light salmon"))))
     '(rainbow-delimiters-depth-4-face ((t (:foreground "dodger blue"))))
     '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
     '(rainbow-delimiters-depth-6-face ((t (:foreground "chocolate"))))
     '(rainbow-delimiters-depth-7-face ((t (:foreground "deep pink"))))
     '(rainbow-delimiters-depth-8-face ((t (:foreground "medium aquamarine"))))
     '(trailing-whitespace ((((class color) (background dark))
			     (:background "firebrick1")))))


    ; make sure the frames have the dark background mode by default
    (setq default-frame-alist (quote (
				      (frame-background-mode . dark)
				      )))))
