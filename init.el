(add-to-list 
 'custom-theme-load-path "~/.emacs.d/elpa/zenburn-theme-2.1")

(add-to-list 
 'exec-path "~/workspace/tools/anaconda3/bin")

(load-theme 'zenburn t)

(add-hook 'python-mode-hook 'elpy-mode)

(setq postgres-connections
      '((betterbeta-development (sql-port 5432)
				(sql-server "betterbeta-development.c2okzdbgrkaw.us-east-1.rds.amazonaws.com")
				(sql-user "betterbetamaster")
				(sql-database "betterbeta"))
	(betterbeta-development-2 (sql-port 5432)
				  (sql-server "betterbeta-development-2.c2okzdbgrkaw.us-east-1.rds.amazonaws.com")
				  (sql-user "betterbetamaster")
				  (sql-database "betterbeta"))
	(betterbeta-development-3 (sql-port 5432)
				  (sql-server "betterbeta-development-3.c2okzdbgrkaw.us-east-1.rds.amazonaws.com")
				  (sql-user "betterbetamaster")
				  (sql-database "betterbeta"))
	(betterbeta-test (sql-port 5432)
			 (sql-server "betterbeta-test.c2okzdbgrkaw.us-east-1.rds.amazonaws.com")
			 (sql-user "betterbetamaster")
			 (sql-database "betterbeta"))
	(betterbeta-production (sql-port 5432)
			       (sql-server "betterbeta-production.c2okzdbgrkaw.us-east-1.rds.amazonaws.com")
			       (sql-user "betterbetamaster")
			       (sql-database "betterbeta"))))

(defun pgsql-connect (name)
  (interactive "SConnection Name: ")
  (let ((conn (cdr (assoc name postgres-connections))))
    (let ((keys (mapcar 'car conn)))
      (mapcar (lambda (key) (set key (car (cdr (assoc key conn))))) keys)
      (call-interactively 'sql-postgres))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "https://marmalade-repo.org/packages/") ("melpa" . "http://melpa.org/packages/")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#151515" :foreground "#c6a57b" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 96 :width normal :foundry "unknown" :family "Ubuntu Mono")))))

