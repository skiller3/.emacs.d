;;; tidy-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "tidy" "tidy.el" (22812 41711 0 0))
;;; Generated autoloads from tidy.el

(autoload 'tidy-build-menu "tidy" "\
Set up the tidy menu in MAP. Used to set up a Tidy menu in your
favourite mode.

\(fn &optional MAP)" t nil)

(autoload 'tidy-parse-config-file "tidy" "\
If `tidy-config-file' is non-nil parse that file setting variables accordingly.

\(fn)" t nil)

(autoload 'tidy-save-settings "tidy" "\
Query saving the current settings to your `tidy-config-file'.
Perhaps put this on your `kill-buffer-hook'.

\(fn &optional CONFIG-FILE)" t nil)

(autoload 'tidy-buffer "tidy" "\
Run the HTML Tidy program on the current buffer.
If PREFIX is non-nil, or if called interactively with a prefix argument,
then Tidy is applied to the currently selected region.  Any error messages
generated by that program are sent to \"*tidy-errors*\" buffer.

\(fn &optional PREFIX)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; tidy-autoloads.el ends here
