
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(if (version< emacs-version "27.0")
    (package-initialize))
(setq custom-file "~/dotfiles/custom.el") ; move all customization to this file

(defun my-tangle-config-org ()
  "This function will write all source blocks from =config.org= into
=config.el= that are ...

- not marked as =tangle: no=
- doesn't have the TODO state =CANCELLED=
- have a source-code of =emacs-lisp="
  (require 'org)
  (let* ((body-list ())
         (output-file "~/dotfiles/config.el")
         (org-babel-default-header-args (org-babel-merge-params org-babel-default-header-args
                                                                (list (cons :tangle output-file)))))
    (message "Writing %s ..." output-file)
    (save-restriction
      (save-excursion
        (org-babel-map-src-blocks "~/dotfiles/config.org"
                                  (let* ((info (org-babel-get-src-block-info 'light))
                                         (tfile (cdr (assq :tangle (nth 2 info))))
                                         (match))
                                    (save-excursion
                                      (catch 'exit
                                        (org-back-to-heading t)
                                        (when (looking-at org-outline-regexp)
                                          (goto-char (1- (match-end 0))))
                                        (when (looking-at (concat " +" org-todo-regexp
								  "\\( +\\|[ \t]*$\\)"))
                                          (setq match (match-string 1)))))
                                    (unless (or (string= "no" tfile)
                                                (string= "CANCELED" match)
                                                (not (string= "emacs-lisp" lang)))
                                      (add-to-list 'body-list body)))))
      (with-temp-file output-file
        (insert ";; Don't edit this file, edit config.org' instead ...\n\n")
        (insert (apply 'concat (reverse body-list))))
      (message "Wrote %s ..." output-file))))

(when (file-newer-than-file-p "~/dotfiles/config.org" "~/dotfiles/config.el")
    (my-tangle-config-org))
(load "~/dotfiles/config.el")
(load custom-file)
