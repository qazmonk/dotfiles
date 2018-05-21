
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" default)))
 '(linum-format " %7i ")
 '(package-selected-packages
   (quote
    (visible-mark use-package undo-tree tuareg tagedit smex slime-company paredit neotree matlab-mode magit lua-mode lorem-ipsum imenu-anywhere image+ ido-ubiquitous highlight-parentheses help-fns+ god-mode gitignore-mode font-lock-studio electric-spacing electric-operator electric-case diminish crontab-mode company-auctex cmake-mode beacon ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
