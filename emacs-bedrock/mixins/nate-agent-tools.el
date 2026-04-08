;;; nate-agent-tools.el --- Tool implementations for nate-agent  -*- lexical-binding: t -*-

;;; Commentary:
;; Tool registrations and supporting UI for nate-agent.
;; The edit_buffer tool is async: it opens an nate-agent-review-mode buffer
;; and returns nil, pausing the agent loop until the user accepts or rejects.
;;
;; Review buffer diff direction: --- current  +++ proposed
;; This means C-c C-a applies a proposed hunk to the current file.
;; Red (-) = what is currently in the file.  Green (+) = what the agent proposes.

;;; Code:

(require 'diff)
(require 'nate-agent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; nate-agent-review-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nate-agent-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'nate-agent-edit-accept-all)
    (define-key map (kbd "C-c C-p") #'nate-agent-edit-accept)
    (define-key map (kbd "C-c C-k") #'nate-agent-edit-reject)
    map)
  "Keymap for nate-agent-review-mode.")

(define-derived-mode nate-agent-review-mode diff-mode "Agent-Review"
  "Major mode for reviewing proposed edits from nate-agent.
Derives from diff-mode.  The diff shows proposed changes (---) vs the
current file (+++).

  Red   (-) lines = what the agent proposes to add
  Green (+) lines = what is currently in the file

\\[diff-apply-hunk]   apply hunk at point to the file
\\[nate-agent-edit-accept]   accept — write Result and close (file unchanged until you save)
\\[nate-agent-edit-reject]   reject — tell the agent the edit was not applied; buffer unchanged"
  (setq header-line-format
        (substitute-command-keys
         "\\[nate-agent-edit-accept-all] accept all edit \\[nate-agent-edit-accept] accept whichever edits were manually applied \\[nate-agent-edit-reject] reject")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Buffer-local state (set by nate-agent--setup-edit-review)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local nate-agent--review-agent-buf  nil "The *nate-agent* buffer.")
(defvar-local nate-agent--review-tool-id    nil "Tool ID for writing the Result.")
(defvar-local nate-agent--review-target-buf nil "Buffer being edited.")
(defvar-local nate-agent--review-prop-file  nil "Temp file holding proposed content.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Accept / reject
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nate-agent-edit-accept-all ()
  "Apply ALL hunks in the review buffer to the target, then accept.
Equivalent to running \\[diff-apply-hunk] on every hunk and then
calling \\[nate-agent-edit-accept].  The target buffer is saved."
  (interactive)
  (diff-apply-buffer)
  (nate-agent-edit-accept))

(defun nate-agent-edit-accept ()
  "Accept as-is: write a success Result and close the review buffer.
Any hunks you applied manually with \\[diff-apply-hunk] are already in
the target buffer.  Use \\[nate-agent-edit-accept-all] to apply all
hunks automatically before accepting."
  (interactive)
  (let ((agent-buf nate-agent--review-agent-buf)
        (tool-id   nate-agent--review-tool-id)
        (target    nate-agent--review-target-buf))
    (kill-buffer (current-buffer))
    (with-current-buffer target (save-buffer))
    (nate-agent--ui-write-tool-result agent-buf tool-id
      (format "Edit partially accepted. %s updated." (buffer-name target)))
    (nate-agent--schedule-step agent-buf)
    (switch-to-buffer agent-buf)))

(defun nate-agent-edit-reject ()
  "Reject the edit: write a rejection Result and close the review buffer.
Prompts for an optional comment to explain the rejection to the agent.
The target buffer is left as-is — you remain in control of its contents."
  (interactive)
  (let* ((agent-buf nate-agent--review-agent-buf)
         (tool-id   nate-agent--review-tool-id)
         (comment   (read-string "Rejection comment (optional): "))
         (result    (if (string-empty-p comment)
                        "Edit rejected by user."
                      (format "Edit rejected by user: %s" comment))))
    (kill-buffer (current-buffer))
    (nate-agent--ui-write-tool-result agent-buf tool-id result)
    (nate-agent--schedule-step agent-buf)
    (switch-to-buffer agent-buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Review buffer setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nate-agent--setup-edit-review (agent-buf tool-id target-buf edits)
  "Create and display a review buffer for proposed EDITS to TARGET-BUF.
EDITS is a list of (OLD-STR . NEW-STR) pairs applied in order.
Diffs proposed (---) vs current file (+++) so that \\[diff-apply-hunk]
applies proposed hunks to the real file."
  (unless (buffer-file-name target-buf)
    (error "edit_buffer requires a file-backed buffer; %s has no file"
           (buffer-name target-buf)))
  (let* ((review-name (format "*nate-agent-edit:%s*" tool-id))
         (prop-file   (make-temp-file "nate-agent-edit-")))
    ;; Build proposed content in a temp file
    (with-temp-file prop-file
      (insert (with-current-buffer target-buf (buffer-string)))
      (dolist (edit edits)
	(let ((old-str (car edit))
	      (new-str (cdr edit)))
	  (goto-char (point-min))
	  (unless (search-forward old-str nil t)
            (delete-file prop-file)
            (error "old_string not found in %s: %S" (buffer-name target-buf) old-str))
	  (replace-match new-str t t))))
    (let ((diff-buf (diff-no-select (buffer-file-name target-buf) prop-file nil t)))
      (with-current-buffer diff-buf
        (nate-agent-review-mode)
        (rename-buffer review-name t)
        (setq-local nate-agent--review-agent-buf  agent-buf)
        (setq-local nate-agent--review-tool-id    tool-id)
        (setq-local nate-agent--review-target-buf target-buf)
        (setq-local nate-agent--review-prop-file  prop-file)
	(setq-local diff-jump-to-old-file t)
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (when (file-exists-p nate-agent--review-prop-file)
                      (delete-file nate-agent--review-prop-file)))
                  nil t))
      (display-buffer (get-buffer review-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tool registrations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nate-agent-register-tool
 "list_buffers"
 "List all open Emacs buffers: their names, major modes, and file paths."
 `((type . "object") (properties . ,nate-agent--empty-props) (required . []))
 (lambda (_input)
   (mapconcat
    (lambda (buf)
      (format "  %-30s  mode:%-22s  file:%s"
              (buffer-name buf)
              (with-current-buffer buf (symbol-name major-mode))
              (or (buffer-file-name buf) "(none)")))
    (buffer-list)
    "\n")))

(nate-agent-register-tool
 "read_buffer"
 "Return the full text contents of an Emacs buffer."
 '((type . "object")
   (properties . ((name . ((type . "string")
                           (description . "Exact buffer name, e.g. \"init.el\"")))))
   (required . ["name"]))
 (lambda (input)
   (let* ((name (gethash "name" input))
          (buf  (get-buffer name)))
     (unless buf (error "No buffer named %S" name))
     (with-current-buffer buf (buffer-string)))))

(nate-agent-register-tool
 "create_buffer"
 "Create a new file at the given path with the given content, and open it in Emacs.
The file's parent directory must already exist.
The buffer is opened for you to review and save with C-x C-s."
 '((type . "object")
   (properties . ((path    . ((type . "string") (description . "Absolute or ~ path for the new file")))
                  (content . ((type . "string") (description . "Initial file contents")))))
   (required . ["path" "content"]))
 (lambda (input)
   (let* ((path    (expand-file-name (gethash "path" input)))
          (content (gethash "content" input)))
     (when (file-exists-p path)
       (error "File already exists: %s" path))
     (unless (file-directory-p (file-name-directory path))
       (error "Parent directory does not exist: %s" (file-name-directory path)))
     (let ((buf (find-file-noselect path)))
       (with-current-buffer buf
         (insert content))
       (display-buffer buf)
       (format "Created %s — review and save with C-x C-s." path))))
 t)

(nate-agent-register-tool
 "shell_command"
 "Run a shell command and return its output (stdout and stderr combined)."
 '((type . "object")
   (properties . ((command . ((type . "string") (description . "Shell command to run")))))
   (required . ["command"]))
 (lambda (input)
   (let ((command (gethash "command" input)))
     (string-trim (shell-command-to-string command))))
 t)  ; destructive — prompts for confirmation

(nate-agent-register-tool
 "lookup_symbol"
 "Look up the *Help* documentation for a Lisp symbol."
 '((type . "object")
   (properties . ((name . ((type . "string") (description . "Symbol name")))))
   (required . ["name"]))
 (lambda (input)
   (let* ((sym-name (gethash "name" input))
          (sym (intern-soft sym-name)))
     (unless sym (error "No symbol found for %S" sym-name))
     (save-window-excursion
       (describe-symbol sym)
       (with-current-buffer (help-buffer) (buffer-string))))))

(nate-agent-register-tool
 "edit_buffer"
 "Replace one or more exact strings in an Emacs buffer with new text.
Each entry in `edits' is an {old_string, new_string} pair.
old_string must match exactly once — make it long enough to be unique.
Edits are applied in order, so later old_strings must match the buffer
after earlier edits have been applied.
Opens a review buffer showing all proposed changes (red) vs current (green);
the agent pauses until you accept (C-c C-a / C-c C-c) or reject (C-c C-k)."
 '((type . "object")
   (properties . ((name  . ((type . "string") (description . "Buffer name")))
                  (edits . ((type  . "array")
                            (description . "Ordered list of replacements to apply")
                            (items . ((type . "object")
                                      (properties
                                       . ((old_string . ((type . "string")
                                                         (description . "Exact text to replace; must appear exactly once")))
                                          (new_string . ((type . "string")
                                                         (description . "Replacement text")))))
                                      (required . ["old_string" "new_string"])))))))
   (required . ["name" "edits"]))
 (lambda (input)
   (let* ((buf-name  (gethash "name" input))
          (edits-vec (gethash "edits" input))
          (tool-id   (gethash "_tool_id" input))
          (agent-buf (gethash "_agent_buf" input))
          (target    (get-buffer buf-name))
          (review    (format "*nate-agent-edit:%s*" tool-id))
          (edits     (mapcar (lambda (e)
                               (cons (gethash "old_string" e)
                                     (gethash "new_string" e)))
                             edits-vec)))
     (unless target (error "No buffer named %S" buf-name))
     (unless (get-buffer review)
       (nate-agent--setup-edit-review agent-buf tool-id target edits))
     (display-buffer (get-buffer review))
     nil)))   ; always async — Result is written by accept/reject

(provide 'nate-agent-tools)
;;; nate-agent-tools.el ends here
