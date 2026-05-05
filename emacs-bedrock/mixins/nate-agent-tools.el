;;; nate-agent-tools.el --- Tool implementations for nate-agent  -*- lexical-binding: t -*-

;;; Code:

(require 'nate-agent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Vterm shell integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Requires the following in ~/.bashrc:
;;
;;   source /path/to/vterm/etc/emacs-vterm-bash.sh
;;   __nate_end()   { vterm_cmd nate-agent-command-end "$1" "$2" "$3"; }
;;
;; The tool wraps every command as:
;;   __nate_begin TOOL-ID AGENT-BUF-NAME; COMMAND; __nate_end TOOL-ID $? AGENT-BUF-NAME
;;
;; nate-agent-command-end is registered in vterm-eval-cmds and fires when
;; the shell calls __nate_end.  It searches the vterm scrollback for the
;; matching __nate_begin line and captures everything between them.

(defun nate-agent--vterm-get-or-create (dir)
  "Return the agent vterm buffer, creating it in DIR if needed."
  (or (get-buffer "*nate-agent-shell*")
      (let ((default-directory dir))
        (vterm "*nate-agent-shell*"))))

(defun nate-agent--vterm-command-end (tool-id exit-code agent-buf-name)
  "Called by vterm when a nate-agent shell command finishes.
Searches the vterm scrollback for the __nate_begin marker matching
TOOL-ID, captures everything between it and the __nate_end marker,
and writes the tool result into AGENT-BUF-NAME before resuming the loop."
  (let ((agent-buf (get-buffer agent-buf-name)))
    (unless agent-buf
      (error "nate-agent-command-end: no buffer named %S" agent-buf-name))
    (let* ((begin-marker (format "__nate_begin %s" tool-id))
           (output
            (save-excursion
              (goto-char (point-max))
              (let ((output-end (vterm--get-beginning-of-line (vterm--get-prompt-point)))) ; start of new prompt line = end of output
                (if (not (search-backward begin-marker nil t))
                    (format "(begin marker %S not found in vterm scrollback)" begin-marker)
                  (goto-char (vterm--get-end-of-line)) ; skip fake wrap newlines to true end of command line
                  (forward-char 1)                     ; step past real newline into output
                  (buffer-substring-no-properties (point) output-end)))))
           (clean  (ansi-color-filter-apply output))
           (result (format "exit:%s\n%s" exit-code (string-trim clean))))
      (nate-agent--ui-write-tool-result agent-buf tool-id result)
      (nate-agent--schedule-step agent-buf))))

;; Register the handler so vterm will call it when the shell emits __nate_end
(add-to-list 'vterm-eval-cmds
             '("nate-agent-command-end" nate-agent--vterm-command-end))

(defun nate-agent--run-shell-command (input)
  "Tool function for run_shell_command."
  (let* ((command        (gethash "command" input))
         (tool-id        (gethash "_tool_id" input))
         (agent-buf      (gethash "_agent_buf" input))
         (agent-buf-name (buffer-name agent-buf))
         (dir            (with-current-buffer agent-buf
                           (or (nate-agent--working-directory) default-directory)))
         (vterm-buf      (nate-agent--vterm-get-or-create dir))
         ;; Wrap command so begin/end markers with tool-id appear in scrollback
         (wrapped        (format "__nate_begin %s \"%s\"; %s; __nate_end %s $? \"%s\""
                                 tool-id agent-buf-name
                                 command
                                 tool-id agent-buf-name)))
    (with-current-buffer vterm-buf
      ;; Exit copy-mode if active: vterm--enter-copy-mode sends XOFF (C-s)
      ;; to the pty, which pauses the shell and prevents our command from
      ;; running until the user manually exits copy-mode.
      (when vterm-copy-mode
        (vterm-copy-mode -1))
      (vterm-send-string wrapped)
      (vterm-send-return))
    (display-buffer vterm-buf)
    nil))  ; async — result written by nate-agent--vterm-command-end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; (old eshell attempt, kept for reference)

;; (defun nate-agent--eshell-get-or-create (dir)
;;   "Return the agent eshell buffer, creating it in DIR if needed."
;;   (or (get-buffer "*nate-agent-eshell*")
;;       (let ((default-directory dir))
;;         (let ((buf (eshell 'new)))
;;           (with-current-buffer buf
;;             (rename-buffer "*nate-agent-eshell*"))
;;           buf))))

;; (defun nate-agent--eshell-run (command eshell-buf output-buf agent-buf tool-id)
;;   "Run COMMAND in ESHELL-BUF's environment, writing output to OUTPUT-BUF.
;; When the command finishes, writes the tool result for TOOL-ID into
;; AGENT-BUF and resumes the agent loop.  Async: returns immediately.
;; OUTPUT-BUF's existence is used as the in-flight guard."
;;   (with-current-buffer eshell-buf
;;     (letrec ((handler
;;               (lambda ()
;;                 (remove-hook 'eshell-post-command-hook handler t)
;;                 (let ((output (with-current-buffer output-buf
;;                                 (string-trim (buffer-string)))))
;;                   (kill-buffer output-buf)
;;                   (nate-agent--ui-write-tool-result agent-buf tool-id output)
;;                   (nate-agent--schedule-step agent-buf)))))
;;       (add-hook 'eshell-post-command-hook handler nil t)
;;       (eshell-eval-command
;;        `(let ((eshell-current-handles
;;                (eshell-create-handles ,output-buf 'insert))
;;               (eshell-current-subjob-p nil))
;;           ,(eshell-parse-command command nil t))
;;        command))))

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
 "open_file"
 "Read file PATH into a buffer and return the buffer name.
If a buffer exists visiting PATH, return that one. If PATH does not exist it is created."
 '((type . "object")
   (properties . ((path . ((type . "string")
                           (description . "Absolute or ~ path of the file")))))
   (required . ["path"]))
 (lambda (input)
   (let* ((path (expand-file-name (gethash "path" input)))
          (buf  (find-file-noselect path)))
     (buffer-name buf))))

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
 t
 (lambda (input)
   (gethash "content" input)))

(nate-agent-register-tool
 "run_shell_command"
 "Run a shell command in the persistent vterm shell buffer and return its output.
The vterm buffer is created if it doesn't exist. Runs in your live shell
environment, inheriting conda env, cwd, aliases, etc.
Requires the __nate_end function to be defined in ~/.bashrc.

IMPORTANT: This is a last-resort tool. Prefer the dedicated Emacs tools for
common tasks: use find_files to look for non-open files, use grep_files to search for strings within a directory, open_file+read_buffer to read files,
search_buffer to search open buffers. Only use run_shell_command for tasks
that genuinely require a running process (build, test, git, execute code, etc.)."
 '((type . "object")
   (properties . ((command . ((type . "string")
                              (description . "Shell command to run")))))
   (required . ["command"]))
 #'nate-agent--run-shell-command
 t
 (lambda (input)
   (list (gethash "command" input) "sh")))

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
 "Replace an exact string in an Emacs buffer with new text.
old_string must match exactly once in the buffer — make it long enough to be unique.
Use grep_files with context_lines or search_buffer to find the exact text first.
Opens a diff preview; accept with C-c C-a / C-c C-c or reject with C-c C-k."
 '((type . "object")
   (properties . ((name  . ((type . "string") (description . "Buffer name")))
                  (old_string . ((type . "string")
                                 (description . "Exact text to replace; must appear exactly once. Preserve all whitespace including tabs.")))
                  (new_string . ((type . "string")
                                 (description . "Replacement text")))))
   (required . ["name" "old_string" "new_string"]))
 (lambda (input)
   (let* ((buf-name   (gethash "name" input))
          (old-string (gethash "old_string" input))
          (new-string (gethash "new_string" input))
          (target     (get-buffer buf-name)))
     (unless target (error "No buffer named %S" buf-name))
     (with-current-buffer target
       (goto-char (point-min))
       (unless (search-forward old-string nil t)
         (error "old_string not found in %s" buf-name))
       (replace-match new-string t t)
       (save-buffer))
     (format "Edit accepted. %s updated." buf-name)))
 t
 (lambda (input)
   (let* ((buf-name   (gethash "name" input))
          (old-string (gethash "old_string" input))
          (new-string (gethash "new_string" input))
          (target     (get-buffer buf-name)))
     (unless target
       (error "No buffer named %S" buf-name))
     (with-current-buffer target
       (goto-char (point-min))
       (unless (search-forward old-string nil t)
         ;; Return detailed error with visible whitespace
         (let* ((escaped (replace-regexp-in-string "\t" "<TAB>" old-string))
                (escaped (replace-regexp-in-string " " "·" escaped))
                (preview (substring escaped 0 (min 200 (length escaped)))))
           (error "old_string not found.\n\nSearched for (first 200 chars, spaces=·, tabs=<TAB>):\n%s\n\nTip: Use grep_files with context_lines or search_buffer to get exact text including whitespace." preview))))
     ;; Generate diff
     (let ((current-file (buffer-file-name target))
           (prop-file    (make-temp-file "nate-agent-edit-")))
       (with-temp-file prop-file
         (insert (with-current-buffer target (buffer-string)))
         (goto-char (point-min))
         (search-forward old-string nil t)  ; we know it exists
         (replace-match new-string t t))
       (let ((diff (shell-command-to-string
                    (format "diff -u %s %s"
                            (shell-quote-argument current-file)
                            (shell-quote-argument prop-file)))))
         (delete-file prop-file)
         (list diff "diff"))))))   

(nate-agent-register-tool
 "get_buffer_local_variable"
 "Return the value of a buffer-local variable in a named buffer.
Useful for inspecting mode-line-format, major-mode, local settings, etc.
Returns the printed representation of the value."
 '((type . "object")
   (properties . ((buffer . ((type . "string") (description . "Buffer name")))
                  (variable . ((type . "string") (description . "Variable name")))))
   (required . ["buffer" "variable"]))
 (lambda (input)
   (let* ((buf-name (gethash "buffer" input))
          (var-name (gethash "variable" input))
          (buf      (get-buffer buf-name))
          (sym      (intern-soft var-name)))
     (unless buf (error "No buffer named %S" buf-name))
     (unless sym (error "No symbol found for %S" var-name))
     (prin1-to-string (buffer-local-value sym buf)))))

(nate-agent-register-tool
 "search_buffer"
 "Search an open buffer for lines matching a regexp.
Returns all matching lines with their line numbers.
Use this instead of grep/shell commands when the buffer is already open in Emacs."
 '((type . "object")
   (properties . ((name   . ((type . "string") (description . "Buffer name")))
                  (regexp . ((type . "string") (description . "Regexp to search for")))))
   (required . ["name" "regexp"]))
 (lambda (input)
   (let* ((buf-name (gethash "name" input))
          (regexp   (gethash "regexp" input))
          (buf      (get-buffer buf-name))
          (results  '()))
     (unless buf (error "No buffer named %S" buf-name))
     (with-current-buffer buf
       (save-excursion
         (goto-char (point-min))
         (while (re-search-forward regexp nil t)
           (push (format "%d: %s"
                         (line-number-at-pos)
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))
                 results)
           (forward-line 1))))
     (if results
         (mapconcat #'identity (nreverse results) "\n")
       (format "No matches for %S in %s" regexp buf-name)))))

(nate-agent-register-tool
 "find_files"
 "List files in a directory using (rg --files), respecting .gitignore.
Use this to explore the filesystem and discover what files are available.
Prefer this over run_shell_command or grep_files for any ls/find/tree-style exploration.
Start with a shallow max_depth (default 2) and drill into subdirectories as needed."
 '((type . "object")
   (properties . ((path      . ((type . "string")
				(description . "Directory to list. Defaults to the agent working directory.")))
		  (glob      . ((type . "string")
				(description . "Optional filename glob to restrict results, e.g. \"*.el\" or \"*.py\"")))
		  (max_depth . ((type . "integer")
				(description . "Maximum directory depth to recurse (default 2). Increase to explore deeper subtrees.")))))
   (required . []))
 (lambda (input)
   (let* ((path      (expand-file-name (or (gethash "path" input) ".")))
          (glob      (gethash "glob" input))
          (max-depth (or (gethash "max_depth" input) 2))
          (args      (concat "rg --files --color never "
                             (format "--max-depth %d " max-depth)
                             (when glob (format "-g %s " (shell-quote-argument glob)))
                             (shell-quote-argument path)))
          (output    (shell-command-to-string args)))
     (if (string-empty-p output)
         (format "No files found in %s" path)
       (string-trim-right output)))))

(nate-agent-register-tool
 "grep_files"
 "Search for a pattern in files on disk using ripgrep (rg).
Use this to search files that may not be open in Emacs.
Use search_buffer instead when the buffer is already open.
Prefer this over run_shell_command for any grep/find/rg codebase exploration."
 '((type . "object")
   (properties
    . ((pattern      . ((type . "string")
                        (description . "Pattern to search for (regexp by default)")))
       (path        . ((type . "string")
                       (description . "File or directory to search. Defaults to the agent working directory.")))
       (glob        . ((type . "string")
                       (description . "Optional filename glob to restrict search, e.g. \"*.el\" or \"*.py\"")))
       (fixed_strings . ((type . "boolean")
                         (description . "If true, treat pattern as a literal string instead of a regexp")))
       (context_lines . ((type . "integer")
                         (description . "Lines of context before and after each match (like rg -C N). Use when you need surrounding text to build a unique old_string for edit_buffer.")))))
   (required . ["pattern"]))
 (lambda (input)
   (let* ((pattern (gethash "pattern" input))
          (path    (expand-file-name
                    (or (gethash "path" input) ".")))
          (glob    (gethash "glob" input))
          (fixed   (gethash "fixed_strings" input))
          (ctx     (gethash "context_lines" input))
          (args    (concat "rg --line-number --with-filename --color never "
                           (when fixed "-F ")
                           (when (and ctx (> ctx 0)) (format "-C %d " ctx))
                           (when glob (format "-g %s " (shell-quote-argument glob)))
                           (shell-quote-argument pattern)
                           " "
                           (shell-quote-argument path)))
          (output  (shell-command-to-string args)))
     (if (string-empty-p output)
         (format "No matches for %S in %s" pattern path)
       (string-trim-right output)))))

(provide 'nate-agent-tools)
;;; nate-agent-tools.el ends here
