;;; nate-agent.el --- Minimal Anthropic LLM agent with Emacs tool calling  -*- lexical-binding: t -*-

;;; Commentary:
;; A from-scratch LLM agent loop using the Anthropic Messages API.
;; Implements tool calling so the model can inspect and edit Emacs buffers.
;;
;; All dependencies are built-in: auth-source, json, url, subr-x.
;;
;; Setup: add your key to ~/.authinfo.gpg (or ~/.authinfo):
;;   machine api.anthropic.com login apikey password sk-ant-XXXX
;;
;; Usage: M-x nate-agent
;;   Type a message and press C-c C-c to send.
;;   The agent calls tools (shown inline) before giving a final response.
;;   Destructive tools (edit_buffer) prompt for confirmation first.

;;; Code:

(require 'auth-source)
(require 'json)
(require 'url)
(require 'subr-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nate-agent-model "claude-sonnet-4-6"
  "Anthropic model to use.")

(defvar nate-agent-max-tokens 8096
  "Maximum tokens for model responses.")

(defvar nate-agent--system-prompt "You are a helpful assistant running inside Emacs. Format all responses using org-mode syntax rather than markdown. Use * for headings, -for lists, ~code~ for inline code, and #+begin_src / #+end_src for code blocks.")

(defvar nate-agent--debug-http t
  "Keep around all http response buffers")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Global State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local nate-agent--last-request nil
  "Last JSON request body sent to the API, for debugging.") ;; TODO-MAYBE change this to a list of all requests?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; API Layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nate-agent-show-last-request ()
  "Pretty-print the last API request into a buffer"
  (interactive)
  (unless nate-agent--last-request
    (user-error "nate-agent--last-reqeuest is nil")
    )
  (let ((last-req nate-agent--last-request)
	(buf (get-buffer-create "*nate-agent-request*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert last-req)
      (json-pretty-print-buffer)
      (json-mode))
    (display-buffer (get-buffer "*nate-agent-request*"))))

(defun nate-agent--api-key ()
  "Retrieve the Anthropic API key from auth-source.
Looks for: machine api.anthropic.com login apikey password sk-ant-..."
  (let ((entry (car (auth-source-search :host "api.anthropic.com"
                                        :require '(:secret)))))
    (unless entry
      (error "No entry for api.anthropic.com in auth-source"))
    (let ((secret (plist-get entry :secret)))
      (encode-coding-string (funcall secret) 'utf-8))))

(defun nate-agent--request (messages tool-defs on-success on-error)
  "POST MESSAGES and TOOL-DEFS to the Anthropic API asynchronously.
ON-SUCCESS is called with the parsed response alist.
ON-ERROR is called with a description of the failure."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("x-api-key"         . ,(nate-agent--api-key))
            ("anthropic-version" . "2023-06-01")
            ("content-type"      . "application/json")))
	 (body (encode-coding-string
		(json-encode
		 `((model      . ,nate-agent-model)
		   (max_tokens . ,nate-agent-max-tokens)
		   (system     . ,nate-agent--system-prompt)
		   (tools      . ,(apply #'vector tool-defs))
		   (messages   . ,(apply #'vector messages))))
		'utf-8))
         (url-request-data
	  body))
    (with-current-buffer (get-buffer "*nate-agent*")
      (setq nate-agent--last-request body))
    (url-retrieve
     "https://api.anthropic.com/v1/messages"
     (lambda (status)
       (when nate-agent--debug-http
	(rename-buffer "*nate-agent-http-resp" t))
       ;; Wrap everything: process sentinel errors are swallowed silently,
       ;; so we catch them here and route to on-error instead.
       (condition-case err
	   (if-let* ((http-err (plist-get status :error)))
	       (funcall on-error "http" (format "%s" http-err))
             (goto-char url-http-end-of-headers)
	     (set-buffer-multibyte t)
             (let* ((json-object-type 'hash-table)
		    (body (json-read)))
               (if-let ((api-err (gethash "error" body)))
		   (funcall on-error "api" (gethash "message" api-err "no message supplied"))
		 (funcall on-success body))))
	 ((error debug)
	  (funcall on-error "lisp" (format "%s" err))
	  )))
     nil t)))   ; nil = no extra callback args, t = silent (don't pop buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tool Layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nate-agent--tool-registry (make-hash-table :test 'equal)
  "Maps tool name (string) to a plist with :fn, :destructive, :api-def.")

(defun nate-agent-register-tool (name description schema fn &optional destructive)
  "Register a tool the model can call.
NAME        — string identifier sent to the model.
DESCRIPTION — tells the model what the tool does.
SCHEMA      — alist for the JSON Schema of the tool's inputs.
FN          — called with the parsed input alist; must return a string.
DESTRUCTIVE — if non-nil, prompt user before running."
  (puthash name
           `(:fn ,fn
             :destructive ,destructive
             :api-def ((name        . ,name)
                       (description . ,description)
                       (input_schema . ,schema)))
           nate-agent--tool-registry))

(defun nate-agent--tool-api-defs ()
  "Return all registered tools as a list of alists for the request body."
  (let (defs)
    (maphash (lambda (_name tool) (push (plist-get tool :api-def) defs))
             nate-agent--tool-registry)
    defs))

(defun nate-agent--execute-tool (name input)
  "Execute tool NAME with INPUT alist.  Returns a result string."
  (let ((tool (gethash name nate-agent--tool-registry)))
    (unless tool
      (error "Unknown tool requested by model: %s" name))
    (when (plist-get tool :destructive)
      (unless (y-or-n-p (format "Allow tool '%s'? " name))
        (user-error "Declined tool '%s'" name)))
    (condition-case err
        (funcall (plist-get tool :fn) input)
      (error (format "Tool error: %s" (error-message-string err))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Built-in Tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; json-encode encodes nil as null and '() as null — not as {}.
;; For tools with no input properties, we need a real empty hash table.
(defconst nate-agent--empty-props (make-hash-table :test 'equal)
  "Empty JSON object ({}) for tools that take no input properties.")

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
 "edit_buffer"
 "Replace an exact string in an Emacs buffer with new text.
old_string must match exactly once — make it long enough to be unique."
 '((type . "object")
   (properties . ((name       . ((type . "string") (description . "Buffer name")))
                  (old_string . ((type . "string")
                                 (description . "Exact text to replace; must appear exactly once")))
                  (new_string . ((type . "string") (description . "Replacement text")))))
   (required . ["name" "old_string" "new_string"]))
 (lambda (input)
   (let* ((name    (gethash "name" input))
          (old-str (gethash "old_string" input))
          (new-str (gethash "new_string" input))
          (buf     (get-buffer name)))
     (unless buf (error "No buffer named %S" name))
     (with-current-buffer buf
       (let ((n (count-matches (regexp-quote old-str) (point-min) (point-max))))
         (cond
          ((= n 0) (error "old_string not found in %S" name))
          ((> n 1) (error "old_string matches %d times in %S; must be unique" n name))
          (t
           (goto-char (point-min))
           (search-forward old-str)
           (replace-match new-str t t)
           (format "Replaced in %s." name)))))))
 t)  ; destructive — prompt before running

(nate-agent-register-tool
 "lookup_symbol"
 "Look up the *Help* documentation for a symbol. Returns the contents of the *Help* buffer"
 '((type . "object")
   (properties . ((name . ((type . "string") (description . "The name of the symbol to look up.")))))
   (required . ["name"]))
 (lambda (input)
   (let* ((sym-name (gethash "name" input))
	  (sym (intern-soft sym-name)))
     (unless sym
       (error "No symbol found for %S" sym-name))
     (save-window-excursion	  ; don't clobber user's window layout
       (describe-symbol sym)	  ; populates *Help*
       (with-current-buffer (help-buffer)
         (buffer-string)))))
 nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Agent Loop
;;;;
;;;; The loop: send history → get response → if tool_use: execute tools,
;;;; append results to history, send again → if text: display, wait for user.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nate-agent--run (buf)
  "Send BUF's conversation history to the API and handle the response."
  (nate-agent--ui-set-status buf "thinking…")
  (nate-agent--request
   (buffer-local-value 'nate-agent--history buf)
   (nate-agent--tool-api-defs)
   (lambda (response) (nate-agent--handle-response buf response))
   (lambda (err-type err)
     (nate-agent--ui-append buf (format "\n* Error: %s\n%s" err-type err))
     (nate-agent--ui-ready buf))))

(defun nate-agent--handle-response (buf response)
  "Append assistant message to history; dispatch on stop_reason."
  (let* ((stop-reason  (gethash "stop_reason" response))
         (content      (gethash "content" response))   ; vector of content blocks
         (content-list (append content nil)))            ; vector → list for dolist
    ;; Record the full assistant message (may contain tool_use + text blocks)
    (with-current-buffer buf
      (setq nate-agent--history
            (append nate-agent--history
                    (list `((role . "assistant") (content . ,content))))))
    (if (string= stop-reason "tool_use")
        (nate-agent--handle-tool-calls buf content-list)
      ;; Terminal response: extract text blocks and display
      (nate-agent--ui-append-response
       buf
       (mapconcat (lambda (block)
                    (when (string= (gethash "type" block) "text")
                      (gethash "text" block)))
                  content-list ""))
      (nate-agent--ui-ready buf))))

(defun nate-agent--handle-tool-calls (buf content-list)
  "Execute every tool_use block in CONTENT-LIST, collect results, loop."
  (let (results)
    (dolist (block content-list)
      (when (string= (gethash "type" block) "tool_use")
        (let* ((id     (gethash "id" block))
               (name   (gethash "name" block))
               (input  (gethash "input" block))
               (result (nate-agent--execute-tool name input)))
          (nate-agent--ui-append-tool-call buf name input result)
          (push `((type        . "tool_result")
                  (tool_use_id . ,id)
                  (content     . ,result))
                results))))
    ;; The API requires tool results in a user-role message
    (with-current-buffer buf
      (setq nate-agent--history
            (append nate-agent--history
                    (list `((role    . "user")
                            (content . ,(apply #'vector (nreverse results))))))))
    (nate-agent--run buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; UI Layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local nate-agent--history nil
  "Conversation history: list of message alists, oldest first.")

(defvar nate-agent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'nate-agent-send)
    map))

(define-derived-mode nate-agent-mode org-mode "Agent"
  "Major mode for the nate-agent conversation buffer."
  (setq nate-agent--history nil))

(defun nate-agent--ui-append (buf text)
  "Append TEXT to the end of BUF. Returns the point at the start of the inserted text"
  (with-current-buffer buf
    (goto-char (point-max))
    (insert text)))

(defun nate-agent--ui-append-literal (buf text)
  "Wrap text in an EXAMPLE block to prevent parsing and append."
  (with-current-buffer buf
    (goto-char (point-max))
    (insert (format "#+begin_example\n%s\n#+end_example"
		    (org-escape-code-in-string text)))))

(defun nate-agent--ui-append-tool-call (buf name input result)
  "Render a tool call and its result into BUF."

  (nate-agent--ui-append buf "\n")
  (let ((start  (nate-agent--ui-append
		 buf
		 (format "** Tool: %s\n***Input\n%s\n***Result\n"
			 name
			 (json-encode input)))))
    (nate-agent--ui-append-literal buf result)
    (with-current-buffer buf
      (save-excursion
	(goto-char start)
	(org-fold-subtree t)))))


(defun nate-agent--ui-append-response (buf text)
  "Render the model's final text response into BUF.
Any headings in TEXT are demoted so that top-level (*) headings
become (***), keeping them nested under the ** Response heading."
  (nate-agent--ui-append buf (format "\n** Response\n"))
  (let ((response-start (with-current-buffer buf (point-max))))
    (nate-agent--ui-append buf (format "%s" text))
    (with-current-buffer buf
      (save-excursion
        (save-restriction
          (narrow-to-region response-start (point-max))
	  (goto-char (point-min))
	  (while (re-search-forward org-heading-regexp nil t)
	    (beginning-of-line)
	    (while (< (org-current-level) 3)
	      (org-demote-subtree))
	    (org-end-of-subtree t t)))))))

 

(defun nate-agent--ui-set-status (buf msg)
  "Update the mode-line status for BUF.  Pass nil to clear."
  (with-current-buffer buf
    (setq mode-line-misc-info (when msg (concat "[" msg "] ")))
    (force-mode-line-update)))

(defun nate-agent--ui-ready (buf)
  "Append a fresh User: prompt to BUF and clear the status."
  (nate-agent--ui-set-status buf nil)
  (with-current-buffer buf
    (goto-char (point-max))
    (insert "\n* User\n")))

(defun nate-agent--current-input ()
  "Extract user input: everything after the last user heading in the buffer."
  (save-excursion
    (goto-char (point-max))
    (if (search-backward "\n* User\n" nil t)
        (string-trim (buffer-substring-no-properties (match-end 0) (point-max)))
      "")))

(defun nate-agent-send ()
  "Send the current user input to the agent."
  (interactive)
  (unless (eq major-mode 'nate-agent-mode)
    (user-error "Not in an agent buffer"))
  (let ((input (nate-agent--current-input)))
    (when (string-empty-p input)
      (user-error "Nothing to send"))
    (setq nate-agent--history
          (append nate-agent--history
                  (list `((role . "user") (content . ,input)))))
    (nate-agent--ui-append (current-buffer) "\n* Assistant")
    (nate-agent--run (current-buffer))))

;;;###autoload
(defun nate-agent ()
  "Open (or switch to) the nate-agent conversation buffer."
  (interactive)
  (let ((buf (get-buffer-create "*nate-agent*")))
    (with-current-buffer buf
      (unless (eq major-mode 'nate-agent-mode)
        (nate-agent-mode)
        (insert "* Nate Agent Info\n")
        (insert (format "Model: %s  |  C-c C-c to send\n" nate-agent-model))
        (insert "Tools: list_buffers, read_buffer, edit_buffer\n")
        (nate-agent--ui-ready buf)))
    (pop-to-buffer buf)))

(provide 'nate-agent)
;;; nate-agent.el ends here

;;;;; NOTES
;; TODO-LONG make the buffer contents itself a full one-to-one representation of the message history needed to continue. Then conversations can be just saved as org files.
;; TODO-NEXT work on this project with this project
;; TODO-NEXT more tools to allow reading from the manual and web searching etc.
;; TODO-NEXT make editing work through a diff interface.
