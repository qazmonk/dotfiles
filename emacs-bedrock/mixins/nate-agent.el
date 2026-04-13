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
(require 'nate-agent-history)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nate-agent-model "claude-sonnet-4-6"
  "Anthropic model to use.")

(defvar nate-agent-max-tokens 8096
  "Maximum tokens for model responses.")

(defvar nate-agent--system-prompt "You are a helpful assistant running inside Emacs. Format all responses using org-mode syntax rather than markdown. Use * for headings, -for lists, ~code~ for inline code, and #+begin_src / #+end_src for code blocks. When proposing edits, you MUST batch all independent tool calls into a single response rather than sequential edit then read. Before emitting any tool call, check whether there are other tool calls you could emit at the same time. If yes, emit them all together. Do not emit a tool call, wait for its result, and then emit another tool call that did not depend on that result. This harness is actively in development by the user so suggest new tools as they come up.")

(defvar-local nate-agent--last-request nil
  "Raw JSON string of the last API request, for debugging.")

(defvar-local nate-agent--last-response nil
  "Raw JSON string of the last API response, for debugging.")

(defvar nate-agent-context-window 200000
  "Input context window size in tokens. Used for the mode-line usage bar.")

(defvar-local nate-agent--last-input-tokens nil
  "Input token count from the most recent API response.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; API Layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nate-agent--show-json-buf (title json-string)
  "Pretty-print JSON-STRING into a buffer named TITLE and display it."
  (let ((buf (get-buffer-create title)))
    (with-current-buffer buf
      (erase-buffer)
      (insert json-string)
      (json-pretty-print-buffer)
      (json-mode))
    (display-buffer buf)))

(defun nate-agent-show-last-request ()
  "Pretty-print the last API request into a buffer."
  (interactive)
  (unless nate-agent--last-request
    (user-error "No request has been made yet"))
  (nate-agent--show-json-buf "*nate-agent-request*" nate-agent--last-request))

(defun nate-agent-show-last-response ()
  "Pretty-print the last API response into a buffer."
  (interactive)
  (unless nate-agent--last-response
    (user-error "No response has been received yet"))
  (nate-agent--show-json-buf "*nate-agent-response*" nate-agent--last-response))

(defun nate-agent--api-key ()
  "Retrieve the Anthropic API key from auth-source.
Looks for: machine api.anthropic.com login apikey password sk-ant-..."
  (let ((entry (car (auth-source-search :host "api.anthropic.com"
                                        :require '(:secret)))))
    (unless entry
      (error "No entry for api.anthropic.com in auth-source"))
    (let ((secret (plist-get entry :secret)))
      (encode-coding-string (funcall secret) 'utf-8))))

(defun nate-agent--request (agent-buf messages tool-defs on-success on-error)
  "POST MESSAGES and TOOL-DEFS to the Anthropic API asynchronously.
ON-SUCCESS is called with the parsed response alist.
ON-ERROR is called with a description of the failure.
AGENT-BUF is used to store the last request/response for debugging."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("x-api-key"         . ,(nate-agent--api-key))
            ("anthropic-version" . "2023-06-01")
            ("content-type"      . "application/json")))
	 (body (encode-coding-string
		(json-encode
		 `((model      . ,nate-agent-model)
		   (max_tokens . ,nate-agent-max-tokens)
		   (system     . [((type . "text")
                                   (text . ,nate-agent--system-prompt)
                                   (cache_control . ((type . "ephemeral"))))])
		   (tools      . ,(apply #'vector tool-defs))
		   (messages   . ,(apply #'vector messages))))
		'utf-8))
         (url-request-data
	  body))
    (with-current-buffer agent-buf
      (setq nate-agent--last-request body))
    (url-retrieve
     "https://api.anthropic.com/v1/messages"
     (lambda (status)
       (let (saved-bt)
	 ;; Wrap everything: process sentinel errors are swallowed silently,
	 ;; so we catch them here and route to on-error instead.
	 (condition-case err
	     ;; Catch backtraces for printing
	     (handler-bind ((error (lambda (_err)
				     (setq saved-bt (with-output-to-string (backtrace))))))
	       (if-let* ((http-err (plist-get status :error)))
		   (funcall on-error "http" (format "%s" (buffer-string)))
		 (goto-char url-http-end-of-headers)
		 (set-buffer-multibyte t)
		 (let* ((response-string (buffer-substring-no-properties (point) (point-max)))
			(json-object-type 'hash-table)
			(body (json-read-from-string response-string)))
		   (with-current-buffer agent-buf
		     (setq nate-agent--last-response response-string))
		   (kill-buffer (current-buffer))
		   (if-let ((api-err (gethash "error" body)))
		       (funcall on-error "api" (gethash "message" api-err "no message supplied"))
		     (funcall on-success body)))))
	   ((error debug)
	    (funcall on-error "lisp" (format "%s\n%s" err saved-bt)))))
       nil t))))   ; nil = no extra callback args, t = silent (don't pop buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tool Layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nate-agent--tool-registry (make-hash-table :test 'equal)
  "Maps tool name (string) to a plist with :fn, :destructive, :api-def.")

(defconst nate-agent--empty-props (make-hash-table :test 'equal)
  "Empty JSON object ({}) for tools that take no input properties.")

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
  "Return all registered tools as a list of alists for the request body.
The last tool definition is stamped with cache_control so the full set
of tool definitions is cached by the API across requests."
  (let (defs)
    (maphash (lambda (_name tool) (push (plist-get tool :api-def) defs))
             nate-agent--tool-registry)
    (append (butlast defs)
            (list (append (car (last defs))
                          '((cache_control . ((type . "ephemeral")))))))))

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
;;;; Agent Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nate-agent--schedule-step (buf)
  (run-with-timer 0.1 nil
		  (lambda ()
		    (when (buffer-live-p buf)
		      (with-current-buffer buf
			(nate-agent-step))))))

(defun nate-agent--run (buf)
  "Send BUF's conversation history to the API and handle the response."
  (nate-agent--ui-set-status buf 'waiting)
  (nate-agent--ui-append buf "\n* Assistant\n")
  (nate-agent--request
   buf
   (with-current-buffer buf (nate-agent--build-history))
   (nate-agent--tool-api-defs)
   (lambda (response) (nate-agent--handle-response buf response))
   (lambda (err-type err)
     (nate-agent--ui-set-assistant-tag buf "end_turn")
     (nate-agent--ui-append-response buf (format "Error (%s):\n#+begin_example\n%s\n#+end_example" err-type (org-escape-code-in-string err)))
     (nate-agent--ui-ready buf))))

(defun nate-agent--handle-response (buf response)
  "Render API response into BUF; dispatch on stop_reason."
  (let* ((stop-reason  (gethash "stop_reason" response))
         (content      (gethash "content" response))   ; vector of content blocks
         (content-list (append content nil))            ; vector → list for dolist
         (usage        (gethash "usage" response))
         (in-tok       (when usage
                        (+ (or (gethash "input_tokens"              usage) 0)
                           (or (gethash "cache_read_input_tokens"    usage) 0)
                           (or (gethash "cache_creation_input_tokens" usage) 0)))))
    (when in-tok
      (with-current-buffer buf (setq nate-agent--last-input-tokens in-tok)))
    (nate-agent--ui-set-assistant-tag buf stop-reason) ; now we have a response, update the tag
    (if (string= stop-reason "tool_use")
        (progn
	  (nate-agent--ui-insert-tool-calls buf content-list)
	  (nate-agent--schedule-step buf))
      ;; Terminal response: extract text blocks and display
      (nate-agent--ui-append-response
       buf
       (mapconcat (lambda (block)
                    (if (string= (gethash "type" block) "text")
                      (gethash "text" block)
		      ""))
                  content-list ""))
      (nate-agent--ui-ready buf))))


(defun nate-agent-step ()
  "Advance the agent by calling the API or executing tools.
In waiting-for-inut state: reads the current * User text and send it.
In needs-continuation state: sends tool results already in the buffer.
In needs-tool-execution state: tries to execute the next pending tool."
  (interactive)
  (unless (eq major-mode 'nate-agent-mode)
    (user-error "Not in agent buffer"))
  (let ((default-directory (or (nate-agent--working-directory)
                               default-directory)))
    (pcase (nate-agent--buffer-state)
      ('waiting-for-input
       (when (string-empty-p (nate-agent--current-input))
	 (user-error "Nothing to send"))
       (nate-agent--run (current-buffer)))
      ('needs-continuation
       (nate-agent--run (current-buffer)))
      (`(needs-tool-execution . ,props)
       (nate-agent--ui-set-status (current-buffer) 'tool)
       (let* ((id (plist-get props :id))
	      (name (plist-get props :name))
	      (input (plist-get props :input)))
	 (puthash "_tool_id" id input)
	 (puthash "_agent_buf" (current-buffer) input)
	 (condition-case err
	     (if-let ((result (nate-agent--execute-tool name input)))
		 (progn
		   (nate-agent--ui-write-tool-result (current-buffer) id result)
		   (nate-agent--schedule-step (current-buffer))))
	   ((user-error error)
	    (nate-agent--ui-write-tool-result (current-buffer) id (format "%s" (cadr err)))
	    (nate-agent--schedule-step (current-buffer))))))
      (state
       (user-error "Cannot step in state: %s" state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; UI Layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local nate-agent--agent-status 'idle
  "Buffer-local agent status for the mode-line. One of: idle, waiting, tool.")

(defvar nate-agent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'nate-agent-step)
    map))

(defface nate-agent-tokens-low  '((t :inherit success :weight bold)) "< 50% context used.")
(defface nate-agent-tokens-mid  '((t :inherit warning :weight bold)) "50-80% context used.")
(defface nate-agent-tokens-high '((t :inherit error   :weight bold)) "> 80% context used.")

(defun nate-agent--token-indicator (n)
  "Return a propertized string showing N tokens vs `nate-agent-context-window'."
  (let* ((pct  (round (* 100.0 (/ (float n) nate-agent-context-window))))
         (face (cond ((>= pct 80) 'nate-agent-tokens-high)
                     ((>= pct 50) 'nate-agent-tokens-mid)
                     (t           'nate-agent-tokens-low)))
         (str  (format "%dk/%dk(%d%%%%)" (/ n 1000)
                       (/ nate-agent-context-window 1000) pct)))
    `(:propertize ,str face ,face)))

(defun nate-agent--mode-line-segment ()
  (let ((status-construct (pcase nate-agent--agent-status
		      ('waiting '(:propertize "waiting..." face warning))
		      ('tool '(:propertize "tool..." face warning))
		      ('idle "idle")
		      (_ nil)))
	(tok-construct (when nate-agent--last-input-tokens
			 (nate-agent--token-indicator nate-agent--last-input-tokens))))
    (list "[" status-construct " | " tok-construct "]")))

(define-derived-mode nate-agent-mode org-mode "Agent"
  "Major mode for the nate-agent conversation buffer."
  (setq-local mode-line-format
              (append (default-value 'mode-line-format)
                      '((:eval (nate-agent--mode-line-segment))))))

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

(defun nate-agent--ui-append-thinking (buf text)
  "Render a thinking text block that came interspersed with tool calls into BUF."
  (with-current-buffer buf
    (goto-char (point-max))
    (let ((start (point)))
      (insert "** Thinking\n")
      (nate-agent--ui-append-literal buf text)
      (save-excursion
	(goto-char start)
	(org-fold-subtree t)))))

(defun nate-agent--ui-append-tool-call (buf name input id)
  "Render a tool call into BUF."
  (with-current-buffer buf
    (goto-char (point-max))
    (insert "\n")
    (let ((start (point-max)))
      (insert (format "** Tool: %s\n" name))
      (org-set-property "TOOL_NAME" name)
      (org-set-property "TOOL_ID" id)
      (goto-char (point-max))
      (insert (format "*** Input\n#+begin_src json\n%s\n#+end_src\n" (json-encode input))))))

(defun nate-agent--ui-write-tool-result (buf id result)
  "Append *** Result under the ** Tool heading matching the id. Folds the tool subtree when done"
  (with-current-buffer buf
    (goto-char (point-max))
    (unless (re-search-backward (concat ":TOOL_ID: +" (regexp-quote id)) nil t)
      (error "No tool heading found for TOOL +ID %s" id))
    (org-back-to-heading t)
    (let ((subtree-start (point)))
      (org-end-of-subtree t t)
      (insert (format "*** Result\n#+begin_example\n%s\n#+end_example\n"
		      (org-escape-code-in-string result)))
      (save-excursion
	(goto-char subtree-start)
	(org-fold-subtree t)))))

(defun nate-agent--ui-insert-tool-calls (buf content-list)
  "Inserts tool_use blocks in CONTENT-LIST."
  (dolist (block content-list) 
    (cond
     ((string= (gethash "type" block) "tool_use")
      (let* ((id     (gethash "id" block))
	     (name   (gethash "name" block))
	     (input  (gethash "input" block)))
	(nate-agent--ui-append-tool-call buf name input id)))
     ((string= (gethash "type" block) "text")
      (nate-agent--ui-append-thinking buf (gethash "text" block))))))

(defun nate-agent--ui-append-response (buf text)
  "Render the model's final text response into BUF.
Any headings in TEXT are demoted so that top-level (*) headings
become (***), keeping them nested under the ** Response heading."
  (nate-agent--ui-append buf (format "** Response\n"))
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

(defun nate-agent--ui-set-assistant-tag (buf tag)
    "Set TAG on the most recent * Assistant heading in BUF."
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (when (re-search-backward "^\\* Assistant" nil t)
          (org-set-tags (list tag)))))) 


(defun nate-agent--ui-set-status (buf status)
  "Update the local variables the mode-line in BUF uses for showing STATUS and last token count."
  (with-current-buffer buf
    (setq nate-agent--agent-status status)
    (force-mode-line-update)))

(defun nate-agent--ui-ready (buf)
  "Append a fresh User: prompt to BUF and clear the status."
  (nate-agent--ui-set-status buf 'idle)
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

(defun nate-agent--working-directory ()
  "Return the WORKING_DIRECTORY property from the * Nate Agent Info heading."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\* Nate Agent Info" nil t)
      (org-entry-get (point) "WORKING_DIRECTORY"))))

;;;###autoload
(defun nate-agent (working-dir)
  "Start a new agent session rooted at WORKING-DIR.
Prompts for the working directory, defaulting to `default-directory'.
Creates a fresh *nate-agent* buffer, inserts the * Nate Agent Info
heading (with WORKING_DIRECTORY property), then the initial * User prompt."
  (interactive
   (list (read-directory-name "Agent working directory: " default-directory)))
  (let ((buf (get-buffer-create (format "*nate-agent [%s]*" working-dir))))
    (with-current-buffer buf
      (unless (eq major-mode 'nate-agent-mode)
        (nate-agent-mode))
      (when (= (buffer-size) 0)
        (let ((dir (expand-file-name working-dir)))
	  (insert "* Nate Agent Info\n")
          (insert (format "Model: %s  |  C-c C-c to send\n" nate-agent-model))
          (insert (format "Tools: %s\n" (mapconcat #'identity
						   (hash-table-keys nate-agent--tool-registry)
						   ", ")))
          (save-excursion
            (re-search-backward "^\\* Nate Agent Info" nil t)
            (org-set-property "WORKING_DIRECTORY" dir))
          (nate-agent--ui-ready (current-buffer)))))
    (pop-to-buffer buf)))

(provide 'nate-agent)
;;; nate-agent.el ends here

;;;;; NOTES
;;; TODO- shell command doesn't seem to really work.
;;; TODO cancel commands
;;; TODO don't hide thinking comments and make them normal responses instead of examples
;;; TODO fix thinking and token usage modeline modifications, make them readable on narrow phone screens
;;; TODO Make tool calls show a short summary in the heading line (eg ** TOOL: read_buffer <buffer name>))
;;; TODO when tool use is denied, ask for a reason
;;; TODO add helper when a tool call is failing and won't write any result
;;; TODO add a tool for grepping and filtering files to stop it running shell commands for that
