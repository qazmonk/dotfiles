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

(defun nate-agent-register-tool (name description schema fn &optional destructive display-fn)
  "Register a tool the model can call.
NAME        — string identifier sent to the model.
DESCRIPTION — tells the model what the tool does.
SCHEMA      — alist for the JSON Schema of the tool's inputs.
FN          — called with the parsed input alist; must return a string.
DESTRUCTIVE — if non-nil, require user approval before running.
DISPLAY-FN  — called with input to produce the approval preview.
               Returns either a string, or (content lang) for a src block."
  (puthash name
           `(:fn ,fn
             :destructive ,destructive
             :display-fn ,display-fn
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

(defun nate-agent--execute-tool (name input status)
  "Execute tool NAME with INPUT alist and STATUS from the tool heading tags.
For destructive tools: if STATUS is nil, call display-fn, write *** Display,
stamp :pending_approval: and return nil to pause the loop.
If STATUS is 'approved, run fn unconditionally.  Non-destructive tools always run."
  (let* ((tool      (gethash name nate-agent--tool-registry))
         (agent-buf (gethash "_agent_buf" input))
         (id        (gethash "_tool_id" input)))
    (unless tool
      (error "Unknown tool requested by model: %s" name))
    (if (and (plist-get tool :destructive) (not (eq status 'approved)))
	(progn
         (nate-agent--ui-tag-tool agent-buf id '("pending_approval"))
	 (pop-to-buffer agent-buf)
	 (with-current-buffer agent-buf
	   (nate-agent--ui-goto-tool agent-buf id))
	 nil)
      (condition-case err
          (funcall (plist-get tool :fn) input)
        (error (format "Tool error: %s" (error-message-string err)))))))

(defun nate-agent-approve-tool ()
  "Approve the pending_approval tool heading at or near point."
  (interactive)
  (unless (eq major-mode 'nate-agent-mode)
    (user-error "Not in agent buffer"))
  (save-excursion
    (nate-agent--back-to-tool-heading)
    (unless (member "pending_approval" (org-get-tags))
      (user-error "Not on a pending_approval tool heading"))
    (org-set-tags (list "approved")))
  (nate-agent--schedule-step (current-buffer)))

(defun nate-agent-decline-tool ()
  "Decline the pending_approval tool heading at or near point."
  (interactive)
  (unless (eq major-mode 'nate-agent-mode)
    (user-error "Not in agent buffer"))
  (save-excursion
    (nate-agent--back-to-tool-heading)
    (unless (member "pending_approval" (org-get-tags))
      (user-error "Not on a pending_approval tool heading"))
    (let* ((id     (org-entry-get (point) "TOOL_ID"))
           (reason (read-string "Reason (optional): "))
           (result (if (string-empty-p reason)
                       "Declined by user."
                     (format "Declined by user: %s" reason))))
      (org-set-tags nil)
      (nate-agent--ui-write-tool-result (current-buffer) id result)))
  (nate-agent--schedule-step (current-buffer)))

(defun nate-agent-abort ()
  "Decline all pending tools and return to idle."
  (interactive)
  (unless (eq major-mode 'nate-agent-mode)
    (user-error "Not in agent buffer"))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":pending_approval:" nil t)
      (org-back-to-heading t)
      (let ((id (org-entry-get (point) "TOOL_ID")))
        (org-set-tags nil)
        (nate-agent--ui-write-tool-result (current-buffer) id "Aborted by user."))))
  (nate-agent--ui-ready (current-buffer)))


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
       (let* ((id     (plist-get props :id))
	      (name   (plist-get props :name))
	      (input  (plist-get props :input))
	      (status (plist-get props :status)))
	 (puthash "_tool_id" id input)
	 (puthash "_agent_buf" (current-buffer) input)
	 (condition-case err
	     (if-let ((result (nate-agent--execute-tool name input status)))
		 (progn
		   (nate-agent--ui-write-tool-result (current-buffer) id result)
		   (nate-agent--schedule-step (current-buffer))))
	   ((user-error error)
	    (nate-agent--ui-write-tool-result (current-buffer) id (format "%s" (cadr err)))
	    (nate-agent--schedule-step (current-buffer))))))
      (state
       (user-error "Cannot step in state: %s" state)))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mode definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nate-agent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'nate-agent-step)
    (define-key map (kbd "C-c C-a") #'nate-agent-approve-tool)
    (define-key map (kbd "C-c C-k") #'nate-agent-decline-tool)
    (define-key map (kbd "C-c C-q") #'nate-agent-abort)
    map))

(define-derived-mode nate-agent-mode org-mode "Agent"
  "Major mode for the nate-agent conversation buffer."
  ;; Put the agent status up front so it's visible on narrow terminals.
  ;; Also strip rarely-useful clutter (mule-info, frame-id, misc-info, etc.).
  (setq-local mode-line-format
              (list
               "%e"
               ;; Window number (winum)
               '(:eval (format winum-format (winum-get-number-string)))
               " "
               ;; Agent status — front and centre
               '(:eval (nate-agent--mode-line-segment))
               " "
               ;; Modified / read-only flag
               '(:propertize "%*" face bold)
               " "
               ;; Buffer name
               'mode-line-buffer-identification
               "  "
               ;; Line / column
               'mode-line-position
               ;; VC branch
               '(vc-mode vc-mode)
               "  "
               ;; Major mode
               'mode-line-modes
               'mode-line-end-spaces)))

(provide 'nate-agent)
;;; nate-agent.el ends here

;;;;; NOTES
;;; TODO cancel: interrupt in-flight API requests and running tool calls
;;; TODO don't fold thinking blocks; render them as normal response text
;;; TODO make tool call headings show a short input summary (eg ** Tool: read_buffer "init.el")
;;; TODO add a guard / error message when a tool call stalls without writing a result
;;; TODO add web search
;;; TODO unify ui and history — they are inverses of the same serialisation process
