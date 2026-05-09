;;; nate-agent.el --- LLM agent with Emacs tool calling via OpenRouter  -*- lexical-binding: t -*-

;;; Commentary:
;; A from-scratch LLM agent loop using the OpenRouter Responses API.
;; Implements tool calling so the model can inspect and edit Emacs buffers.
;;
;; All dependencies are built-in: auth-source, json, url, subr-x.
;;
;; Setup: add your key to ~/.authinfo.gpg (or ~/.authinfo):
;;   machine openrouter.ai login apikey password sk-or-XXXX
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

(defvar nate-agent-model "minimax/minimax-m2.7" 
  "Model to use, as an OpenRouter model string e.g. \"anthropic/claude-sonnet-4-5\".")

(defvar nate-agent-max-tokens (expt 2 16)
  "Maximum tokens for model responses.")

(defvar nate-agent-reasoning-effort "high"
  "Default reasoning effort level for models that support it.
Can be nil, \"low\", \"medium\", or \"high\".
When non-nil, sends a reasoning object with this effort level to the API.
OpenRouter translates this to appropriate parameters for each provider.")

(defvar nate-agent--system-prompt "You are a helpful assistant running inside Emacs. Format all responses using org-mode syntax rather than markdown. Use * for headings, -for lists, ~code~ for inline code, and #+begin_src / #+end_src for code blocks. When proposing edits, you MUST batch all independent tool calls into a single response rather than sequential edit then read. Before emitting any tool call, check whether there are other tool calls you could emit at the same time. If yes, emit them all together. Do not emit a tool call, wait for its result, and then emit another tool call that did not depend on that result. This harness is actively in development by the user so suggest new tools as they come up.

Tool preference order — always use the highest-priority applicable tool:
1. search_buffer / read_buffer / list_buffers / get_buffer_local_variable — for any buffer already open in Emacs.
2. open_file then read_buffer / search_buffer — to read a file from disk.
3. find_files — to explore the filesystem and discover available files; start shallow and drill down.
4. grep_files — to rg across files on disk that are not yet open. Only use after finding relevant files using find_files.
5. run_shell_command — LAST RESORT ONLY. Do not use shell commands to read files, search code, or explore the filesystem; the dedicated tools above are safer and faster. Reserve run_shell_command for tasks that genuinely require a running process (e.g. building, testing, git operations, or Python execution).")

(defvar-local nate-agent--last-request nil
  "Raw JSON string of the last API request, for debugging.")

(defvar-local nate-agent--last-response nil
  "Raw JSON string of the last API response, for debugging.")

(defvar nate-agent-api-url "https://openrouter.ai/api/v1/responses"
  "OpenRouter Responses API endpoint.")

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
  "Retrieve the OpenRouter API key from auth-source.
Looks for: machine openrouter.ai login apikey password sk-or-..."
  (let ((entry (car (auth-source-search :host "openrouter.ai"
                                        :require '(:secret)))))
    (unless entry
      (error "No entry for openrouter.ai in auth-source"))
    (let ((secret (plist-get entry :secret)))
      (encode-coding-string (funcall secret) 'utf-8))))



(defun nate-agent--request (agent-buf messages tool-defs on-success on-error)
  "POST MESSAGES and TOOL-DEFS to the OpenRouter Responses API asynchronously.
ON-SUCCESS is called with the parsed response alist.
ON-ERROR is called with a description of the failure.
AGENT-BUF is used to store the last request/response for debugging.

Adds a '* Request' heading to AGENT-BUF so that `nate-agent--buffer-state'
can detect an in-flight request and prevent duplicate sends.  The request
buffer is killed in the callback after successful parsing or error."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " (nate-agent--api-key)))
            ("content-type"  . "application/json")))
         (reasoning-effort (nate-agent--reasoning-effort agent-buf))
         (body (encode-coding-string
                (json-encode
                 (append
                  `((model            . ,(nate-agent--model agent-buf))
                    (max_output_tokens . ,nate-agent-max-tokens)
                    (instructions     . ,nate-agent--system-prompt)
                    (tools            . ,(apply #'vector tool-defs))
                    (input            . ,(apply #'vector messages)))
                  (when reasoning-effort
                    `((reasoning . ((effort . ,reasoning-effort)))))
                  (when (string-prefix-p "anthropic/" (nate-agent--model agent-buf))
                    '((cache_control . ((type . "ephemeral")))))))
                'utf-8))
         (url-request-data body))
    (with-current-buffer agent-buf
      (setq nate-agent--last-request body))
    (let ((req-buf (url-retrieve nate-agent-api-url
                                 (lambda (status)
                                   ;; Wrap everything: process sentinel errors are swallowed silently,
                                   ;; so we catch them here and route to on-error instead.
                                   ;; Only catch 'error', not 'debug' - allows debugger to run on lisp errors.
                                   (condition-case err
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
                                             (funcall on-success body))))
                                     (error
                                      (funcall on-error "lisp" err))))
                                 nil t)))
      ;; url-retrieve returns a buffer whose name starts with a space (so it's
      ;; hidden from buffer lists). Org property values can't preserve a leading
      ;; space, so rename to a visible name we can round-trip through the
      ;; REQUEST_BUFFER property.
      ;; We must suppress the process query before renaming, since rename-buffer
      ;; prompts for confirmation if the buffer has a live process.
      (with-current-buffer req-buf
        (when-let ((proc (get-buffer-process (current-buffer))))
          (set-process-query-on-exit-flag proc nil))
        (rename-buffer (generate-new-buffer-name
                        (format "*nate-agent-request-%s*"
                                (format-time-string "%s%3N")))))
      ;; Now we have the buffer name — insert heading and set property in one go.
      (nate-agent--ui-append-request agent-buf req-buf))))


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
Each tool is wrapped in the OpenRouter/OpenAI function tool format.
Also includes the openrouter:web_search server tool."
  (let (defs)
    ;; Add web search server tool
    (push '((type . "openrouter:web_search")
            (parameters . ((max_results . 5))))
          defs)
    ;; Add user-registered function tools
    (maphash (lambda (_name tool)
	       (let ((api-def (plist-get tool :api-def)))
                 (push `((type     . "function")
                         (name        . ,(alist-get 'name        api-def))
                         (description . ,(alist-get 'description api-def))
                         (parameters  . ,(alist-get 'input_schema api-def)))
		       defs)))
             nate-agent--tool-registry)
    defs))

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
          (nate-agent--ui-tag-tool agent-buf id '("pending" "pending_approval"))
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
    (let ((tags (org-get-tags)))
      (setq tags (remove "pending_approval" tags))
      (org-set-tags (cons "approved" tags))))
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
      (nate-agent--ui-write-tool-result (current-buffer) id result)))
  (nate-agent--schedule-step (current-buffer)))

(defun nate-agent-cancel-request ()
  "Cancel any in-flight API requests by killing their response buffers.
Searches for '* Request' headings and kills the associated url-retrieve buffers."
  (interactive)
  (unless (eq major-mode 'nate-agent-mode)
    (user-error "Not in agent buffer"))
  (save-excursion
    (goto-char (point-min))
    (let ((cancelled 0))
      (while (re-search-forward "^\\* Request$" nil t)
        (org-back-to-heading t)
        (let ((req-buf-name (org-entry-get (point) "REQUEST_BUFFER")))
	  (print req-buf-name)
          (when req-buf-name
	    (print  (get-buffer req-buf-name))
	    (when-let ((req-buf (get-buffer req-buf-name)))
	      (print  req-buf)
	      (when (kill-buffer req-buf)
		(setq cancelled (1+ cancelled))))))
        (goto-char (org-end-of-subtree t t)))
      (if (= cancelled 0)
          (message "No in-flight requests to cancel")
        (message "Cancelled %d request(s)" cancelled)))))

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
  (nate-agent--request
   buf
   (with-current-buffer buf (nate-agent--build-history))
   (nate-agent--tool-api-defs)
   (lambda (response) (nate-agent--handle-response buf response))
   (lambda (err-type err)
     (pcase err-type
       ("lisp"
	;; Lisp error - enter the debugger
	(debug err))
       (_
	;; HTTP or API error - display in buffer
	(nate-agent--ui-append buf "\n* Assistant\n")
	(nate-agent--ui-set-assistant-tag buf "error")
	(nate-agent--ui-append buf (format "** Error (%s)\n#+begin_example\n%s\n#+end_example\n" err-type (org-escape-code-in-string (format "%s" err))))
	(nate-agent--ui-ready buf))))))



(defun nate-agent--handle-response (buf response)
  "Render API response into BUF; dispatch on output type.
Filters out openrouter:web_search items and handles annotations in messages."
  (let* ((output      (gethash "output" response))     ; vector of output items
         (output-list (append output nil))              ; vector → list
         (usage       (gethash "usage" response))
         (in-tok      (when usage
                        (+ (or (gethash "input_tokens"  usage) 0)
                           (or (gethash "output_tokens" usage) 0)))))
    (when in-tok
      (with-current-buffer buf (setq nate-agent--last-input-tokens in-tok)))
    ;; Filter out web_search items - they're server-side, not client tools
    (setq output-list (seq-remove (lambda (item)
                                    (string= (gethash "type" item) "openrouter:web_search"))
                                  output-list))
    ;; Detect tool use: any output item with type "function_call"
    (let ((tool-calls (seq-filter (lambda (item)
                                    (string= (gethash "type" item) "function_call"))
                                  output-list)))
      (if tool-calls
          ;; Tool use: write assistant with text (if any), then tools
          (progn
            (nate-agent--ui-insert-text-output buf output-list)
            (nate-agent--ui-insert-tool-calls buf output-list)
            (nate-agent--schedule-step buf))
        ;; Terminal response: write text output (reasoning/message headings)
        (nate-agent--ui-insert-text-output buf output-list)
        (nate-agent--ui-ready buf)))))


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

(defun nate-agent--model (buf)
  "Return the MODEL property from the * Nate Agent Info heading in BUF.
Falls back to `nate-agent-model' if no property is set."
  (with-current-buffer buf 
    (or (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^\\* Nate Agent Info" nil t)
            (org-entry-get (point) "MODEL")))
	nate-agent-model)))

(defun nate-agent--reasoning-effort (buf)
  "Return the REASONING_EFFORT property from the * Nate Agent Info heading in BUF.
Falls back to `nate-agent-reasoning-effort' if no property is set."
  (with-current-buffer buf 
    (or (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^\\* Nate Agent Info" nil t)
            (org-entry-get (point) "REASONING_EFFORT")))
	nate-agent-reasoning-effort)))

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
      (setq-local default-directory working-dir)
      (when (= (buffer-size) 0)
        (let ((dir (expand-file-name working-dir)))
	  (insert "* Nate Agent Info\n")
          (insert (format "Tools: %s\n" (mapconcat #'identity
						   (hash-table-keys nate-agent--tool-registry)
						   ", ")))
          (save-excursion
            (re-search-backward "^\\* Nate Agent Info" nil t)
            (org-set-property "WORKING_DIRECTORY" dir)
            (org-set-property "MODEL" (nate-agent--model (current-buffer)))
            (org-set-property "REASONING_EFFORT" (or nate-agent-reasoning-effort "")))
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

  ;; Set the working directory to the agent dir, not the file location
  (setq-local default-directory  (nate-agent--working-directory))
  
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
;;; TODO make tool call headings show a short input summary (eg ** Tool: read_buffer "init.el")
;;; TODO add an interactive function for when a tool call stalls without writing a result, some kind of "fix-buffer-state"
;;; TODO unify ui and history — they are inverses of the same serialisation process
;;; TODO fix shell command, sometimes the output starts before the whole begin/end line is fully written to the terminal. I think this happens when the command string contrains newlines. I think it only happens on slow commands. Change to a new approach
;;; TODO for create_buffer, guess src mode from filename to display.

;;; TODO on lisp error trigger debugger? possibly implemented but never tested
;;; TODO auto truncate long responses from some tools like find_files
;;; TODO python script tool to avoid piping and escaping multi-line strings into a shell command
;;; TODO better escaping of responses from the agent, sometimes when talking about the harness it returns tool headings that mess up the rest
;;; TODO search_buffer should return context lines
;;; TODO better tracking of in-flight requests
