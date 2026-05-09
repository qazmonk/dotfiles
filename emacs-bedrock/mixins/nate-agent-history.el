;;; nate-agent-history.el --- History parsing and state detection  -*- lexical-binding: t -*-

;;; Commentary:
;; Reconstructs the OpenRouter Responses API input array from the agent org buffer.
;;
;; Structure (each API input item maps to a heading):
;;
;;   * Nate Agent Info
;;   :PROPERTIES:
;;   :WORKING_DIRECTORY: /path/to/dir
;;   :MODEL: model-name
;;   :END:
;;
;;   * User
;;   <user text>
;;
;;   * Assistant                                                        :reasoning:
;;   <reasoning text>
;;
;;   * Assistant                                                        :message:
;;   <response text>
;;
;;   * Tool: edit_buffer
;;   :PROPERTIES:
;;   :TOOL_NAME: edit_buffer
;;   :TOOL_ID: call_abc123
;;   :END:
;;   :executed:
;;   ** Input
;;   ** Result
;;
;; Each heading maps to API input items:
;;   - * User -> message with role "user"
;;   - * Assistant :reasoning: -> reasoning item
;;   - * Assistant :message: -> message with role "assistant"
;;   - * Tool: -> function_call + function_call_output items
;;
;; Tags on tools: :pending: on new tools, changes to :executed: when result written.
;; Destructive tools also get :pending_approval: until approved.

;;; Code:

(require 'org-element)
(require 'seq)
(require 'subr-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nate-agent--parse-heading (h)
  (save-excursion
   (save-restriction
     (narrow-to-region (org-element-property :begin h) (org-element-property :end h))
     (org-element-parse-buffer))))

(defun nate-agent--subtree-text (h)
  "Return trimmed buffer text of H's full subtree contents."
  (let ((b (org-element-property :contents-begin h))
        (e (org-element-property :contents-end h)))
    (when (and b e) (string-trim (buffer-substring-no-properties b e)))))

(defun nate-agent--child-example (h)
  "Return unescaped value of first example-block under H, or nil."
  (org-element-map h 'example-block
    (lambda (b) (org-unescape-code-in-string (org-element-property :value b)))
    nil t))

(defun nate-agent--child-src (h)
  "Return value of first src-block under H, or nil."
  (org-element-map h 'src-block
    (lambda (b) (org-element-property :value b)) nil t))

(defun nate-agent--find-child-heading (h title)
  "Return first direct child headline of H whose :raw-value equals TITLE."
  (let ((found ()))
    (org-element-map (nate-agent--parse-heading h) 'headline
      (lambda (h) (when (string= title (org-element-property :raw-value h))
		    (push h found))))
    (if found
	(car found)
      nil)))

(defun nate-agent--tool-properties (tool)
  "Get the properties and input of a '** Tool:' heading <tool>.
Returns a plist of (:id :name :input :status).
Status is 'pending, 'approved, or nil (non-destructive, run immediately)."
  (let* ((pos    (org-element-property :begin tool))
         (id     (org-entry-get pos "TOOL_ID"))
         (name   (org-entry-get pos "TOOL_NAME"))
         (tags   (org-element-property :tags tool))
         (status (cond ((member "pending_approval" tags) 'pending)
                       ((member "approved"         tags) 'approved)
                       ((member "executed"         tags) 'executed)
                       ((member "pending"          tags) 'pending)
                       (t                               nil)))
	 (inp-h  (nate-agent--find-child-heading tool "Input"))
         (input  (when inp-h
		   (let ((json-object-type 'hash-table))
                     (json-read-from-string (nate-agent--child-src inp-h))))))
    `(:id ,id :name ,name :input ,input :status ,status)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nate-agent--parse-tool-heading (h)
  "Parse a '* Tool:' heading H.
Returns (function-call-item . function-call-output-or-nil).
Only includes tools with :executed: tag (completed tools)."
  (cl-destructuring-bind (&key id name input status) (nate-agent--tool-properties h)
    (when (eq status 'executed)
      (let* ((res-h  (nate-agent--find-child-heading h "Result"))
             (result (when res-h (nate-agent--child-example res-h))))
        (cons `((type      . "function_call")
                (id        . ,id)
                (call_id   . ,id)
                (name      . ,name)
                (arguments . ,(json-encode input)))
              (when result
                `((type    . "function_call_output")
                  (call_id . ,id)
                  (output  . ,result))))))))

(defun nate-agent--parse-reasoning-assistant (h)
  "Parse an :reasoning: * Assistant heading H.
Returns a reasoning input item."
  (let ((text (nate-agent--child-example h)))
    (when (and text (not (string-empty-p text)))
      `((type    . "reasoning")
        (content . [((type . "reasoning_text") (text . ,text))])))))

(defun nate-agent--parse-message-assistant (h)
  "Parse a :message: * Assistant heading H.
Returns a message input item with role assistant."
  (let ((text (nate-agent--child-example h)))
    (when (and text (not (string-empty-p text)))
      `((type    . "message")
        (role    . "assistant")
        (content . [((type . "output_text") (text . ,text))])))))

(defun nate-agent--build-history ()
  "Reconstruct the OpenRouter Responses API input array from the current buffer.
Walk top-level headings in order:
  * Nate Agent Info -> skipped (metadata)
  * User            -> message item with role \"user\" (skipped if empty)
  * Assistant       -> reasoning or message item based on tag
  * Tool: ...       -> function_call + function_call_output (if executed)
  * Request         -> skipped (API request in flight or completed)
All other top-level headings are ignored."
  (let ((tree (org-element-parse-buffer))
        history)
    (org-element-map tree 'headline
      (lambda (h)
        (let ((level (org-element-property :level h))
              (title (org-element-property :raw-value h))
              (tags  (org-element-property :tags h)))
          (cond
           ;; Level 1 headings
           ((= level 1)
            (cond
             ((string= title "User")
              (let ((text (nate-agent--subtree-text h)))
                (when (and text (not (string-empty-p text)))
                  (push `((type    . "message")
                          (role    . "user")
                          (content . [((type . "input_text") (text . ,text))]))
                        history))))
             ((string= title "Assistant")
              (cond
               ((member "reasoning" tags)
                (when-let ((item (nate-agent--parse-reasoning-assistant h)))
                  (push item history)))
               ((member "message" tags)
                (when-let ((item (nate-agent--parse-message-assistant h)))
                  (push item history)))))
	     ((string-prefix-p "Tool: " title)
              (when-let ((pair (nate-agent--parse-tool-heading h)))
                (push (car pair) history)
                (when (cdr pair) (push (cdr pair) history))))))))
      nil nil 'headline))
    (nreverse history)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; State detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nate-agent--buffer-state ()
  "Return a symbol describing the current agent state in this buffer.

  waiting-for-input    -- last top-level heading is * User
  in-progress          -- last * Assistant has no tag (API call in flight)
  needs-tool-execution -- a * Tool: heading has :pending: tag but no :pending_approval:
  needs-approval       -- a * Tool: heading has :pending_approval: tag
  needs-continuation   -- last heading is * Tool: with :executed:, all tools done
  idle                 -- last heading is * Assistant :reasoning: or :message:"
  (let* ((tree (org-element-parse-buffer))
         last-h
	 pending)
    (org-element-map tree 'headline
      (lambda (h)
	(when (= (org-element-property :level h) 1) (setq last-h h))
        (let ((tags (org-element-property :tags h))
              (title (org-element-property :raw-value h)))
          (when (and (member "pending" tags) (string-prefix-p "Tool: " title))
            (push h pending))))
      nil nil nil)
    (setq pending (nreverse pending))
    (if (null last-h)
        'idle
      (let ((title (org-element-property :raw-value last-h))
            (tags  (org-element-property :tags last-h)))
        (cond
         ;; Pending tools that need execution
         (pending
          (let* ((tool (car pending))
                 (props (nate-agent--tool-properties tool))
                 (tool-tags (org-element-property :tags tool)))
            (if (member "pending_approval" tool-tags)
                (append '(needs-approval) props)
              (append '(needs-tool-execution) props))))
         ;; Last heading is a User - waiting for input
         ((string= title "User") 'waiting-for-input)
         ;; Last heading is an executed tool - need to send results back
         ((and (string-prefix-p "Tool: " title) (member "executed" tags))
          'needs-continuation)
         ;; Last heading is a Request - waiting for response
         ((string= title "Request") 'in-progress)
         ;; Last heading is an Assistant with content - idle
         ((and (string= title "Assistant") (or (member "reasoning" tags) (member "message" tags)))
          'idle)
         ((member "error" tags) 'idle)
         (t 'in-progress))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Interactive testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nate-agent-show-history ()
  "Pretty-print the history reconstructed from the current buffer."
  (interactive)
  (unless (eq major-mode 'nate-agent-mode)
    (user-error "Not in an agent buffer"))
  (let* ((history (nate-agent--build-history))
         (buf (get-buffer-create "*nate-agent-history*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (json-encode history))
      (json-pretty-print-buffer)
      (json-mode))
    (display-buffer buf)))

(provide 'nate-agent-history)
;;; nate-agent-history.el ends here

