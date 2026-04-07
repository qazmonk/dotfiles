;;; nate-agent-history.el --- History parsing and state detection  -*- lexical-binding: t -*-

;;; Commentary:
;; Reconstructs the Anthropic API message history from the agent org buffer.
;;
;; One * Assistant heading is inserted per API call.  A tool-call round
;; therefore produces two consecutive * Assistant headings:
;;
;;   * User
;;   <user text>
;;
;;   * Assistant                                             :tool_use:
;;   ** Thinking
;;   #+begin_example
;;   <thinking text>
;;   #+end_example
;;   ** Tool: list_buffers
;;   :PROPERTIES:
;;   :TOOL_NAME: list_buffers
;;   :TOOL_ID:  toolu_abc123
;;   :END:
;;   *** Input
;;   #+begin_src json
;;   {}
;;   #+end_src
;;   *** Result
;;   #+begin_example
;;   <result text>
;;   #+end_example
;;
;;   * Assistant                                             :end_turn:
;;   ** Response
;;   <response text>
;;
;; A :tool_use: heading -> assistant message (thinking+tool_use) + user message (tool_results).
;; An :end_turn: heading -> assistant message (text).
;; Untagged * Assistant (API in flight) -> contributes nothing to history.

;;; Code:

(require 'org-element)
(require 'seq)
(require 'subr-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (seq-find (lambda (child)
              (and (eq (org-element-type child) 'headline)
                   (string= title (org-element-property :raw-value child))))
            (org-element-contents h)))

(defun nate-agent--direct-tool-headings (h)
  "Return list of direct '** Tool:' child headings of H."
  (seq-filter (lambda (child)
                (and (eq (org-element-type child) 'headline)
                     (string-prefix-p "Tool: " (org-element-property :raw-value child))))
              (org-element-contents h)))

(defun nate-agent--tool-properties (tool)
  "Get the properties and input of a '** Tool:' heading <tool>. 
Returns a plist of (:id :name :input)."
  (let* ((pos    (org-element-property :begin tool))
         (id     (org-entry-get pos "TOOL_ID"))
         (name   (org-entry-get pos "TOOL_NAME"))
	 (inp-h  (nate-agent--find-child-heading tool "Input"))
         (input  (when inp-h
		   (let ((json-object-type 'hash-table))
                     (json-read-from-string (nate-agent--child-src inp-h))))))
    `(:id ,id :name ,name :input ,input)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nate-agent--parse-tool-heading (h)
  "Parse a '** Tool:' heading H.
Returns (use-block . result-or-nil)."
  (cl-destructuring-bind (&key id name input) (nate-agent--tool-properties h)
    (let* ((res-h  (nate-agent--find-child-heading h "Result"))
           (result (when res-h (nate-agent--child-example res-h))))
      (cons `((type . "tool_use") (id . ,id) (name . ,name) (input . ,input))
            (when result
              `((type . "tool_result") (tool_use_id . ,id) (content . ,result)))))))

(defun nate-agent--parse-tool-use-assistant (h)
  "Parse a :tool_use: * Assistant heading H.
Returns a list of up to 2 API message alists:
  - An assistant message (thinking text blocks + tool_use blocks).
  - A user message (tool_result blocks) -- omitted if any Result is missing."
  (let (content results)
    (dolist (child (org-element-contents h))
      (when (eq (org-element-type child) 'headline)
        (let ((title (org-element-property :raw-value child)))
          (cond
           ((string= title "Thinking")
            (when-let ((text (nate-agent--child-example child)))
              (push `((type . "text") (text . ,text)) content)))
           ((string-prefix-p "Tool: " title)
            (let ((pair (nate-agent--parse-tool-heading child)))
              (push (car pair) content)
              (when (cdr pair) (push (cdr pair) results))))))))
    (nconc
     (when content
       (list `((role . "assistant")
               (content . ,(apply #'vector (nreverse content))))))
     (when (and results
                ;; Only emit tool results if every tool has a Result block
                (= (length results) (length (nate-agent--direct-tool-headings h))))
       (list `((role . "user")
               (content . ,(apply #'vector (nreverse results)))))))))

(defun nate-agent--parse-end-turn-assistant (h)
  "Parse an :end_turn: * Assistant heading H.
Returns a list with one assistant message, or nil if ** Response is absent."
  (when-let* ((resp-h (nate-agent--find-child-heading h "Response"))
              (text   (nate-agent--subtree-text resp-h)))
    (list `((role . "assistant")
            (content . [((type . "text") (text . ,text))])))))

(defun nate-agent--build-history ()
  "Reconstruct the Anthropic API message list from the current buffer.
Walk top-level headings in order:
  * User      -> user message (skipped if empty)
  * Assistant -> decoded by tag (:tool_use:, :end_turn:; untagged -> skipped)
All other top-level headings are ignored."
  (let ((tree (org-element-parse-buffer))
        history)
    (org-element-map tree 'headline
      (lambda (h)
        (when (= (org-element-property :level h) 1)
          (let ((title (org-element-property :raw-value h))
                (tags  (org-element-property :tags h)))
            (cond
             ((string= title "User")
              (let ((text (nate-agent--subtree-text h)))
                (when (and text (not (string-empty-p text)))
                  (push `((role . "user")
                          (content . [((type . "text") (text . ,text))]))
                        history))))
             ((string= title "Assistant")
              (cond
               ((member "tool_use" tags)
                (dolist (msg (nate-agent--parse-tool-use-assistant h))
                  (push msg history)))
               ((member "end_turn" tags)
                (dolist (msg (nate-agent--parse-end-turn-assistant h))
                  (push msg history)))))))))
      nil nil 'headline)
    (let* ((msgs     (nreverse history))
           (content  (alist-get 'content (car (last msgs))))
           (last-i   (1- (length content))))
      (aset content last-i
            (append (aref content last-i)
                    '((cache_control . ((type . "ephemeral"))))))
      msgs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; State detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nate-agent--buffer-state ()
  "Return a symbol describing the current agent state in this buffer.

  waiting-for-input    -- last top-level heading is * User
  in-progress          -- last * Assistant has no tag (API call in flight)
  needs-tool-execution -- last * Assistant :tool_use: has a Tool heading missing *** Result
  needs-continuation   -- last * Assistant :tool_use: has all results
  idle                 -- last * Assistant is :end_turn: or :error:"
  (let* ((tree (org-element-parse-buffer))
         last-h)
    (org-element-map tree 'headline
      (lambda (h) (when (= (org-element-property :level h) 1) (setq last-h h)))
      nil nil 'headline)
    (if (null last-h)
        'idle
      (let ((title (org-element-property :raw-value last-h))
            (tags  (org-element-property :tags last-h)))
        (cond
         ((not (string= title "Assistant")) 'waiting-for-input)
         ((member "end_turn" tags) 'idle)
         ((member "error"    tags) 'idle)
         ((member "tool_use" tags)
	  (let ((pending (seq-find (lambda (tool) (null (nate-agent--find-child-heading tool "Result")))
				   (nate-agent--direct-tool-headings last-h))))
	    (if (null pending)
		'needs-continuation
	      (append '(needs-tool-execution) (nate-agent--tool-properties pending)))))
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
