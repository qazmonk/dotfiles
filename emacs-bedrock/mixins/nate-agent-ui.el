;;; nate-agent-ui.el --- UI layer for nate-agent  -*- lexical-binding: t -*-

;;; Commentary:
;; Mode definition, keybindings, mode-line, buffer rendering, and
;; tool approval commands for nate-agent.

;;; Code:

(require 'nate-agent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mode-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local nate-agent--agent-status 'idle
  "Buffer-local agent status for the mode-line. One of: idle, waiting, tool.")

(defface nate-agent-tokens-low  '((t :inherit success :weight bold)) "< 50% context used.")
(defface nate-agent-tokens-mid  '((t :inherit warning :weight bold)) "50-80% context used.")
(defface nate-agent-tokens-high '((t :inherit error   :weight bold)) "> 80% context used.")

(defun nate-agent--token-indicator (n)
  "Return a mode-line construct showing N tokens vs `nate-agent-context-window'."
  (let* ((pct  (round (* 100.0 (/ (float n) nate-agent-context-window))))
         (face (cond ((>= pct 80) 'nate-agent-tokens-high)
                     ((>= pct 50) 'nate-agent-tokens-mid)
                     (t           'nate-agent-tokens-low)))
         (str  (format "%dk/%dk(%d%%%%)" (/ n 1000)
                       (/ nate-agent-context-window 1000) pct)))
    `(:propertize ,str face ,face)))

(defun nate-agent--mode-line-segment ()
  "Render the agent status and token count for the mode-line."
  (let ((status-construct (pcase nate-agent--agent-status
                            ('waiting '(:propertize "waiting..." face warning))
                            ('tool    '(:propertize "tool..."    face warning))
                            ('idle    "idle")
                            (_        nil)))
        (tok-construct (when nate-agent--last-input-tokens
                         (nate-agent--token-indicator nate-agent--last-input-tokens))))
    (list "[" status-construct " | " tok-construct "]")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tool heading helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nate-agent--back-to-tool-heading ()
  "Move point to the enclosing ** Tool: heading, error if not found."
  (org-back-to-heading t)
  (while (> (org-current-level) 1)
    (outline-up-heading 1 t))
  (unless (string-prefix-p "Tool: " (org-entry-get (point) "ITEM"))
    (user-error "Not inside a tool heading")))

(defun nate-agent--ui-goto-tool (buf id)
  (goto-char (point-max))
  (re-search-backward (concat ":TOOL_ID: +" (regexp-quote id)) nil t))

(defun nate-agent--ui-tag-tool (buf id tags)
  "Set TAGS on the tool heading with ID in BUF."
  (with-current-buffer buf
    (save-excursion
      (when (nate-agent--ui-goto-tool buf id)
        (org-back-to-heading t)
        (org-set-tags tags)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Buffer rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nate-agent--ui-append (buf text)
  "Append TEXT to the end of BUF."
  (with-current-buffer buf
    (goto-char (point-max))
    (insert text)))

(defun nate-agent--ui-append-assistant (buf text)
  "Append TEXT to BUF under the current * Assistant heading.
Demotes any headings in TEXT to level 2+ so they nest properly."
  (let ((start (with-current-buffer buf (point-max))))
    (nate-agent--ui-append buf text)
    (with-current-buffer buf
      (save-excursion
        (save-restriction
          (narrow-to-region start (point-max))
          (goto-char (point-min))
          (while (re-search-forward org-heading-regexp nil t)
            (beginning-of-line)
            (when (< (org-current-level) 2)
              (org-demote-subtree))
            (org-end-of-subtree t t)))))))

(defun nate-agent--ui-insert-text-output (buf output-list)
  "Insert non-function output items (reasoning, message) from OUTPUT-LIST into BUF.
Writes separate * Assistant headings for reasoning and message items.
Returns non-nil if any text was written."
  (let (text-written)
    (dolist (item output-list)
      (let ((type (gethash "type" item)))
        (when-let ((data (and (or (string= type "reasoning") (string= type "message"))
                              (nate-agent--extract-text-content item type))))
          (nate-agent--ui-append buf "\n* Assistant\n")
          (nate-agent--ui-set-assistant-tag buf type)
          (nate-agent--ui-append-assistant buf data)
          ;; Auto-fold reasoning messages
          (when (string= type "reasoning")
            (with-current-buffer buf
              (save-excursion
                (goto-char (point-max))
                (re-search-backward "^\\* Assistant" nil t)
                (org-fold-subtree t))))
          (setq text-written t))))
    text-written))

(defun nate-agent--extract-text-content (item type)
  "Extract text from ITEM's content array based on TYPE.
TYPE should be \"reasoning\" or \"message\". Returns nil if empty."
  (let* ((content (gethash "content" item))
         (content-list (when content (append content nil)))
         (block-type (if (string= type "reasoning") "reasoning_text" "output_text")))
    (when content-list
      (let ((text (mapconcat (lambda (block)
                              (when (string= (gethash "type" block) block-type)
                                (gethash "text" block)))
                            content-list "")))
        (when (and text (not (string-empty-p text)))
          text)))))


(defun nate-agent--ui-append-tool-call (buf name input id)
  "Render a tool call heading into BUF."
  (with-current-buffer buf
    (goto-char (point-max))
    (insert "\n")
    (insert (format "* Tool: %s\n" name))
    (org-set-property "TOOL_NAME" name)
    (org-set-property "TOOL_ID" id)
    (org-back-to-heading)
    (org-set-tags '("pending"))    
    (goto-char (point-max))
    (insert "\n")
    (let ((input-start (point)))
      (insert (format "** Input\n#+begin_src json\n%s\n#+end_src\n" (json-encode input)))
      (save-excursion
        (goto-char input-start)
        (org-fold-subtree t)))))

(defun nate-agent--ui-write-tool-result (buf id result)
  "Append ** Result under the * Tool heading matching ID in BUF.
Changes :pending: tag to :executed: and folds the subtree."
  (with-current-buffer buf
    (goto-char (point-max))
    (unless (re-search-backward (concat ":TOOL_ID: +" (regexp-quote id)) nil t)
      (error "No tool heading found for TOOL_ID %s" id))
    (org-back-to-heading t)
    ;; Change :pending: to :executed:, remove :pending_approval:/:approved:
    (let ((tags (org-get-tags)))
      (setq tags (cl-remove-if (lambda (t) (member t '("pending" "pending_approval" "approved"))) tags))
      (org-set-tags (cons "executed" tags)))
    (let ((subtree-start (point)))
      (org-end-of-subtree t t)
      (insert (format "** Result\n#+begin_example\n%s\n#+end_example\n"
                      (org-escape-code-in-string result)))
      (save-excursion
        (goto-char subtree-start)
        (org-fold-subtree t)))))

(defun nate-agent--ui-insert-tool-calls (buf output-list)
  "Insert function_call items from OUTPUT-LIST into BUF.
If JSON parsing of tool arguments fails, treat it as a failed tool call."
  (dolist (item output-list)
    (when (string= (gethash "type" item) "function_call")
      (let* ((name  (gethash "name" item))
             (id    (gethash "call_id" item))
             (args  (gethash "arguments" item)))  ; JSON string
        ;; First: try to parse JSON and append tool heading
        ;; If this fails, we haven't written anything yet
        (condition-case err
            (let* ((input (let ((json-object-type 'hash-table))
                            (json-read-from-string args)))
                   (tool  (gethash name nate-agent--tool-registry)))
              (unless tool
                (error "Unknown tool requested by model: %s" name))
              ;; Write the tool call heading
              (nate-agent--ui-append-tool-call buf name input id)
              ;; Now try to write the display (if any)
              ;; If this fails, the tool call is already written, so just write error result
              (condition-case display-err
                  (let* ((display-fn (plist-get tool :display-fn))
                         (display    (when display-fn (funcall display-fn input)))
                         (content    (if (consp display) (car display) display))
                         (lang       (when (consp display) (cadr display))))
                    (when content
                      (nate-agent--ui-write-display buf id content lang)))
                (error
                 ;; Display failed - tool heading already written, just add result
                 (nate-agent--ui-write-tool-result buf id (format "Tool validation error: %s" (error-message-string display-err)))
                 (nate-agent--schedule-step buf))))
          (error
           ;; JSON parse error - tool call not written yet, write it with empty input + error
           (nate-agent--ui-append-tool-call buf name (make-hash-table :test 'equal) id)
           (nate-agent--ui-write-tool-result buf id (format "Tool error: %s" (error-message-string err)))
           (nate-agent--schedule-step buf)))))))

(defun nate-agent--ui-write-display (buf id content &optional lang)
  "Write a ** Display block under the tool heading with ID in BUF.
CONTENT is the display string. LANG is the src block language (nil = example block)."
  (with-current-buffer buf
    (save-excursion
      (nate-agent--ui-goto-tool buf id)
      (org-back-to-heading t)
      (org-end-of-subtree t t)
      (if lang
          (insert (format "** Display\n#+begin_src %s\n%s\n#+end_src\n"
                          lang (org-escape-code-in-string content)))
        (insert (format "** Display\n#+begin_example\n%s\n#+end_example\n"
                        (org-escape-code-in-string content)))))))

(defun nate-agent--ui-append-error (buf text)
  "Append error TEXT to BUF under a ** Response heading."
  (nate-agent--ui-append buf (format "** Response\n%s\n" text)))

(defun nate-agent--ui-set-assistant-tag (buf tag)
  "Set TAG on the most recent * Assistant heading in BUF."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward "^\\* Assistant" nil t)
        (org-set-tags (list tag))))))

(defun nate-agent--ui-set-status (buf status)
  "Set STATUS in BUF's mode-line segment."
  (with-current-buffer buf
    (setq nate-agent--agent-status status)
    (force-mode-line-update)))

(defun nate-agent--ui-ready (buf)
  "Set BUF status to idle and append a fresh * User prompt."
  (nate-agent--ui-set-status buf 'idle)
  (with-current-buffer buf
    (goto-char (point-max))
    (insert "\n* User\n")))

(defun nate-agent--ui-append-request (buf req-buf)
  "Append a '* Request' heading to BUF with REQUEST-BODY in a folded json src block.
REQ-BUF is the url-retrieve buffer, whose name is stored in the REQUEST_BUFFER
property so `nate-agent-cancel-request' can find it."
  (with-current-buffer buf
    (goto-char (point-max))
    (insert "\n")
    (let ((beg (point)))
      (insert "* Request\n")
      (org-set-property "REQUEST_BUFFER" (buffer-name req-buf))
      (save-excursion
	(goto-char beg)
	(org-fold-subtree t)))))


(provide 'nate-agent-ui)
;;; nate-agent-ui.el ends here
