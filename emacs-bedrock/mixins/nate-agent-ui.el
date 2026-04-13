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
         (str  (format "%dk/%dk(%d%%)" (/ n 1000)
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
  (while (> (org-current-level) 2)
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

(defun nate-agent--ui-append-literal (buf text)
  "Wrap TEXT in an example block and append to BUF."
  (with-current-buffer buf
    (goto-char (point-max))
    (insert (format "#+begin_example\n%s\n#+end_example"
                    (org-escape-code-in-string text)))))

(defun nate-agent--ui-append-thinking (buf text)
  "Render a thinking text block into BUF, folded."
  (with-current-buffer buf
    (goto-char (point-max))
    (let ((start (point)))
      (insert "** Thinking\n")
      (nate-agent--ui-append-literal buf text)
      (save-excursion
        (goto-char start)
        (org-fold-subtree t)))))

(defun nate-agent--ui-append-tool-call (buf name input id)
  "Render a tool call heading into BUF."
  (with-current-buffer buf
    (goto-char (point-max))
    (insert "\n")
    (insert (format "** Tool: %s\n" name))
    (org-set-property "TOOL_NAME" name)
    (org-set-property "TOOL_ID" id)
    (goto-char (point-max))
    (insert (format "*** Input\n#+begin_src json\n%s\n#+end_src\n" (json-encode input)))))

(defun nate-agent--ui-write-tool-result (buf id result)
  "Append *** Result under the ** Tool heading matching ID in BUF. Folds the subtree."
  (with-current-buffer buf
    (goto-char (point-max))
    (unless (re-search-backward (concat ":TOOL_ID: +" (regexp-quote id)) nil t)
      (error "No tool heading found for TOOL_ID %s" id))
    (org-back-to-heading t)
    (let ((subtree-start (point)))
      (org-end-of-subtree t t)
      (insert (format "*** Result\n#+begin_example\n%s\n#+end_example\n"
                      (org-escape-code-in-string result)))
      (save-excursion
        (goto-char subtree-start)
        (org-fold-subtree t)))))

(defun nate-agent--ui-insert-tool-calls (buf content-list)
  "Insert tool_use and thinking blocks from CONTENT-LIST into BUF."
  (dolist (block content-list)
    (cond
     ((string= (gethash "type" block) "tool_use")
      (let* ((name (gethash "name" block))
             (input (gethash "input" block))
             (id (gethash "id" block))
	     (tool (gethash name nate-agent--tool-registry)))
	(unless tool
	  (error "Unknown tool requested by model: %s" name))
	(nate-agent--ui-append-tool-call buf name input id)	
	(let* ((display-fn (plist-get tool :display-fn))
               (display    (when display-fn (funcall display-fn input)))
               (content    (if (consp display) (car display) display))
               (lang       (when (consp display) (cadr display))))
          (when content
            (nate-agent--ui-write-display buf id content lang)))))
     ((string= (gethash "type" block) "text")
      (nate-agent--ui-append-thinking buf (gethash "text" block))))))

(defun nate-agent--ui-write-display (buf id content &optional lang)
  "Write a *** Display block under the tool heading with ID in BUF.
CONTENT is the display string. LANG is the src block language (nil = example block)."
  (with-current-buffer buf
    (save-excursion
      (nate-agent--ui-goto-tool buf id)
      (org-back-to-heading t)
      (org-end-of-subtree t t)
      (if lang
          (insert (format "*** Display\n#+begin_src %s\n%s\n#+end_src\n"
                          lang (org-escape-code-in-string content)))
        (insert (format "*** Display\n#+begin_example\n%s\n#+end_example\n"
                        (org-escape-code-in-string content)))))))

(defun nate-agent--ui-append-response (buf text)
  "Render the model's final TEXT into BUF under a ** Response heading.
Top-level headings in TEXT are demoted to keep them nested under ** Response."
  (nate-agent--ui-append buf "** Response\n")
  (let ((response-start (with-current-buffer buf (point-max))))
    (nate-agent--ui-append buf text)
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

(provide 'nate-agent-ui)
;;; nate-agent-ui.el ends here
