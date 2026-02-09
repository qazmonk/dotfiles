(require 'ox)
(require 'ox-html)

;; Define the video link type
(org-link-set-parameters 
 "video"
 :export #'org-video-export
 :face 'org-link
 :complete (lambda (&optional arg) (concat
				    "iframe:"
				    (substring (org-link-complete-file arg) 5))))

(defun org-video-export (path desc backend)
  "Export video links to HTML video tags."
  (when (eq backend 'html)
    (let ((video-attrs (if desc 
                          (format " style=\"%s\"" desc)
                        "")))
      (format "<div class=\"animation-container\">\n  <video controls autoplay loop muted%s>\n    <source src=\"%s\" type=\"video/mp4\">\n    Your browser does not support the video tag.\n  </video>\n</div>\n" 
              video-attrs path))))


;; Define the iframe link type
(org-link-set-parameters 
 "iframe"
 :export #'org-iframe-export
 :face 'org-link
 :complete (lambda (&optional arg) (concat
				    "iframe:"
				    (substring (org-link-complete-file arg) 5))))

(defun org-iframe-export (path desc backend)
  "Export iframe links to HTML iframe elements."
  (when (eq backend 'html)
    (let ((iframe-attrs (if desc desc "")))
      (format "<iframe src=\"%s\" %s></iframe>" path desc))))

(defun my-org-html-item (item contents info)
  "Transcode an ITEM element from Org to HTML.
This is a wrapper around org-html-item meant to handle description lists
specially. This function changes dt and dd tags into a <div> with the
<dt> contents styled as a title."
  (let* ((plain-list (org-element-parent item))
	 (type (org-element-property :type plain-list))
	 (counter (org-element-property :counter item))
	 (checkbox (org-element-property :checkbox item))
	 (tag (let ((tag (org-element-property :tag item)))
		(and tag (org-export-data tag info)))))
    (pcase type
      (`descriptive
       (my-org-html-format-description-list-item
	contents type checkbox info tag))
      (other-type
       (org-html-format-list-item
	contents type checkbox info (or tag counter))))))

(defun my-org-html-format-description-list-item (contents type checkbox info tag)
  "Format a descriptive list item into HTML.
CONTENTS is the item contents. TYPE is `descriptive'. CHECKBOX checkbox
type is nil or one of symbols `on', `off', or `trans'. INFO is the info
plist."
  (concat
   "<div class=\"titled-figure\">"
   (format "<h4>%s</h4>"  tag)
   contents
   "</div>"))

;; Override the description list export to handle iframe links
(defun my-org-html-plain-list (plain-list contents info)
  "Custom handling for description lists in results blocks."
  (let* ((type (org-element-property :type plain-list))
         (parent (org-element-property :parent plain-list))
         (parent-type (when parent (org-element-property :type parent))))
    
    ;; If this is a description list inside a results block, handle specially
    (pcase type
      (`descriptive (format "<div class=\"titled-figure-list\">%s</div>" contents))
      (other-type (org-html-plain-list plain-list contents info)))))

(defun my-org-html-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((lang (org-element-property :language src-block))
	 (code (org-html-format-code src-block info))
	 (label (let ((lbl (org-html--reference src-block info t)))
		  (if lbl (format " id=\"%s\"" lbl) ""))))
    (format "<pre><code class=\"language-%s\">%s</code></pre>" lang code)))

;; (defun my-org-html-special-block (special-block contents info)
;;   "Handle custom result blocks."
;;   (let ((type (org-element-property :type special-block)))
;;     (cond
;;      ;; Handle results blocks - just wrap in a div with class
;;      ((string= type "results")
;;       (format "<div class=\"results-group\">\n%s</div>\n" contents))
     
;;      ;; Fall back to default for everything else
;;      (t (org-html-special-block special-block contents info)))))


;; (defun my-process-results-items (contents)
;;   "Convert description list items to result divs with iframes."
;;   (replace-regexp-in-string
;;    "<dt>\\(.*?\\)</dt>\\s-*<dd>\\(.*?\\)</dd>"
;;    (lambda (match)
;;      (let ((method-name (match-string 1 match))
;;            (file-path (string-trim (match-string 2 match))))
;;        (format "<div class=\"result-item\">\n  <h4>%s</h4>\n  <iframe src=\"%s\"></iframe>\n</div>\n"
;; 	       method-name file-path)))
;;    contents))

;; ;; Custom source block handler for Prism.js
;; (defun my-org-html-src-block (src-block _contents info)
;;   "Transcode a SRC-BLOCK element from Org to HTML with Prism.js classes."
;;   (let* ((lang (org-element-property :language src-block))
;;          (code (org-html-format-code src-block info))
;;          (label (let ((lbl (and (org-element-property :name src-block)
;;                                (org-export-get-reference src-block info))))
;;                   (if lbl (format " id=\"%s\"" lbl) "")))
;;          (klipsify  (and  (plist-get info :html-klipsify-src)
;;                           (member lang '("javascript" "js"
;;                                          "ruby" "scheme" "clojure" "php" "html")))))
;;     (if (not lang) (format "<pre class=\"example\"%s>\n%s</pre>" label code)
;;       (format "<div class=\"org-src-container\">\n%s%s\n</div>"
;;               ;; Build caption.
;;               (let ((caption (org-export-get-caption src-block)))
;;                 (if (not caption) ""
;;                   (let ((listing-number
;;                          (format
;;                           "<span class=\"listing-number\">%s </span>"
;;                           (format
;;                            (org-html--translate "Listing %d:" info)
;;                            (org-export-get-ordinal
;;                             src-block info nil #'org-html--has-caption-p)))))
;;                     (format "<label class=\"org-src-name\">%s%s</label>"
;;                             listing-number
;;                             (org-trim (org-export-data caption info))))))
;;               ;; Contents.
;;               (if klipsify
;;                   (format "<pre><code class=\"src src-%s\"%s%s>%s</code></pre>"
;;                           lang
;;                           label
;;                           (if (string= lang "html")
;;                               " data-editor-type=\"html\"" "")
;;                           code)
;;                 (format "<pre class=\"language-%s\"%s><code>%s</code></pre>"
;;                         lang label code))))))


;; Register the custom backend with menu entry
(org-export-define-derived-backend 'my-html 'html
  :menu-entry '(?m "Export to My HTML"
                   ((?h "As HTML file" my-org-export-to-html)
                    (?H "To HTML buffer" my-org-export-as-html)
                    (?o "As HTML file and open" my-org-export-to-html-and-open)))
  :translate-alist '((plain-list . my-org-html-plain-list)
		     (item . my-org-html-item)
		     (src-block . my-org-html-src-block)))

;; Define the export functions
(defun my-org-export-as-html (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a My HTML buffer."
  (interactive)
  (org-export-to-buffer 'my-html "*Org My HTML Export*"
    async subtreep visible-only body-only ext-plist (lambda () (html-mode))))

(defun my-org-export-to-html (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a My HTML file."
  (interactive)
  (let* ((extension (concat "." (or (plist-get ext-plist :html-extension) "html")))
         (file (org-export-output-file-name extension subtreep)))
    (org-export-to-file 'my-html file async subtreep visible-only body-only ext-plist)))

(defun my-org-export-to-html-and-open (&optional async subtreep visible-only body-only ext-plist)
  "Export to My HTML and open in browser."
  (interactive)
  (let ((file (my-org-export-to-html async subtreep visible-only body-only ext-plist)))
    (when file (browse-url-of-file file))))
