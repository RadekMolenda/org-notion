;;; org-notion.el --- keep org and notion in sync

;; Author: qrczeno
;; Created: 01-Jun-2019
;; Version: 0.1
;; Keywords: org, org-mode, notion
;; Package-Requires: ((s "1.9"))

;;; Commentary:

;; Usage:
;; Define the following properties your org note like that
;; * Notion page I would like to keep in sync
;;   :PROPERTIES:
;;   :NOTION_ID: 0dc73850-a48c-4096-ab71-3eeeda48522e
;;   :NOTION_TOKEN_V2_FILE_PATH: ~/token_v2.gpg
;;   :END:
;;
;; while cursor being on note run
;;   `org-node-push' to push the changes
;;   `org-node-fetch' to fetch the changes

;; TODO: use (org-map-tree (lambda () (message (format "%s" (org-get-heading t t t t)))))
;;       to publish tree as page
;; TODO: links and todo lists in notion is a must
;;; Code:

(defvar org-notion--get-record-values "https://www.notion.so/api/v3/getRecordValues")
(defvar org-notion--sumbit-transaction-url "https://www.notion.so/api/v3/submitTransaction")
(defvar org-notion--notion-id "NOTION_ID")

(require 's)
(require 'dash)
(require 'json)
(require 'ox)
(require 'cl-lib)

(defun export-as-notion-block (&optional async subtreep visible-only)
  "export as notion to buffer"
  (interactive)
  (org-export-to-buffer 'notion "*notion export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))


(defun org-notion--format-plain-text (c _i)
  "Default notion format H C I."
  (format "[\"%s\"]" c))

(defun notion-format (&optional h c i)
  "Default notion format H C I."
  (format "[%s]" h))

(defun org-notion-link (link contents _i)
  "Format LINK, insert CONTENTS."
  (let ((url (org-element-property :raw-link link)))
    (format "[%s,[[\"a\",\"%s\"]]" contents url)))

(defun org-notion--format-template (contents _i)
  "Just print CONTENTS."
  (message (format "%s" contents))
  contents)

(defun org-notion--format-headline (&optional h _c i)
  "Format headline H I."
  ;;(message (format "HEADLINE %s" (org-export-data (org-element-property :title h) i)) )
  (format "[%s]" (org-export-data (org-element-property :title h) i)))

(org-export-define-backend 'notion
  '(
    (plain-text . org-notion--format-plain-text)
    (headline . org-notion--format-headline)
    (entity . notion-format)
    (bold . notion-format)
    (center-block . notion-format)
    (clock . notion-format)
    (code . notion-format)
    (drawer . notion-format)
    (dynamic-block . notion-format)
    (example-block . notion-format)
    (export-block . notion-format)
    (export-snippet . notion-format)
    (fixed-width . notion-format)
    (footnote-definition . notion-format)
    (footnote-reference . notion-format)
    (horizontal-rule . notion-format)
    (inline-src-block . notion-format)
    (inlinetask . notion-format)
    (italic . notion-format)
    (item . notion-format)
    (keyword . notion-format)
    (latex-environment . notion-format)
    (latex-fragment . notion-format)
    (line-break . notion-format)
    (link . org-notion-link)
    (node-property . notion-format)
    (paragraph . notion-format)
    (plain-list . notion-format)
    (planning . notion-format)
    (property-drawer . notion-format)
    (quote-block . notion-format)
    (radio-target . notion-format)
    (section . notion-format)
    (special-block . notion-format)
    (src-block . notion-format)
    (statistics-cookie . notion-format)
    (strike-through . notion-format)

    (subscript . notion-format)
    (superscript . notion-format)
    (table . notion-format)
    (table-cell . notion-format)
    (table-row . notion-format)
    (target . notion-format)

    (template . org-notion--format-template)
    (timestamp . notion-format)
    (underline . notion-format)
    (verbatim . notion-format)
    (verse-block . notion-format)
    ))


(defun get-string-from-file (file-path)
  "Return FILE_PATH file content."
  (with-temp-buffer (insert-file-contents file-path)
                    (delete-blank-lines)
                    (buffer-string)))



(defun org-notion--token-v2 ()
  "Get the token."
  (save-excursion (let ((token-path (or (cdr (assoc "NOTION_TOKEN_V2_FILE_PATH"
                                                    (org-entry-properties)))
                                        "~/token_v2.gpg")))
                    (s-trim (format "token_v2=%s" (get-string-from-file token-path))))))

(defun org-notion--uuid (uuid-string)
  "Tries to parse UUID-STRING and returns uuid or nil if failed."
  (s-join "-" (cdr (s-match "^\\([[:alnum:]]\\{8\\}\\)-?\\([[:alnum:]]\\{4\\}\\)-?\\([[:alnum:]]\\{4\\}\\)-?\\([[:alnum:]]\\{4\\}\\)-?\\([[:alnum:]]\\{12\\}\\)$" uuid-string))))

(defun org-notion--request (url payload cb)
  "Send PAYLOAD post json request to URL and call a CB in response buffer."
  (let* ((url-request-method "POST")
         (token-v2 (org-notion--token-v2))
         (url-request-extra-headers `(("Content-Type" . "application/json")
                                      ("Cookie" . ,(org-notion--token-v2))))
         (url-request-data payload))
    (progn
      (message (format "Payload %s" payload))
      (url-retrieve url cb))))

(defun fetch-block (notion-id callback)
  "Given the NOTION-ID fetch the corresponding blocks."
  (org-notion--request org-notion--get-record-values (json-encode `(("requests" . ((("id" . ,notion-id) ("table" . "block")))))) callback))

(defun push-title (notion-id title callback)
  "Publish the item under point."
  (message (format "notion-id %s title %s" notion-id title))
  (org-notion--request
   org-notion--sumbit-transaction-url
   (json-encode `(("operations" .
                   ((("args" . [[,title]])
                     ("command" . "set")
                     ("path" . ("properties" "title"))
                     ("table" . "block")
                     ("id" . ,notion-id))))))
   callback))


;;;###autoload
(defun org-notion-send-block ()
  "Import notion block as org item.  NOTION-ID is notion uuid of imported block."
  (interactive)
  (let* ((notion-id (org-notion--uuid (org-entry-get (point) org-notion--notion-id)))
         (title (substring-no-properties (org-get-heading t t t t)))
         (callback (lambda (_status) (message "OK"))))
    (push-title notion-id title callback)))

;;;###autoload
(defun org-notion-import-block (notion-id)
  "Import notion block as org item.  NOTION-ID is notion uuid of imported block."
  (interactive "sNotion id: ")
  (let ((notion-id (org-notion--uuid notion-id)))
    (progn (message (format "Importing %s" notion-id))
           (org-insert-heading-after-current)
           (org-set-property org-notion--notion-id notion-id)
           (setq org-notion--current-org-notion-buffer (current-buffer))
           (fetch-block notion-id (lambda (_status)
                                    (with-current-buffer (current-buffer)
                                      (search-forward "\n\n")
                                      (let* ((data (json-read))
                                             (first-result (elt (alist-get 'results data) 0))
                                             (value (alist-get 'value first-result))
                                             (properties (alist-get 'properties value))
                                             (title (elt (elt (alist-get 'title properties) 0)
                                                         0)))
                                        (message (format "title: %s" title))
                                        (switch-to-buffer org-notion--current-org-notion-buffer)
                                        (insert (format " %s" title)))))))))


;;;###autoload
(defun org-notion-open ()
  "Fetch the notion block and update the note accordingly."
  (interactive)
  (let ((notion-id (cdr (assoc org-notion--notion-id (org-entry-properties))))
        (notion-namespace (cdr (assoc "NOTION_NAMESPACE" (org-entry-properties)))))
    (if notion-id (browse-url (format "https://www.notion.so/%s/%s" notion-namespace notion-id))
      (message "please define NOTION_ID as property"))))


(provide 'org-notion)
;;; org-notion.el ends here
