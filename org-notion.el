;;; org-notion.el --- keep org and notion in sync

;; Author: qrczeno
;; Created: 01-Jun-2019
;; Version: 1.0
;; Keywords: org, org-mode, notion
;; Package-Requires: ((s "1.9") (dash "2.19.1"))

;;; Commentary:

;; Usage:
;; Define the following properties your org note like that
;; * Notion page I would like to keep in sync
;;   :PROPERTIES:
;;   :NOTION_PAGE_ID: 0dc73850-a48c-4096-ab71-3eeeda48522e
;;   :END:
;;
;; while cursor being on note run
;;   `org-notion-import' to push the changes
;;   `org-notion-fetch' to fetch the changes

;; TODO: use (org-map-tree (lambda () (message (format "%s" (org-get-heading t t t t)))))
;;       to publish tree as page
;; TODO: links and todo lists in notion is a must
;;; Code:

(defvar org-notion--notion-api-url "https://api.notion.com")
(defvar org-notion--page-id-property-name "NOTION_PAGE_ID")
(defvar org-notion--notion-password-machine "notion.so")

(require 's)
(require 'dash)
(require 'json)
(require 'ox)
(require 'cl-lib)
(require 'auth-source)

(defun org-notion--get-password (host)
  "Get the password for the specified HOST from auth-sources."
  (let ((entry (auth-source-search :host host :max 1)))
    (when entry
      (let ((secret (plist-get (car entry) :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

(defun org-notion--make-request (method url &optional payload)
  "Make the request to URL given METHOD and return parsed json object."
  (let* ((url-request-method method)
         (token (org-notion--get-password org-notion--notion-password-machine))
         (url-request-extra-headers `(("Content-Type" . "application/json")
                                      ("Notion-Version" . "2022-06-28")
                                      ("Authorization" . ,(s-concat "Bearer " token))))
         (url-request-data payload))
    (with-temp-buffer (url-insert-file-contents url) (json-read))))


(defun org-notion--retrieve-page (notion-id)
  "Given NOTION-ID fetch the page and return parsed json."
  (org-notion--make-request  "GET" (s-concat "https://api.notion.com/v1/pages/" notion-id)))


(defun org-notion--retrieve-children-block (notion-id)
  "Given NOTION-ID fetch the page and return parsed json."
  (org-notion--make-request  "GET" (s-concat "https://api.notion.com/v1/blocks/" notion-id "/children?page_size=100")))

(defun org-notion--get-title (data)
  "Extract the 'title' from the json DATA."
  (let* ((properties (cdr (assoc 'properties data)))
          (name (cdr (assoc 'Name properties)))
          (title-info (cdr (assoc 'title name)))
          (title (elt title-info 0))
          (title-text (cdr (assoc 'plain_text title))))
    title-text))


(defun org-notion--export-as-notion-block (callback)
  "Export as notion to buffer and call CALLBACK."
  (interactive)
  (org-export-to-buffer 'notion "*notion export*"
    nil nil nil nil nil callback))

;; TODO: refactor to use notion api

(defun org-notion--format-plain-text (c _i)
  "Default notion format H C I."
  (format "[\"%s\"]" c))

(defun notion-format (&optional h c i)
  "Default notion format H C I."
  (format "[%s]" h))

(defun org-notion-link (link contents _i)
  "Format LINK, insert CONTENTS."
  (let ((url (org-element-property :raw-link link))
        (text (org-notion--get-text link)))
    (format "[\"%s \",[[\"a\",\"%s\"]]]" text url)))

(defun org-notion--format-template (contents _i)
  "Just print CONTENTS."
(replace-regexp-in-string "\\] *\\[" "\],\[" contents))

(defun org-notion--format-headline (&optional h _c i)
  "Format headline H I."
  (format "[%s]" (org-export-data (org-element-property :title h) i)))

(defun org-notion--format-bold (&optional h c i)
  "Format bold H I."
  (format "[\"%s\",[[\"b\"]]]" (org-notion--get-text h)))

(defun org-notion--format-italic (&optional h c i)
  "Format italic H I."
  (format "[\"%s\",[[\"i\"]]]" (org-notion--get-text h) ))

(defun org-notion--get-text (element)
  (let ((start (org-element-property :contents-begin element))
        (end (org-element-property :contents-end element)))
    (buffer-substring start end)))

(org-export-define-backend 'notion
  '(
    (plain-text . org-notion--format-plain-text)
    (headline . org-notion--format-headline)
    (entity . notion-format)
    (bold . org-notion--format-bold)
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
    (italic . org-notion--format-italic)
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

(defun org-notion--insert-page-content (notion-id)
  "Given NOTION-ID fetch page data and insert title and set property name to NOTION-ID."
  (let ((notion-json (org-notion--retrieve-page notion-id)))
    (org-insert-heading-respect-content)
    (insert (org-notion--get-title notion-json))
    (org-entry-put nil org-notion--page-id-property-name notion-id)))

(defun org-notion--insert-block-children-content (notion-id)

  "Given NOTION-ID fetch page children and import blocks to org file."

  (let ((notion-json (org-notion--retrieve-children-block notion-id)))
    (org-insert-heading-respect-content t)
    (org-do-demote)
    (insert "all other cool stuff is going here")))

;;;###autoload
(defun org-notion-send-block ()
  "Import notion block as org item.  NOTION-ID is notion uuid of imported block."
  (interactive)
  (let* ((notion-id (org-notion--uuid (org-entry-get (point) org-notion--notion-id)))
         (title (substring-no-properties (org-get-heading t t t t)))
         (callback (lambda (_status) (message "OK"))))
    (push-title notion-id title callback)))


;; TODO: this is where refactoring ends

;;;###autoload
(defun org-notion-import-page (notion-id)
  "Import notion block as org item.  NOTION-ID is notion uuid of imported block."
  (org-notion--insert-page-content notion-id)
  (org-notion--insert-block-children-content notion-id))


(provide 'org-notion)
;;; org-notion.el ends here
