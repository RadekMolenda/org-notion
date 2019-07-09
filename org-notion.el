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

;;; Code:

(defvar org-notion--get-record-values "https://www.notion.so/api/v3/getRecordValues")
(defvar org-notion--sumbit-transaction-url "https://www.notion.so/api/v3/submitTransaction")
(defvar org-notion--notion-id "NOTION_ID")

(require 's)
(require 'dash)
(require 'json)

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
(defun org-notion-fetch ()
  "Fetch the notion block and display it in a message."
  (interactive)
  (let* ((notion-id (cdr (assoc "NOTION_ID" (org-entry-properties)))))
    (if notion-id (let* ((response (fetch-json-block notion-id))
                         (results (alist-get 'results response)))
                    (message (format "org-notion-fetch results: %s" results)))
      (message "please define NOTION_ID as property"))))

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
