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

(defvar org-notion--notion-url "https://www.notion.so/api/v3/getRecordValues")
(defvar org-notion--notion-id "NOTION_ID")

(require 's)
(require 'json)

(defun get-string-from-file (file-path)
  "Return FILE_PATH file content."
  (with-temp-buffer (insert-file-contents file-path)
                    (delete-blank-lines)
                    (buffer-string)))

(defun org-notion--request-payload (notion-id)
  "Transform NOTION-ID to request-payload."
  (let ((requests (make-hash-table))
        (requests-block (make-hash-table)))
    (setf (gethash "id" requests-block) notion-id)
    (setf (gethash "table" requests-block) "block")
    (setf (gethash "requests" requests)
          (list requests-block))
    (json-encode requests)))

(defun org-notion--token-v2 ()
  "Get the token."
  (save-excursion (let ((token-path (or (cdr (assoc "NOTION_TOKEN_V2_FILE_PATH"
                                                    (org-entry-properties)))
                                        "~/token_v2.gpg")))
                    (s-trim (format "token_v2=%s" (get-string-from-file token-path))))))


;; TODO: move to an actual notion client accepting response
(defun fetch-block (notion-id cb)
  "Given the NOTION-ID fetch the corresponding blocks."
  (let* ((url-request-method "POST")
         (token-v2 (org-notion--token-v2))
         (url-request-extra-headers `(("Content-Type" . "application/json")
                                      ("Cookie" . ,(org-notion--token-v2))))
         (url-request-data (org-notion--request-payload notion-id)))
    (message (org-notion--request-payload notion-id))
    (url-retrieve org-notion--notion-url cb)))

;;;###autoload
(defun org-notion-import-block (notion-id)
  "Import notion block as org item.  NOTION-ID is notion uuid of imported block."
  (interactive "sNotion id: ")
  (progn (message (format "Importing %s" notion-id))
         (org-insert-heading-after-current)
         ;; (insert (format " %s" notion-id))
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
                                           (insert (format " %s" title))))))))

;;;###autoload
(defun org-notion-push ()
  "Push the note to notion."
  (interactive)
  (message "org-node-push"))

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
