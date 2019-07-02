(defvar notion-id "94a6fa90-5800-4c4d-9381-984748668cbc")


;; "https://www.notion.so/api/v3/getRecordValues"
(require 's)

(setq requests (make-hash-table))
(setq requests-block (make-hash-table))

(setf (gethash "id" requests-block) notion-id)
(setf (gethash "table" requests-block) "block")
(setf (gethash "requests" requests) (list requests-block))
(json-encode requests)
(json-encode-plist '("requests" ("id" ,notion-id "table" "block")))
(defun fetch-block (notion-id)
  "Send ARGS to URL as a POST request."
  (let ((a-url "https://www.notion.so/api/v3/getRecordValues")
        (url-request-method "POST")
        (url-request-extra-headers
          '(("Content-Type" . "application/json")
            ("Cookie" . token-v2)))
        (url-request-data (json-encode requests)))
    (url-retrieve-synchronously a-url)))

(defun fetch-json-block (notion-id)
   (with-current-buffer (fetch-block notion-id)
     ; there's probably a better way of stripping the headers
     (delete-region (point-min) (point))
     (buffer-string)))

(with-current-buffer (fetch-block notion-id)
  (buffer-string))

(alist-get "results" (car (json-read-from-string (fetch-json-block notion-id))))
(defun my-switch-to-url-buffer (status)
  "Switch to the buffer returned by `url-retrieve`.
The buffer contains the raw HTTP response sent by the server."
(switch-to-buffer (current-buffer)))

(fetch-json-block notion-id)
