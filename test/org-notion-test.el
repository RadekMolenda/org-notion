;;; org-notion-test.el --- Test for org-notion.el

;; Copyright (C) 2023 by Radek Molenda

;; Author: Radek Molenda <radek.molenda@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)
(require 'cl-macs)


(require 'org-notion)

(ert-deftest fetching-page-calls-the-right-api ()
  "Fetching page calls make request"
  (cl-letf (((symbol-function 'org-notion--make-request)
             (lambda (method url &optional payload)
               (should (equal method "GET"))
               (should (equal url "https://api.notion.com/v1/pages/xxxx-xxxx-xxxx-xxxxxxxx")))))
    (org-notion--fetch-page "xxxx-xxxx-xxxx-xxxxxxxx")))

(setq stub-org-notion--fetch-page (lambda (page-id)
            (let ((buffer (find-file-noselect "test/fixtures/valid-page-response.txt")))
              (with-current-buffer buffer (setq buffer-read-only t) buffer))))

(ert-deftest importing-page ()
  "Importing page builds buffer with correct org data in it"
  (cl-letf (((symbol-function 'org-notion--fetch-page)
             stub-org-notion--fetch-page))
           (should (s-equals? (org-notion-import-page "whatever") "yes yes yes"))))



;;; test.el ends here
