;;; org-notion-test.el --- Test for org-notion.el

;; Copyright (C) 2016 by Radek MOLENDA

;; Author: Radek MOLENDA <radek.molenda@gmail.com>

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

(require 'org-notion)

(ert-deftest parse-uuid ()
  "'parse-uuid' test"
  (let ((id (org-notion--uuid "d8a60223-5a72-44c4-b075-145ecb173e05")))
    (should (s-equals? id "d8a60223-5a72-44c4-b075-145ecb173e05")))
  (let ((id (org-notion--uuid "d8a602235a7244c4b075145ecb173e05")))
    (should (s-equals? id "d8a60223-5a72-44c4-b075-145ecb173e05")))
  (let ((id (org-notion--uuid "some-garbage")))
    (should (s-equals? id ""))))

(ert-deftest parsing-item ()
  "Parses the heading item correctly"
    (with-temp-buffer
      (org-mode)
      (insert "* yes [[http://www.example.com][hello]] link *bold* /italics/")
      (goto-char (point-min))
      (push-mark (point-max))
      (setq mark-active t)
      (export-as-notion-block (lambda ()
                                (let ((expected-string "[[\"yes \"],[\"hello \",[[\"a\",\"http://www.example.com\"]]],[\"link \"],[\"bold\",[[\"b\"]]],[\"italics\",[[\"i\"]]]]\n"))
(should (s-equals? (buffer-string) expected-string))
                                  )))
      )
    )

;; (ert-deftest parsing-headline-with-additional-text ()
;;   "Parses the headline item correctly"
;;     (with-temp-buffer
;;       (org-mode)
;;       (insert "* some headline\n  some text")
;;       (goto-char (point-min))
;;       (push-mark (point-max))
;;       (setq mark-active t)
;;       (export-as-notion-block (lambda ()
;;                                 (let ((expected-string "[[\"some headline\"]]\n[[\"some text\"]]\n"))
;; (should (s-equals? (buffer-string) expected-string))
;;                                   )))
;;       )
;;     )


;;; test.el ends here
