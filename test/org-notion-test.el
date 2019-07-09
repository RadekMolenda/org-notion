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
    (should (= id "d8a60223-5a72-44c4-b075-145ecb173e05")))
  (let ((id (org-notion--uuid "d8a602235a7244c4b075145ecb173e05")))
    (should (= id "d8a60223-5a72-44c4-b075-145ecb173e05")))
  (let ((id (org-notion--uuid "some-garbage")))
    (should (= id ""))))

;;; test.el ends here
