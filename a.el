;;; a.el --- Utility functions for dealing with alists

;; Copyright © 2013  David Leatherman

;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/alist
;; Version: 0.1.0
;; Package-Requires: ((dash "1.2.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; a.el is meant to provide a useful set of utility functions for
;; alists in emacs.  Initially inspired by the frustration of typing
;; (cdr (assoc 'key alist)) everytime I simply wanted a-get.  Alists
;; aren't particularly fast, especially at larger sizes, so use them
;; and this library where it makes sense.

;; History

;; 0.1.0 - Initial alpha

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(defun a--assert-even-list (lst)
  "Internal utility function to ensure that LST is comprised of an
even number of elements"
  (if (evenp (length lst))
      t
    (error "a-put requires an equal number of keys to values")))

(defun a-into (alist list-of-lists)
  "Converts a list of lists into an alist"
  (when (a--assert-even-list alist)
    (-reduce-from (lambda (alst kv)
                    (cons
                     ;; make into a dotted pair
                     (cons (car kv) (cadr kv))
                     (a-remove alst (car kv))))
                  alist
                  list-of-lists)))

(defun a-remove (alist key)
  "Returns a copy of ALIST with the element associates with KEY
removed"
  (remove (assoc key alist) alist))

(defun a-put (alist &rest kv-pairs)
  "Returns a copy of ALIST with KEY set to VALUE"
  (when (a--assert-even-list kv-pairs)
      (a-into alist kv-pairs)))

(defun a-get (alist key &optional not-found)
  "Return the value associated with KEY in the ALIST or nil if the key
is not found.  Optionally return NOT-FOUND rather than nil when the
key is not present."
  (or (cdr (assoc key alist)) not-found))

(defun a-get-in (alist key-list &optional not-found)
  "Return the value in a nested alist structure.  Returns nil or the
not-found value if the key is not present."
  (or (-reduce-from 'a-get alist key-list) not-found))

(defun a-keys (alist)
  "Return the list of keys in ALIST"
  (mapcar 'car alist))

(defun a-vals (alist)
  "Return the list of values in ALIST"
  (mapcar 'cdr alist))

(defun a-merge (&rest alists)
  "Merge the elements of the passed alists.  Works from left to right,
so elements in alists appearing later in the arguments will override
elements with the same keys that appear earlier."
  (-reduce
   (lambda (alist1 alist2)
     (-reduce-from
      (lambda (alst elt) (a-put alst (car elt) (cdr elt)))
      alist1
      alist2))
   alists))

;; TODO?
;; merge-with
;; maybe a destructuring let
;; mapkeys and mapvals

(provide 'a)

;; a.el ends here
