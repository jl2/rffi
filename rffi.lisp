;; rffi.lisp
;; Copyright (c) 2024 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :rffi)

(defclass crate-version ()
  ())

(defclass rust-function ()
  ())

(defclass rust-datatype ()
  ())


(defclass crate ()
  ((descriptor :type string :initarg :crate-descriptor)
   (version :type crate-version)
   (path :type path))
  (:documentation "Metadata about a crate."))

(defun find-module (descriptor)
  ;; TODO: Use cargo to find the crate metadata
  ;; TODO: This isn't even (necessarily) a crate now...
  (make-instance 'crate
                 :crate-descriptor descriptor)
  )

(defmacro use (descriptor)
  (let ((crate (find-module descriptor)))
    ;; TODO: Use the crate to generate a package...
    crate))
