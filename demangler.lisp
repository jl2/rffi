;; demangler.lisp
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

;; This may, eventually, demangle Rust names to determine type information.
;; *OR* I may find a better way to get type information.

(defun demangle-identifier (identifier-string)
  nil)

;; (defstruct (rust-symbol-parser (:conc-name rsp-))
;;   "State used while parsing a mangled Rust symbol name into a symbolic representation."
;;   (text "" :type string)
;;   (cur-pos 0 :type fixnum))

;; (defmethod print-object ((object rust-symbol-parser) stream)
;;   "Try to pretty print an sgf-parser"
;;   (with-slots (cur-pos text) object
;;     (declare (type fixnum cur-pos)
;;              (type string text)
;;              (type stream stream))

;;     (format stream "(sgf-parser: pos: ~a cur-char: ~c"
;;             cur-pos
;;             (current-char object))))

;; (defun create-parser (my-string)
;;   "Create a Rust symbol parser for a string."
;;   (make-rust-symbol-parser :text my-string))

;; (defun finished-p (parser)
;;   "t if the parser has read the whole string.  nil otherwise"
;;   (with-slots (text cur-pos) parser
;;     (declare (type fixnum cur-pos)
;;              (type string text))
;;     (>= cur-pos (length text))))
;; (defun whitespace-p (char)
;;   "Return t if a character is a whitespace, nil otherwise."
;;   (declare (type character char))
;;   #+sbcl (sb-unicode:whitespace-p char)
;;   #-sbcl (cl-unicode:has-property char "whitespace"))

;; (defun skip-whitespace (parser)
;;   "Advance to the next non-whitespace character."
;;   (with-slots (text cur-pos) parser
;;     (loop
;;       :while (and
;;               (not (finished-p parser))
;;               (whitespace-p (aref text cur-pos)))
;;       :do (incf cur-pos))
;;     parser))

;; (defun current-char (parser)
;;   "Character currently being read."
;;   (with-slots (text cur-pos) parser
;;     (declare (type fixnum cur-pos)
;;              (type simple-string text))
;;     (cond ((finished-p parser)
;;            nil)
;;           (t
;;            (aref text cur-pos)))))

;; (defun read-until (parser char)
;;   "Return a string from cur-pos until the first instance of char, and advance the parser."
;;   (with-slots (text cur-pos) parser
;;     (declare (type fixnum cur-pos)
;;              (type string text)
;;              (type character char))
;;     (let* ((idx (position char text :start cur-pos))
;;            (str (subseq text
;;                         cur-pos
;;                         idx)))
;;       (setf cur-pos idx)
;;       (skip-whitespace parser)
;;       str)))


;; (defun match (parser char)
;;   "Validate that the character at cur-pos matches char and advance the parser."
;;   (with-slots (cur-pos text) parser

;;     (when  (char/= (current-char parser) char)
;;       (error "Expected ~c but found ~c" (current-char parser) char))
;;     (incf (rsp-cur-pos parser))
;;     (skip-whitespace parser)
;;     (current-char parser)))

;; (defun decode-symbol (symbol-string)
;;   (let ((parser (create-parser symbol-string)))
;;     (match parser #\_)
;;     (match parser #\R)
;;     (case (current-char parser)
;;       (#\N
       
;;        (match parser #\;)
;;        (loop :for property = (read-sgf-property parser)
;;              :while property
;;              :collecting property)))))
