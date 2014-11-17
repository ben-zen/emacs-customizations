;;; uuid-gen.el --- Tools to generate UUIDs for various uses.

;; Copyright (C) 2014 Ben Lewis
;; Licensed under the MIT license; see included LICENSE file.

;; Author: Ben Lewis <benjf5@gmail.com>
;; Created: 13 Oct 2014
;; Keywords: tools
;; Version: 0.1alpha3

(random t)

(if (null (integer-or-marker-p #xffffff))
    (error "This package requires an emacs installation with integers larger than 24 bits."))

(defconst uuid-classes '(class-4 class-5)
  "Classes of UUID supported by this library that can be generated.")

(defconst uuid-bitness (+ (ceiling (log most-positive-fixnum 2)) 1)
  "Checks the maximum integer size available in a live emacs instance, determining how UUIDs are stored internally.")

(defconst uuid-namespace-dns
  '((#x6ba7 #xb810)
    #x9dad
    #x11d1
    #x80b4
    (#x00 #xc0 #x4f #xd4 #x30 #xc8))
  "Used for generating a class 3 or 5 UUID where the name string is a fully-qualified domain name.")

(defconst uuid-namespace-url
  '((#x6ba7 #xb811)
    #x9dad
    #x11d1
    #x80b4
    (#x00 #xc0 #x4f #xd4 #x30 #xc8))
  "Used for generating a class 3 or 5 UUID where the name string is a URL.")

(defconst uuid-namespace-iso-oid
  '((#x6ba7 #xb812)
    #x9dad
    #x11d1
    #x80b4
    (#x00 #xc0 #x4f #xd4 #x30 #xc8))
  "Used for generating a class 3 or 5 UUID where the name string is an ISO OID.")

(defconst uuid-namespace-x500
  '((#x6ba7 #xb814)
    #x9dad
    #x11d1
    #x80b4
    (#x00 #xc0 #x4f #xd4 #x30 #xc8))
  "Used for generating a class 3 or 5 UUID where the name string is an X.500 DN (in DER or text format).")

(defun uuid-generate-name-byte-string (name)
  "Generate a sequence of bytes in network byte order representing the characters in the name provided, and has a
potential to stack-overflow on very long names. Potential improvements may be necessary; currently also only a unibyte
function."
  (let ((name-list (string-to-list name))
	(char-merge (lambda (char-list)
		      (if (null char-list)
			  ""
			(concat
			 (byte-to-string (car char-list))
			 (funcall char-merge (cdr char-list)))))))
    (funcall char-merge name-list)))

(defun uuid-generate-uuid-byte-string (uuid)
  "Produce a sequence of bytes in a unibyte string that contains the same value as the uuid provided, in network byte
order."
  (concat
   (let ((first-stanza (car uuid))
	 (second-stanza (car (cdr uuid)))
	 (third-stanza (car (cddr uuid)))
	 (fourth-stanza (car (cdr (cddr uuid))))
	 (uuid-node (car (cddr (cddr uuid)))))
     (if (consp first-stanza)
         (concat (byte-to-string (lsh (cadr first-stanza) -8))
                 (byte-to-string (logand (cadr first-stanza) #xff))
                 (byte-to-string (lsh (car first-stanza) -8))
                 (byte-to-string (logand (car first-stanza) #xff)))
       (concat (byte-to-string (lsh first-stanza -24))
               (byte-to-string (logand (lsh first-stanza -16) #xff))
               (byte-to-string (logand (lsh first-stanza -8) #xff))
               (byte-to-string (logand first-stanza #xff))))
   (byte-to-string (lsh second-stanza -8))
   (byte-to-string (logand second-stanza #xff))
   (byte-to-string (lsh third-stanza -8))
   (byte-to-string (logand third-stanza #xff))
   (byte-to-string (lsh fourth-stanza -8))
   (byte-to-string (logand fourth-stanza #xff))
   (let ((char-merge (lambda (char-list)
                       (if (null char-list)
                           ""
                         (concat
                          (byte-to-string (car char-list))
                          (funcall char-merge (cdr char-list)))))))
     (funcall char-merge uuid-node)))))

(defun uuid-generate-hash (ns-uuid name hash-function)
  "Generates a hash using the supplied hash function (accepts symbols 'md5, 'sha1.)"
  (let ((hash-data (concat (uuid-generate-uuid-byte-string ns-uuid)
			   (uuid-generate-name-byte-string name))))
    (cond ((eq 'md5 hash-function) (md5 hash-data))
	  ((eq 'sha1 hash-function) (sha1 hash-data))
	  (t ""))))

(defun uuid-gen-rand-num (mask)
  "Strips the lower two bytes out of a randomly-generated value, and masks the result."
  (logand (lsh (random) -16) mask))

(defun uuid-gen-rand-word ()
  "Generate a random word (16-bit integer value)."
  (if (>= uuid-bitness 56)
      (uuid-gen-rand-num #xffff)
    (logior (lsh (uuid-gen-rand-num #xff) 8)
            (uuid-gen-rand-num #xff))))

(defun uuid-gen-rand-dword ()
  "Generate a random double-word (32-bit value).
On systems with integer sizes smaller than 56 bits, return a list of two words.
This leaves a large gap in-between the usual 64- and 32-bit implementations, but
caution is preferable over undesirable consequences."
  (if (>= uuid-bitness 56)
      (uuid-gen-rand-num #xffffffff)
    (list
     (uuid-gen-rand-word)
     (uuid-gen-rand-word))))

(defun uuid-create-nil ()
  "Returns the nil uuid, \"00000000-0000-0000-0000-000000000000\"."
  (if (>= uuid-bitness 56)
      '(0 0 0 0 (0 0 0 0 0 0))
    '((0 0) 0 0 0 (0 0 0 0 0 0))))

(defun uuid-create-class-4 ()
  "Generates a class 4 UUID; all stanzas are random except for the upper nybble
of the third stanza and the upper two bits of the fourth stanza."
  (list
   (uuid-gen-rand-dword)
   (uuid-gen-rand-word)
   (logior (lsh (uuid-gen-rand-word) -4) #x4000)
   ;; Mark the third stanza with the UUID version number
   (logior (lsh (uuid-gen-rand-word) -2) #x8000)
   ;; Make sure the fourth stanza's top two bits are #b10.
   (list
    (uuid-gen-rand-num #xff)
    (uuid-gen-rand-num #xff)
    (uuid-gen-rand-num #xff)
    (uuid-gen-rand-num #xff)
    (uuid-gen-rand-num #xff)
    (uuid-gen-rand-num #xff))))

(defun uuid-print-string (uuid)
  "Formats the supplied UUID as a string, complete with enclosing double quotes."
  (concat
   "\""
   (let ((first-stanza (car uuid)))
     (if (consp first-stanza)
         (format "%04X%04X" (car first-stanza) (cadr first-stanza))
       (format "%08X" first-stanza)))
   "-"
   (format "%04X" (cadr uuid))
   "-"
   (format "%04X" (caddr uuid))
   "-"
   (format "%04X" (cadddr uuid))
   "-"
   (let
       ((uuid-chars (car (cddddr uuid)))
        (char-merge (lambda (char-list string)
                      (if (null char-list)
                          string
                        (concat (format "%02X" (car char-list))
                                (funcall char-merge (cdr char-list) string))))))
     (funcall char-merge uuid-chars ""))
   "\""))

(defun uuid-print-win32-struct (uuid)
  "Formats the supplied UUID as a Win32 GUID struct, i.e. { DWORD, WORD, WORD, CHAR[8] }."
  (concat
   "{ "
   (let ((first-stanza (car uuid)))
     (if (consp first-stanza)
         (format "0x%04X%04X" (car first-stanza) (cadr first-stanza))
       (format "0x%08X" first-stanza)))
   ", "
   (format "0x%04X" (cadr uuid))
   ", "
   (format "0x%04X" (caddr uuid))
   ", "
   "{ "
   (let ((third-short (cadddr uuid)))
     (format "0x%02X, 0x%02X"
             (lsh third-short -8) (logand third-short #xff)))
   ", "
   (let ((uuid-chars (car (cddddr uuid)))
         (char-print (lambda (char-list)
                       (if (null char-list)
                           ""
                         (concat (format "0x%02X%s"
                                         (car char-list)
                                         (if (null (cdr char-list))
                                             ""
                                           ", "))
                                 (funcall char-print (cdr char-list)))))))
     (funcall char-print uuid-chars))
   " } }"))

(defun insert-uuid ()
  "Generates a Class 4 UUID (like \"1F78D796-26FB-41C6-BCF3-E68AB900A627\") and inserts it at the mark."
  (interactive)
  (insert
   (uuid-print-string (uuid-create-class-4))))

(defun insert-win32-uuid ()
  "Generates a Class 4 UUID and inserts it in Win32 struct format at the point."
  (interactive)
  (insert (uuid-print-win32-struct (uuid-create-class-4))))

(provide 'uuid-gen)

;;; uuid-gen.el ends here
