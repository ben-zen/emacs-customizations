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
  "Used for generating a class 3 or 5 UUID where the name string is an X.500 DN
in DER or text format.")

(defun uuid-generate-name-byte-string (name)
  "Generate a sequence of bytes in network byte order representing the
characters in the name provided, and has a potential to stack-overflow on very
long names. Potential improvements may be necessary; currently also only a
unibyte function."
  (let ((name-list (string-to-list name))
	(char-merge (lambda (char-list)
		      (if (null char-list)
			  ""
			(concat
			 (byte-to-string (car char-list))
			 (funcall char-merge (cdr char-list)))))))
    (funcall char-merge name-list)))

(defun uuid-generate-uuid-byte-string (uuid)
  "Produce a sequence of bytes in a unibyte string that contains the same value
as the uuid provided, in network byte order."
  (let ((first-stanza (car uuid))
	(second-stanza (car (cdr uuid)))
	(third-stanza (car (cddr uuid)))
	(fourth-stanza (car (cdr (cddr uuid))))
	(uuid-node (car (cddr (cddr uuid)))))
    (concat
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
	  (t (error "Hash function not supported.")))))

(defun uuid-format-string-as-node-name (chars)
  "Formats a list of 12 characters as a list of six bytes."
  (let ((char-l (string-to-list chars))
        (format-nodes (lambda (char-list)
                        (if (null char-list)
                            nil
                          (cons
                           (logior
                            (lsh (string-to-number (string (car char-list)) 16) 4)
                            (string-to-number (string (cadr char-list)) 16))
                           (funcall format-nodes (cddr char-list)))))))
    (funcall format-nodes char-l)))

(defun uuid-format-string-as-short (chars)
  "Formats a list of four characters as a 16-bit number."
  (logior (lsh (string-to-number (substring chars 0 1) 16) 12)
          (lsh (string-to-number (substring chars 1 2) 16) 8)
          (lsh (string-to-number (substring chars 2 3) 16) 4)
          (string-to-number (substring chars 3) 16)))

(defun uuid-format-string-as-long (chars)
  "Formats a list of eight characters as a 32-bit number; on systems that do not
support 32-bit numbers, formats the list as two 16-bit numbers (first is high
bits, second is low bits.)"
  (if (>= uuid-bitness 56)
      (logior (lsh (uuid-format-string-as-short (substring chars 0 4)) 16)
              (uuid-format-string-as-short (substring chars 4))))
    (list (uuid-format-string-as-short (substring chars 0 4))
          (uuid-format-string-as-short (substring chars 4)))))

(defun uuid-format-hash-as-uuid (hash hash-alg)
  "Takes the first 128 bytes of the provided hash (as a unibyte string) and
constructs a UUID from them; hash-alg may be either 'md5 or 'sha1."
  (let ((first-stanza-characters (substring hash 0 8))
        (second-stanza-characters (substring hash 8 12))
        (third-stanza-characters (substring hash 12 16))
        (fourth-stanza-characters (substring hash 16 20))
        (node-name-characters (substring hash 20 32)))
    (list
     (uuid-format-string-as-long first-stanza-characters)
     (uuid-format-string-as-short second-stanza-characters)
     (logior
      (logand (uuid-format-string-as-short third-stanza-characters)
              #x0FFF)
      (cond ((eq hash-alg 'md5) #x3000)
            ((eq hash-alg 'sha1) #x5000)
            (t
             (error
              "This function can only generate SHA-1 or MD5-based UUIDs."))))
      (logior (logand (uuid-format-string-as-short fourth-stanza-characters)
                      #x3FFF)
              #x8000)
          (uuid-format-string-as-node-name node-name-characters))))

(defun uuid-gen-rand-num (mask)
  "Strips the lower two bytes out of a randomly-generated value, and masks the
result."
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

(defun uuid-create-class-3 (ns-uuid name)
  "Generates a class 3 UUID (MD5 hash) with the supplied namespace and name."
  (uuid-format-hash-as-uuid (uuid-generate-hash ns-uuid name 'md5) 'md5))

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

(defun uuid-create-class-5 (ns-uuid name)
  "Generates a class 5 UUID (SHA-1 hash) with the supplied namespace and name."
  (uuid-format-hash-as-uuid (uuid-generate-hash ns-uuid name 'sha1) 'sha1))

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
   (format "%04X" (car (cddr uuid)))
   "-"
   (format "%04X" (cadr (cddr uuid)))
   "-"
   (let
       ((uuid-chars (car (cddr (cddr uuid))))
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
   (format "0x%04X" (car (cddr uuid)))
   ", "
   "{ "
   (let ((third-short (cadr (cddr uuid))))
     (format "0x%02X, 0x%02X"
             (lsh third-short -8) (logand third-short #xff)))
   ", "
   (let ((uuid-chars (car (cddr (cddr uuid))))
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
  "Generates a Class 4 UUID (like \"1F78D796-26FB-41C6-BCF3-E68AB900A627\") and
inserts it at the mark."
  (interactive)
  (insert
   (uuid-print-string (uuid-create-class-4))))

(defun insert-win32-uuid ()
  "Generates a Class 4 UUID and inserts it in Win32 struct format at the point."
  (interactive)
  (insert (uuid-print-win32-struct (uuid-create-class-4))))

(provide 'uuid-gen)

;;; uuid-gen.el ends here
