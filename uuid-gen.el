;;; uuid-gen.el --- Tools to generate UUIDs for various uses.

;; Copyright (C) 2014 Ben Lewis

;; Author: Ben Lewis <benjf5@gmail.com>
;; Created: 13 Oct 2014
;; Keywords: tools
;; Version: 0.1alpha

(random t)

(defun uuid-gen-rand-num (mask)
  "Strips the lower two bytes out of a randomly-generated value, and masks the result."
  (logand (lsh (random) -16) mask))

;; Eventually, support multiple types of UUIDs. (defconst uuid-class-4)

(defun uuid-create-nil ()
  "Returns the nil uuid, \"00000000-0000-0000-0000-000000000000\"."
  '(0 0 0 0 (0 0 0 0 0 0)))

(defun uuid-create-class-4 ()
  (list
   (uuid-gen-rand-num #xffffffff)
   (uuid-gen-rand-num #xffff)
   (logior (lsh (uuid-gen-rand-num #xffff) -4) #x4000)
   (logior (lsh (uuid-gen-rand-num #xffff) -4)
          (lsh (logior (uuid-gen-rand-num #x3) #x8) 12))
   (list
    (uuid-gen-rand-num #xff)
    (uuid-gen-rand-num #xff)
    (uuid-gen-rand-num #xff)
    (uuid-gen-rand-num #xff)
    (uuid-gen-rand-num #xff)
    (uuid-gen-rand-num #xff))))

(defun uuid-print-string (uuid)
  "Formats the supplied UUID as a string, complete with double quotes."
  (concat
   "\""
   (format "%08X" (car uuid))
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
   (format "0x%08X" (car uuid))
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
  "Generates a uuid (like '1F78D796-26FB-41C6-BCF3-E68AB900A627') and inserts it at the mark."
  (interactive)
  (insert
   (uuid-print-string (uuid-create-class-4))))

(provide 'uuid-gen)

;;; uuid-gen.el ends here
