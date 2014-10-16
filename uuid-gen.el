;;; uuid-gen.el --- Tools to generate UUIDs for various uses.

;; Copyright (C) 2014 Ben Lewis

;; Author: Ben Lewis <benjf5@gmail.com>
;; Created: 13 Oct 2014
;; Keywords: tools

(random t)

(defun uuid-gen-rand-num (mask)
  "Strips the lower two bytes out of a randomly-generated value."
  (logand (lsh (random) -16) mask))

(defconst uuid-class-4
  )


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
    (uuid-gen-rand-num #xff)
    (uuid-gen-rand-num #xff)
    (uuid-gen-rand-num #xff))))

(defun uuid-generate-string (uuid)
  (concat
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
     (funcall char-merge uuid-chars ""))))

(defun insert-uuid ()
  "Generates a uuid (like '1F78D796-26FB-41C6-BCF3-E68AB900A627') and inserts it at the mark."
  (interactive)
  (insert
   (uuid-generate-string (uuid-create-class-4))))

(provide 'uuid-gen)

;;; uuid-gen.el ends here
