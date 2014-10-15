;; Copyright (C) 2014 Ben Lewis

(random t)

(defun gen-rand-num (mask)
  "Strips the lower two bytes out of a randomly-generated value."
  (logand (lsh (random) -16) mask))

(defun insert-uuid ()
  "Generates a uuid (like '1F78D796-26FB-41C6-BCF3-E68AB900A627') and inserts it at the mark."
  (interactive)
  (insert
   (format "%08X-%04X-%04X-%04X-%06X%06X"
           (gen-rand-num #xffffffff)
           (gen-rand-num #xffff)
           (gen-rand-num #xffff)
           (gen-rand-num #xffff)
           (gen-rand-num #xffffff)
           (gen-rand-num #xffffff))))
