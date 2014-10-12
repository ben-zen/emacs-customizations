;; Copyright (C) 2014 Ben Lewis <benjf5@gmail.com>
;; Licensed under the MIT license; see included LICENSE file.

(require 'cc-mode)

;; define SAL faces
(defface sal-function-param-face
  '((t (:inherit font-lock-builtin-face))) ;; might not want builtin-face
  "Face for SAL function parameter annotations.")
(defvar sal-function-param-face 'sal-function-param-face)
;; Additional faces for other parameter types should be added later.

(defconst nonparametric-function-annotation-keywords
  (list
   '("\\<_\\(In\\(out\\)?\\(_opt\\)?\\(_z\\)?\\|Out\\(ptr\\(_opt\\)?\\(_result\\(\\(\\(_maybenull\\|_nullonfailure\\)\\(_z\\)?\\)\\|_z\\)\\)?\\|\\(_opt\\)?\\|ref\\(_result\\(_maybenull\\|_nullonfailure\\)\\)\\)?\\|Ret\\(_null\\|_notnull\\|_z\\|_maybenull\\(_z\\)?\\)\\)_\\>"
     0 sal-function-param-face))
  ;; Capturing how SAL is represented in a regex is hard.
  "Keywords for annotating function parameters.")

(defun sal-mode-add-font-locks ()
  (font-lock-add-keywords
   nil
   nonparametric-function-annotation-keywords t))

(define-minor-mode sal-mode
  "Support Microsoft SAL2 in C++-mode."
  :lighter " SAL"
  (sal-mode-add-font-locks))

(provide 'sal-mode)

