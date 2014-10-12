;; Copyright (C) 2014 Ben Lewis <benjf5@gmail.com>
;; Licensed under the MIT license; see included LICENSE file.

(require 'cc-defs)
(require 'cc-fonts)

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

;; Here we start adding new cc-mode-based stuff to handle the fontifying
(c-lang-defconst sal-nonparametric-function-param-annotation-kwds
  c++ ("_In_" "_Inout_" "_In_opt_" "_Inout_opt_" "_In_z_" "_Inout_z_"
       "_In_opt_z_" "_Inout_opt_z_" "_Out_" "_Out_opt_" "_Outptr_"
       "_Outptr_opt_" "_Outptr_opt_result_z_" "_Outptr_opt_result_maybenull_"
       "_Outptr_opt_result_nullonfailure_" "_Outptr_opt_result_maybenull_z_"
       "_Outptr_opt_result_nullonfailure_z_" "_Outref_result_maybenull_"
       "_Outref_result_nullonfailure_" "_Ret_null_" "_Ret_notnull_" "_Ret_z_"
       "_Ret_maybenull_z_"))


;;(c-lang-defconst sal-nonparametric-matchers
;;  c++ `(,@(when (c-lang-const sal-nonparametric-function-param-annotation-kwds)
            ;; this will need to eventually be extended to handle more complex
            ;; cases than just 
                  

(defun sal-mode-add-font-locks ()
  (font-lock-add-keywords
   nil
   nonparametric-function-annotation-keywords t))

(define-minor-mode sal-mode
  "Support Microsoft SAL2 in C++-mode."
  :lighter " SAL"
  (sal-mode-add-font-locks))

(provide 'sal-mode)

