(in-package #:cl)

(defpackage #:cl-netrc
  (:documentation "Read netrc files")
  (:nicknames #:netrc)
  (:use #:cl
        #:alexandria
        #:split-sequence)
  (:export #:make-netrc
           #:lookup))
