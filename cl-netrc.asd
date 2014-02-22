(asdf:defsystem #:cl-netrc
  :serial t
  :description "Read netrc files"
  :author "Jimmy Lu <gongchuo.lu@gmail.com>"
  :depends-on (#:osicat
               #:alexandria
               #:split-sequence
               #:optima)
  :components ((:file "package")
               (:file "cl-netrc")))
