(in-package #:cl-netrc)

(defparameter *default-paths* '("~/.netrc" "~/.netrc.gpg"))

(defclass netrc ()
  ((path :reader path
         :initarg :path
         :initform (error "Must supply a path to the netrc file"))
   (entries :accessor entries
            :initform nil)))

(defun make-netrc (&optional path)
  (unless path
    (setf path (loop for p in *default-paths* thereis (probe-file p))))
  (make-instance 'netrc :path path))

(defun check-permissions (path)
  (let ((right-perm '(:user-read :user-write))
        (perm (osicat:file-permissions path)))
    (unless (equal right-perm perm)
      (error "Permission for ~s should be ~s, but are ~s"
             path right-perm perm))))

(defun decode-file (path-string)
  (let ((process (run-program "gpg" (list "--batch"
                                          "--quiet"
                                          "--decrypt" path-string)
                              :output :stream :search t)))
    (unless (zerop (process-exit-code process))
      (error "Failed to decrypt ~s" path-string))
    (process-output process)))

(defun tokenize (input)
  (loop for line = (read-line input nil)
        while line
        for content = (first (split-sequence #\# line :count 1))
        nconc (split-sequence-if (rcurry #'find #(#\Space #\Tab))
                                 content
                                 :remove-empty-subseqs t)))

(defun parse (tokens)
  (labels ((parse (tokens entries)
             (if tokens
                 (optima:cmatch (first tokens)
                   ("machine"
                    (parse (cddr tokens)
                           (cons (list (second tokens)) entries)))
                   ("default"
                    (parse (rest tokens)
                           (cons '(:default) entries)))
                   ((or "login" "password" "account" "port")
                    (nconcf (first entries)
                            (list (make-keyword (string-upcase (first tokens)))
                                  (second tokens)))
                    (parse (cddr tokens) entries)))
                 entries)))
    (nreverse (parse tokens nil))))

(defmethod initialize-instance :after ((netrc netrc) &key)
  (with-slots (path entries) netrc
    (when-let ((truename (probe-file path)))
      (let ((path-string (namestring truename)))
        (check-permissions path-string)
        (with-open-stream (in (if (ends-with-subseq ".gpg" path-string)
                                  (decode-file path-string)
                                  (open path-string)))
          (setf entries (parse (tokenize in))))))))

(defun netrc-get (netrc machine)
  (rest (assoc machine (entries netrc) :test #'string=)))
