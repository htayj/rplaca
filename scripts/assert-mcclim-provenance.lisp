(in-package :cl-user)

(require :asdf)

(defun assert-guix-mcclim-provenance ()
  "Fail unless the CLIM editor stack resolves from the active Guix profile."
  (let* ((environment (uiop:getenv "GUIX_ENVIRONMENT"))
         (profile (and environment
                       (uiop:ensure-directory-pathname environment))))
    (unless profile
      (error "Missing GUIX_ENVIRONMENT while checking McCLIM provenance."))
    (let ((expected-root
            (truename
             (merge-pathnames #P"share/common-lisp/source/cl-mcclim/"
                              profile))))
      (dolist (name '(:clim-core :clim :mcclim :mcclim-clx
                      :esa-mcclim :drei-mcclim :clim-debugger))
        (let* ((system (asdf:find-system name))
               (source (truename (asdf:system-source-file system))))
          (unless (uiop:subpathp source expected-root)
            (error "ASDF system ~S resolved outside pinned McCLIM tree: ~A"
                   name source))
          (format *trace-output* "[rplaca-env] ~A => ~A~%"
                  name source))))))

(assert-guix-mcclim-provenance)
