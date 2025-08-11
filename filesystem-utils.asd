(asdf:defsystem filesystem-utils
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A collection of utilities for filesystem interaction."
  :homepage "https://shinmera.com/docs/filesystem-utils/"
  :bug-tracker "https://shinmera.com/project/filesystem-utils/issues"
  :source-control (:git "https://shinmera.com/project/filesystem-utils.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "documentation"))
  :depends-on (:trivial-features
               :pathname-utils
               :documentation-utils
               (:feature (:not :mezzano) :cffi)
               (:feature :sbcl :sb-posix))
  :in-order-to ((asdf:test-op (asdf:test-op :filesystem-utils-test))))
