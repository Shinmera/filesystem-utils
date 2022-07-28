#|
 This file is a part of Filesystem-Utils
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem filesystem-utils
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A collection of utilities for filesystem interaction."
  :homepage "https://Shinmera.github.io/filesystem-utils/"
  :bug-tracker "https://github.com/Shinmera/filesystem-utils/issues"
  :source-control (:git "https://github.com/Shinmera/filesystem-utils.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "documentation"))
  :depends-on (:trivial-features
               :documentation-utils
               (:feature :sbcl :sb-posix))
  :in-order-to ((asdf:test-op (asdf:test-op :filesystem-utils-test))))
