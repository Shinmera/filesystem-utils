#|
 This file is a part of Colleen
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem filesystem-utils-test
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Tests for the filesystem-utils system."
  :homepage "https://Shinmera.github.io/filesystem-utils/"
  :bug-tracker "https://github.com/Shinmera/filesystem-utils/issues"
  :source-control (:git "https://github.com/Shinmera/filesystem-utils.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:filesystem-utils :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :filesystem-utils-test)))
