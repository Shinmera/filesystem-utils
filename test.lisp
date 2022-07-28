#|
 This file is a part of Filesystem-Utils
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:filesystem-utils-test
  (:nicknames #:org.shirakumo.filesystem-utils.test)
  (:use #:cl #:parachute)
  (:local-nicknames (#:fs #:org.shirakumo.filesystem-utils)))
(in-package #:org.shirakumo.filesystem-utils.test)

(define-test filesystem-utils)

