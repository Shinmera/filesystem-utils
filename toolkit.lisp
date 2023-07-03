(in-package #:org.shirakumo.filesystem-utils)

;;; Support
#+(and sbcl win32)
(defun string->wstring (string)
  (let* ((count (sb-alien:alien-funcall (sb-alien:extern-alien "MultiByteToWideChar" (function sb-alien:int sb-alien:int (integer 32) sb-alien:c-string sb-alien:int sb-alien:int sb-alien:int))
                                        65001 0 string -1 0 0))
         (ptr (sb-alien:make-alien sb-alien:short count)))
    (sb-alien:alien-funcall (sb-alien:extern-alien "MultiByteToWideChar" (function sb-alien:int sb-alien:int (integer 32) sb-alien:c-string sb-alien:int (* T) sb-alien:int))
                            65001 0 string -1 ptr count)
    ptr))

#+(and sbcl win32)
(defun wstring->string (pointer)
  (let* ((count (sb-alien:alien-funcall (sb-alien:extern-alien "WideCharToMultiByte" (function sb-alien:int sb-alien:int (integer 32) (* T) sb-alien:int sb-alien:int sb-alien:int sb-alien:int sb-alien:int))
                                        65001 0 pointer -1 0 0 0 0))
         (string (sb-alien:make-alien sb-alien:char count)))
    (sb-alien:alien-funcall (sb-alien:extern-alien "WideCharToMultiByte" (function sb-alien:int sb-alien:int (integer 32) (* T) sb-alien:int (* T) sb-alien:int sb-alien:int sb-alien:int))
                            65001 0 pointer -1 string count 0 0)
    (prog1 (sb-alien:cast string sb-alien:c-string)
      (sb-alien:free-alien string))))

(defun runtime-directory ()
  (pathname-utils:to-directory
   (pathname-utils:parse-native-namestring
    #+abcl (first ext:*command-line-argument-list*)
    #+allegro (first (sys:command-line-arguments))
    #+(or clasp ecl) (si:argv 0)
    #+clisp (first (coerce (ext:argv) 'list))
    #+clozure (first ccl:*command-line-argument-list*)
    #+(or cmucl scl) (first extensions:*command-line-strings*)
    #+gcl (first si:*command-args*)
    #+lispworks (first sys:*line-arguments-list*)
    #+mkcl (mkcl:argv 0)
    #+sbcl (first sb-ext:*posix-argv*)
    #+xcl (first system:*argv*)
    #-(or abcl allegro clasp ecl clisp clozure cmucl scl gcl lispworks mkcl sbcl xcl)
    "")))

(defun temporary-directory ()
  (pathname-utils:parse-native-namestring
   (or #+windows (pathname-utils::getenv "TEMP")
       #+darwin (pathname-utils::getenv "TMPDIR")
       #+linux (pathname-utils::getenv "XDG_RUNTIME_DIR")
       #+windows "~/AppData/Local/Temp"
       #-windows "/tmp")
   :as :directory))

(defun make-temporary-file (&key (name NIL name-p) (type "dat"))
  (if name-p
      (make-pathname :name name :type type :defaults (temporary-directory))
      (loop for path = (make-pathname :name (format NIL "~36r-~36r" (get-universal-time) (random #xFFFFFFFFFFFFFFFF))
                                      :type type :defaults (temporary-directory))
            do (unless (file-exists-p path) (return path)))))

(defun call-with-temporary-file (function &rest args)
  (let ((path (apply #'make-temporary-file args)))
    (ensure-directories-exist path)
    (unwind-protect (funcall function path)
      (ensure-deleted path))))

(defmacro with-temporary-file ((path &rest args) &body body)
  `(call-with-temporary-file (lambda (,path) ,@body) ,@args))

(defun current-directory ()
  #+(or abcl genera xcl) (truename *default-pathname-defaults*)
  #+allegro (excl::current-directory)
  #+clisp (ext:default-directory)
  #+clozure (ccl:current-directory)
  #+(or cmucl scl) (pathname-utils:parse-unix-namestring (nth-value 1 (unix:unix-current-directory)) :as :directory)
  #+cormanlisp (pathname (pl::get-current-directory))
  #+(or clasp ecl) (ext:getcwd)
  #+gcl (let ((*default-pathname-defaults* #p"")) (truename #p""))
  #+lispworks (hcl:get-working-directory)
  #+mkcl (mk-ext:getcwd)
  #+sbcl (sb-ext:parse-native-namestring (sb-unix:posix-getcwd/))
  #+xcl (extensions:current-directory)
  #-(or abcl genera xcl allegro clisp clozure cmucl scl cormanlisp clasp ecl gcl lispworks mkcl sbcl xcl)
  (truename *default-pathname-defaults*))

(defun (setf current-directory) (new)
  (let ((pathname (pathname new)))
    #+(or abcl genera xcl) (setf *default-pathname-defaults* (truename pathname))
    #+allegro (excl:chdir pathname)
    #+clisp (ext:cd pathname)
    #+clozure (setf (ccl:current-directory) pathname)
    #+(or cmucl scl) (unix:unix-chdir (ext:unix-namestring pathname))
    #+cormanlisp (unless (zerop (win32::_chdir (namestring pathname)))
                   (error "Could not set current directory to ~A" x))
    #+(or clasp ecl) (ext:chdir pathname)
    #+gcl (system:chdir pathname)
    #+lispworks (hcl:change-directory pathname)
    #+mkcl (mk-ext:chdir pathname)
    #+sbcl (sb-posix:chdir (sb-ext:native-namestring pathname))
    #-(or abcl allegro clasp clisp clozure cmucl cormanlisp ecl gcl genera lispworks mkcl sbcl scl xcl)
    (setf *default-pathname-defaults* pathname)
    pathname))

(defun call-with-current-directory (function directory)
  (let ((current (current-directory)))
    (if (pathname-utils:pathname-equal current directory)
        (funcall function)
        (progn
          (setf (current-directory) directory)
          (unwind-protect (funcall function)
            (setf (current-directory) current))))))

(defmacro with-current-directory ((directory) &body body)
  `(call-with-current-directory (lambda () ,@body) ,directory))

(defun ensure-deleted (pathname)
  (when (file-exists-p pathname)
    (delete-file* pathname)))

(defun truename* (pathname)
  (let ((pathname (pathname-utils:pathname* pathname)))
    (or (ignore-errors (truename pathname))
        ;; this is here because trying to find the truename of a directory pathname WITHOUT supplying
        ;; a trailing directory separator, causes an error on some lisps.
        #+(or clisp gcl) (ignore-errors (truename (pathname-utils:force-directory pathname))))))

(defun file-exists-p (pathname)
  (ignore-errors
   (let ((pathname (pathname-utils:to-physical pathname)))
     #+allegro (probe-file pathname :follow-symlinks T)
     #+gcl (truename* pathname)
     #+clisp (ext:probe-pathname pathname)
     #-(or allegro clisp gcl)
     (probe-file pathname))))

(defun directory* (directory &rest args &key &allow-other-keys)
  #+allegro (apply #'directory directory :directories-are-files NIL :follow-symbolic-links NIL args)
  #+(or clozure digitool) (apply #'directory directory :follow-links NIL args)
  #+clisp (apply #'directory directory :circle T :if-does-not-exist :ignore args)
  #+(or cmucl scl) (apply #'directory directory :follow-links NIL :truenamep NIL args)
  #+lispworks (apply #'directory directory :link-transparency NIL args)
  #+sbcl (apply #'directory directory :resolve-symlinks NIL args)
  #-(or allegro clozure digitool clisp cmucl scl lispworks sbcl)
  (apply #'directory directory args))

(defun list-contents (directory)
  ;; FIXME: This sucks
  (nconc (list-files directory)
         (list-directories directory)))

(defun list-files (directory)
  (let* ((directory (pathname-utils:pathname* directory))
         (entries (ignore-errors (directory* (merge-pathnames pathname-utils:*wild-file* directory)))))
    (remove-if #'directory-p entries)))

(defun list-directories (directory)
  (let* ((directory (pathname-utils:to-directory directory)))
    (let* (#-(or abcl cormanlisp genera xcl)
           (wild (merge-pathnames
                  #-(or abcl allegro cmucl lispworks sbcl scl xcl)
                  pathname-utils:*wild-directory*
                  #+(or abcl allegro cmucl lispworks sbcl scl xcl) "*.*"
                  directory))
           (dirs
             #+(or abcl xcl) (system:list-directory directory)
             #+cormanlisp (cl::directory-subdirs directory)
             #+genera (handler-case (loop for (p . k) in (fs:directory-list directory)
                                          when (eql :directory k) collect p)
                        (fs:directory-not-found () nil))
             #+clozure (ignore-errors (directory* wild :directories T :files NIL))
             #+mcl (ignore-errors (directory* wild :directories T))
             #-(or abcl xcl cormanlisp genera clozure mcl) (directory* wild)))
      (loop for path in dirs
            when (directory-p path)
            collect (pathname-utils:force-directory path)))))

(defun list-hosts ()
  (when (pathname-host *default-pathname-defaults*)
    (list (pathname-host *default-pathname-defaults*))))

(defun list-devices (&optional host)
  (declare (ignore host))
  #+(or windows win32 ms-windows)
  (progn
    #+sbcl (sb-alien:with-alien ((strings (array (integer 16) 1024)))
             (let ((count (sb-alien:alien-funcall (sb-alien:extern-alien "GetLogicalDriveStringsW" (function (integer 32) (integer 32) (array (integer 16) 1024)))
                                                  1024 strings))
                   (base (sb-sys:sap-int (sb-alien:alien-sap strings)))
                   (start 0)
                   (devices ()))
               (dotimes (i count devices)
                 (when (= 0 (sb-alien:deref strings i))
                   (push (string-right-trim ":\\" (wstring->string (sb-sys:int-sap (+ base (* 2 start))))) devices)
                   (setf start (1+ i))))))))

(defun resolve-symbolic-links (pathname)
  #-allegro
  (if (or (typep pathname 'logical-pathname)
          (not (pathname-utils:absolute-p pathname)))
      pathname
      (or (file-exists-p pathname)
          (pathname-utils:normalize-pathname pathname)))
  #+allegro
  (if (physical-pathname-p pathname)
      (or (ignore-errors (excl:pathname-resolve-symbolic-links pathname)) pathname)
      pathname))

(defun directory-p (file)
  #+(or abcl xcl) (extensions:probe-directory file)
  #+allegro (excl:probe-directory file)
  #+clozure (ccl:directoryp file)
  #+cmucl (= #o040000 (logand #o170000 (nth-value 3 (unix:unix-stat (namestring file)))))
  #+(or clasp ecl mkcl) (eql :directory (ext:file-kind file NIL))
  #+sbcl (eql :directory (sb-impl::native-file-kind (namestring (truename file))))
  #+lispworks (lw:file-directory-p x)
  #-(or abcl xcl allegro clasp clozure cmucl ecl mkcl sbcl lispworks)
  (pathname-utils:directory-p file))

(defun file-p (file)
  #+clozure (eql :file (ccl::%file-kind (nth-value 1 (ccl::%stat (namestring file)))))
  #+cmucl (= #o0100000 (logand #o170000 (nth-value 3 (unix:unix-stat (namestring file)))))
  #+(or clasp ecl mkcl) (eql :file (ext:file-kind file NIL))
  #+sbcl (eql :file (sb-impl::native-file-kind (namestring (truename file))))
  #-(or clasp clozure cmucl ecl mkcl sbcl)
  (and (not (directory-p file))
       (not (symbolic-link-p file))))

(defun symbolic-link-p (file)
  #+clozure (ccl::path-is-link file)
  #+cmucl (= #o120000 (logand #o170000 (nth-value 3 (unix:unix-stat (pathname-utils:native-namestring file)))))
  #+(or clasp ecl mkcl) (eql :link (ext:file-kind file NIL))
  #+sbcl (eql :symlink (sb-impl::native-file-kind (pathname-utils:native-namestring file)))
  ;; Try to guess by resolving the file and the directory of it separately.
  #-(or clasp clozure ecl cmucl mkcl sbcl)
  (string/= (namestring (resolve-symbolic-links file))
            (namestring (merge-pathnames (resolve-symbolic-links (pathname-utils:to-directory file)) file))))

(defun create-symbolic-link (link-file destination-file)
  #+(and sbcl unix) (sb-posix:symlink destination-file link-file)
  #+(and sbcl win32) (let ((src (string->wstring (pathname-utils:native-namestring link-file)))
                           (dst (string->wstring (pathname-utils:native-namestring destination-file))))
                       (unwind-protect (when (= 0 (sb-alien:alien-funcall (sb-alien:extern-alien "CreateSymbolicLinkW" (function (integer 32) (* (integer 16)) (* (integer 16)) (integer 32)))
                                                                          src dst (if (directory-p destination-file) #x3 #x2)))
                                         (error "Failed to create symlink."))
                         (sb-alien:free-alien src)
                         (sb-alien:free-alien dst)))
  #-sbcl (error "Cannot create symbolic links."))

(defun rename-file* (file to)
  (let ((file (pathname-utils:to-physical file))
        (to (pathname-utils:to-physical to)))
    #+clisp
    (progn (funcall 'require "syscalls")
           (funcall (find-symbol (string :copy-file) :posix) file to :method :rename))
    #-clisp
    (rename-file file to
                 #+(or clasp clisp clozure ecl) :if-exists
                 #+clozure :rename-and-delete #+(or clasp ecl) t)))

(defun copy-file (file to &key replace skip-root)
  (cond ((directory-p file)
         (let ((to (if skip-root
                       to
                       (pathname-utils:subdirectory to (pathname-utils:directory-name file)))))
           (ensure-directories-exist to)
           (dolist (file (list-contents file))
             (copy-file file to :replace replace))))
        (T
         (let ((to (make-pathname :name (pathname-name file)
                                  :type (pathname-type file)
                                  :defaults to)))
           (when (or (not (file-exists-p to))
                     (ecase replace
                       ((T) T)
                       ((NIL) NIL)
                       (:if-newer (< (file-write-date to) (file-write-date file)))))
             #+allegro (excl.osi:copy-file file to)
             #+ecl (ext:copy-file file to)
             #-(or allegro ecl)
             (with-open-file (out to :element-type '(unsigned-byte 8) :direction :output :if-exists :rename-and-delete)
               (with-open-file (in file :element-type '(unsigned-byte 8) :direction :input :if-does-not-exist :error)
                 (let ((buffer (make-array 8096 :element-type '(unsigned-byte 8))))
                   (declare (dynamic-extent buffer))
                   (loop for read = (read-sequence buffer in)
                         while (< 0 read)
                         do (write-sequence buffer out :end read))))))))))

(defun delete-directory (file)
  #+allegro (excl.osi:delete-directory-and-files file :if-does-not-exist NIL)
  #+clozure (ccl:delete-directory directory-pathname)
  #+genera (fs:delete-directory directory-pathname :confirm nil)
  #+sbcl (sb-ext:delete-directory file :recursive T)
  #-(or allegro clozure genera sbcl)
  (progn
    (dolist (file (list-contents file))
      (delete-file* file))
    #+clisp (ext:delete-directory file)
    #+(or cmucl scl) (multiple-value-bind (ok errno)
                         (unix:unix-rmdir (native-namestring file))
                       (unless ok
                         #+cmucl (error "Error number ~A when trying to delete directory ~A" errno file)
                         #+scl (error "~@<Error deleting ~S: ~A~@:>" file (unix:get-unix-error-msg errno))))
    #+cormanlisp (win32:delete-directory file)
    #+(or clasp ecl) (si:rmdir file)
    #+lispworks (lw:delete-directory file)
    #+mkcl (mkcl:rmdir file)
    #-(or clisp cmucl scl cormanlisp clasp ecl lispworks mkcl)
    (delete-file file)))

(defun delete-file* (file)
  (cond ((directory-p file)
         (delete-directory file))
        (T
         (delete-file file))))
