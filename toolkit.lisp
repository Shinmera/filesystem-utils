(in-package #:org.shirakumo.filesystem-utils)

;;; Support
#+(and windows cffi)
(defun wstring->string (pointer &optional (chars -1))
  (let ((bytes (cffi:foreign-funcall "WideCharToMultiByte" :int 65001 :int32 0 :pointer pointer :int chars :pointer (cffi:null-pointer) :int 0 :pointer (cffi:null-pointer) :pointer (cffi:null-pointer) :int)))
    (cffi:with-foreign-object (string :uchar bytes)
      (cffi:foreign-funcall "WideCharToMultiByte" :int 65001 :int32 0 :pointer pointer :int chars :pointer string :int bytes :pointer (cffi:null-pointer) :pointer (cffi:null-pointer) :int)
      (let ((babel::*suppress-character-coding-errors* T))
        (cffi:foreign-string-to-lisp string :encoding :utf-8)))))

#+(and windows cffi)
(defun string->wstring (string &optional buffer)
  (cffi:with-foreign-string (string string)
    (let* ((chars (cffi:foreign-funcall "MultiByteToWideChar" :int 65001 :int32 0 :pointer string :int -1 :pointer (cffi:null-pointer) :int 0 :int))
           (pointer (or buffer (cffi:foreign-alloc :uint16 :count chars))))
      (cffi:foreign-funcall "MultiByteToWideChar" :int 65001 :int32 0 :pointer string :int -1 :pointer pointer :int chars :int)
      pointer)))

#+(and windows cffi)
(defmacro with-wstring ((var string) &body body)
  `(let ((,var (string->wstring ,string)))
     (unwind-protect (let ((,var ,var)) ,@body)
       (cffi:foreign-free ,var))))

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
  #+sbcl (parse-native-namestring (sb-posix:getcwd) :as :directory)
  #+xcl (extensions:current-directory)
  #-(or abcl genera xcl allegro clisp clozure cmucl scl cormanlisp clasp ecl gcl lispworks mkcl sbcl xcl)
  (truename *default-pathname-defaults*))

(defun (setf current-directory) (new)
  (let ((pathname (pathname new)))
    #+(or abcl genera xcl) (setf *default-pathname-defaults* (truename pathname))
    #+allegro (excl:chdir pathname)
    #+clisp (ext:cd pathname)
    #+clozure (setf (ccl:current-directory) pathname)
    #+(or cmucl scl) (unix:unix-chdir (native-namestring pathname))
    #+cormanlisp (unless (zerop (win32::_chdir (native-namestring pathname)))
                   (error "Could not set current directory to ~A" x))
    #+(or clasp ecl) (ext:chdir pathname)
    #+gcl (system:chdir pathname)
    #+lispworks (hcl:change-directory pathname)
    #+mkcl (mk-ext:chdir pathname)
    #+sbcl (sb-posix:chdir (native-namestring pathname))
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

#+(and unix cffi)
(cffi:defcstruct (dirent :class dirent :conc-name dirent-)
  (inode :size)
  (offset :size)
  (length :uint16)
  (type :uint8)
  (name :char))

#+(and unix cffi)
(defun dirent-path (entry)
  (declare (type cffi:foreign-pointer entry))
  (declare (optimize speed (safety 0)))
  (let* ((off (cffi:foreign-slot-offset '(:struct dirent) 'name))
         (name (cffi:inc-pointer entry off))
         (maxlen (- (the (unsigned-byte 16) (dirent-length entry)) off))
         (len (cffi:foreign-funcall "strnlen" :pointer name :size maxlen :size)))
    ;; For whatever reason using CFFI's native string length counter
    ;; breaks, so we use strnlen above, which works correctly.
    (cffi:foreign-string-to-lisp name :count len :encoding :utf-8)))

#+(and unix cffi)
(defun dotpathp (name)
  (declare (type cffi:foreign-pointer name))
  (declare (optimize speed (safety 0)))
  ;; Check if the name is either "." or ".."
  (let ((a (cffi:mem-aref name :char 0))
        (b (cffi:mem-aref name :char 1))
        (c (cffi:mem-aref name :char 2)))
    (or (= 0 a)
        (and (= (char-code #\.) a) (= 0 b))
        (and (= (char-code #\.) a) (= (char-code #\.) b) (= 0 c)))))

#+(and unix cffi)
(defun fd-path (fd)
  (declare (type fixnum fd))
  (declare (optimize speed (safety 0)))
  (cffi:with-foreign-objects ((path :char 1024))
    (cffi:foreign-funcall "snprintf" :pointer path :size 1024 :string "/proc/self/fd/%i" :int fd)
    (let ((size (cffi:foreign-funcall "readlink" :pointer path :pointer path :size 1024 :int)))
      (when (< 0 size)
        (let ((str (cffi:foreign-string-to-lisp path :count (1+ size))))
          (setf (char str (1- (length str))) #-windows #\/ #+windows #\\)
          str)))))

#+(and windows cffi)
(cffi:defcstruct (find-data :conc-name find-data-)
  (attributes :uint32)
  (creation-time :uint64)
  (access-time :uint64)
  (write-time :uint64)
  (size :uint32)
  (reserved-0 :uint32)
  (reserved-1 :uint32)
  (name :uint16 :count 260)
  (alternate-name :uint16 :count 14))

#+(and windows cffi)
(defun dotpathp (name)
  (declare (type cffi:foreign-pointer name))
  (declare (optimize speed (safety 0)))
  ;; Check if the name is either "." or ".."
  (let ((a (cffi:mem-aref name :uint16 0))
        (b (cffi:mem-aref name :uint16 1))
        (c (cffi:mem-aref name :uint16 2)))
    (or (= 0 a)
        (and (= (char-code #\.) a) (= 0 b))
        (and (= (char-code #\.) a) (= (char-code #\.) b) (= 0 c)))))

#+(and windows cffi)
(defun find-path (data)
  (declare (type cffi:foreign-pointer data))
  (declare (optimize speed (safety 0)))
  (wstring->string (cffi:foreign-slot-pointer data '(:struct find-data) 'name)))

(defun map-directory (function path &key (type T) recursive)
  (unless (pathname-utils:directory-p path)
    (error "The path is not a directory pathname."))
  #+(and cffi windows)
  (cffi:with-foreign-objects ((data '(:struct find-data)))
    (labels ((mapdir (path)
               (let ((handle (with-wstring (path (format NIL "\\\\?\\~a*" path))
                               (cffi:foreign-funcall "FindFirstFileW" :pointer path :pointer data :pointer))))
                 (when (= (cffi:pointer-address handle) (1- (ash 1 64)))
                   (error "The file does not exist or is not accessible:~%  ~a" path))
                 (unwind-protect
                      (loop for attributes = (find-data-attributes data)
                            unless (dotpathp (cffi:foreign-slot-pointer data '(:struct find-data) 'name))
                            do (cond ((logbitp 4 attributes) ; Directory
                                      (when (or (eql type T) (eql type :directory))
                                        (funcall function (format NIL "~a~a\\" path (find-path data))))
                                      (when recursive
                                        (mapdir (format NIL "~a~a\\" path (find-path data)))))
                                     ((logbitp 6 attributes) ; Device file
                                      NIL)
                                     ((logbitp 16 attributes) ; Virtual
                                      NIL)
                                     (T
                                      (when (or (eql type T) (eql type :file))
                                        (funcall function (format NIL "~a~a" path (find-path data))))))
                            while (cffi:foreign-funcall "FindNextFileW" :pointer handle :pointer data :bool))
                   (cffi:foreign-funcall "FindClose" :pointer handle)))))
      (mapdir (native-namestring path))))
  #+(and cffi unix)
  (labels ((mapdir (fd dir)
             (loop for entry = (cffi:foreign-funcall "readdir" :pointer dir :pointer)
                   until (cffi:null-pointer-p entry)
                   do (let ((name (cffi:foreign-slot-pointer entry '(:struct dirent) 'name)))
                        (unless (dotpathp name)
                          (labels ((dir (fd)
                                     (when (or (eql type T) (eql type :directory))
                                       (let ((path (fd-path fd)))
                                         (when path
                                           (funcall function path))))
                                     (if recursive
                                         (opendir fd)
                                         (cffi:foreign-funcall "close" :int fd :int)))
                                   (file ()
                                     (when (or (eql type T) (eql type :file))
                                       ;; TODO: we concat two strings here, it'd be a lot faster to only cons up one
                                       (funcall function (format NIL "~a~a" (fd-path fd) (dirent-path entry))))))
                            (case (dirent-type entry)
                              (0 ; Unknown, need to probe the actual file
                               (let ((inner (cffi:foreign-funcall "openat" :int fd :pointer name :int 592128 :int)))
                                 (if (= -1 inner)
                                     (file)
                                     (dir inner))))
                              (4        ; Directory
                               (dir (cffi:foreign-funcall "openat" :int fd :pointer name :int 592128 :int)))
                              (8        ; Regular
                               (file))))))))
           (opendir (fd)
             (let ((handle (cffi:foreign-funcall "fdopendir" :int fd :pointer)))
               (if (cffi:null-pointer-p handle)
                   (cffi:foreign-funcall "close" :int fd :int)
                   (unwind-protect (mapdir fd handle)
                     ;; Closedir closes the FD as well, so don't call close.
                     (cffi:foreign-funcall "closedir" :pointer handle :int))))))
    (let* ((path (native-namestring path))
           (fd (cffi:foreign-funcall "open" :string path :int 592128 :int)))
      (if (= -1 fd)
          (error "The file does not exist or is not accessible:~%  ~a" path)
          (opendir fd))))
  #-(and cffi (or unix windows))
  (dolist (entry (directory (if recursive
                                (merge-pathnames pathname-utils:*wild-inferiors* path)
                                (merge-pathnames pathname-utils:*wild-file* path))))
    (when (or (eql T type)
              (and (eql :directory type) (directory-p entry))
              (and (eql :file type) (file-p entry)))
      (funcall function entry))))

(defmacro do-directory ((file directory &key (type T) recursive return) &body body)
  (let ((thunk (gensym "THUNK")))
    `(block NIL
       (flet ((,thunk (,file) ,@body))
         (declare (dynamic-extent #',thunk))
         (map-directory #',thunk ,directory :type ,type :recursive ,recursive)
         ,return))))

(defun list-contents (directory &key recursive)
  (let ((directory (pathname-utils:to-directory directory)))
    (let ((files ()))
      (do-directory (file directory :type T :recursive recursive :return files)
        (push (etypecase file
                (string (pathname-utils:parse-native-namestring file))
                (pathname file))
              files)))))

(defun list-files (directory)
  (let ((directory (pathname-utils:to-directory directory)))
    #+cffi
    (let ((files ()))
      (do-directory (file directory :type :file :return files)
        (push (pathname-utils:parse-native-namestring file) files)))
    #-cffi
    (let* ((directory (pathname-utils:pathname* directory))
           (entries (ignore-errors (directory* (merge-pathnames pathname-utils:*wild-file* directory)))))
      (remove-if #'directory-p entries))))

(defun list-directories (directory)
  (let ((directory (pathname-utils:to-directory directory)))
    #+cffi
    (let ((files ()))
      (do-directory (file directory :type :directory :return files)
        (push (pathname-utils:parse-native-namestring file) files)))
    #-cffi
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

#+(and cffi unix)
(cffi:defcstruct (mntent :conc-name mntent-)
  (name :string)
  (directory :string)
  (type :string)
  (options :string)
  (frequency :int)
  (pass-number :int))

(defun list-devices (&optional host)
  (declare (ignore host))
  #+(and cffi windows)
  (cffi:with-foreign-objects ((strings :uint16 1024))
    (let ((off 0)
          (len (cffi:foreign-funcall "GetLogicalDriveStringsW" :int32 1024 :pointer strings :int32)))
      (loop for str = (wstring->string (cffi:inc-pointer strings (* 2 off)))
            collect (string-right-trim ":\\" str)
            do (incf off (1+ (length str)))
            while (< off len))))
  #+(and cffi unix)
  (let ((mnt (cffi:foreign-funcall "setmntent" :string "/etc/mtab" :string "r" :pointer)))
    (unless (cffi:null-pointer-p mnt)
      (unwind-protect
           (loop for mntent = (cffi:foreign-funcall "getmntent" :pointer mnt :pointer)
                 until (cffi:null-pointer-p mntent)
                 collect (pathname-utils:parse-native-namestring
                          (mntent-directory mntent) :as :directory))
        (cffi:foreign-funcall "endmntent" :pointer mnt)))))

(defun device (pathname)
  (or (pathname-device pathname)
      ;; FIXME: Implement on more implementations
      #+(or posix unix linux bsd)
      (progn #+ccl (nth-value 9 (ccl::%stat (native-namestring pathname)))
             #+cmucl (nth-value 1 (unix:unix-stat (native-namestring pathname)))
             #+sbcl (sb-posix:stat-dev (sb-posix:stat pathname)))))

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
  #+clasp (eql :directory (si:file-kind file NIL))
  #+clozure (ccl:directoryp file)
  #+cmucl (= #o040000 (logand #o170000 (nth-value 3 (unix:unix-stat (native-namestring file)))))
  #+(or ecl mkcl) (eql :directory (ext:file-kind file NIL))
  #+sbcl (eql :directory (sb-impl::native-file-kind (native-namestring (truename file))))
  #+lispworks (lw:file-directory-p x)
  #-(or abcl xcl allegro clasp clozure cmucl ecl mkcl sbcl lispworks)
  (pathname-utils:directory-p file))

(defun file-p (file)
  #+clasp (eql :file (si:file-kind file NIL))
  #+clozure (eql :file (ccl::%file-kind (nth-value 1 (ccl::%stat (native-namestring file)))))
  #+cmucl (= #o0100000 (logand #o170000 (nth-value 3 (unix:unix-stat (native-namestring file)))))
  #+(or ecl mkcl) (eql :file (ext:file-kind file NIL))
  #+sbcl (eql :file (sb-impl::native-file-kind (native-namestring (truename file))))
  #-(or clasp clozure cmucl ecl mkcl sbcl)
  (and (not (directory-p file))
       (not (symbolic-link-p file))))

(defun symbolic-link-p (file)
  #+clasp (eql :link (si:file-kind file NIL))
  #+clozure (ccl::path-is-link file)
  #+cmucl (= #o120000 (logand #o170000 (nth-value 3 (unix:unix-stat (native-namestring file)))))
  #+(or ecl mkcl) (eql :link (ext:file-kind file NIL))
  #+sbcl (eql :symlink (sb-impl::native-file-kind (native-namestring file)))
  ;; Try to guess by resolving the file and the directory of it separately.
  #-(or clasp clozure ecl cmucl mkcl sbcl)
  (string/= (native-namestring (resolve-symbolic-links file))
            (native-namestring (merge-pathnames (resolve-symbolic-links (pathname-utils:to-directory file)) file))))

(defun create-symbolic-link (link-file destination-file)
  (let ((src (native-namestring link-file))
        (dst (native-namestring destination-file)))
    #+(and cffi unix)
    (unless (= 0 (cffi:foreign-funcall "symlink" :string dst :string src :int))
      (error "Failed to create symlink."))
    #+(and cffi win32)
    (let ((src (string->wstring src))
          (dst (string->wstring dst)))
      (unwind-protect (when (= 0 (cffi:foreign-funcall "CreateSymbolicLinkW" :pointer src :pointer dst :int32 (if (directory-p destination-file) #x3 #x2) :int32))
                        (error "Failed to create symlink."))
        (cffi:foreign-free src)
        (cffi:foreign-free dst)))
    #-cffi (declare (ignore src dst))
    #-cffi (error "Cannot create symbolic links.")))

(defun create-hard-link (link-file destination-file)
  (let ((src (native-namestring link-file))
        (dst (native-namestring destination-file)))
    #+(and cffi unix)
    (unless (= 0 (cffi:foreign-funcall "link" :string dst :string src :int))
      (error "Failed to create link."))
    #+(and cffi win32)
    (let ((src (string->wstring src))
          (dst (string->wstring dst)))
      (unwind-protect (when (= 0 (cffi:foreign-funcall "CreateHardLinkW" :pointer src :pointer dst :pointer (cffi:null-pointer) :int32))
                        (error "Failed to create link."))
        (cffi:foreign-free src)
        (cffi:foreign-free dst)))
    #-cffi (declare (ignore src dst))
    #-cffi (error "Cannot create hard links.")))

(defun rename-file* (file to)
  (let ((file (pathname-utils:to-physical file))
        (to (merge-pathnames (pathname-utils:to-physical to)
                             (make-pathname :name :unspecific :type :unspecific))))
    (cond ((pathname-utils:pathname= file to))
          ((equal (device file) (device (pathname-utils:to-directory to)))
           #+clisp
           (progn (funcall 'require "syscalls")
                  (funcall (find-symbol (string :copy-file) :posix) file to :method :rename))
           #-clisp
           (rename-file file to
                        #+(or clasp clisp clozure ecl) :if-exists
                        #+clozure :rename-and-delete #+(or clasp ecl) t))
          (T
           (copy-file file to :replace T)
           (ensure-deleted file)))
    to))

(defun copy-file (file to &key replace skip-root)
  (cond ((directory-p file)
         (let ((to (if skip-root
                       to
                       (pathname-utils:subdirectory to (pathname-utils:directory-name file)))))
           (ensure-directories-exist to)
           (dolist (file (list-contents file))
             (copy-file file to :replace replace))))
        (T
         (let ((to (if (and (null (pathname-name to)) (null (pathname-type to)))
                       (make-pathname :name (pathname-name file)
                                      :type (pathname-type file)
                                      :defaults to)
                       to)))
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

(defun empty-directory-p (file)
  (and (directory-p file)
       ;; FIXME: this sucks and is slow if the directory is very much not empty.
       (null (directory (merge-pathnames pathname-utils:*wild-file* file)))))

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
