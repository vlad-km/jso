;;; -*- mode:lisp; coding:utf-8 -*-

;;;
;;; JS object manipulation functions
;;; This file is part of the :jso package
;;; Copyright © 2018 Vladimir Mezentsev
;;;
;;; Intended for use in Moren Environment
;;; 

(defpackage :jso
  (:use :cl)
  (:export #:mk
           #:camel-case
           #:map-to #:mt
           #:keys #:ks
           #:to-list #:tl
           #:copy #:cp
           #:om
           #:del
           #:mcall #:_get #:_set))


(eval-when (:compile-toplevel :load-toplevel :execute)
    (unless (find :jso *features*)
        (push :jso *features*)))


(in-package :jso)

;;; make js-object
;;; (mk "name" "val" "next" (make-js-object "name2" "val2"))
;;; => {name: "val", next: {name2: "val2"}}
(defun mk (&rest kv)
    (let ((obj (jscl::new))
          (idx 0)
          (key-val))
        (if (oddp (length kv))
            (error "JSO: Too few arguments"))
        (dolist (it kv)
            (if (oddp idx)
                (setf (jscl::oget obj key-val) it)
                (setq key-val it))
            (incf idx))
        obj))


;;; camel case util
;;; (camel-case 'abc-def) => "abcDef"
;;; (camel-case '_-abc_--Def) => "_Abc_Def"
(defun camel-case (name)
    (prog ((chars (string-downcase (symbol-name name)))
           (len)
           (upcase nil)
           (ch)
           (peek)
           (idx 0)
           (result))
       (setq len (length chars))
       (if (= len 0) (go exit))
     rdr
       (if (= idx len) (go eol))
       (setq ch (char chars idx)
             idx (1+ idx))
       (if (char= #\- ch) (go minus))
       (push (if upcase (char-upcase ch) ch) result)
       (setq upcase nil)
       (go rdr)
     minus
       (setq upcase t)
       (go rdr)
     eol
       (setq result (apply 'jscl::concat (reverse result)))
     Exit
       (return result)))

;;; map js-object
;;; js object iterator
;;; (map-to obj  #'(lambda (x y) (print x)) )
;;; "aaa"
;;; "bbb"
(defun mt (jso fn)
    (prog
        ((it)
         (keys (jscl::vector-to-list (#j:Object:keys jso)))
         (result))
     rdr
       (setq it (jscl::js-to-lisp (pop keys)))
       (unless it (go exit))
       (push (funcall fn it (jscl::oget jso it)) result)
       (go rdr)
     exit
       (return (reverse result))))

(jscl::fset 'map-to #'mt)


;;; get js-object keys
;;; Return list object keys
;;; => ("bbb" "aaa")
(defun ks (jso)
    (prog ((result)
           (it)
           (keys (jscl::vector-to-list (#j:Object:keys jso))))
     rdr
       (setq it (pop keys))
       (unless it (go exit))
       (push (jscl::js-to-lisp it) result)
       (go rdr)
     exit
       (return-from ks  (reverse result))))

(jscl::fset 'keys #'ks)


;;; js-object to list
;;; => ("aa" 1 "bb" 2)
;;; only plain object
(defun tl (jso)
    (prog
        ((keys (jscl::vector-to-list (#j:Object:keys jso)))
         (it)
         (result))
     rdr
       (setq it (jscl::js-to-lisp (pop keys)))
       (unless it (go exit))
       (push (list it (jscl::oget jso it)) result)
       (go rdr)
     exit
       (return (reverse result))))

(jscl::fset 'to-list #'tl)


;;;
;;; copies js object
;;; returning a new object with inherite properties
;;;
(defun cp (jso)
    (#j:Object:assign (jscl::new) jso))

(jscl::fset 'copy #'cp)


;;;
;;; Merge few objects to new object
;;; The identical properties are replaced
;;; See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/assign
;;; for details
;;;
(defun om (&rest jso)
    (apply #j:Object:assign (jscl::new) jso))

;;;
;;; delete properties from obj
;;; use (delete-property key obj) from JSCL
;;;
(defun del (jso prop)
    (jscl::delete-property prop jso))


;;; js object method call
(defmacro mcall ((obj &rest methods) &body body)
    `(funcall ((jscl::oget ,obj ,@methods "bind") ,obj ,@body)))

;;; js object get/set macro

;;; (jso:_get (obj "aaa" "bbb" "ccc"))
;;; => (oget obj "aaa" "bbb" "ccc")
;;;
(defmacro _get ((obj &rest methods))
    `(jscl::oget ,obj ,@methods ))


;;; (jso:_set (obj "aaa" ) (new))
;;; => (setf (oget obj "aaa") (new))
;;; (jso:_set (obj "aaa" "bbb") (new))
;;; (jso:_set (obj "aaa" "bbb" "ccc") "place")
;;; obj => {aaa: {bbb: {ccc: "place"}}}
(defmacro _set ((obj &rest methods) &body body)
    `(setf (jscl::oget ,obj ,@methods) ,@body))

(in-package :cl-user)
;;;; EOF
