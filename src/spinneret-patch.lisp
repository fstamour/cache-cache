;;;; This is a temporary hack™

(in-package #:local-gitlab)

;;; patching spinneret ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is a version equivalent to spinneret::tag-body-parts, but a
;; tad faster
#+(or)
(defun spinneret::tag-body-parts* (form)
  "Pull the attributes off the front of BODY and return the attributes
and the body."
  (loop
    :for rest = form :then (cddr rest)
    :for (key value) = rest
    :while (and rest (keywordp key))
    :append (list key value) :into attr
    :finally (return (values attr rest))))

(defun transform-symbol (symbol)
  (intern
   (serapeum:string-replace-all "--" (symbol-name symbol)  ":")
   (symbol-package symbol)))

;; (transform-symbol 'test--i)
;; => |TEST:I|

;; this version does some stuff to work with vuejs
(defun spinneret::tag-body-parts (form)
  "Pull the attributes off the front of BODY and return the attributes
and the body."
  (loop
    :for rest = form :then (cddr rest)
    :for (key value) = rest
    :while (and rest (symbolp key))
    :append (list (transform-symbol key) value) :into attr
    :finally (return (values attr rest))))

#+(or)
(progn
  (time
   (loop
     :repeat 1000000
     :do (spinneret::tag-body-parts
          '(:v-for "result in searchResultList"
            :v-bind--result "result"
            "blah"))))
  (time
   (loop
     :repeat 1000000
     :do (spinneret::tag-body-parts*
          '(:v-for "result in searchResultList"
            :|v-bind:result| "result"
            "blah")))))

;; This is needed so that spinneret closes all the tags, which is
;; needed by vuejs' templates.
(setf spinneret:*html-style* :tree)

;; Used by spinneret::valid-attribute? via spinneret::unvalidated-attribute?
(pushnew "v-" spinneret:*unvalidated-attribute-prefixes* :test #'string=)

;; (setf spinneret::*end-tag-optional* nil)
;; then: re-evaluate functions.lisp's last expression

;;; end patching spinneret ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
