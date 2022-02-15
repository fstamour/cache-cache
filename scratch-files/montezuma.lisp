;;;; Trying to index the issues using Montezuma
;;;;
;;;; Not kept because with 3000 issues, searching only in the title,
;;;; it was faster to just "brute-force" the search.
;;;;
;;;; and I could probably create a lightweight indexing scheme that
;;;; would be even faster...

(ql:quickload #:montezuma)

(:local-nicknames
 ;; "i" for "indexing"
 (#:i #:montezuma))


(defparameter *index* (make-instance 'montezuma:index)
  "In-memory index.")

(defun issue-document (issue)
  "Create a document (alist of montezuma:document) from ISSUE (hash-table)"
  (let ((doc (make-instance 'montezuma:document)))
    (loop
      :for (field stored tokenizep) :in '(("id" t nil)
                                          ("title" nil t)
                                          ("description" nil t))
      :for value = (gethash field issue)
      :unless (eq :null value)
        :do
           (i:add-field
            doc
            (i:make-field field ; key
                          ;; value
                          (etypecase value
                            (number (format nil "~s" value))
                            (string value))
                          :stored stored
                          :index (if tokenizep
                                     :tokenized
                                     :untokenized))))
    (i:add-field
     doc
     (i:make-field "type" "issue" :stored t :index :untokenized))
    doc))

#+ (or)
(progn
  (issue-document (elt *issues* 0))

  ;; Index all issues
  (time
   (loop :for issue :across *issues*
         :do (montezuma:add-document-to-index
              *index*
              (issue-document issue))))

  ;; Optimize the index
  (time
   (i:optimize *index*)))

#+ (or)
(defun search-issue (query)
  "Search with montezuma"
  (let ((result '()))
    (i:search-each *index* query
                   #'(lambda (doc score)
                       (push (cons doc score) result)))
    (nreverse result)))

#+ (or)
(progn
  (search-issue "title:\"update\"")
  (search-issue "description:\"update"))
