(in-package #:local-gitlab)


(ql:quickload "trivial-benchmark")


(trivial-benchmark:with-timing (100)
  (find-issues "gitlab"))
; -                SAMPLES  TOTAL      MINIMUM   MAXIMUM   MEDIAN    AVERAGE    DEVIATION
; REAL-TIME        100      0.584927   0.004651  0.096082  0.004797  0.005849   0.009075
; RUN-TIME         100      0.609375   0         0.09375   0         0.006094   0.01146
; USER-RUN-TIME    100      0.5625     0         0.0625    0         0.005625   0.009249
; SYSTEM-RUN-TIME  100      0.03125    0         0.03125   0         0.000312   0.003109
; PAGE-FAULTS      100      0          0         0         0         0          0.0
; GC-RUN-TIME      100      93.75      0         93.75     0         0.9375     9.328008
; BYTES-CONSED     100      819744656  8191504   8257136   8191536   8197446.5  18767.557
; EVAL-CALLS       100      0          0         0         0         0          0.0
;  => NIL


(defun ngrams (n string)
  "Generate all N-grams of STRING, the list is not deduplicated."
  (loop :for i :below (- (length string) 3)
        :for ngram = (make-array
                      n
                      :element-type (array-element-type string)
                      :displaced-to string
                      :displaced-index-offset i)
        :collect ngram))

(defun normalized-ngrams (n string)
  (remove-duplicates
   (mapcar #'string-downcase
           (ngrams n string))
   :test #'string=))


(normalized-ngrams 3 "abcdefgAbcd")

(defparameter *index*
  (let ((index (make-hash-table :test #'equal)))
    (maphash
     #'(lambda (id issue)
         (loop :for ngram :in (normalized-ngrams
                               3
                               (issue-title issue))
               :do (pushnew id (gethash ngram index))))
     *issues*)
    index))

(defun narrow-down-using-index (index query &optional candidates)
  (loop
    :for ngram :in (normalized-ngrams 3 query)
    :do
       (setf candidates
             (if candidates
                 ;; Update candidates
                 (intersection
                  candidates
                  (gethash ngram index))
                 ;; Initialize candidates
                 (gethash ngram index)))
       ;; No candidates left, early exit
    :while candidates
    :finally (return candidates)))

(length
 (narrow-down-using-index *index* "gitlab"))

(trivial-benchmark:with-timing (100)
  (let ((query "gitlab"))
    (search-in-list
     query
     (mapcar
      #'issue-by-id
      (narrow-down-using-index *index* query))
     :key #'issue-title)))
; -                SAMPLES  TOTAL     MINIMUM   MAXIMUM   MEDIAN   AVERAGE   DEVIATION
; REAL-TIME        100      0.023952  0.000224  0.000632  0.00023  0.00024   0.000042
; RUN-TIME         100      0.03125   0         0.015625  0        0.000312  0.002188
; USER-RUN-TIME    100      0         0         0         0        0         0.0
; SYSTEM-RUN-TIME  100      0.015625  0         0.015625  0        0.000156  0.001555
; PAGE-FAULTS      100      0         0         0         0        0         0.0
; GC-RUN-TIME      100      0         0         0         0        0         0.0
; BYTES-CONSED     100      7208112   65520     131072    65536    72081.12  19658.295
; EVAL-CALLS       100      0         0         0         0        0         0.0
;  => NIL

(/
 0.00024
 0.005849)
;; It took 4% of the (real) time


(defun find-issues* (query)
  "Return the issues that contains all the parts of QUERY in their KEY."
  (search-in-list/and
   (split-sequence:split-sequence #\Space query :remove-empty-subseqs t)
   (a:hash-table-values *issues*)
   :key #'issue-title))
