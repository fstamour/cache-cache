
;; TODO Not used yet

;; TODO maybe rename to "reference"


;; TODO This should be a global variable (see serapeum), or perhaps a constant?
;; see https://docs.gitlab.com/ee/user/markdown.html#gitlab-specific-references for more
(defvar *gitlab-sigil-alist* '(("issues" . "#")
                               ("merge_requests" . "!")
                               ("epics" . "&")
                               ("milestones" . "%")))
