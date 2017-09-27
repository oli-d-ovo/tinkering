;; -*- lexical-binding: t -*-

(setf (get '$is 'single-match) 'match-is)
(setf (get '$or 'single-match) 'match-or)
(setf (get '$and 'single-match) 'match-and)
(setf (get '$not 'single-match) 'match-not)

(setf (get '$* 'segment-match) 'segment-match)
(setf (get '$+ 'segment-match) 'segment-match+)
(setf (get '$? 'segment-match) 'segment-match?)
(setf (get '$if 'segment-match) 'match-if)

(defun match-is (var-and-pred input bindings)
  (let* ((var (first (var-and-pred)))
         (pred (second var-and-pred))
         (new-bindings (pat-match var input bindings)))
    (if (or (eq new-bindings fail)
            (not (funcall pred input)))
        fail
      new-bindings)))

(defun match-and (patterns input bindings)
  (cond ((eq bindings fail) fail)
        ((null patterns) bindings)
        (t (match-and (rest patterns) input
                      (pat-match (first patterns) input
                                 bindings)))))

(defun match-or (patterns input bindings)
  (if (null patterns)
      fail
    (let ((new-bindings (pat-match (first patterns)
                                   input bindings)))
      (if (eq new-bindings fail)
          (match-or (rest patterns) input bindings)
        new-bindings))))

(defun match-not (patterns input bindings)
  (if (match-or patterns input bindings)
      fail
    bindings))

(defun match-if (pattern input bindings)
  (and (progv (mapcar #'car bindings) (mapcar #'cdr bindings)
            (print pattern)
            (eval (second (first pattern))))
       (pat-match* (rest pattern input bindings))))
