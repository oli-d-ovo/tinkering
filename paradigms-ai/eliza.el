;; -*- lexical-binding: t -*-

(defvar *eliza-rules*
  '(((($* $x) hello ($* $y))
     (What's crackin' holmes?))

    ((($* $x) I want ($* $y))
     (Why do you want $y , yo?)
     (Say you got $y \. Then what?)
     (What would it mean if you got $y \?))))

(defun eliza ()
  "Need to make this more Elispy"
  (loop
   (print 'eliza>)
   (message (present-response (read)))))

(defun present-response (resp)
  (mapconcat 'identity
             (mapcar #'symbol-name (apply #'append resp))
             " "))

(defconst fail nil)

(defconst no-bindings '((t . t)))

(defun get-binding (var bindings)
  (assoc var bindings))

(defun binding-val (binding)
  (cdr binding))

(defun lookup (var bindings)
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  (cons (cons var val)
        (unless (eq bindings no-bindings)
          bindings)))

(defun simple-equal (x y)
  (if (or (atom x) (atom y))
      (eql x y)
    (and (simple-equal (first x) (first y))
         (simple-equal (rest x) (rest y)))))

(defun pat-match (pattern input)
  (pat-match* pattern input no-bindings))

(defun pat-match* (pattern input bindings)
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)
         (segment-match pattern input bindings 0))
        ((and (consp pattern) (consp input))
         (pat-match* (rest pattern) (rest input)
                     (pat-match* (first pattern) (first input)
                                 bindings)))
        (t fail)))

(defun variable-p (x)
  (and (symbolp x) (char-equal (string-to-char (symbol-name x)) ?$)))

(defun match-variable (var input bindings)
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(defun segment-pattern-p (pattern)
  (and (consp pattern)
       (consp (first pattern))
       (equal (first (first pattern)) '$*)))

(defun segment-match (pattern input bindings start)
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
      (let ((pos (position (first pat) input
                           :start start :test #'equal)))
        (if (null pos)
            fail
          (let ((b2 (pat-match* pat (subseq input pos)
                                (match-variable var (subseq input 0 pos) bindings))))
            (if (eq b2 fail)
                (segment-match pattern input bindings (+ pos 1))
              b2)))))))

(defun rule-pattern (rule) (first rule))

(defun rule-responses (rule) (rest rule))
