;; -*- lexical-binding: t -*-

(defconst fail nil)

(defconst no-bindings '((t . t)))

(defun pat-match (pattern input)
  (pat-match* pattern input no-bindings))

(defun pat-match* (pattern input bindings)
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)
         (segment-matcher pattern input bindings))
        ((single-pattern-p pattern)
         (single-matcher pattern input bindings))
        ((and (consp pattern) (consp input))
         (pat-match* (rest pattern) (rest input)
                     (pat-match* (first pattern) (first input)
                                 bindings)))
        (t fail)))

(defun* rule-based-translator (input rules &key (matcher #'pat-match)
                                     (rule-if #'first)
                                     (rule-then #'rest)
                                     (action #'sublis))
  (some #'(lambda (rule)
            (let ((result (funcall matcher (funcall rule-if rule)
                                   input)))
              (if (not (eq result fail))
                  (funcall action result (funcall rule-then rule)))))
        rules))

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

(defun variable-p (x)
  (and (symbolp x) (char-equal (string-to-char (symbol-name x)) ?$)))

(defun match-variable (var input bindings)
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(defun single-pattern-p (pattern)
  (and (consp pattern)
       (single-match-fn (first pattern))))

(defun single-match-fn (x)
  (when (symbolp x) (get x 'single-match)))

(defun single-matcher (pattern input bindings)
  (funcall (single-match-fn (first pattern))
           (rest pattern) input bindings))

(defun segment-pattern-p (pattern)
  (and (consp pattern) (consp (first pattern))
       (segment-match-fn (first (first pattern)))))

(defun segment-match-fn (x)
  (when (symbolp x) (get x 'segment-match)))

(defun segment-matcher (pattern input bindings)
  (funcall (segment-match-fn (first (first pattern)))
           pattern input bindings))

(defun segment-match (pattern input bindings)
  (segment-match* pattern input bindings 0))

(defun segment-match+ (pattern input bindings)
  (segment-match* pattern input bindings 1))

(defun segment-match? (pattern input bindings)
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (or (pat-match* (cons var pat) input bindings)
        (pat-match* pat input bindings))))

(defun segment-match* (pattern input bindings start)
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
      (let ((pos (first-match-pos (first pat) input start)))
        (if (null pos)
            fail
          (let ((b2 (pat-match* pat (subseq input pos)
                                (match-variable var (subseq input 0 pos) bindings))))
            (if (eq b2 fail)
                (segment-match* pattern input bindings (+ pos 1))
              b2)))))))

(defun first-match-pos (pat1 input start)
  (cond ((and (atom pat1) (not (variable-p pat1)))
         (position pat1 input :start start :test #'equal))
        ((< start (length input)) start)
        (t nil)))
