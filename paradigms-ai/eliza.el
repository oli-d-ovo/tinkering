;; -*- lexical-binding: t -*-

(defvar *eliza-rules*
  '(((($* $x) hello ($* $y))
     (What\'s crackin holmes\?))

    ((($* $x) I want ($* $y))
     (Why do you want $y , yo?)
     (Say you got $y \. Then what?)
     (What would it mean if you got $y \?))))

(defun eliza ()
  (loop
   (print (eliza* (read-from-minibuffer "eliza> ")))))

(defun eliza* (input)
  (present-response
   (use-eliza-rules (interpret-input input))))

(defun use-eliza-rules (input)
  (rule-based-translator input *eliza-rules*
                         :action #'(lambda (bindings responses)
                                     (sublis (switch-viewpoint bindings)
                                             (seq-random-elt responses)))))
