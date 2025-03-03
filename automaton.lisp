;;Requied Alphabet.lisp


(defun string-to-list (string)
  (coerce string 'list))


      
(defun automaton (delta)
  (lambda (string)
    (let ((word (string-to-list string)))
      (funcall delta word))))

      

    

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  ;; ([a b c] -> state)  --> (([a b c]  state) [a b c])
  (defun gen-funcion (chars arrow next-state)
    (declare (ignore arrow))
    (list (list chars next-state) chars))
            
  ;; (([a b c] -> state) ([d] -> state2)) --> (  (([a b c]  state) [a b c]) ...
            
  (defun gen-function-list (f-list)
    (mapcar (lambda (f)
              (gen-funcion (first f) (second f) (third f)))
            f-list))
     
  ;; -> (values (state-functions) (asserts))
  (defun generate-state (alphabet name ending-state-p &rest functions)
    (let* ((data (gen-function-list functions))
           (func (mapcar #'first data))
           (asserts (mapcar #'second data)))
      
      
      (values (state-function name func ending-state-p)
              (generate-asserts asserts alphabet))))

  (defun state-function (name states on-finish)
    `(,name (word)
            (if word
                (cond 
                  ,@(mapcar (lambda (item)
                              `((alphabet-member (car word) ,(first item))  (,(second item) (rest word))))
                            states))
              ,on-finish)))

  (defun generate-asserts (asserts alphabet)
    (mapcar (lambda (c)
              `(unless (sub-alphabet-p ,c ,alphabet)
                (error "in transition function ~s is not subset of alphabet" ,c)))
            asserts))

)
(defmacro defautomaton (name (alphabet &key (as 'sigma)) &body states)
  (let ((vals (mapcar (lambda (state)
                        (multiple-value-list (apply #'generate-state as state)))
                      states)))
      
    `(let ((,as ,alphabet))
       (declare (ignorable ,as))
       (labels ,(mapcar #'first vals)
         ,@(car (mapcar #'second vals))
         (setf (fdefinition ',name) (automaton #'delta))
         ',name))))




(defautomaton containts-abc-p ([a-z] :as sigma)
  (delta nil 
         ([a] -> a-state)
         ([b-z] -> delta))

  (a-state nil
           ([a] -> a-state)
           ([b] -> b-state)
           ([c-z] -> delta))

  (b-state nil ([b d-z] -> delta)
           ([c] -> sink-state)
           ([a] -> a-state))
                                        
  (sink-state t
              (sigma -> sink-state)))



  
  