(defun alphabet (&rest rest)
  (cons 'alphabet (remove-duplicates
                   (copy-list
                    (mapcan (lambda (c)
                              (if (consp c)
                                  (rest c)
                                (list c)))
                            rest)))))

(defun members (alphabet)
  (rest alphabet))

(defun alphabet-member (val alphabet)
  (member val (members alphabet)))

(defun sub-alphabet-p (subalphabet alphabet)
  (subsetp (members subalphabet)
           (members alphabet)))

;;Tohle se mapuje pri pouziti [] na kazdy symbol

(defun eval-member (member)
  (cond ((keywordp member) (eval-keyword member))
        ((symbolp member) (eval-symbol member))
        ((numberp member) (digit-char member))
        ((consp member) (eval-consp (first member) (rest member)))))

(defun eval-consp (op args)
  (case op
    ('quote (list 'quote (first args)))
    ('var (first args))
    ('alphabet (cons op args))))


;;Specialni operatory jako - pro ruzne char sety
(defun eval-specials (first op second)
  (case op
    (#\- `(alphabet ,@(range (char-code first)
                               (char-code second) :key 'code-char)))
    (otherwise nil)))


;;Evaluace na zaklade typu

(defun eval-symbol (keyword)
  (let ((name (string-downcase (symbol-name keyword))))
    (cond ((string= name "space") #\space)
          ((string= name "quote") #\')
          ((= (length name) 3) 
           (eval-specials (aref name 0)
                          (aref name 1)
                          (aref name 2)))
          (t (char name 0)))))


(defun eval-keyword (keyword)
  (let ((name (symbol-name keyword)))
    (cond ((string= name "SPACE") #\space)
          ((string= name "QUOTE") #\')
          ((= (length name) 3) 
           (eval-specials (aref name 0)
                          (aref name 1)
                          (aref name 2)))
          (t (char name 0)))))
           

;;funkce range

(defun range (from to &key (key #'identity))
  (unless (> from to)
    (cons (funcall key from) (range (+ from 1) to :key key))))
  

(defun eval-alphabet (list)
  (mapcar 'eval-member list))

    



;;; Symbol makra a zavorky

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-language-from-brackets (stream)
    (eval-alphabet (read-delimited-list #\] stream t)))

  

  (defun left-brack-reader (stream char)
    (declare (ignore char))
    `(alphabet ,@(read-language-from-brackets stream)))

  (defun right-paren-reader (stream char)
    (declare (ignore stream))
    (error "Non-balanced ~s encountered." char))

  (set-macro-character #\[ #'left-brack-reader) 
  (set-macro-character #\] #'right-paren-reader)


  (defun left-brace-reader (stream char)
    (declare (ignore char))
    `(maybe-i-will-need-this-in-future-so-now-its-undefined ,(car (read-delimited-list #\} stream t))))

  (set-macro-character #\{ #'left-brace-reader) 
  (set-macro-character #\} #'right-paren-reader)

  ;; Zařídíme, aby editor rozuměl hranatým a složeným závorkám
(editor::set-vector-value
 (slot-value editor::*default-syntax-table* 'editor::table) '(#\[ #\{) 2)
(editor::set-vector-value
 (slot-value editor::*default-syntax-table* 'editor::table) '(#\] #\}) 3)
)