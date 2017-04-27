(defun add-to-each (in-set-set out-set-set element)
  (cond
    ((= (list-length in-set-set) 0)  out-set-set)
    (T (add-to-each (cdr in-set-set) (cons (append (list element) (car in-set-set)) out-set-set) element)))) 
    

(defun power-set (input-set output-set-set)
  (cond
    ((= (list-length input-set) 0) output-set-set)
    (T (power-set (cdr input-set) (append (add-to-each output-set-set '() (car input-set)) output-set-set)))))
    
;;; UTILITY JUNK

(defun print-list (in-list)
  (cond
    ((= (list-length in-list) 0) (format t "~CEND OF LIST~C" #\newline #\newline))
    (T (print (car in-list))
       (print-list (cdr in-list)))))

(defun print-list-of-lists (list-of-lists i)
  (cond
    ((= (list-length list-of-lists) 0) (format t "END OF LIST OF LISTS~C" #\newline))
    (T (format t "LIST ~D" i)
       (print-list (car list-of-lists))
       (print-list-of-lists (cdr list-of-lists) (+ i 1)))))

(let ((some-input '(a b)))
  (print-list some-input)
  (format t "~CPower set list:~C" #\newline #\newline)
  (print-list-of-lists (power-set some-input '('())) 0))
