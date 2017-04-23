(defun signif-digits (input-set-size)
  (cond
    ((= input-set-size 0) 1) 
    (T input-set-size)))   
      
(defun power-set-size (input-set-size)
  (expt 2 input-set-size))

(defun hexchar-to-binstr (char)
  (cond 
    ((string-equal char "0") "0000")
    ((string-equal char "1") "0001")
    ((string-equal char "2") "0010")
    ((string-equal char "3") "0011")
    ((string-equal char "4") "0100")
    ((string-equal char "5") "0101")
    ((string-equal char "6") "0110")
    ((string-equal char "7") "0111")
    ((string-equal char "8") "1000")
    ((string-equal char "9") "1001")
    ((string-equal char "A") "1010")
    ((string-equal char "B") "1011")
    ((string-equal char "C") "1100")
    ((string-equal char "D") "1101")
    ((string-equal char "E") "1110")
    ((string-equal char "F") "1111")))

(defun hexstr-to-binstr (hexstr outstr)
  (cond
    ((string-equal hexstr "") outstr)
    (T (hexstr-to-binstr (subseq hexstr 1) (concatenate 'string outstr (hexchar-to-binstr (subseq hexstr 0 1)))))))

(defun int-to-binstr (integer setsize)
  (concatenate 'string (string-of-zeroes (- (signif-digits setsize) (length (hexstr-to-binstr (write-to-string integer :base 16) ""))))   
                       (hexstr-to-binstr (write-to-string integer :base 16) "")))

(defun n-binstrs (n set-size)
  (cons (int-to-binstr n set-size) (cond 
                                     ((= n 0) '()) 
                                     (T (n-binstrs (- n 1) set-size)))))

(defun subset-from-binstr (set binstr i)
  (cond
    ((= i (signif-digits (list-length set))) '())
    (T (cond
         ((string-equal (subseq binstr i (+ i 1)) "1") (cons (nth i set)
                                                        (subset-from-binstr set binstr (+ i 1))))
         ((string-equal (subseq binstr i (+ i 1)) "0")  (subset-from-binstr set binstr (+ i 1)))))))

(defun powerset (inputset binstrs i)
  (cond
    ((= i (power-set-size (list-length inputset))) '())
    (T (cons (subset-from-binstr inputset (nth i binstrs) 0) (powerset inputset binstrs (+ i 1))))))

;;; UTILITY JUNK

(defun string-of-zeroes (length)
  (cond
    ((< length 1) "")
    ((= length 1) "0")
    (T (concatenate 'string "0" (string-of-zeroes (- length 1))))))

(defun print-list (list)
  (cond
    ((= (list-length list) 0) (format t "~CEND OF LIST~C" #\newline #\newline))
    (T (print (car list))
       (print-list (cdr list)))))

(defun print-list-of-lists (list-of-lists i)
  (cond
    ((= (list-length list-of-lists) 0) (format t "END OF LIST OF LISTS~C" #\newline))
    (T (format t "LIST ~D" i)
       (print-list (car list-of-lists))
       (print-list-of-lists (cdr list-of-lists) (+ i 1)))))

;;; TEST OUTPUT
;;;(let ((input-set '()))
;;;  ;(print-list-of-lists (powerset input-set (n-binstrs (power-set-size (list-length input-set)) (list-length input-set)) 0) 0)
;;;  ;(format t "power-set-size: ~D" (power-set-size (list-length input-set)))
;;;  ;(print-list (n-binstrs (power-set-size (list-length input-set)) (list-length input-set)))
;;;  )

(let ((some-input '()))
  (format t "input list:")
  (print-list some-input)
  (format t "~CPower-set-size: ~D~C" #\newline (power-set-size (list-length some-input)) #\newline)
  (format t "~CBinary string list:" #\newline)
  (print-list (n-binstrs (power-set-size (list-length some-input)) (list-length some-input)))
  (format t "~CPower set list:~C" #\newline #\newline)
  (print-list-of-lists (powerset some-input (n-binstrs (power-set-size (list-length some-input)) (list-length some-input)) 0) 0))
