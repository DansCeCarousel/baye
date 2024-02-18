(ql:quickload :rutils)
(ql:quickload :vgplot)
(defun myplot (x y)
  (vgplot:plot x y))
(defun linspace (start stop &optional (step 1))
  (loop for i from start below stop by step collect i))
;;;
;;load file from python,\
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))
(defun audio-list (filename)
  (mapcar 'read-from-string (get-file filename)))
(defparameter a-sound (audio-list "aa.txt"))
(defparameter a-index (linspace 0 (length a-sound)))
(defparameter a-phoneme (third (rtl:group 1500 a-sound)))
(defparameter a-phoneme-index (linspace 0 (length a-phoneme)))
;;(defparameter data (loop for i from 0 below 20 by 0.5 collect (+ 3 (random 0.1)) ))
(defparameter datab (loop for i from 0.001 below 100 by 1 collect (+  (* 1 (cos (* 0.3 i))) (* 1 (cos (* 0.7 i))) (* 1 (cos (* 0.1 i))) (* 1 (cos (* 0.9 i))))))
(defparameter d5 (loop for i from 0 below 10 collect (* i 4.9)))
(defparameter ll (loop for i from 0.001 below 100 by 1 collect i))
(defparameter linear-data (mapcar 'list d5 ll))
(defparameter ddata (mapcar 'list datab ll))
;;(defparameter data5 (alexandria:shuffle (append datab data)) )
(defparameter ndata (append ddata linear-data))
(defparameter new-data (alexandria:shuffle (copy-seq ndata)))
;;(defparameter ll2 (append ll0 ll0))
(defparameter ll0 (loop for i from 0 below 10 by 0.5 collect i))

;;gaussian function
(defun gauss (x &key mu sigma )
  (* (exp (- (/ (expt (- x mu) 2)
                (* 2 (expt sigma 2)))))
     (/ 1 (* sigma (sqrt (* 2 pi ))))))
(defun minus-mean (data)
  (mapcar (lambda (x) (- x (mean data))) data))
(defun mean (data)
  (/ (reduce '+ data) (length data)))
(defun std (data)
  (sqrt (/ (reduce '+ data :key (lambda (x) (expt (- x (mean data)) 2))) (length data))))
;;
(defun foo ( y a )
  (* (exp (- (/ (expt (- y a ) 2)
                (* 2 (expt 1 2)))))
     (/ 1 (* 1 (sqrt (* 2 pi ))))))
(defun foo2 ( y l a )
  (* (exp (- (/ (expt (- y (* a (cos (* 3 l)))  ) 2)
                (* 2 (expt 1 2)))))
     (/ 1 (* 1 (sqrt (* 2 pi ))))))
(defun foo3 (y l b)
  (* (exp (- (/ (expt (- y (* b l)  ) 2)
                (* 2 (expt 1 2)))))
     (/ 1 (* 1 (sqrt (* 2 pi ))))))
(defun foo4 (x a1 w1 a2 w2 a3 w3 a4 w4 )
  (+ (* a1 (cos (* w1 x))) (* a2 (cos (* w2 x))) (* a3 (cos (* w3 x))) (* a4 (cos (* w4 x)))))
(defun likelihood (  a b q r)
  (reduce '* (mapcar (lambda ( d)
                       (let ((f (first d)) (s (second d)))
                         (+ (* q (foo2 f s a )) (* r (foo3 f s b)))))
                     ndata)))
(defun lkl ( a1  a2 a3 a4 w1  w2  w3  w4 )
  (* (exp (- 
           (/ (reduce '+ (mapcar (lambda (d ti)
                                   (expt (- d    (foo4 ti a1 w1 a2 w2 a3 w3 a4 w4))  2)) 
                                 datab ll))
              (* 2 (expt 1 2)))))
     (expt  (/ 1 (* 1  (* 2 pi))) (/ (length ll)  2)) ))
(defun  outa1 (a2 a3 a4 w1 w2 w3 w4 )
  (loop for i from 0.01 below 10 by 0.1 sum (* 0.1 0.1 (lkl i a2 a3 a4 w1 w2 w3 w4))))
(defun  outa2 ( a3 a4 w1  w2 w3 w4 )
  (loop for i from 0.01 below 10 by 0.1 sum (* 0.1 0.1 (outa1 i a3 a4 w1 w2 w3 w4))))
(defun  outa3 ( a4 w1 w2 w3 w4 )
  (loop for i from 0.01 below 10 by 0.1 sum (* 0.1 0.1 (outa2 i a4 w1 w2 w3 w4))))
(defun  outa4 (w1 w2 w3 w4 )
  (loop for i from 0.01 below 10 by 0.1 sum (* 0.1 0.1 (outa3  i w1 w2 w3 w4))))
(defun  outw1 (w2 w3 w4 )
  (loop for i from 0.01 below 1 by 0.1 sum (* 1 0.1 (outa4 i w2 w3 w4))))
(defun outw2 (w3 w4)
  (loop for i from 0.001 below 1 by 0.1 sum (* 1 0.1 (outw1 i w3 w4))))
(defun outw3 (w4 )
  (loop for i from 0.001 below 1 by 0.1 sum (*  1 0.1 (outw2 i w4 ))))
(defun outphi (w )
  (loop for i from 0.001 below 1 by 0.1 sum (*  0.0001 0.1 (lkl w i))))
(defun prop2 (w)
  (* (/ 1 10) (outw3 w)))
(defun mresults (lo hi &optional (step 0.5))
  (let* ((z (loop for i from lo below hi by step sum (* step (prop2 (+ i 0))  )))
         (mean (/ (loop for i from lo below hi by step sum (* step i (prop2 i))) z)))
    mean))
;;













(defun lkl2 (  a  b)
  (* (exp (- 
           (/ (reduce '+ (mapcar (lambda (d ti)
                                   (expt (- d    (+   (* b b ti) (*  a ti)))  2)) 
                                 data0   ll0))
              (* 2 (expt 1 2)))))
     (expt  (/ 1 (* 1  (* 2 pi)))  (/ (length ll0) 2)) ))
(defun  lkl3 (a )
  (loop for i from 0.001 below 10 by 0.5 sum (* 0.5 0.1 (lkl2 a  i))))

(defun prop (a b f)
  (* (/ 1 10) (+ (* f (lkl a)) (* (- 1 f) (lkl2 b)))))
(defun prop0 (a b)
  (loop for i from 0.001 below 1.001 by 0.1 sum (* 0.1 (prop a b i))))
;;
(defun power-set (lst)
  (if (eq nil lst) '(())
      (let ((power-set-cdr (power-set (cdr lst))))
        (append power-set-cdr
                (mapcar #'(lambda (subset) (cons (car lst) subset))
                        power-set-cdr)))))
(defun pset (list)
  (remove-if-not #'consp (power-set list)))
