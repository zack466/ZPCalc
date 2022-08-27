;; ALL OF THE CREDIT FOR ALL THIS CODE GOES TO Yang Xiaofeng
;; Taken from https://github.com/nakrakiiya/cl-prime-maker
;; I just wanted to avoid adding quicklisp as a dependency

(in-package :zpcalc/util)

;; Many applications require the use of large prime numbers. The this program can be used to generate large primes and for primality testing.
;; only big enough numbers are supported

(declaim (optimize (speed 3)))

;; for small prime numbers
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-prime-list-for-range (maximum)
    (declare (type fixnum maximum))
    (let ((result-array (make-array (list (1+ maximum)) :initial-element t)))
      ;; init for 0, 1
      (setf (aref result-array 0) nil)
      (setf (aref result-array 1) nil)
      ;; process the rest
      (loop
         for base-num from 2 below (1+ (floor maximum 2))
         do
           (let ((n (* base-num 2)))
             (loop
                (if (<= n maximum)
                    (progn
                      (setf (aref result-array n) nil)
                      (incf n base-num))
                    (return)))))
      result-array)))

(defparameter +primes-below-65535+ #.(make-prime-list-for-range 65535))

(defun pow (a b m)  
  "Computes V = (A^B) mod M. It's much faster than (mod (expt a b) m)."
  (declare (type integer a b m))
  (cond
    ((= b 1)
     (mod a m))
    ((= b 2)
     (mod (* a a) m))
    (t
     (let* ((b1 (truncate (/ b 2)))
            (b2 (- b b1))
            ;; B2 = B1 or B1+1
            (p (pow a b1 m)))
       (if (= b2 b1)
           (mod (* p p) m)
           (mod (* p p a) m))))))

;; random:uniform
(declaim (inline random-uniform))
(defun random-uniform (n)
  (declare (type integer n))
  (1+ (random n)))

;; new_seed
(declaim (inline new-seed))
(defun new-seed ()
  (setq *random-state* (make-random-state t)))

;(declaim (inline make/2))
(defun make/2 (n d)
  (declare (type integer n d))
  (if (= n 0)
      d
      (make/2 (1- n) (+ (* 10 d) (1- (random-uniform 10))))))

(defun make (n)
  "make(n) -> I: Generates a random integer I with N decimal digits. "
  (new-seed)
  (make/2 n 0))

;; Fermat's little theorem states that if N is prime then A^N mod N = A. So
;; to test if N is prime we choose some random A which is less than N and
;; compute A^N mod N. If this is not equal to A then N is definitely not a
;; prime. If the test succeeds then A might be a prime (certain composite
;; numbers pass the Fermat test, these are called pseudo-primes), if we
;; perform the test over and over again then the probability of mis-classifying
;; the number reduces by roughly one half each time we perform the test. After
;; (say) one hundred iterations the probability of mis-classifying a number
;; is approximately 2^-100. So we can be fairly sure that the classification
;; is correct.

;(declaim (inline primep/2 primep/3))
(defun primep/3 (ntest n len)
  (declare (type integer ntest n len))
  (if (= ntest 0)
      t
      (let* ((k (random-uniform len))
             ;; A is a random number less than N
             (a (make k)))
        (if (< a n)
            (when (= a
                     (pow a n n))
              (primep/3 (1- ntest) n len))
            (primep/3 ntest n len)))))

(defun primep/2 (d ntests)
  (declare (type integer d ntests))
  (let ((n (1- (length (write-to-string d)))))
    (primep/3 ntests d n)))

(defun primep (n)
  "Tests if N is a prime number. Returns T if N is a prime number. Returns NIL otherwise.
NOTES:
* If n <= 65535, the detection of whether a number is prime can always get the correct answer.
* If n > 65535, the detection of whether a number is prime is based on the Fermat's little theorem.
"
  (declare (type integer n))
  (if (<= n 1)
      nil
      (if (<= n 65535)
          (aref +primes-below-65535+ n)
          (progn (new-seed)
                 (primep/2 n 100)))))

;(declaim (inline make-prime/2))
(defun make-prime/2 (k p)
  (if (= k 0)
      (error "impossible")
      (if (primep p)
          p
          (make-prime/2 (1- k) (1+ p)))))

(defun make-prime (k)
  "Generates a random prime P with at least K decimal digits. Returns nil when k <= 0. Returns NIL otherwise. K should be an INTEGER. "
  (declare (type integer k))
  (when (> k 0)
    (new-seed)
    (let ((n (make k)))
      (if (> n 3)
          (let* ((max-tries (- n 3))
                 (p1 (make-prime/2 max-tries (1+ n))))
            p1)
          (make-prime k)))))


;; Generating the Nth prime. (S.M.Ruiz 2000)
;; According to http://zh.wikipedia.org/wiki/%E7%B4%A0%E6%95%B0%E5%85%AC%E5%BC%8F  .

(defvar *ruiz-pis* (make-hash-table))   ; cache pi(k)
(defvar *ruiz-pis-part1* (make-hash-table)) ; cache a sub-part of pi(k)
(defvar *ruiz-results* (make-hash-table))   ; cache the results of p(n)

(defun compute-ruiz-pis-part1 (j)
  (let ((result-from-hash (gethash j *ruiz-pis-part1*)))
    (if (null result-from-hash)
        (let* ((s-max (floor (sqrt j)))
               (sigma1 (loop
                          for s from 1 to s-max
                          summing
                            (- (floor (/ (1- j) s))
                               (floor (/ j s)))))
               (result (floor (* (/ 2 j)
                                 (1+ sigma1)))))
          (setf (gethash j *ruiz-pis-part1*) result)
          result)              
        result-from-hash)))

(defun compute-ruiz-pi (k)
  (let ((result-from-hash (gethash k *ruiz-pis*)))
    (if (null result-from-hash)
        (let ((result (cond
                        ((= k 1) 0)
                        ((= k 2)
                         (1+ (compute-ruiz-pis-part1 2)))
                        (t (+ 1
                              (compute-ruiz-pis-part1 k)
                              (compute-ruiz-pi (1- k)))))))
          (setf (gethash k *ruiz-pis*) result)
          result)
        result-from-hash)))

(defun get-nth-prime (n)
  (declare (type integer n))
  "Generate the Nth prime number when N >= 1. Otherwise this function always returns 2."
  (if (>= n 1)
      (let ((result-from-hash (gethash n *ruiz-results*)))
        (if (null result-from-hash)
            (let ((result (1+ (loop
                                 for k from 1 to (* 2 (1+ (floor (* n (log n)))))
                                 summing (- 1 (floor (/ (compute-ruiz-pi k)
                                                        n)))))))
              (setf (gethash n *ruiz-results*) result)
              result)
            result-from-hash))
      2))
