(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "ieee-floats")
  (ql:quickload "cl-binary")
  (ql:quickload "anaphora"))

(defpackage :cl-abc
  (:use :common-lisp :ieee-floats :cl-binary :anaphora))

(in-package :cl-abc)

;;; random -------------------------------------------------------
(defun get-random-val (&rest args)
  (assert (<= (length args) 2))
  (let* ((u-bound (or (and (cdr args) (second args))
                      (car args)))
         (l-bound (or (and (cdr args) (car args))
                      0))
         (base (abs l-bound))
         (max (- u-bound l-bound)))
    (- (random (coerce max 'double-float))
       base)))

(defun get-random-list (n &rest params)
  (loop for i from 1 to n collect (apply #'get-random-val params)))


;;; definitions --------------------------------------------------
(defclass bee ()
  ((position :accessor b-pos
             :initarg :pos
             :initform nil)
   (trial :accessor b-trial
          :initarg :trial
          :initform 0)))

(defclass bee-func ()
  ((func :reader bf-func
         :initarg :func)
   (value-gen :reader bf-value-gen
              :initarg :value-gen)))

(defclass bee-colony-spec ()
  ((func :reader bcs-func
         :initarg :func
         :type bee-func)
   (bees-count :reader bcs-bcount
               :initarg :bees-count)
   (max-trial :accessor bcs-max-trial
              :initarg :max-trial
              :initform nil)
   (fitness :reader bcs-fitness
            :initarg :fitness-func)
   (stop :reader bcs-stop
         :initarg :stop)))

(defvar *abc-spec* nil)
(defparameter *best-ever* nil)

;;; main functions ------------------------------------------------------
(defgeneric get-fitness (bee))
(defgeneric get-value (bee))
(defgeneric b-pos-list (bee))
(defgeneric initialize-abc (abc-spec))
(defgeneric get-probability (bee population))


(defmethod print-object ((obj bee) stream)
  (format stream "#<BEE ~A; ~A; ~A>" (get-fitness obj) (get-value obj) (b-trial obj)))

(defmethod get-value ((bee bee))
  (get-value (b-pos bee)))

(defmethod get-value ((sln vector))
  (apply (bf-func (bcs-func *abc-spec*)) (coerce sln 'list)))

(defmethod get-fitness ((bee bee))
  (get-fitness (b-pos bee)))

(defmethod get-fitness ((sln vector))
  (apply (bcs-fitness *abc-spec*) (coerce sln 'list)))

(defmethod get-probability ((bee bee) population)
  (/ (get-fitness bee)
     (reduce #'+ population
             :key #'get-fitness
             :initial-value 0)))

(defmethod b-pos-list ((bee bee))
  (coerce (b-pos bee) 'list))

(defmethod initialize-abc ((abc-spec bee-colony-spec))
  (setf (bcs-max-trial abc-spec)
        (or (bcs-max-trial abc-spec)
            (round (/ (* (bcs-bcount abc-spec)
                         (length (funcall (bf-value-gen (bcs-func abc-spec)))))
                      2))))
  abc-spec)

(defun worker-solution (population i)
  (let ((cur-sln (b-pos (aref population i))))
    (coerce
     (loop for j from 0 to (1- (length cur-sln))
        collect (let ((x (aref cur-sln j)))
                  (+ x (* (get-random-val 0 1)
                          (- x (aref (b-pos (aref population (random (length population))))
                                     j))))))
     'vector)))

(defun onlooker-choice (population)
  (let* ((random-val (get-random-val 0 1))
         (acc 0))
    (loop for i from 0 to (1- (length population))
       do (when (<= random-val (incf acc (get-probability (aref population i) population)))
            (return-from onlooker-choice i)))
    (error "Something went wrong!~%")))

(defun onlooker-solution ()
  (funcall (bf-value-gen (bcs-func *abc-spec*))))

(defun abc-search (bc-spec)
  (let* ((*abc-spec* (initialize-abc bc-spec))
         (*best-ever* (onlooker-solution))
         (population (coerce
                      (loop for i from 0 to (1- (bcs-bcount *abc-spec*))
                         collect (make-instance
                                  'bee
                                  :pos (funcall (bf-value-gen (bcs-func *abc-spec*)))))
                      'vector)))
    (labels ((%iterate ()
               ;; workers phase
               (loop for i from 0 to (1- (length population))
                  do (let ((new-sln (worker-solution population i)))
                       (if (> (get-fitness new-sln) (get-fitness (aref population i)))
                           (progn
                             (setf (b-pos (aref population i)) new-sln)
                             (setf (b-trial (aref population i)) 0))
                           (incf (b-trial (aref population i))))))
               ;; onlookers phase
               (loop for i from 0 to (1- (length population))
                  do (let ((bee-index (onlooker-choice population))
                           (new-sln (onlooker-solution)))
                       (if (> (get-fitness new-sln) (get-fitness (aref population bee-index)))
                           (progn
                             (setf (b-pos (aref population bee-index)) new-sln)
                             (setf (b-trial (aref population bee-index)) 0))
                           (incf (b-trial (aref population bee-index))))))
               ;; replace abandoned solutions
               (loop for i from 0 to (1- (length population))
                  do (when (> (b-trial (aref population i)) (bcs-max-trial *abc-spec*))
                       (when (> (get-fitness (aref population i)) (get-fitness *best-ever*))
                         (setf *best-ever* (b-pos (aref population i))))
                       (setf (b-pos (aref population i)) (onlooker-solution))
                       (setf (b-trial (aref population i)) 0)))))

      (let ((i 0))
        (loop while (not (funcall (bcs-stop *abc-spec*) i))
           do (progn (%iterate)
                     (incf i)))))

    ;; return best solution
    (let* ((top-bee (aref (sort population #'> :key #'get-fitness) 0))
           (res (if (> (get-fitness *best-ever*) (get-fitness top-bee))
                    (make-instance 'bee
                                   :pos *best-ever*)
                    top-bee)))
      (values (b-pos res) (get-value res)))))


;;; dots parse/dump -----------------------------------------------------
(defclass RGB-spec ()
  ((alpha :accessor alpha
          :initarg :alpha
          :initform 1)
   (red :accessor red
        :initarg :red
        :initform 0)
   (green :accessor green
          :initarg :green
          :initform 0)
   (blue :accessor blue
         :initarg :blue
         :initform 0)))

(defclass point ()
  ((RGB-spec :accessor point-rgb
             :initarg :rgb
             :initform (make-instance 'RGB-spec))
   (x :accessor point-x
      :initarg :x
      :initform 0)
   (y :accessor point-y
      :initarg :y
      :initform 0)
   (is-centroid :accessor is-centroid
                :initarg :is-centroid
                :initform 0)))

(defmethod print-object ((p point) stream)
  (format stream "#<POINT ~A; ~A (~A; ~A; ~A) is-centroid: ~A>"
          (point-x p) (point-y p)
          (red (point-rgb p)) (green (point-rgb p)) (blue (point-rgb p))
          (is-centroid p)))

(defun bits-int (bit-vector)
  "Create a positive integer from a bit-vector."
  (reduce #'(lambda (first-bit second-bit)
              (+ (* first-bit 2) second-bit))
          bit-vector))

(defun int-bits (integer &key size rev)
  "Create a bit-vector from a positive integer."
  (labels ((integer->bit-list (int &optional accum)
             (cond ((> int 0)
                    (multiple-value-bind (i r) (truncate int 2)
                      (integer->bit-list i (push r accum))))
                   ((null accum) (push 0 accum))
                   (t accum))))
    (let* ((bitarr (integer->bit-list integer))
           (out (or (and size
                         (append (make-list (- size (length bitarr))
                                            :initial-element 0)
                                 bitarr))
                    bitarr)))
      (coerce (or (and rev (reverse out)) out) 'bit-vector))))

(defun read-double (stream)
  (aand (read-u64 stream) (decode-float64 it)))


(defun read-point (stream)
  (let ((x (read-double stream))
        (y (read-double stream))
        (alpha (read-u32 stream))
        (r (read-u32 stream))
        (g (read-u32 stream))
        (b (read-u32 stream))
        (cnt (read-u32 stream)))
    (when (and x y alpha r g b cnt)
      (make-instance 'point
                     :x x :y y
                     :is-centroid cnt
                     :rgb (make-instance 'RGB-spec
                                         :alpha alpha
                                         :red r
                                         :green g
                                         :blue b)))))

(defun read-points (file)
  (with-open-file (stream file
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (loop for point = (read-point stream)
       while point
       collect point)))

;;; tested functions ----------------------------------------------------
(defun min-fitness-func (func &rest args)
  (let ((val (apply func args)))
    (if (>= val 0)
        (/ 1 (1+ val))
        (1+ (abs val)))))

;;; x => [-100; 100]
;;; d = 50
(defun sphere (&rest xs)
  (reduce #'+ (mapcar (lambda (x) (expt x 2)) xs)))


;;; TESTS ---------------------------------------------------------------
(defun abc-search-aux (func val-gen sn-count &optional fitness-func)
  (abc-search (make-instance 'bee-colony-spec
                             :func (make-instance 'bee-func
                                                  :func func
                                                  :value-gen val-gen)
                             :bees-count sn-count
                             :fitness-func fitness-func
                             :stop (lambda (iter) (> iter 200)))))

(defparameter *all-runs* (make-hash-table :test #'equal))
(defparameter *debug* t)

(defmacro def-run (name (&rest params) &body body)
  `(progn
     (defun ,name (,@params)
       (if *debug*
           (progn ,@body)
           (handler-case (progn ,@body)
             (error (e)
               (declare (ignore e))
               0))))
     (setf (gethash (string ',name) *all-runs*) #',name)))

(def-run sphere-run (&optional (sn-count 100))
  (abc-search-aux #'sphere
                  (lambda () (map 'vector #'identity (get-random-list 5 -100 100)))
                  sn-count
                  (lambda (&rest args) (apply #'min-fitness-func #'sphere args))))



;;; TESTS ---------------------------------------------------------------
(defmacro test-aux (&body func)
  `(time
    (multiple-value-bind (params val) (progn ,@func)
      (format t "params:~%~A~%val: ~A~%" params val))))

(defparameter *all-tests* (make-hash-table :test #'equal))

(defmacro def-test (name (&rest params) &body body)
  `(progn
     (defun ,name (,@params)
       (if *debug*
           (progn ,@body)
           (handler-case (progn ,@body)
             (error (e)
               (declare (ignore e))
               (format t "ERROR!~%")))))
     (setf (gethash (string ',name) *all-tests*) #',name)))

(def-test sphere-test ()
  (test-aux (sphere-run 50)))
