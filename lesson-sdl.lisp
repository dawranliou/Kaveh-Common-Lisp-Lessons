(defpackage #:lesson-01
  (:use :cl))

(in-package #:lesson-01)

(require :sdl2)
(require :sdl2kit)
(require :cl-opengl)

;;;; SDL setup =================================================================

(defclass game-window (kit.sdl2:gl-window)
  ((scene :accessor scene :initarg :scene :initform nil)))

(defmethod initialize-instance :after ((w game-window) &key &allow-other-keys)
  (setf (kit.sdl2:idle-render w) nil)
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :flat)
  (gl:clear :color-buffer)
  ;; (gl:matrix-mode :projection)
  ;; (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod kit.sdl2:render ((w game-window))
  (gl:clear-color 0 0 0 0)
  (gl:clear :color-buffer)
  (with-slots (scene) w
    (when scene
      (draw scene))))

(defmethod kit.sdl2:keyboard-event :after ((window game-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (when (eq :scancode-escape scancode)
      (kit.sdl2:close-window window))))

(defun redraw ()
  (kit.sdl2:render *window*))

;;;; transform ==========================================================

(defclass transform ()
  ((translate :accessor translate :initarg :translate :initform (p! 0.0 0.0))
   (rotate :accessor rotate :initarg :rotate :initform 0.0)
   (scale :accessor scale :initarg :scale :initform (p! 1.0 1.0))))

(defmethod (setf rotate) (val (self transform))
  (setf (slot-value self 'rotate) (coerce val 'single-float)))

(defmethod translate-by ((self transform) (p point))
  (setf (translate self) (p+ (translate self) p)))

(defmethod rotate-by ((self transform) (r number))
  (setf (rotate self) (+ (rotate self) r)))

(defmethod scale-by ((self transform) (p point))
  (setf (scale self) (p* (scale self) p)))

(defmethod scale-by ((self transform) (s number))
  (setf (scale self) (p* (scale self) s)))

(defmethod reset-transform ((self transform))
  (setf (translate self) (p! 0.0 0.0))
  (setf (rotate self) 0.0)
  (setf (scale self) (p! 1.0 1.0)))

(defmethod print-object ((self transform) stream)
  (print-unreadable-object (self stream :type t :identity nil)
    (format stream ":TRANSLATE ~a :ROTATE ~a :SCALE ~a" (translate self) (rotate self) (scale self))))

;;;; shape =====================================================================

(defclass shape ()
  ((transform :accessor transform :initarg :transform :initform (make-instance 'transform))
   (show-axis? :accessor show-axis? :initarg :show-axis? :initform nil)))

(defmethod draw ((self shape))
  ;; subclass responsibility
  )

(defmethod translate-by ((self shape) (p point))
  (translate-by (transform self) p)
  self)

(defmethod rotate-by ((self shape) (r number))
  (rotate-by (transform self) r)
  self)

(defmethod scale-by ((self shape) (p point))
  (scale-by (transform self) p)
  self)

(defmethod scale-by ((self shape) (s number))
  (scale-by (transform self) s)
  self)

(defmethod reset-transform ((self shape))
  (reset-transform (transform self))
  self)

(defmethod draw :before ((self shape))
  (let ((xform (transform self)))
    (gl:push-matrix)
    (gl:translate (x (translate xform)) (y (translate xform)) 0.0)
    (gl:rotate (rotate xform) 0.0 0.0 1.0)
    (gl:scale (x (scale xform)) (y (scale xform)) 1.0)))

(defmethod draw :after ((self shape))
  (when (show-axis? self)
    (draw-axis self))
  (gl:pop-matrix))

(defmethod draw-axis ((self shape))
  (gl:line-width 3.0)
  (gl:begin :lines)
  ;; x axis (red)
  (gl:color 0.8 0.2 0.2)
  (gl:vertex 0.0 0.0 0.0)
  (gl:vertex 0.25 0.0 0.0)
  ;; y axis (green)
  (gl:color 0.2 0.8 0.2)
  (gl:vertex 0.0 0.0 0.0)
  (gl:vertex 0.0 0.25 0.0)
  (gl:end))

(defmacro for-scene-shapes (func)
  `(mapcar ,func (shapes *scene*)))

;;;; scene =====================================================================

(defclass scene ()
  ((shapes :accessor shapes :initarg :shapes :initform '())))

(defmethod add-shape ((self scene) (s shape))
  (push s (shapes self))
  s)

(defmethod clear-shapes ((self scene))
  (setf (shapes self) '()))

(defmethod draw ((self scene))
  (dolist (s (shapes self))
    (draw s)))

;;;; run graphics ==============================================================

(defvar *scene* (make-instance 'scene))
(defvar *window* nil)

(kit.sdl2:define-start-function run (&key (w 512) (h 512))
  (setf *scene* (make-instance 'scene))
  (setf *window* (make-instance 'game-window :scene *scene* :w w :h h)))

;; (run)

;;;; utils =====================================================================

(defun rand2 (a b)
  (if (= a b)				;doesn't like (random 0)
      a
      (let ((lo (min a b))
            (hi (max a b))
            (*random-state* (make-random-state t)))
        (+ lo (random (coerce (- hi lo) 'float))))))

(defun rand1 (a)
  (rand2 (- a) a))

(defun strcat (&rest strings)
  (apply #'concatenate 'string strings))

(defun symcat (&rest syms)
  (intern (apply #'concatenate 'string (mapcar #'symbol-name syms))))

;;;; points ====================================================================

(defclass point ()
  ((x :accessor x :initarg :x :initform 0.0)
   (y :accessor y :initarg :y :initform 0.0)))

;;; >>> setf with coerce
(defmethod (setf x) (val (self point))
  (setf (slot-value self 'x) (coerce val 'single-float)))

(defmethod (setf y) (val (self point))
  (setf (slot-value self 'y) (coerce val 'single-float)))

(defmethod print-object ((self point) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "[~a, ~a]" (x self) (y self))))

(defun p! (x y)
  (make-instance 'point :x (coerce x 'single-float)
                        :y (coerce y 'single-float)))

(defmethod p+ ((p1 point) (p2 point))
  (p! (+ (x p1) (x p2))
      (+ (y p1) (y p2))))

(defmethod p* ((p1 point) (p2 point))
  (p! (* (x p1) (x p2))
      (* (y p1) (y p2))))

(defmethod p* ((p1 point) (s number))
  (p! (* (x p1) s)
      (* (y p1) s)))

(defun p-mag (p)
  (sqrt (+ (* (x p) (x p)) (* (y p) (y p)))))

(defun p-dist (p1 p2)
  (sqrt (+ (expt (- (x p2) (x p1)) 2)
           (expt (- (y p2) (y p1)) 2))))

;;; polygonal shape class ======================================================

(defclass polygon-shape (shape)
  ((is-closed-shape? :accessor is-closed-shape? :initarg :is-closed-shape? :initform t)
   (points :accessor points :initarg :points :initform '())))

(defmethod add-point ((self polygon-shape) (p point))
  (push p (points self)))

(defmethod draw ((self polygon-shape))
  (gl:color 1.0 1.0 1.0)
  (gl:line-width 3.0)
  (if (is-closed-shape? self)
      (gl:begin :line-loop)
      (gl:begin :line-strip))
  (dolist (p (points self))
    (gl:vertex (x p) (y p) 0.0))
  (gl:end))

(defun make-square-shape (length)
  (let ((v (/ length 2.0)))
    (make-instance 'polygon-shape
                   :points (list (p!    v     v )
                                 (p!    v  (- v))
                                 (p! (- v) (- v))
                                 (p! (- v)    v )))))

(defmacro with-redraw (&body body)
  `(let ((result (progn ,@body)))
     (redraw)
     result))

#|
(defparameter *sq* (make-square-shape 1.0))

(defmacro with-redraw (&body body)
  `(let ((result (progn ,@body)))
     (redraw)
     result))

(with-redraw
  (add-shape *scene* *sq*))

(with-redraw
  (clear-shapes *scene*))

(with-redraw
  (dolist (shape (shapes *scene*))
    (randomize-points shape (p! 0.05 0.05))))

|#
;;;; generating shapes =========================================================
(defun make-circle-shape (diameter &optional (num-points 64))
  (let ((radius (/ diameter 2.0))
        (angle-delta (/ (* 2 pi) num-points))
        (shape (make-instance 'polygon-shape)))
    (dotimes (i num-points)
      (let ((angle (* i angle-delta)))
        (add-point shape (p! (* (sin angle) radius) (* (cos angle) radius)))))
    shape))

#|
(with-redraw
  (clear-shapes *scene*)
  (add-shape *scene* (make-circle-shape 1.0)))
|#

(defmacro with-clear-and-redraw (&body body)
  `(progn
     (clear-shapes *scene*)
     (prog1 (progn ,@body)
       (redraw))))

(defmacro def-polygon-shape (name num)
  `(defun ,(symcat 'make- name '-shape) (size)
     (make-circle-shape size ,num)))

(def-polygon-shape eql-tri 3)		;make-eql-tri-shape
(def-polygon-shape diamond 4)		;make-diamond-shape
(def-polygon-shape pentagon 5)		;make-pentagon-shape
(def-polygon-shape hexagon 6)		;make-hexagon-shape
(def-polygon-shape heptagon 7)		;make-heptagon-shape
(def-polygon-shape octagon 8)		;make-octagon-shape

(defun make-sine-curve-shape (&optional (num-segments 64))
  (let ((angle-delta (/ (* 2 pi) num-segments))
        (shape (make-instance 'polygon-shape :is-closed-shape? nil)))
    (dotimes (i (1+ num-segments))
      (let ((angle (* i angle-delta)))
        (add-point shape (p! (/ angle (* 2 pi)) (sin angle)))))
    shape))

(defmacro def-trig-curve-shape (func-name func)
  `(defun ,(symcat 'make- func-name '-curve-shape) (&optional (num-points 64))
     (let ((angle-delta (/ (* 2 pi) (- num-points 1)))
           (shape (make-instance 'polygon-shape :is-closed-shape? nil)))
       (dotimes (i num-points)
         (let ((angle (* i angle-delta)))
           (add-point shape (p! (/ angle (* 2 pi)) (,func angle)))))
       shape)))

(defun make-spiral-shape (diameter &optional (num-points 64) (num-loops 1.0))
  (let ((radius-delta (/ (/ diameter 2.0) (1- num-points)))
        (angle-delta (/ (* 2 pi) num-points))
        (shape (make-instance 'polygon-shape :is-closed-shape? nil)))
    (dotimes (i num-points)
      (let ((radius (* i radius-delta))
            (angle (* i angle-delta num-loops)))
        (add-point shape (p! (* (sin angle) radius)
                             (* (cos angle) radius)))))
    shape))

(defmethod randomize-points ((self polygon-shape) (delta point))
  (setf (points self)
        (mapcar (lambda (p)
                    (let ((offset (p! (rand1 (x delta)) (rand1 (y delta)))))
                      (p+ p offset)))
                (points self))))

(defun lerp (f lo hi)
  (+ lo (* f (- hi lo))))

(defclass marker-shape (shape)
  ((size :accessor size :initarg :size :initform 0.1)))

(defmethod draw ((self marker-shape))
  (gl:color 1.0 0.0 0.0)
  (gl:line-width 3.0)
  (gl:begin :lines)
  (let ((s (/ (size self) 2.0)))
    (gl:vertex 0.0     s  0.0)
    (gl:vertex 0.0  (- s) 0.0)
    (gl:vertex    s  0.0  0.0)
    (gl:vertex (- s) 0.0  0.0))
  (gl:end))

(defun make-shapes-at-points (shape-fn points)
  (dolist (p points)
    (let ((s (funcall shape-fn)))
      (translate-to s p)
      (add-shape *scene* s))))

(defun scatter-shapes (shape-fn points)
  (mapc (lambda (p)
          (translate-to (add-shape *scene* (funcall shape-fn)) p))
        points))

(defun grid-points (nx ny bounds-lo bounds-hi)
  (let ((points '()))
    (dotimes (i nx)
      (let* ((fx (/ i (- nx 1.0)))
             (x (lerp fx (x bounds-lo) (x bounds-hi))))
        (dotimes (j ny)
          (let* ((fy (/ j (- ny 1.0)))
                 (y (lerp fy (y bounds-lo) (y bounds-hi))))
            (push (p! x y) points)))))
    points))

(defun random-points (n bounds-lo bounds-hi)
  (loop for i from 1 to n
        collect (p! (rand2 (x bounds-lo) (x bounds-hi))
                    (rand2 (y bounds-lo) (y bounds-hi)))))

(defun print-spaces (num)
  (dotimes (i num)
    (princ " ")))

;;;; group ==============================================================

(defclass group (shape)
  ((children :accessor children :initarg :children :initform '())))

(defmethod add-child ((self group) (s shape))
  (push s (children self))
  s)

(defmethod draw ((self group))
  (mapc #'draw (children self)))

(defun make-group (&rest shapes)
  (make-instance 'group :children shapes))

(defmethod print-hierarchy ((self scene) &optional (indent 0))
  (print-spaces indent)
  (format t "~a~%" self)
  (dolist (shape (shapes self))
    (print-hierarchy shape (+ indent 2))))

(defmethod print-hierarchy ((self shape) &optional (indent 0))
  (print-spaces indent)
  (format t "~a~%" self))

(defmethod print-hierarchy :after ((self group) &optional (indent 0))
  (dolist (child (children self))
    (print-hierarchy child (+ indent 2))))

;;; show polygon-shape number of points
(defmethod print-object ((self polygon-shape) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "[~a]" (length (points self)))))

;;; traverse hierarchy and execute function
(defmethod do-hierarchy ((self shape) func &key (test nil))
  (when (or (null test) (funcall test self))
    (funcall func self))
  self)

(defmethod do-hierarchy :after ((self group) func &key (test nil))
  (dolist (child (children self))
    (do-hierarchy child func :test test))
  self)

(defun make-row-group (n spacing shape)
  (make-instance 'group
                 :children (loop for i to n
                                 collect (make-instance
                                          'group
                                          :children (list shape)
                                          :transform (make-instance 'transform
                                                                    :translate (p* spacing (p! i i)))))))

(defun scatter-group (shape-fn points)
  (make-instance 'group :children (mapcar (lambda (p)
                                            (translate-to (funcall shape-fn) p))
                                          points)))

(defun make-hex-tri-group (levels)
  (scale-to (scatter-group (lambda ()
                             (if (= 0 levels)
                                 (make-hexagon-shape 1.0)
                                 (make-hex-tri-group (- levels 1))))
                           (list (p! -.5 -.433)
                                 (p!  .5 -.433)
                                 (p!   0  .433)))
            0.5))

(defun make-square-cross-group (levels)
  (scale-to (scatter-group (lambda ()
                             (if (= 0 levels)
                                 (make-square-shape 0.8)
                                 (make-square-cross-group (- levels 1))))
                           (list (p!  0.0  0.0)
                                 (p! -1.0  0.0)
                                 (p!  1.0  0.0)
                                 (p!  0.0 -1.0)
                                 (p!  0.0  1.0)
                                 (p! -2.0  0.0)
                                 (p!  2.0  0.0)
                                 (p!  0.0 -2.0)
                                 (p!  0.0  2.0)))
            0.25))

(defun make-square-x-group (levels)
  (scale-to (scatter-group (lambda ()
                             (if (= 0 levels)
                                 (make-square-shape 1.5)
                                 (make-square-x-group (- levels 1))))
                           (list (p! -1.0 -1.0)
                                 (p!  1.0  1.0)
                                 (p!  1.0 -1.0)
                                 (p! -1.0  1.0)
                                 (p! -2.0 -2.0)
                                 (p!  2.0  2.0)
                                 (p!  2.0 -2.0)
                                 (p! -2.0  2.0)))
            0.25))


;;; generalized recursive group function
(defun make-recursive-group (levels base-shape-fn points scaling)
  (scale-to (scatter-group (lambda ()
                             (if (= 0 levels)
                                 (funcall base-shape-fn)
                                 (make-recursive-group (- levels 1)
                                                       base-shape-fn
                                                       points
                                                       scaling)))
                           points)
            scaling))

(defmethod is-leaf? ((self shape))
  t)

(defmethod is-leaf? ((self group))
  nil)

(defun randomize-size (shape lo hi)
  (let* ((scale (rand2 lo hi)))
    (scale-by (transform shape) scale)))

(defmethod jitter-translate ((self shape) (jitter point))
  (translate-by (transform self)
                (p! (rand1 (x jitter)) (rand1 (y jitter))))
  self)

(defmethod jitter-rotate ((self shape) (jitter number))
  (rotate-by (transform self) (rand1 jitter))
  self)

(defmethod jitter-scale ((self shape) (jitter number))
  (scale-by (transform self) (+ 1.0 (rand1 jitter)))
  self)

(defun make-recursive-group-2 (levels base-shape-fn points scaling translate rotate size)
  (let ((group (scatter-group (lambda ()
                                (if (= 0 levels)
                                    (funcall base-shape-fn)
                                    (make-recursive-group-2 (- levels 1)
                                                            base-shape-fn
                                                            points
                                                            scaling
                                                            translate
                                                            rotate
                                                            size)))
                              points)))
    (jitter-translate group translate)
    (jitter-rotate group rotate)
    (jitter-scale group size)
    (scale-by group scaling)))

(defun make-wobbly-cross (levels)
  (make-recursive-group-2 levels
                          (lambda () (make-square-shape 0.8))
                          (list (p!  0.0  0.0)
                                (p! -1.0  0.0)
                                (p!  1.0  0.0)
                                (p!  0.0 -1.0)
                                (p!  0.0  1.0)
                                (p! -2.0  0.0)
                                (p!  2.0  0.0)
                                (p!  0.0 -2.0)
                                (p!  0.0  2.0))
                          0.25
                          (p! 0.0 0.0)
                          10.0
                          0.5))

(defun randomize-leaf-sizes (group lo hi)
  (do-hierarchy group (lambda (shape) (randomize-size shape lo hi))
                      :test #'is-leaf?))

(defun randomize-node-sizes (group lo hi)
  (do-hierarchy group (lambda (shape) (randomize-size shape lo hi))))

;; (run)
