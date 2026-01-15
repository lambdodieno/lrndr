(defparameter *width* 150)
(defparameter *height* 150)
(defparameter *screen* (make-array (list *height* *width*) :initial-element #\Space))

(defun clear-screen-buffer ()
  (loop for y from 0 below *height* do
    (loop for x from 0 below *width* do
      (setf (aref *screen* y x) #\Space))))

(defun render-screen ()
  (format t "~C[H" #\Escape)
  (dotimes (y *height*)
    (dotimes (x *width*)
      (princ (aref *screen* y x)))
    (terpri)))

(defun draw-pixel (x y)
  (let ((ix (floor x))
        (iy (floor y)))
    (when (and (>= ix 0) (< ix *width*)
               (>= iy 0) (< iy *height*))
      (setf (aref *screen* iy ix) #\#))))

(defun draw-line (x0 y0 x1 y1)
  (let* ((x0 (round x0)) (y0 (round y0))
         (x1 (round x1)) (y1 (round y1))
         (dx (abs (- x1 x0)))
         (dy (abs (- y1 y0)))
         (sx (if (< x0 x1) 1 -1))
         (sy (if (< y0 y1) 1 -1))
         (err (- dx dy)))
    (loop
       do (draw-pixel x0 y0)
       (when (and (= x0 x1) (= y0 y1)) (return))
       (let ((e2 (* 2 err)))
         (when (> e2 (- dy))
           (decf err dy)
           (incf x0 sx))
         (when (< e2 dx)
           (incf err dx)
           (incf y0 sy))))))

(defun rotate-point (v dx dy dz)
  (let* ((x (first v)) (y (second v)) (z (third v))
       
         (y1 (- (* y (cos dx)) (* z (sin dx))))
         (z1 (+ (* y (sin dx)) (* z (cos dx))))
     
         (x2 (+ (* x (cos dy)) (* z1 (sin dy))))
        
         (x3 (- (* x2 (cos dz)) (* y1 (sin dz))))
         (y3 (+ (* x2 (sin dz)) (* y1 (cos dz))))
         
         (scale (min *width* *height*))
         (px (+ (* x3 scale 2.0) (/ *width* 2)))
         (py (+ (* y3 scale) (/ *height* 2))))
    (list px py)))

(defun split-string (str &optional (sep #\Space))
  (let ((parts '())
        (start 0))
    (loop for i from 0 below (length str) do
      (when (char= (char str i) sep)
        (push (subseq str start i) parts)
        (setf start (1+ i))))
    (push (subseq str start) parts)
    (remove "" (nreverse parts) :test #'string=)))

(defun parse-float (s)
  (with-input-from-string (in s)
    (read in)))

(defun load-obj (filename)
  (let ((vertices '())
        (faces '()))
    (with-open-file (stream filename :if-does-not-exist :error)
      (loop for line = (read-line stream nil)
            while line do
            (let ((tokens (split-string line #\Space)))
              (cond
                ((string= (car tokens) "v")
                 (push (mapcar #'parse-float (cdr tokens)) vertices))
                ((string= (car tokens) "f")
                 (push (mapcar (lambda (s)
                                 (parse-integer (car (split-string s #\/))))
                               (cdr tokens)) faces))))))
    (values (coerce (nreverse vertices) 'vector) 
            (nreverse faces))))

(defun draw-obj (vertices faces dx dy dz)
  (clear-screen-buffer)
  (let ((projected-verts (make-array (length vertices))))
    (dotimes (i (length vertices))
      (setf (aref projected-verts i) 
            (rotate-point (aref vertices i) dx dy dz)))
    
    (dolist (face faces)
      (loop for i from 0 below (length face)
            for next = (mod (1+ i) (length face))
            do (let ((v0 (aref projected-verts (1- (nth i face))))
                     (v1 (aref projected-verts (1- (nth next face)))))
                 (draw-line (first v0) (second v0) (first v1) (second v1))))))
  (render-screen))

(defun run-animation (filename)
  (multiple-value-bind (verts faces) (load-obj filename)
    (format t "~C[2J" #\Escape) 
    (loop for angle from 0 by 0.05 do
      (draw-obj verts faces (* 0.7 angle) (* 1.1 angle) (* 0.3 angle))
      (sleep 0.03))))

