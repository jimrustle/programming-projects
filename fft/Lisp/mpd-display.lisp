(ql:quickload "lispbuilder-sdl")
(ql:quickload "bordeaux-fft")

;(proclaim '(optimize (speed 3) (safety 0)))

(defparameter *file* (open "/tmp/mpd.fifo" :element-type '(signed-byte 16)))
(defparameter *signal* 0)
(defparameter *default-colour* sdl:*red*)

(defun get-signal ()
  (let ((vector (make-array 512 :initial-element 0.0 :element-type '(float))))
    (read-sequence vector *file*)
    (map 'vector (lambda (x) (+ (floor (/ x 256.0)) 128)) vector)))

(defun make-fft-vec (signal-vector)
  (map 'vector #'abs (bordeaux-fft:sfft signal-vector)))

(defun normalize (fft-vector)
  (let ((maximum (reduce #'max fft-vector :start 1)))
    (if (< 0 maximum)
        (map 'vector (lambda (x) (floor (* (/ x maximum) 256))) fft-vector))))

(defun draw-line (signal-vector)
  (dotimes (i 511)
    (sdl:draw-line-* i (aref signal-vector i) (+ i 1) (aref signal-vector (+ i 1))
                     :color *default-colour* :clipping nil)))

(defun draw-fft (draw-fft-vector)
  (do ((i 1 (1+ i))) ((= i 256))
    (let ((y (aref draw-fft-vector i)))
      (sdl:draw-rectangle-* (* 2 i) (- 512 y) 2 y :color *default-colour*))))

(defun mpd-display ()
  (sdl:with-init ()
    (sdl:window 512 512 :title-caption "Common Lisp MPD Visualiser")
    (setf (sdl:frame-rate) 60)

    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:KEY key)
                       (when (sdl:KEY= key :SDL-KEY-q)
                         (progn
                           (sdl:push-quit-event)
                           (close *file*))))
      (:idle ()
        (sdl:clear-display sdl:*white*)
        (draw-line (get-signal))
        (draw-fft (normalize (make-fft-vec (get-signal))))
        (sdl:update-display)))))
