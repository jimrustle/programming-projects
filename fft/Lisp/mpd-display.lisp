(ql:quickload "lispbuilder-sdl")
(ql:quickload "bordeaux-fft")
(ql:quickload "cl-opengl")

;(proclaim '(optimize (speed 0) (safety 3) (debug 3)))

(defparameter *default-colour* sdl:*red*)
(defparameter *spectro-array* (make-array '(256 512) :initial-element 0.0f0))

(defmacro do-i-range (start increment end &rest body)
  `(do ((i ,start ,increment)) (,end) ,@body))

(defun get-signal (file)
  (let ((vector (make-array 1024 :initial-element 0.0 :element-type '(float))))
    (read-sequence vector file)
    (map 'vector (lambda (x) (+ (floor (/ x 256.0)) 128)) vector)))

;
; Abstract these two normalizations
;
(defun normalize (fft-vector)
  (let ((maximum (reduce #'max fft-vector :start 1)))
    (if (< 0 maximum)
        (map 'vector (lambda (x) (floor (* (/ x maximum) 255))) fft-vector))))

(defun normalize-gl (fft-vector)
  (let ((maximum (reduce #'max fft-vector :start 1)))
    (if (< 0.0 maximum)
        (map 'vector (lambda (x) (- 1.0 (/ x maximum))) fft-vector))))

(defun make-fft-vec (signal-vector)
  (map 'vector #'abs (bordeaux-fft:sfft signal-vector)))

; Can this be optimized?
(defun shift-spectrogram (array)
  (do ((i 1 (1+ i))) ((= i 256))
    (dotimes (n 512)
      (setf (aref array (1- i) n) (aref array i n)))))

(defun fill-end-spectrogram-fft (array signal)
  (dotimes (i 512)
    (setf (aref array 255 i) (aref signal i))))

(defun draw-spectro-gl (array)
  (dotimes (x 255)
    (gl:with-primitives :quad-strip
      (dotimes (y 255)
        (let ((colour (aref array x (1+ y))))
          (gl:color colour colour colour)
          (gl:vertex x y)
          (gl:vertex x y))))))

(defun draw-line-gl (signal-vector)
  (gl:with-primitives :line-strip
    (dotimes (i 1024)
      (gl:vertex i (aref signal-vector i)))))

(defun draw-fft-gl (draw-fft-vector)
  (do-i-range 1 (1+ i) (= i 256)
              (let ((y (+ 256 (aref draw-fft-vector i))))
                (gl:with-primitive :polygon
                  (gl:vertex (+ 511 (* i 2)) y)
                  (gl:vertex (+ 511 (* i 2)) 256)
                  (gl:vertex (+ 511 (1+ (* i 2))) 256)
                  (gl:vertex (+ 511 (1+ (* i 2))) y)))))

(defun mpd-display ()
  (let ((mpd-file (open "/tmp/mpd.fifo" :element-type '(signed-byte 16))))
    (sdl:with-init ()
      (sdl:window 1024 512
                  :opengl t
                  :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1))
                  :title-caption "Common Lisp MPD Visualiser")
      (gl:clear-color 1 1 1 0)
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (gl:ortho 0 1024 0 512 0 1)

      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event (:KEY key)
                         (when (sdl:KEY= key :SDL-KEY-q)
                           (sdl:push-quit-event)
                           (close mpd-file)))
        (:idle ()
               (gl:clear :color-buffer-bit)
               (gl:color 0 0 0)
               (draw-line-gl (get-signal mpd-file))
               (draw-fft-gl (normalize (make-fft-vec (get-signal mpd-file))))
               (shift-spectrogram *spectro-array*)
                (fill-end-spectrogram-fft *spectro-array*
                                          (normalize-gl
                                           (make-fft-vec (get-signal mpd-file))))
                (draw-spectro-gl *spectro-array*)
               (gl:flush)
               (sdl:update-display))))))
