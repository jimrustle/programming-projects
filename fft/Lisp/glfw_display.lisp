
(ql:quickload "cl-opengl")
(ql:quickload "cl-glfw3")
(ql:quickload "bordeaux-fft")

(proclaim '(optimize (speed 3) (safety 0)))

(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :q) (eq action :press))
    (glfw:set-window-should-close)))

(defun draw-line-signal (audio-signal)
  (gl:clear :color-buffer)
  (gl:color 1 0 0)
  (gl:with-primitives :line-strip
    (loop for point across audio-signal
          for xval of-type single-float from 0.0f0 to 1024.0f0
          do (gl:vertex xval point))))

(defun calc-audio-fft (audio-signal)
  (flet ((normalize (fft-vec)
           (let ((maximum (reduce #'max fft-vec :start 1)))
             (if (< 0.0d0 maximum)
                 (map 'vector (lambda (x) (/ x maximum)) fft-vec)
                 fft-vec))))
  (normalize  (map 'vector #'abs (bordeaux-fft:sfft audio-signal)))))

(defun draw-fft (draw-fft-vector)
  (loop for i from 0 to 255
    do (let ((y (+ 256 (aref draw-fft-vector i)))
             (x (* i 2)))
      (gl:with-primitive :polygon
        (gl:vertex (+ 511 x) y)
        (gl:vertex (+ 511 x) 256)
        (gl:vertex (+ 511 (1+ x)) 256)
        (gl:vertex (+ 511 (1+ x)) y)))))

(defun draw-spectrogram (spec)
  (loop for line in spec
        for x from 0 to 510
        do (gl:with-primitives :quad-strip
             (loop for colorval across line
                   for y from 256 to 511
                   do (let ((cv (- 1 colorval)))
                        (gl:color cv cv cv)
                        (gl:vertex x y)
                        (gl:vertex (1+ x) y))))))

(defun make-spectrogram-object ()
  (flet ((make-spectrogram ()
           (loop for i from 1 to 512
                 collecting (make-array 256 :initial-element 0.0d0))))
    (let ((s (make-spectrogram)))
      (lambda (fft-line) (setf s (append (cdr s) (list (make-array 256 :displaced-to fft-line))))
        s))))

(defun get-audio-signal (file)
  (flet ((shift-mul (v)
           (declare (type (signed-byte 16) v))
           (the (signed-byte 16) (+ (ash v -8) 128))))
    (let ((signal-vec (make-array 1024 :initial-element 0
                                  :element-type '(signed-byte 16)
                                  :fill-pointer nil)))
      (read-sequence signal-vec file)
      (map 'vector #'shift-mul signal-vec))))

(defun init-gl ()
  (gl:viewport 0 0 1024 512)
  (gl:clear-color 1 1 1 0)
  (gl:ortho 0 1024 0 512 0 1))

(defun main ()
   (glfw:with-init-window (:title "Common Lisp MPD Visualiser" :width 1024 :height 512)
    (glfw:set-key-callback 'key-callback)
    (init-gl)
    (with-open-stream (mpd-file (open "/tmp/mpd.fifo" :element-type '(signed-byte 16)))
      (let ((s (make-spectrogram-object)))
        (loop until (glfw:window-should-close-p)
              do (let* ((audio-signal (get-audio-signal mpd-file))
                        (fft-vec (calc-audio-fft audio-signal)))
                   (draw-line-signal audio-signal)
                   (draw-fft (map 'vector (lambda (v) (* 256 v)) fft-vec))
                   (draw-spectrogram (funcall s fft-vec)))
              do (glfw:swap-buffers)
              do (glfw:poll-events))))))
