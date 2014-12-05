open Bigarray
open Complex
open Batteries

module FFT = Fftw3.S

(* source of fft_r2c from *)
(* https://github.com/Chris00/fftw-ocaml/blob/master/examples/delta.ml *)
let fft_r2c data =
  let dim = Array.length data in
  let x = FFT.Array1.create FFT.complex c_layout dim in
  let y = FFT.Array1.create FFT.complex c_layout dim in (* Fourier vars *)
  let plan = FFT.Array1.dft FFT.Forward x y in
  Array.iteri (fun i v -> x.{i} <- { re = v ; im = 0.0 }) data ;
  FFT.exec plan;
  Array.sub (Array.init dim (fun i -> y.{i})) 0 256

let read_create_array f =
  Array.init 1024 (fun _ -> (BatIO.read_i16 f) / 256 + 128)

let draw_line l =
  GlDraw.color (1.0, 0.0, 0.0);
  GlDraw.begins `line_strip;
  Array.iteri (fun idx hd -> GlDraw.vertex2 (float_of_int idx, hd)) l;
  GlDraw.ends ()

let draw_spectrogram s =
  let draw_spec_line x l =
    let normalize l =
      let maximum = Array.fold_left (fun x y -> if x > y then x else y) 0.0
      (Array.sub l 3 252) in
      if maximum > 2.0 then Array.map (fun x -> x /. maximum) l else l in
    let line = normalize l in
    GlDraw.begins `quad_strip;
    Array.iteri (fun y intensity ->
                 let colorval = 1.0 -. intensity in
                 let ypos = float_of_int(y + 256) in
                 GlDraw.color (colorval, colorval, colorval);
                 GlDraw.vertex2 (x, ypos);
                 GlDraw.vertex2 (x +. 1.0, ypos))
               line;
    GlDraw.ends () in
  BatDeque.iteri (fun x l -> draw_spec_line (float_of_int x) l) s

let draw_fft fft_signal =
  let draw_rect x y =
    GlDraw.begins `quads;
    GlDraw.color (1.0, 0.0, 0.0);

    let xnew = 2.0 *. x +. 512.0 in
    let ynew = y +. 256.0 in
    GlDraw.vertex2 (xnew, ynew);
    GlDraw.vertex2 (xnew, 256.0);
    GlDraw.vertex2 (xnew +. 1.0, 256.0);
    GlDraw.vertex2 (xnew +. 1.0, ynew);

    GlDraw.ends () in
  Array.iteri (fun i v -> draw_rect (float_of_int i) (min (v /. 20.0) 256.0))
              fft_signal

let draw signal fft_signal s =
  GlClear.clear [`color];
  draw_line signal;
  draw_fft fft_signal;
  draw_spectrogram s;
  Gl.flush ();
  Glut.swapBuffers()

let deque_generator () =
  let rec loop n res =
    if n = 512 then res else
      loop (n + 1) (BatDeque.snoc res (Array.init 256 (fun _ -> 0.0))) in
  let spectrogram = ref (loop 0 BatDeque.empty) in
  (fun fft_signal ->
    match BatDeque.front !spectrogram with
      Some (_, xs) -> spectrogram := BatDeque.snoc xs fft_signal; !spectrogram
    | None -> !spectrogram)

let () =
  ignore (Glut.init Sys.argv);
  Glut.initWindowSize 1024 512;
  Glut.initDisplayMode ~double_buffer:true ();
  ignore (Glut.createWindow "MPD Display");
  GlMat.ortho ~x:(0., 1024.0) ~y:(0., 512.0) ~z:(0., 1.);

  GlClear.color (1.0, 1.0, 1.0);

  let f = BatFile.open_in "/tmp/mpd.fifo" in
  let s = deque_generator () in
  Glut.displayFunc (fun () ->
                    let signal = Array.map (float_of_int) (read_create_array f) in
                    let mag c = sqrt(c.re *. c.re +. c.im *. c.im) in
                    let fft_signal = Array.map (mag) (fft_r2c signal) in
                    draw signal fft_signal (s fft_signal));;

  Glut.idleFunc ~cb:(Some Glut.postRedisplay);
  Glut.keyboardFunc ~cb:(fun ~key ~x ~y -> if key=113 then exit 0);
  Glut.mainLoop ()
