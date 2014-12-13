
module Main where
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.Raw

import System.Exit (exitSuccess)
import Control.Monad (zipWithM_, unless)

import System.IO
import Data.ByteString (hGet, unpack)
import Data.Word

import Data.Sequence
import Math.FFT.Base (dftRC)
import Data.Array.CArray (array, elems)
import Data.Array.CArray.Base
import Data.Complex
import Data.Foldable (toList)

everySecond :: [a] -> [a]
everySecond (_:a:rest) = a : everySecond rest
everySecond _ = []

scale :: [Word8] -> [Word8]
scale = map (\x -> (x + 128) `mod` 255)

initGL :: GLFW.Window -> IO ()
initGL _ = do
  glHint gl_DONT_CARE gl_FASTEST
  glClearColor 1.0 1.0 1.0 0.0
  glOrtho 0.0 1024.0 0.0 512.0 0.0 1.0

shutdown :: Handle -> GLFW.Window -> IO ()
shutdown stream win = do
  GLFW.destroyWindow win
  GLFW.terminate
  hClose stream
  putStrLn "Program quit."
  exitSuccess
  return ()

generateSpectrogram :: Seq [Double]
generateSpectrogram =
  Data.Sequence.replicate 512 $ Prelude.replicate 256 0

drawLineSignal :: [Double] -> IO ()
drawLineSignal signal = do
    glBegin gl_LINE_STRIP
    zipWithM_ glVertex2f [1.0 .. 1024.0] $ map realToFrac signal
    glEnd

drawFFT :: [Double] -> IO ()
drawFFT fftSignal = do
  let drawRect x y = do
        glBegin gl_QUADS
        glColor3f 1 0 0
        let xnew = 2 * x + 512
        let ynew = y + 256
        glVertex2f xnew ynew
        glVertex2f xnew 256
        glVertex2f (xnew + 1) 256
        glVertex2f (xnew + 1) ynew
        glEnd

  zipWithM_ (\x y -> drawRect x $ min 256 y)
    [1 .. 256]
    $ map (\x -> realToFrac x / 20) fftSignal

drawSpectrogram :: Seq [Double] -> IO ()
drawSpectrogram spectrogram = do
  let drawSpecLine x l =
        let normalize line =
              let m = 0.0625 * maximum line in
              if m > 2 then map (/ m) line else line in
         let line = normalize l in do
          glBegin gl_QUAD_STRIP
          zipWithM_ (\y intensity ->
                      let colorval = 1 - realToFrac intensity
                          ypos = y + 256 in do
                       glColor3f colorval colorval colorval
                       glVertex2f x ypos
                       glVertex2f (x + 1) ypos)
                    [1 .. 256]
                    line
          glEnd
  zipWithM_ drawSpecLine [1 .. 512] $ toList spectrogram

keyPressed :: GLFW.KeyCallback
keyPressed win GLFW.Key'Q _ GLFW.KeyState'Pressed _ = GLFW.setWindowShouldClose win True
keyPressed _   _          _ _                     _ = return ()

mainDrawLoop :: Seq [Double] -> Handle -> GLFW.Window -> IO ()
mainDrawLoop spectrogram stream win = do
  shouldClose <- GLFW.windowShouldClose win
  unless shouldClose $ do
    glClear $ fromIntegral gl_COLOR_BUFFER_BIT
    glColor3f 1 0 0
    contents <- hGet stream 2048
    let signal = map fromIntegral . scale $ everySecond $ unpack contents
    let fftIn = array (1, 1024) $ Prelude.zip [1 .. 1024] signal :: CArray Int Double
    let fftSignal = map (realPart . abs) $ elems $ dftRC fftIn
    let newSpectrogram = case viewl spectrogram of
                              _ :< end -> (|>) end fftSignal

    drawLineSignal signal
    drawFFT fftSignal
    drawSpectrogram newSpectrogram

    GLFW.pollEvents
    GLFW.swapBuffers win
    mainDrawLoop newSpectrogram stream win

main :: IO ()
main = do
     True <- GLFW.init
     Just win <- GLFW.createWindow 1024 512 "Haskell MPD Visuaizer"
                 Nothing Nothing
     GLFW.makeContextCurrent (Just win)

     stream <- openFile "/tmp/mpd.fifo" ReadMode
     GLFW.setKeyCallback win (Just keyPressed)

     initGL win
     let s = generateSpectrogram
     mainDrawLoop s stream win

     shutdown stream win
