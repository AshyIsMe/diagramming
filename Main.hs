module Main where

import           Codec.Picture
import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.Rasterific as DR
import           Graphics.UI.WX
import           Graphics.UI.WXCore.WxcTypes
import qualified Data.ByteString.Lazy as B

main :: IO ()
main
  = start hello

hello :: IO ()
hello = do 
  f         <- frame [text := "Hello!"]
  editor    <- textCtrl f [wrap := WrapNone]
  p         <- panel f [on paint := paintBalls]
  runButton <- button f [text := "Run", on command := run editor]
  quit      <- button f [text := "Quit", on command := close f]
  set f [layout := minsize (sz 400 300) $
                   margin 10 (column 5 [fill $ floatCentre (widget p)
                                      ,fill $ floatCentre (widget editor)
                                      ,floatCentre (widget runButton)
                                      ,floatCentre (widget quit)
                                      ])]

run :: Textual w => w -> IO ()
run e = do
  t <- get e text
  putStrLn t

ball :: Bitmap ()
ball = bitmap "ball.bmp"


paintBalls :: DC a -> Rect -> IO ()
paintBalls  dc viewArea
  = do set dc [brushColor := red, brushKind := BrushSolid]
       drawBitmap dc ball (Point 10 10) True []
       drawBitmap dc ball (Point 20 20) True []
       drawBitmap dc ball (Point 30 30) True []
       testcircle <- imageCreateFromPixels (Size 50 50) testcircle
       drawImage dc testcircle (Point 50 30) []

c :: D.Diagram DR.B D.R2
c = D.circle 4
testimage :: Image PixelRGBA8
testimage = D.renderDia DR.Rasterific (DR.RasterificOptions (D.Width 50)) c
testbitmap :: B.ByteString
testbitmap = encodeBitmap testimage
{-testcircle :: IO (Graphics.UI.WXCore.WxcClassTypes.Image ())-}
{-testcircle = imageCreateFromPixels (Size 50 50) $ (map (Color . fromIntegral) $ B.unpack testbitmap)-}
testcircle = (map (Color . fromIntegral) $ B.unpack testbitmap)
