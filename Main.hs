module Main where

import           Codec.Picture
import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.Rasterific as DR
import           Graphics.UI.WX
import           Graphics.UI.WXCore.WxcTypes
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
  {-writeBitmap "circle.bmp" testimage-}
  saveBmpImage "circle.bmp" $ ImageRGBA8 testimage
  savePngImage "circle.png" $ ImageRGBA8 testimage
  start hello

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

{-circlepng :: Graphics.UI.WXCore.WxcClassTypes.Image ()-}
circlepng = image "circle.png"

paintBalls :: DC a -> Rect -> IO ()
paintBalls  dc viewArea
  = do set dc [brushColor := red, brushKind := BrushSolid]
       {-drawBitmap dc ball (Point 10 10) True []-}
       {-drawBitmap dc ball (Point 20 20) True []-}
       {-drawBitmap dc ball (Point 30 30) True []-}
       {-testcircle <- imageCreateFromPixels (Size 50 50) testcircle-}
       testcircle <- imageCreateFromPixels (Size 50 50) $ renderToWxImage testimage
       drawImage dc testcircle (Point 0 0) []
       drawImage dc circlepng (Point 100 0) []

c :: D.Diagram DR.B D.R2
c = D.circle 1 # D.fc D.green
testimage :: Image PixelRGBA8
testimage = D.renderDia DR.Rasterific (DR.RasterificOptions (D.Width 50)) c
testbitmap :: Either String B.ByteString
testbitmap = encodeDynamicBitmap $ ImageRGBA8 testimage
testcircle :: [Color]
testcircle = case testbitmap of
  Right bs -> byteStringToColorList bs
  Left e -> error e

byteStringToColorList :: B.ByteString -> [Color]
byteStringToColorList bs = _byteStringToColorList $ B.unpack bs

_byteStringToColorList :: [Word8] -> [Color]
_byteStringToColorList (r:g:b:a:ws) = [colorRGBA r g b a] ++ _byteStringToColorList ws
_byteStringToColorList bs = undefined

renderToWxImage :: Image PixelRGBA8 -> [Color]
renderToWxImage i = do
  case encodeDynamicBitmap $ ImageRGBA8 i of
    Right bs -> byteStringToColorList bs
    Left e -> error e


