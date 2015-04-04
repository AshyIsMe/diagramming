module Main where

import           Codec.Picture
import           Data.List.Split (chunksOf)
import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.Rasterific as DR
import           Graphics.UI.WX
import           Graphics.UI.WXCore.WxcTypes
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
  {-writeBitmap "circle.bmp" testimage-}
  savePngImage "testimage.png" $ ImageRGBA8 testimage
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
                                      ,(row 2 [floatCentre (widget runButton)
                                              ,floatCentre (widget quit)])
                                      ])]

run :: Textual w => w -> IO ()
run e = do
  t <- get e text
  putStrLn t

paintBalls :: DC a -> Rect -> IO ()
paintBalls  dc viewArea
  = do set dc [brushColor := red, brushKind := BrushSolid]
       testcircle <- imageCreateFromPixels (Size 100 100) $ renderToWxImage testimage 100
       drawImage dc testcircle (Point 0 0) []

c :: D.Diagram DR.B D.R2
{-c = (D.square (sqrt 2) # D.fc D.red) `D.atop` D.circle 1 # D.fc D.green-}
c = (D.text "yo" # D.fc D.blue) `D.atop` (D.square (sqrt 2) # D.fc D.red) `D.atop` D.unitCircle # D.fc D.green

testimage :: Image PixelRGBA8
{-testimage = D.renderDia DR.Rasterific (DR.RasterificOptions (D.Width 50)) c-}
testimage = D.renderDia DR.Rasterific (DR.RasterificOptions (D.Dims 100 100)) c

renderToWxImage :: Image PixelRGBA8 -> Int -> [Color]
renderToWxImage i width = do
  case encodeDynamicBitmap $ ImageRGBA8 i of
    Right bs -> byteStringToColorList bs width
    Left e -> error e

byteStringToColorList :: B.ByteString -> Int -> [Color]
byteStringToColorList bs width = _byteStringToColorList wordlist
  where wordlist = concat . reverse $ chunksOf width $ B.unpack bs

_byteStringToColorList :: [Word8] -> [Color]
_byteStringToColorList (b:g:r:a:ws) = [colorRGBA r g b a] ++ _byteStringToColorList ws
_byteStringToColorList [] = error "_byteStringToColorList []"


