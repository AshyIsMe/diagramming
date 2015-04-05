{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

module Main where
import           Codec.Picture
import           Data.List.Split (chunksOf)
import qualified Data.Vector.Storable as V (Vector,toList)
import           Data.Bits
import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.Rasterific as DR
import           Graphics.UI.WX
import           Graphics.UI.WXCore.WxcTypes
import           Data.Typeable
import qualified Language.Haskell.Interpreter as H


deriving instance Typeable D.Any

code :: String
code = "circle 1 :: Diagram B R2"

diagramInterpreter :: H.Interpreter (D.Diagram DR.B D.R2)
diagramInterpreter =  do
  H.setImportsQ [("Prelude", Nothing), ("Diagrams.Prelude", Nothing), ("Diagrams.Backend.Rasterific", Nothing)]
  H.interpret "circle 1 :: Diagram B R2" (H.as :: D.QDiagram DR.B D.R2 D.Any)

main :: IO ()
main = do
  {-writeBitmap "circle.bmp" testimage-}
  savePngImage "testimage.png" $ ImageRGBA8 testimage
  {-r <- H.runInterpreter $ H.loadModules ["Prelude", "Diagrams.Prelude", "Diagrams.Backend.Rasterific"]-}
                            {->> H.setImports ["Prelude", "Diagrams.Prelude", "Diagrams.Backend.Rasterific"]-}
                            {->> H.interpret "circle 1 :: Diagram B R2" (H.as :: D.QDiagram DR.B D.R2 D.Any)-}
  r <- H.runInterpreter diagramInterpreter
  case r of
    Left e -> do error $ show e
                 start mainWindow
    Right is -> do putStrLn "diagram bitches"
                   start mainWindow

mainWindow :: IO ()
mainWindow = do 
  f         <- frame [text := "Hello!"]
  editor    <- textCtrl f [wrap := WrapNone]
  p         <- panel f [on paint := paintBalls]
  runButton <- button f [text := "Run", on command := run editor]
  quit      <- button f [text := "Quit", on command := close f]
  set f [layout := minsize (sz 640 480) $
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
       testcircle <- imageCreateFromPixels (Size 100 100) testimagecolors
       drawImage dc testcircle (Point 0 0) []

diagram :: D.Diagram DR.B D.R2
{-diagram = (D.square (sqrt 2) # D.fc D.red) `D.atop` D.circle 1 # D.fc D.green-}
diagram = (D.text "yo" # D.fc D.blue) `D.atop` (D.square (sqrt 2) # D.fc D.red) `D.atop` D.unitCircle # D.fc D.green

testimage :: Image PixelRGBA8
testimage = D.renderDia DR.Rasterific (DR.RasterificOptions (D.Dims 100 100)) diagram

imaged :: V.Vector Word8
imaged = imageData testimage
--Pack 4 Word8s into one Word and then construct a Color
testimagecolors :: [Color]
testimagecolors = map (Color . buildWord) $ chunksOf 4 $ V.toList $ imageData testimage
  where buildWord [a,b,c,d] = (fromIntegral a `shiftL` 24) .|. 
                              (fromIntegral b `shiftL` 16) .|. 
                              (fromIntegral c `shiftL` 8) .|. 
                              fromIntegral d
