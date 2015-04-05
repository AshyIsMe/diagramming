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

diagramInterpreter :: String -> H.Interpreter (D.Diagram DR.B D.R2)
diagramInterpreter diagramhaskell = do
  H.setImportsQ [("Prelude", Nothing), ("Diagrams.Prelude", Nothing), ("Diagrams.Backend.Rasterific", Nothing)]
  H.interpret diagramhaskell (H.as :: D.QDiagram DR.B D.R2 D.Any)

main :: IO ()
main = do
  {-writeBitmap "circle.bmp" testimage-}
  savePngImage "testimage.png" $ ImageRGBA8 testimage
  start mainWindow
  {-r <- H.runInterpreter $ diagramInterpreter code-}
  {-case r of-}
    {-Left e -> do error $ show e-}
                 {-start mainWindow-}
    {-Right is -> do putStrLn "diagram bitches"-}
                   {-start mainWindow-}

mainWindow :: IO ()
mainWindow = do 
  f         <- frame [text := "Hello!"]
  editor    <- textCtrl f [wrap := WrapNone, text := "circle 1 :: Diagram B R2"]
  p         <- panel f [on paint := paintBalls]
  {-runButton <- button f [text := "Run", on command := run editor]-}
  runButton <- button f [text := "Run", on command := run editor]
  quit      <- button f [text := "Quit", on command := close f]
  set f [layout := minsize (sz 640 480) $
                   margin 10 (column 5 [fill $ floatCentre (widget p)
                                      ,fill $ floatCentre (widget editor)
                                      ,(row 2 [floatCentre (widget runButton)
                                              ,floatCentre (widget quit)])
                                      ])]

testWindow :: (D.Diagram DR.B D.R2) -> IO ()
testWindow d = do
  f <- frame [text := "testing window"]
  p <- panel f [on paint := paintDiagram d]
  set f [layout := minsize (sz 640 480) $
                   fill (widget p)]

paintDiagram :: (D.Diagram DR.B D.R2) -> DC a -> Rect -> IO ()
paintDiagram d dc r = do
  dia <- imageCreateFromPixels (Size (rectWidth r) (rectHeight r)) $
            renderDiagram d (rectWidth r) (rectHeight r)
  drawImage dc dia (Point 0 0) []

run :: Textual w => w -> IO ()
run e = do
  t <- get e text
  putStrLn t
  r <- H.runInterpreter $ diagramInterpreter t
  case r of
    Left e -> do error $ show e
                 testWindow $ (D.text "Error Bitches" # D.fc D.red)
    Right is -> do putStrLn "Running diagram"
                   testWindow is


paintBalls :: DC a -> Rect -> IO ()
paintBalls dc viewArea = do 
  set dc [brushColor := red, brushKind := BrushSolid]
  testcircle <- imageCreateFromPixels (Size 100 100) testimagecolors
  drawImage dc testcircle (Point 0 0) []

diagram :: D.Diagram DR.B D.R2
{-diagram = (D.square (sqrt 2) # D.fc D.red) `D.atop` D.circle 1 # D.fc D.green-}
diagram = (D.text "yo" # D.fc D.blue) `D.atop` (D.square (sqrt 2) # D.fc D.red) `D.atop` D.unitCircle # D.fc D.green

testimage :: Image PixelRGBA8
testimage = D.renderDia DR.Rasterific (DR.RasterificOptions (D.Dims 100 100)) diagram

--Pack 4 Word8s into one Word and then construct a Color
testimagecolors :: [Color]
testimagecolors = map (Color . buildWord) $ chunksOf 4 $ V.toList $ imageData testimage
  where buildWord [a,b,c,d] = (fromIntegral a `shiftL` 24) .|. 
                              (fromIntegral b `shiftL` 16) .|. 
                              (fromIntegral c `shiftL` 8) .|. 
                              fromIntegral d

renderDiagram :: (D.Diagram DR.B D.R2) -> Int -> Int -> [Color]
renderDiagram d w h = do
  map (Color . buildWord) $ chunksOf 4 $ V.toList $ imageData image
    where buildWord [a,b,c,d] = (fromIntegral a `shiftL` 24) .|. 
                                (fromIntegral b `shiftL` 16) .|. 
                                (fromIntegral c `shiftL` 8) .|. 
                                fromIntegral d
          image = D.renderDia DR.Rasterific (DR.RasterificOptions (D.Dims width height)) d
          width = fromIntegral w
          height = fromIntegral h
