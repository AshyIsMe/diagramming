{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

module Main where
import           Codec.Picture
import           Data.List.Split (chunksOf)
import qualified Data.Vector.Storable as V (toList)
import           Data.Bits
import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.Rasterific as DR
import           Graphics.UI.WX
import           Graphics.UI.WXCore.WxcTypes
import           Data.Typeable
import qualified Language.Haskell.Interpreter as H


deriving instance Typeable D.Any

exampleDiagram :: String
exampleDiagram = "circle 0.5 # fc red `atop` circle 1 # fc green `atop` square 2 :: Diagram B R2"

main :: IO ()
main = do
  start mainWindow

mainWindow :: IO ()
mainWindow = do 
  f         <- frame [text := "Hello!"]
  editor    <- textCtrl f [wrap := WrapNone, text := exampleDiagram]
  runButton <- button f [text := "Run", on command := run editor]
  quit      <- button f [text := "Quit", on command := close f]
  set f [layout := minsize (sz 640 480) $
                   margin 10 (column 5 [fill $ floatCentre (widget editor)
                                      ,(row 2 [floatCentre (widget runButton)
                                              ,floatCentre (widget quit)])
                                      ])]

renderWindow :: (D.Diagram DR.B D.R2) -> IO ()
renderWindow d = do
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
    Left e -> do renderWindow $ (D.text ("Error Bitches: " ++ show e) # D.fc D.red)
    Right is -> do putStrLn "Running diagram"
                   renderWindow is

diagramInterpreter :: String -> H.Interpreter (D.Diagram DR.B D.R2)
diagramInterpreter diagramhaskell = do
  H.setImportsQ [("Prelude", Nothing), ("Diagrams.Prelude", Nothing), ("Diagrams.Backend.Rasterific", Nothing)]
  H.interpret diagramhaskell (H.as :: D.QDiagram DR.B D.R2 D.Any)


renderDiagram :: (D.Diagram DR.B D.R2) -> Int -> Int -> [Color]
renderDiagram d w h = do
  map (Color . buildWord) $ chunksOf 4 $ V.toList $ imageData image
          --Pack 4 Word8s into one Word and then construct a Color
    where buildWord [a,b,c,d] = (fromIntegral a `shiftL` 24) .|. 
                                (fromIntegral b `shiftL` 16) .|. 
                                (fromIntegral c `shiftL` 8) .|. 
                                fromIntegral d
          image = D.renderDia DR.Rasterific (DR.RasterificOptions (D.Dims width height)) d
          width = fromIntegral w
          height = fromIntegral h
