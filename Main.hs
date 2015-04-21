{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

module Main where
import           Codec.Picture
import qualified Control.Monad.State as S
import           Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Storable as V (toList)
import           Data.Bits
import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD.Vector as D
import qualified Diagrams.Backend.Rasterific as DR
import           Graphics.UI.WX
import           Graphics.UI.WXCore.WxcTypes
import           Data.Typeable
import qualified Language.Haskell.Interpreter as H


deriving instance Typeable D.Any

--AA TODO: Sort out a way to pass state between invocations of the update() function the user defines
{-type DiagramState = S.State (Map.Map String Double) (D.Diagram DR.B D.R2)-}

exampleDiagram :: String
{-exampleDiagram = "(\\num -> beside (e (num @@ rad)) (circle 1 # fc yellow) (square 2 # fc green)) :: Double -> Diagram B R2"-}
exampleDiagram = unlines ["-- Uncomment the line below for a simple Diagram",
                          "-- beside (e (10 @@ rad)) (circle 1 # fc yellow) (circle 2 # fc green) :: Diagram B R2",
                          "-- Otherwise the following will be run each frame passing in a Double that is incremented by 0.01 each time",
                          "(\\num -> beside (e (num @@ rad)) (circle 1 # fc yellow) (circle 1 # fc green)) :: Double -> Diagram B R2"]

main :: IO ()
main = start mainWindow

mainWindow :: IO ()
mainWindow = do 
  f             <- frame [text := "Hello!"]
  editor        <- textCtrl f [wrap := WrapNone, text := exampleDiagram]
  runButton     <- button f [text := "Run static diagram", on command := run editor]
  runTestButton <- button f [text := "Run animated diagram", on command := runTest editor]
  quit          <- button f [text := "Quit", on command := close f]
  set f [layout := minsize (sz 640 480) $
                   margin 10 (column 5 [--fill $ floatCentre (widget editor),
                                        fill $ floatCentre (widget editor)
                                        ,row 3 [floatCentre (widget runTestButton)
                                                ,floatCentre (widget runButton)
                                                ,floatCentre (widget quit)]
                                      ])]

runTest :: Textual w => w -> IO ()
runTest e = do
  t <- get e text
  putStrLn t
  r <- H.runInterpreter $ animatedDiagramInterpreter t
  case r of
    Left e -> do print e
                 renderWindow (D.text (show e) # D.fc D.red)
    Right is -> do putStrLn "Running diagram"
                   renderAnimation is 

renderAnimation :: (Double -> D.Diagram DR.B D.R2) -> IO ()
renderAnimation fun = do
  vNum <- varCreate 0.1
  f <- frame [text := "testing window"]
  p <- panel f [on paint := paintAnimation vNum fun]
  t <- timer f [interval := 60, on command := nextNum vNum p]
  set f [layout := minsize (sz 640 480) $
                   fill (widget p)]

paintAnimation :: Var Double -> (Double -> D.Diagram DR.B D.R2) -> DC a -> Rect -> IO ()
paintAnimation vNum f dc r = do
  num <- varGet vNum
  dia <- imageCreateFromPixels (Size (rectWidth r) (rectHeight r)) $
            {-let d = D.beside (D.r2 (num,1)) (D.circle 1 D.# D.fc D.yellow) (D.circle 1 D.# D.fc D.green) :: D.Diagram DR.B D.R2-}
            {-let d = D.beside (D.e (num D.@@ D.rad)) (D.circle 1 D.# D.fc D.yellow) (D.circle 1 D.# D.fc D.green) :: D.Diagram DR.B D.R2-}
            let d = f num
            in renderDiagram d (rectWidth r) (rectHeight r)
  drawImage dc dia (Point 0 0) []

nextNum :: Var Double -> Panel () -> IO ()
nextNum vNum p = do
  varUpdate vNum (+ 0.1)
  repaint p

renderWindow :: D.Diagram DR.B D.R2 -> IO ()
renderWindow d = do
  f <- frame [text := "testing window"]
  p <- panel f [on paint := paintDiagram d]
  set f [layout := minsize (sz 640 480) $
                   fill (widget p)]

paintDiagram :: D.Diagram DR.B D.R2 -> DC a -> Rect -> IO ()
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
    Left e -> do print e
                 renderWindow (D.text (show e) # D.fc D.red)
    Right is -> do putStrLn "Running diagram"
                   renderWindow is

diagramInterpreter :: String -> H.Interpreter (D.Diagram DR.B D.R2)
diagramInterpreter diagramhaskell = do
  H.setImportsQ [("Prelude", Nothing), 
                 ("Diagrams.Prelude", Nothing), 
                 ("Diagrams.TwoD.Vector", Nothing), 
                 ("Diagrams.Backend.Rasterific", Nothing)]
  H.interpret diagramhaskell (H.as :: D.QDiagram DR.B D.R2 D.Any)

animatedDiagramInterpreter :: String -> H.Interpreter (Double -> D.Diagram DR.B D.R2)
animatedDiagramInterpreter diagramhaskell = do
  H.setImportsQ [("Prelude", Nothing), 
                 ("Diagrams.Prelude", Nothing), 
                 ("Diagrams.TwoD.Vector", Nothing), 
                 ("Diagrams.Backend.Rasterific", Nothing)]
  H.interpret diagramhaskell (H.as :: Double -> D.QDiagram DR.B D.R2 D.Any)


renderDiagram :: D.Diagram DR.B D.R2 -> Int -> Int -> [Color]
renderDiagram d w h = 
  map (Color . buildWord) $ chunksOf 4 $ V.toList $ imageData image
          --Pack 4 Word8s into one Word and then construct a Color
    where buildWord [a,b,c,d] = (fromIntegral a `shiftL` 24) .|. 
                                (fromIntegral b `shiftL` 16) .|. 
                                (fromIntegral c `shiftL` 8) .|. 
                                fromIntegral d
          image = D.renderDia DR.Rasterific (DR.RasterificOptions (D.Dims width height)) d
          width = fromIntegral w
          height = fromIntegral h
