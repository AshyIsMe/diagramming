{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

module Main where
import           Codec.Picture
import qualified Control.Monad.State as S
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
{-exampleDiagram = "circle 0.5 # fc yellow `atop` circle 1 # fc green `atop` square 2 :: Diagram B R2"-}
exampleDiagram = "beside (r2 (1,1)) (circle 1 # fc yellow) (square 2 # fc green) :: Diagram B R2"


animateDiagram :: String
{-testCode = "(+) 42 :: Integer -> Integer"-}
animateDiagram = "blah :: State (Double, Double) (Diagram B R2) -> State (Double, Double) (Diagram B R2)"

main :: IO ()
main = do
  start mainWindow

mainWindow :: IO ()
mainWindow = do 
  f              <- frame [text := "Hello!"]
  editor         <- textCtrl f [wrap := WrapNone, text := exampleDiagram]
  runButton      <- button f [text := "Run", on command := run editor]
  animatedEditor <- textCtrl f [wrap := WrapNone, text := animateDiagram]
  runTestButton  <- button f [text := "Run animation test", on command := runTest animatedEditor]
  quit           <- button f [text := "Quit", on command := close f]
  set f [layout := minsize (sz 640 480) $
                   margin 10 (column 5 [fill $ floatCentre (widget editor)
                                      ,(row 2 [floatCentre (widget runButton)
                                              ,floatCentre (widget quit)])
                                      ,fill $ floatCentre (widget animatedEditor)
                                      ,floatCentre (widget runTestButton)
                                      ])]

runTest :: Textual w => w -> IO ()
runTest e = do
  t <- get e text
  putStrLn t
  renderAnimation

renderAnimation :: IO ()
renderAnimation = do
  vNum <- varCreate 0.1
  f <- frame [text := "testing window"]
  p <- panel f [on paint := paintAnimation vNum]
  t <- timer f [interval := 20, on command := nextNum vNum p]
  set f [layout := minsize (sz 640 480) $
                   fill (widget p)]

paintAnimation :: Var Double -> DC a -> Rect -> IO ()
paintAnimation vNum dc r = do
  num <- varGet vNum
  dia <- imageCreateFromPixels (Size (rectWidth r) (rectHeight r)) $
            let d = D.beside (D.r2 (num,1)) (D.circle 1 D.# D.fc D.yellow) (D.square 2 D.# D.fc D.green) :: D.Diagram DR.B D.R2
            in renderDiagram d (rectWidth r) (rectHeight r)
  drawImage dc dia (Point 0 0) []

nextNum :: Var Double -> Panel () -> IO ()
nextNum vNum p = do
  varUpdate vNum (+ 0.1)
  repaint p

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
    Left e -> do putStrLn $ (show e) 
                 renderWindow $ (D.text (show e) # D.fc D.red)
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
