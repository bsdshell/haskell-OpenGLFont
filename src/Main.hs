-- | To be able to run this example,

--

-- * install the GLFW-b library (for example using cabal)

-- 

-- * download the STIX2 font files in OTF format from <https://github.com/stipub/stixfonts>, 

-- and put them in this directory (5 files with @.otf@ extension)

--

-- * compile this source file with @ghc -O --make -threaded@

--

{-# LANGUAGE OverloadedStrings #-}
module Main where

--------------------------------------------------------------------------------


import Data.Char hiding ( Space )
import Data.List ( isPrefixOf )
import Data.String

import Control.Monad
import Data.IORef
import System.IO.Unsafe as Unsafe

import qualified Data.Map as Map ; import Data.Map (Map)

import Graphics.Rendering.OpenGL as GL
-- import Graphics.UI.GLFW ( Window )
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GLU.Matrix as GM

import Graphics.Rendering.MiniTypeset

import GL -- src/GL.hs
import UnicodeMath  -- src/UnicodeMath.hs

import qualified AronGraphic as AG
import qualified AronModule as AM

--------------------------------------------------------------------------------


-- | Font height

theHeight = Height 40

--------------------------------------------------------------------------------

-- * The \"document\" we want to render (normally you want to generate this)


document :: Document String
document 
  = Identified "document" 
  $ vcatR
      [ block12
--      , space

      , hcatT [ vcat [ summation , hcatT [ over , space, fracs ] ] , (margin' 50 40 20 30 block3) ]
      , block4
      ]

mydocument :: Document String
mydocument 
  = Identified "document" 
  $ vcatR
      [
        -- block4
        summation,
        line4a,
        line4b
      ]
  
block12 = hcat [ delimited Brace block1 , Space , block2 ]

block1 = Identified "block1" 
       $ vcat  [line1a,line1b,line1c,line1d]

block2 = vcatR [line2a,line2b,line2c,line2d,line2e]

block3 = Identified "block3" 
       $ vcat [ rgb 1 1 0 (string greeks), rgb 0.3 0.4 1 subsup, white equ ]

block4 = vcat [line4a,line4b,line4c,line4d,line4e,line4f,line4g]

line1a = "Lorem ipsum dolor sit amet,"
line1b = rgb 0.7 0 0 
       $ "consectetur adipiscing elit,"
line1c = Identified "line1c"
       $ italic      
       $ "sed do eiusmod tempor incididunt"
line1d = hcat
       [ "ut labore et "
       , Identified "dolore" $ boldItalic $ rgb 0 0 0.75 $ "dolore"
       , " magn{a} aliqua."
       ]

line2a = hcat [ "Ut enim ad " , Identified "bbox1" "minim" , " veniam," ]
line2b = Identified "line2b"
       $ bold
       $ "quis nostrud exercitation"
line2c = "ullamco laboris nisi ut"
line2d = rgb 0 0.4 0
       $ boldItalic
       $ "aliquip ex ea commodo"
line2e = hcat [ "conse" , Identified "bbox2" ("qua") , "t." ]

line4a = hcat [ "In printed documents " , blue (underline "underlinin" <|> "g") , " is generally avoided," ]
line4b = hcat [ "with " , white (overline (italic "italics")) , " or " , green (hcat [ overline "sma" , "ll " , overline "caps"]) , " often used instead, or" ]
line4c = "(especially in headings) using capitalization or bold type."
line4d = hcat [ "In a ", strike "manuscript to be typeset" , ", various forms of underlining" ]
line4e = "were therefore conventionally used to indicate that text should"
line4f = hcat [ "be set in special type such as italics, " , underline "part of a procedure" ]
line4g = "known as markup."
 
over1 = rgb 1.0 0.2 0.3 $ vcatR  ["overlay on top","of each other"]
over2 = rgb 0.9 0.9 0.2 $ vcatR ["////","\\\\\\\\","####"]
over  = Overlay (AlignRight,AlignBottom) [ over1 , over2 ]

equ = hcat
  [ string $ take 8 math_test 
  , Identified "formula" $ rgb 0.5 0.5 1 $ string $ take 5 (drop 8 math_test)
  , string $ drop 13 math_test
  ]

fracs = blue $ hcat [ "1+", fraction (hcat [" xy+",fraction "z" "w"]) "pq" , space , choose "n" "k" , space , (choose' Square "m" "t") ]
  
subsup = vcat
  [ black "see if the gaps between lines..."
  , hcat [ supscript (char pp) "m"     , space 
         , subSup "X" ("alma","(7)")   , space
         , subscript mm "g"            , space 
         , subSup (char zz) ("p","2")  , space
         ]
  , hcat [ supscript "10" "10" , space
         , supscript "10" (supscript "10" "10") , space
         , supscript "10" (supscript "10" (supscript "10" "10")) , space
         , supscript "10" (supscript "10" (Identified "bbox_scr" (supscript "10" (supscript "10" "10")))) , space
         , supscript "X" "5" , space
         , supscript (supscript "X" "5") "7" , space
         , supscript "x" "2" , "y" , supscript "z" "3"
         ]
  , black "...increase or not when using subscripts"
  ]

summation = vcat 
  [ "summation" 
  , white $ Identified "zz" $ Delimited Paren  
      [ hcat [         aboveBelow (char sum_) (char infty , "n=0") , "f(n)"  , space ] 
      , hcat [ space , below "lim" (string ['x',rightarrow,infty]) , " g(x)" , space ]
      , " xyz A"
      ]
  , "and limits"
  ]

mm = char '\x2133'

--------------------------------------------------------------------------------


-- | An enum encoding the font files we use

data MyFontFile
  = Stix2TextRegular
  | Stix2TextBold
  | Stix2TextItalic
  | Stix2TextBoldItalic 
  | Stix2Math
  deriving (Eq,Ord,Show)

-- | An enum encoding our typeface variations

data MyStyle
  = MyRegular 
  | MyBold
  | MyItalic
  | MyBoldItalic
  | MyMath
  deriving (Eq,Ord,Show)

-- | Mapping standard typeface variations to ours

myStyleMap :: BasicStyle -> MyStyle
myStyleMap s = case s of
  Regular    -> MyRegular 
  Bold       -> MyBold
  Italic     -> MyItalic
  BoldItalic -> MyBoldItalic

-- | Mapping typeface variatons to abstract fonts (not always necessary) 

myStyleDefaultFont :: MyStyle -> MyFontFile
myStyleDefaultFont style = case style of
  MyRegular    -> Stix2TextRegular
  MyBold       -> Stix2TextBold
  MyItalic     -> Stix2TextItalic
  MyBoldItalic -> Stix2TextBoldItalic 
  MyMath       -> Stix2Math

-- | Mapping abstract font files to concrete font files

myFontFileMap :: MyFontFile -> FilePath
myFontFileMap ff = case ff of
  -- /Users/cat/myfile/bitbucket/stackproject/OpenGLFont/font/
  Stix2TextRegular    -> "./font/STIXTwoText-Regular.otf"
  Stix2TextBold       -> "./font/STIXTwoText-Bold.otf"
  Stix2TextItalic     -> "./font/STIXTwoText-Italic.otf"
  Stix2TextBoldItalic -> "./font/STIXTwoText-BoldItalic.otf"
  Stix2Math           -> "./font/STIXTwoMath-Regular.otf"

-- | Mapping (style,codepoint) pairs to (abstract) font files.

-- For example mathematical symbols are not present in the regular fonts, so 

-- we always map them to the math font.

--

myCharMap :: MyStyle -> Char -> MyFontFile  
myCharMap MyMath  _ = Stix2Math
myCharMap style ch
  | o <= 0x2100  = myStyleDefaultFont style
  | o >= 0xfb00  = myStyleDefaultFont style
  | otherwise    = Stix2Math
  where
    o = ord ch

-- | Our \"multifont\" configuration

myUFC :: UserFontConfig MyFontFile MyStyle 
myUFC = UserFontConfig
  { _ufcFontFiles     = myFontFileMap
  , _ufcCharMap       = myCharMap
  , _ufcStyleMap      = myStyleMap
  , _ufcLineGapFactor = 1.0
  }

{-# NOINLINE theMultiFont #-}
theMultiFont :: IORef (MultiFont MyFontFile MyStyle)
theMultiFont = Unsafe.unsafePerformIO $ newIORef $ error "multifont not loaded"

--------------------------------------------------------------------------------


display :: GLFW.Window -> Double -> IO ()
display window time = do
  
  clearColor $=! (Color4 0.5 0.5 0.5 1)  
  clear [ColorBuffer,DepthBuffer]
    
  -- from GL.h
  -- Just Set Up Projective and modelView matrix
  --
  {--
    setWindowCoordSystem :: IO ()
    setWindowCoordSystem = do
      (w,h) <- readIORef theWindowSize
      viewport $=! (Position 0 0 , Size (fromIntegral w) (fromIntegral h))
      matrixMode $=! Projection
      loadIdentity
      GL.ortho 0 (fromIntegral w) (fromIntegral h) 0 (-1) (1::Double)
      matrixMode $=! Modelview 0
      loadIdentity
  --}
  setWindowCoordSystem
  -- It seems following line can not be shown
  -- GL.translate (Vector3 (-0.42) (0.49) 0 :: Vector3 GLdouble)
    
  mf <- readIORef theMultiFont

  -- create layout

  lout <- createLayout mf theHeight document
  -- lout <- createLayout mf theHeight mydocument
  -- render string from a file
  {--
  ls <- AM.readFileList "/tmp/kk"
  let docs = map string ls
  lout <- createLayout mf theHeight $ Identified "document" $ vcatR
                                                      [
                                                        -- block4
                                                        summation,
                                                        line4a,
                                                        line4b,
                                                        hcat docs 
                                                      ]
  --}

  -- top-left corner of the rendered text

  let pos0 = Pos 16 16

  -- query bounding box positions, and render them
  usertable  <- dryrunLayout lout pos0
  blend $=! Enabled
  blendFunc $=! (SrcAlpha,One) -- MinusSrcAlpha)

  let isbbox = isPrefixOf "bbox" 
  color $ Color4 1 1 1 (0.1 :: Double)
  mapM_ renderOuterBoxQuad    $ Map.elems $ Map.filterWithKey (\k v -> not (isbbox k)) $ usertable
  color $ Color4 1 0 0 (0.2 :: Double)
  mapM_ renderBoundingBoxQuad $ Map.elems $ Map.filterWithKey (\k v ->      isbbox k ) $ usertable 
  blend $=! Disabled

  -- render the text

  renderLayout lout pos0
  
    
  return ()

--------------------------------------------------------------------------------


initMultifont = do
  mf <- newMultiFont myUFC 
  writeIORef theMultiFont mf  
  return ()

{-|

initGL :: IO () -> (() -> Window -> Double -> IO ()) -> IO ()

initGL :: IO precalc -> (precalc -> Window -> Double -> IO ()) -> IO()

initMultifont :: IO ()

display :: Window -> Double -> IO ()


-}
main = do
  initGL initMultifont (\() -> display) 

--------------------------------------------------------------------------------

