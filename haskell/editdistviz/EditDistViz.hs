{-# LANGUAGE OverloadedStrings #-}

-- ghc --make EditDistViz.hs -main-is EditDistViz.main

module EditDistViz where

import Data.Array
import Data.List (intersperse)

import Control.Arrow ((***))

import qualified Data.Set as S

-- write file
import System.IO

data Cell a = Val {val::Int, reasons::[Reason a]}
 deriving (Show)

data Reason a = Init | X a | Y a | Eq a | Neq a
 deriving (Show)

data SVG = SVG {mainElement::XMLElement}
data XMLString = XMLString {unescaped::String}
data XMLAttribute = Attr {attrName::XMLString, attrValue::XMLString}
data XMLElement = Tag {tagName::XMLString, attributes::[XMLAttribute], content::[Either XMLElement XMLString]}

instance Show XMLString where
    show (XMLString unescaped) = concatMap f unescaped
        where f '<' = "&lt;"
              f '>' = "&gt;"
              f '&' = "&amp;"
              f '\'' = "&apos;"
              f '"' = "&quot;"
              f x = [x]

mkTag x a =
    Tag
      (XMLString x)
      (map (uncurry Attr . (XMLString *** XMLString)) a)

rgb r g b =
  "rgb(" ++ show (255 * r) ++
     "," ++ show (255 * g) ++
     "," ++ show (255 * b) ++ ")"

red   = rgb 1 0 0
yello = rgb 1 1 0
green = rgb 0 1 0
cyan  = rgb 0 1 1
blu   = rgb 0 0 1
purpl = rgb 1 0 1

{- trace successful path back from (m,n) point to origin -}
successpath table =
  let (_, (m,n)) = bounds table
      aux' (0,0)  = []
      -- arbitrary preference? no, let's show all successful paths.
      aux' (i,j) =
         let diag = any (\x -> case x of (Eq _) -> True ; (Neq _) -> True ; _ -> False) (reasons (table ! (i,j)))
             horz = any (\x -> case x of (X _) -> True ; _ -> False) (reasons (table ! (i,j)))
             vert = any (\x -> case x of (Y _) -> True ; _ -> False) (reasons (table ! (i,j)))
         in
           [(i-1,j-1) | diag] ++ [(i-1,j) | horz] ++ [(i,j-1) | vert]
      aux front accum =
          case S.minView front of
            Nothing         -> accum
            Just ((i,j), s) ->
                let contributions = aux' (i,j)
                in
                  aux (S.union s     (S.fromList contributions))
                      (S.union accum (S.fromList (map (\x -> (x, (i,j))) contributions)))
  in
    aux (S.fromList [(m,n)]) (S.empty)

toSVG :: Array (Int,Int) (Cell Char) -> SVG
toSVG table =
    let (_, (m,n)) = bounds table
        -- square edge width
        sqmeasure  = 50.0 :: Float
        rmeasure   =  5.0
        -- stroke line width:
        smeasure   =  1.0
        width      = sqmeasure * fromIntegral m
        height     = sqmeasure * fromIntegral n
        valmaxf    = fromIntegral $ maximum (map val (elems table))
        palette    = cycle [red, yello, green, cyan, blu, purpl]
        s_path     = S.toList (successpath table)
    in
      SVG $ 
      mkTag "svg"
       [("xmlns","http://www.w3.org/2000/svg"), ("width", show width), ("height", show height)
       ,("viewBox", show (-0.5*sqmeasure) ++ " " ++ show (-0.5*sqmeasure) ++ " " ++ show (width + 1*sqmeasure) ++ " " ++ show (height + 1*sqmeasure))
       ]
       $
       -- white background
       [ Left (mkTag "rect" [("x", show $ -0.5*sqmeasure),
                             ("y", show $ -0.5*sqmeasure),
                             ("width", show $ width + 1*sqmeasure),
                             ("height", show $ height + 1*sqmeasure),
                             ("fill", "white")
                            ] [])]
       ++
       -- horizontal lettering
       [ Left (mkTag "text"
               [("x", show x), ("y", show (-sqmeasure/4)),
                ("fill", rgb 0.0 0.0 0.0)
               ]
               [Right $ XMLString c])
       | i <- [1..m]
       , let c = case reasons (table ! (i, 0)) of [X c] -> [c] ; _ -> ""
       , let x = fromIntegral i * sqmeasure - 0.5 * sqmeasure ]
       ++
       -- vertical lettering
       [ Left (mkTag "text"
               [("y", show y), ("x", show (-sqmeasure/4)),
                ("fill", rgb 0.0 0.0 0.0)
               ]
               [Right $ XMLString c])
       | j <- [1..n]
       , let c = case reasons (table ! (0, j)) of [Y c] -> [c] ; _ -> ""
       , let y = fromIntegral j * sqmeasure - 0.5 * sqmeasure ]
       ++
       -- little square tiles
       [ Left (mkTag "rect"
               [("x", show rx), ("y", show ry),
                ("width", show sqmeasure),
                ("height", show sqmeasure),
                ("fill", color),
                ("fill-opacity", show 1.0),
                ("stroke", color),
                ("stroke-width", show smeasure)
               ] [])
       | i <- [0..m-1], j <- [0..n-1]
       , let rx = fromIntegral i * sqmeasure
       , let ry = fromIntegral j * sqmeasure
       , let vf = fromIntegral $ val (table ! (i+1,j+1))
       , let shade = vf / valmaxf
       , let color = rgb shade shade shade ]
       ++
       -- highlight successful path(s)
       [ Left (mkTag "line"
               [("x1", show x1)
               ,("y1", show y1)
               ,("x2", show x2)
               ,("y2", show y2)
               ,("stroke", color)
               ,("stroke-width", show s'measure)]
               [])
       | ((i',j'), (i,j)) <- s_path
       -- a nice possibility: draw a thicker line first
       , let s'measure = 5.0
       , let x1 = fromIntegral i' * sqmeasure
       , let y1 = fromIntegral j' * sqmeasure
       , let x2 = fromIntegral i  * sqmeasure
       , let y2 = fromIntegral j  * sqmeasure
       , let color = rgb 0.0 0.0 1.0 ]
       ++
       -- dashed lines for same letter
       [ Left (mkTag "line"
               [("x1", show x1)
               ,("y1", show y1)
               ,("x2", show x2)
               ,("y2", show y2)
               ,("stroke", color)
               ,("stroke-width", show s'measure)
               ,("stroke-dasharray", show 5)]
               [])
       | i <- [0..m-1], j <- [0..n-1]
       , let s'measure = smeasure
       , let x1 = fromIntegral i * sqmeasure
       , let y1 = fromIntegral j * sqmeasure
       , let x2 = fromIntegral (i+1) * sqmeasure
       , let y2 = fromIntegral (j+1) * sqmeasure
       , let rr = reasons (table ! (i+1,j+1))
       , any (\x -> case x of (Eq _) -> True ; _ -> False) rr
       , let color = palette !! val (table ! (i, j)) ]
       ++
       -- horizontal
       [ Left (mkTag "line"
               [("x1", show x1)
               ,("y1", show y1)
               ,("x2", show x2)
               ,("y2", show y2)
               ,("stroke", color)
               ,("stroke-width", show s'measure)]
               [])
       | i <- [0..m-1], j <- [0..n-1]
       , let s'measure = smeasure
       , let x1 = fromIntegral i * sqmeasure
       , let y1 = fromIntegral (j+1) * sqmeasure
       , let x2 = fromIntegral (i+1) * sqmeasure
       , let y2 = fromIntegral (j+1) * sqmeasure
       , let rr = reasons (table ! (i+1,j+1))
       , any (\x -> case x of (X _) -> True ; _ -> False) rr
       , let color = palette !! val (table ! (i, j)) ]
       ++
       -- vertical
       [ Left (mkTag "line"
               [("x1", show x1)
               ,("y1", show y1)
               ,("x2", show x2)
               ,("y2", show y2)
               ,("stroke", color)
               ,("stroke-width", show s'measure)]
               [])
       | i <- [0..m-1], j <- [0..n-1]
       , let s'measure = smeasure
       , let x1 = fromIntegral (i+1) * sqmeasure
       , let y1 = fromIntegral j * sqmeasure
       , let x2 = fromIntegral (i+1) * sqmeasure
       , let y2 = fromIntegral (j+1) * sqmeasure
       , let rr = reasons (table ! (i+1,j+1))
       , any (\x -> case x of (Y _) -> True ; _ -> False) rr
       , let color = palette !! val (table ! (i, j))]
       ++
       -- diagonal, different letter
       [ Left (mkTag "line"
               [("x1", show x1)
               ,("y1", show y1)
               ,("x2", show x2)
               ,("y2", show y2)
               ,("stroke", color)
               ,("stroke-width", show s'measure)]
               [])
       | i <- [0..m-1], j <- [0..n-1]
       , let s'measure = smeasure
       , let x1 = fromIntegral i * sqmeasure
       , let y1 = fromIntegral j * sqmeasure
       , let x2 = fromIntegral (i+1) * sqmeasure
       , let y2 = fromIntegral (j+1) * sqmeasure
       , let rr = reasons (table ! (i+1,j+1))
       , any (\x -> case x of (Neq _) -> True ; _ -> False) rr
       , let color = palette !! val (table ! (i, j)) ]
       ++
       [ Left (mkTag "circle"
               [("cx", show cx), ("cy", show cy), ("r", show rmeasure),
                ("fill", rgb 0.0 0.0 (vf / valmaxf)),
                ("fill-opacity", show 1)
               ] [])
       | i <- [0..m], j <- [0..n]
       , let cx = fromIntegral i * sqmeasure
       , let cy = fromIntegral j * sqmeasure
       , let vf = fromIntegral $ val (table ! (i,j)) ]

instance Show SVG where
  show (SVG elt) = show elt

instance Show XMLElement where
  show (Tag name attrs content) =
      concat $ ["<", show name] ++
               [" "| not (null attrs)] ++
               intersperse " " (map show attrs) ++ ["/>" | null content] ++
               [">" | not (null content)] ++
               [case c of
                  Left  c -> show c
                  Right c -> show c | c <- content] ++
               ["</" ++ show name ++ ">" | not (null content)]

instance Show XMLAttribute where
  show (Attr name value) =
      concat [show name, "=\"", show value, "\""]

edtable :: (Eq a) => [a] -> [a] -> Array (Int, Int) (Cell a)
edtable v w =
    let res = array ((0,0), (length v , length w)) $
              [((0,0), Val 0 [Init])] ++
              [((x,0), Val x [X vx]) | (x,vx) <- zip [1..] v] ++
              [((0,y), Val y [Y wy]) | (y,wy) <- zip [1..] w] ++
              [((x,y), k)
              | (x,vx) <- zip [1..] v
              , (y,wy) <- zip [1..] w
              , let nx = 1 + val (res ! (x-1, y))
                    ny = 1 + val (res ! (x, y-1))
                    nd = (if vx == wy then 0 else 1) + val(res ! (x-1, y-1))
                    n  = minimum [nx, ny, nd]
              , let k = Val n $
                        [X vx | n == nx] ++
                        [Y wy | n == ny] ++
                        [Eq vx | n == nd, vx == wy] ++
                        [Neq vx | n == nd, vx /= wy]
              ]
    in res

-- (s1,s2) = ("preternatural", "unintentional")
(s1,s2) = ("efghabcd", "abcdefgh")

main :: IO ()
main = do
  (pathOfTempFile, h) <- openTempFile "/tmp" "edtable.svg"
  putStrLn ("Writing to " ++ pathOfTempFile)
  hPutStr h $ show $ toSVG (edtable s1 s2)
  hClose h

