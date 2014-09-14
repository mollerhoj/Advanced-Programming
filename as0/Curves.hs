module Curves
( point
, curve
, Axis (..)
, Curve (..)
, Point (..)
, connect
, rotate
, translate
, reflect
, bbox
, toList
, width
, height
, toSVG
) 
where

import Text.Printf (printf)

data Point = Point (Double,Double) deriving (Show)

instance Eq Point where
  Point (x1, y1) == Point (x2, y2) =
    strangeRound x1 == strangeRound x2 && 
    strangeRound y1 == strangeRound y2

point :: (Double, Double) -> Point
point (x, y) = Point (x, y)

strangeRound :: Double -> Double
strangeRound x = fromIntegral (floor (x * 100) :: Integer) / 100

rev :: Point -> Point
rev (Point (x, y)) = Point (-x, -y)

data Curve = Curve Point [Point] deriving (Eq)

curve :: Point -> [Point] -> Curve
curve = Curve

toList :: Curve -> [Point]
toList (Curve x xs) = x : xs

segments :: Curve -> [(Point,Point)]
segments (Curve p1 (p2:xs)) = (p1,p2) : segments (Curve p2 xs)
segments (Curve _ [])      = []

connect :: Curve -> Curve -> Curve
connect (Curve c1 cs1) c2 = Curve c1 (cs1 ++ toList c2)

translate :: Curve -> Point -> Curve
translate (Curve x xs) p = translate' (Curve x xs) (move p (rev x))

translate' :: Curve -> Point -> Curve
translate' (Curve x1 []) p      = Curve (move x1 p) []
translate' (Curve x1 (x2:xs)) p = Curve (move x1 p) (toList (translate' (Curve x2 xs) p))

rotate :: Curve -> Double -> Curve
rotate c d = rotateRad c (d / 180 * pi)

rotateRad :: Curve -> Double -> Curve
rotateRad (Curve x1 []) a      = Curve (rotateRadPoint x1 a) []
rotateRad (Curve x1 (x2:xs)) a = Curve (rotateRadPoint x1 a) (toList $ rotateRad (Curve x2 xs) a)

rotateRadPoint :: Point -> Double -> Point
rotateRadPoint (Point (x, y)) a = Point (x * cos(-a) - y * sin(-a), x * sin(-a) + y * cos(-a))

data Axis = Vertical | Horizontal

reflect :: Curve -> Axis -> Double -> Curve
reflect (Curve x1 []) a r = Curve (reflectPoint x1 a r) []
reflect (Curve x1 (x2:xs)) a r = Curve (reflectPoint x1 a r) (toList $ reflect (Curve x2 xs) a r)

reflectPoint :: Point -> Axis -> Double -> Point
reflectPoint (Point (x, y)) Vertical r = Point (reflectDouble x r, y)
reflectPoint (Point (x, y)) Horizontal   r = Point (x, reflectDouble y r)

reflectDouble :: Double -> Double -> Double
reflectDouble i r = 2 * r - i
 
move :: Point -> Point -> Point
move (Point (x, y)) (Point (dx, dy)) = Point (x+dx, y+dy)

bbox :: Curve -> (Point,Point)
bbox (Curve x xs) = (foldl minPoint x xs,foldl maxPoint x xs)

minPoint :: Point -> Point -> Point
minPoint (Point (x1,y1)) (Point (x2,y2)) = Point(min x1 x2,min y1 y2)

maxPoint :: Point -> Point -> Point
maxPoint (Point (x1,y1)) (Point (x2,y2)) = Point(max x1 x2,max y1 y2)

width :: Curve -> Double
width c = width' (bbox c)

width' :: (Point,Point) -> Double
width' (Point (x1, _),Point (x2, _)) = abs (x1 - x2)

height :: Curve -> Double
height c = height' (bbox c)

height' :: (Point,Point) -> Double
height' (Point (_, y1),Point (_, y2)) = abs (y1 - y2)

-- Rendering
toSVG :: Curve -> String
toSVG c = svgHead (width c) (height c) ++ svgBody c ++ svgTail

svgHead :: Double -> Double -> String
svgHead w h = 
                    unlines [ "<svg xmlns='http://www.w3.org/2000/svg'",
                    "  width='" ++ svgDouble w ++ "px' height='" ++ svgDouble h ++ "px' version='1.1'>",
                    "<g>"]

svgBody :: Curve -> String
svgBody c = svgBody' (segments c)

svgBody' :: [(Point,Point)] -> String
svgBody' (ps:xs) = svgLine ps ++ svgBody' xs
svgBody' _ = ""

svgLine :: (Point,Point) -> String
svgLine (Point (x1, y1),Point (x2, y2)) =
        unlines [ "<line style='stroke-width: 2px; stroke: black; fill:white'",
                  "  x1='" ++ svgDouble x1 ++ "' x2='" ++ svgDouble x2 ++ "' y1='" ++ svgDouble y1 ++ "' y2='" ++ svgDouble y2 ++ "' />" ]

svgDouble :: Double -> String
svgDouble = printf "%.2f"

svgTail :: String
svgTail = unlines [ "</g>",
                    "</svg>" ]
