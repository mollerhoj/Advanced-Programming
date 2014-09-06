module Curves
( point
, curve
, Axis (..)
, Curve
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
) where

import Text.Printf

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

type Curve = [Point]
curve :: Point -> [Point] -> Curve
curve p l = p : l

toList :: Curve -> [Point]
toList c = c

seqments :: Curve -> [(Point,Point)]
seqments (p1:p2:xs) = (p1,p2) : seqments (p2 : xs)
seqments _          = []

connect :: Curve -> Curve -> Curve
connect c1 c2 = c1 ++ c2

translate :: Curve -> Point -> Curve
translate [] _     = []
translate (x:xs) p = translate' (x:xs) (move p (rev x))

translate' :: Curve -> Point -> Curve
translate' [] _              = []
translate' (x:xs) p = move x p : translate' xs p

data Axis = Vertical | Horizontal

reflect :: Curve -> Axis -> Double -> Curve
reflect [] _ _ = []
reflect (x:xs) a r = reflectPoint x a r : reflect xs a r

reflectPoint :: Point -> Axis -> Double -> Point
reflectPoint (Point (x, y)) Vertical r = Point (reflectDouble x r, y)
reflectPoint (Point (x, y)) Horizontal   r = Point (x, reflectDouble y r)

reflectDouble :: Double -> Double -> Double
reflectDouble i r = 2 * r - i

move :: Point -> Point -> Point
move (Point (x, y)) (Point (dx, dy)) = Point (x+dx, y+dy)

bbox :: Curve -> (Point,Point)
bbox (p1:p2:xs) = bbox' (p1:p2:xs) (p1,p2)
bbox _          = (Point (0, 0),Point (0, 0))

bbox' :: Curve -> (Point,Point) -> (Point,Point)
bbox' [] (p1,p2) = (p1,p2)
bbox' (Point (x1, y1):xs) (Point (x0, y0),Point (x2, y2))
    | x1 < x0   = bbox' (Point (x1, y1):xs) (Point (x1, y0),Point (x2, y2))
    | x1 > x2   = bbox' (Point (x1, y1):xs) (Point (x0, y0),Point (x1, y2))
    | y1 < y0   = bbox' (Point (x1, y1):xs) (Point (x0, y1),Point (x2, y2))
    | y1 > y2   = bbox' (Point (x1, y1):xs) (Point (x0, y0),Point (x2, y1))
    | otherwise = bbox' xs (Point (x0, y0),Point (x2, y2))

width :: Curve -> Double
width c = width' (bbox c)

width' :: (Point,Point) -> Double
width' (Point (x1, _),Point (x2, _)) = abs (x1 - x2)

height :: Curve -> Double
height c = height' (bbox c)

height' :: (Point,Point) -> Double
height' (Point (_, y1),Point (_, y2)) = abs (y1 - y2)

rotate :: Curve -> Double -> Curve
rotate c d = rotateRad c (d / 180 * pi)

rotateRad :: Curve -> Double -> Curve
rotateRad [] _     = []
rotateRad (x:xs) a = rotateRadPoint x a : rotateRad xs a

rotateRadPoint :: Point -> Double -> Point
rotateRadPoint (Point (x, y)) a = Point (x * cos(-a) - y * sin(-a), x * sin(-a) + y * cos(-a))

-- Rendering
toSVG :: Curve -> String
toSVG c = svgHead (width c) (height c) ++ svgBody c ++ svgTail

svgHead :: Double -> Double -> String
svgHead w h = 
                    unlines [ "<svg xmlns='http://www.w3.org/2000/svg'",
                    "  width='" ++ svgDouble w ++ "px' height='" ++ svgDouble h ++ "px' version='1.1'>",
                    "<g>"]

svgBody :: Curve -> String
svgBody c = svgBody' (seqments c)

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
