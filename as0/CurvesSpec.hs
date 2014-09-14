import Curves

test1  = point(0,0.01) == Point (0.009,0.01)
test2  = curve (point(1,1)) [point(2,2)] == Curve (point(1,1)) [point(2,2)]
test3  = connect (Curve (point(1,1)) []) (Curve (point(2,2)) []) == Curve (Point(1,1)) [point(2,2)] 
test4  = rotate (Curve (point(1,2)) []) 180 == Curve (point(-1,-2)) []
test5  = translate (Curve (point(1,2)) [point(2,1)]) (point(4,4)) == Curve (point(4,4)) [point(5,3)]
test6  = reflect (Curve (point(1,2)) []) Vertical 3 == Curve (point(5,2)) []
test61 = reflect (Curve (point(1,2)) []) Horizontal 3 == Curve (point(1,4)) []
test7  = bbox (Curve (point(1,2)) [point(3,4),point(2,0)]) == (point(1,0),point(3,4))
test8  = toList (Curve (point(1,2)) []) == [point(1,2)]
test9  = width  (Curve (point(1,2)) [point(3,4),point(2,0)]) == 2
test10 = height (Curve (point(1,2)) [point(3,4),point(2,0)]) == 4
test11 = hilbert $ hilbert $ hilbert $ hilbert $ curve (point (0,0)) []

hilbert :: Curve -> Curve
hilbert c = c0 `connect` c1 `connect` c2 `connect` c3
   where  w = width c
          h = height c
          p = 6

          ch = reflect c Horizontal 0

          c0 = ch `rotate` (-90) `translate` point (w+p+w, h+p+h)
          c1 = c `translate` point (w+p+w, h)
          c2 = c
          c3 = ch `rotate` 90 `translate` point (0, h+p)

toFile :: Curve -> FilePath -> IO ()
toFile c p = writeFile p (toSVG c)

main :: IO()
main = do
  putStrLn $ show test1
  putStrLn $ show test2
  putStrLn $ show test3
  putStrLn $ show test4
  putStrLn $ show test5
  putStrLn $ show test6
  putStrLn $ show test61
  putStrLn $ show test7
  putStrLn $ show test8
  putStrLn $ show test9
  putStrLn $ show test10
  toFile test11 "test.html"

