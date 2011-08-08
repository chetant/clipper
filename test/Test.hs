import Algebra.Clipper

main = do
  let p1 = Polygon [IntPoint 0 0, IntPoint 100 0, IntPoint 100 100, IntPoint 0 100]
      p2 = Polygon [IntPoint 0 0, IntPoint 100 0, IntPoint 50 200]
  soln <- execute ctIntersection (Polygons [p1]) (Polygons [p2])
  area <- polygonArea p1
  clk <- polygonIsClockwise p1
  putStrLn $ "Intersection:" ++ show soln
  putStrLn $ "Area:" ++ show area
  putStrLn $ "Is Clockwise?:" ++ show clk

