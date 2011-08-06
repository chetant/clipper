{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
module Algebra.Clipper where

import Foreign
import Foreign.C.Types
import Data.Int(Int64)
import Data.Word(Word64)

#include <clipper.hpp>

-- enum ClipType { ctIntersection, ctUnion, ctDifference, ctXor };
newtype ClipType = ClipType Int
#enum ClipType, ClipType, ctIntersection = ctIntersection, ctUnion = ctUnion, ctDifference = ctDifference, ctXor = ctXor

-- enum PolyType { ptSubject, ptClip };
newtype PolyType = PolyType Int
#enum PolyType, PolyType, ptSubject = ptSubject, ptClip = ptClip

-- enum PolyFillType { pftEvenOdd, pftNonZero };
newtype PolyFillType = PolyFillType Int
#enum PolyFillType, PolyFillType, pftEvenOdd = pftEvenOdd, pftNonZero = pftNonZero

-- typedef signed long long long64;
-- typedef unsigned long long ulong64;

-- struct IntPoint {
--   long64 X;
--   long64 Y;
--   IntPoint(long64 x = 0, long64 y = 0): X(x), Y(y) {};
-- };

data IntPoint = IntPoint
    {
      pointX :: #{type long64}
    , pointY :: #{type long64}
    }

-- typedef std::vector< IntPoint > Polygon;
-- typedef std::vector< Polygon > Polygons;

data Polygon
type PolygonPtr = Ptr Polygon

data Polygons
type PolygonsPtr = Ptr Polygons

-- struct ExPolygon {
--   Polygon  outer;
--   Polygons holes;
-- };
-- typedef std::vector< ExPolygon > ExPolygons;

data ExPolygon
type ExPolygonPtr = Ptr ExPolygon

data ExPolygons
type ExPolygonsPtr = Ptr ExPolygons

-- extern "C" {
-- typedef void * polygon;
-- typedef void * polygons;

--   polygon polygon_new(int numPoints);
foreign import ccall "static clipper.hpp polygon_new"
        polygonNew :: #{type int} -> IO PolygonPtr

--   void polygon_addPoint(polygon poly, long64 x, long64 y)
foreign import ccall "static clipper.hpp polygon_new"
        polygonAddPoint :: PolygonPtr -> #{type long64} -> #{type long64} -> IO ()

--   void polygon_free(polygon poly);
foreign import ccall "static clipper.hpp polygon_free"
        polygonFree :: PolygonPtr -> IO ()

--   int polygon_isClockwise(polygon poly, int useFullInt64Range)
foreign import ccall "static clipper.hpp polygon_isClockwise"
        polygonIsClockwise :: PolygonPtr -> #{type int} -> IO #{type int}

--   double polygon_getArea(polygon poly,, int useFullInt64Range)
foreign import ccall "static clipper.hpp polygon_getArea"
        polygonArea :: PolygonPtr -> #{type int} -> IO #{type double}

--   polygons polygons_new(int numPolys);
foreign import ccall "static clipper.hpp polygons_new"
        polygonsNew :: #{type int}  -> IO PolygonsPtr

--   void polygons_addPoly(polygons polys, polygon poly);
foreign import ccall "static clipper.hpp polygons_addPoly"
        polygonsAddPoly :: PolygonsPtr -> PolygonPtr -> IO ()

--   void polygons_free(polygons poly);
foreign import ccall "static clipper.hpp polygons_new"
        polygonsFree :: PolygonsPtr -> IO ()

--   clipper clipper_new();
--   void clipper_addPolygon(clipper c, polygon poly, PolyType ptype);
--   void clipper_addPolygons(clipper c, polygons poly, PolyType ptype);
--   void clipper_executePoly(clipper c, ClipType ctype, polygons soln);
--   void clipper_free(clipper c);

-- }
