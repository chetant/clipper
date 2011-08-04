{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecl #-}
module Algebra.Clipper where

import Foreign
import Foreign.C.Types
import Data.Int(Int64)
import Data.Word(Word64)

#include <clipper.hpp>

-- enum ClipType { ctIntersection, ctUnion, ctDifference, ctXor };
#{enum ClipType, ClipType
 , ctIntersection
 , ctUnion
 , ctDifference
 , ctXor
 }

-- enum PolyType { ptSubject, ptClip };
#{enum PolyType, PolyType
 , ptSubject
 , ptClip
 }

-- enum PolyFillType { pftEvenOdd, pftNonZero };
#{enum PolyFillType, PolyFillType
 , pftEvenOdd
 , pftNonZero
 }

-- typedef signed long long long64;
-- typedef unsigned long long ulong64;

-- struct IntPoint {
--   long64 X;
--   long64 Y;
--   IntPoint(long64 x = 0, long64 y = 0): X(x), Y(y) {};
-- };

data IntPoint = IntPoint
    {
      pointX :: Int64
    , pointY :: Int64
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

data ExPolygons
type ExPolygonPtr = Ptr ExPolygon

-- bool IsClockwise(const Polygon &poly, bool UseFullInt64Range = true);
-- double Area(const Polygon &poly, bool UseFullInt64Range = true);
-- bool OffsetPolygons(const Polygons &in_pgs, Polygons &out_pgs, const float &delta);

-- extern "C" {

--   typedef void * polygon;
--   typedef void * polygons;

--   polygon polygon_new(int numPoints);
foreign import ccall "static clipper.hpp polygon_new"
        newPolygon :: IO PolygonPtr

--   void polygon_free(polygon poly);
foreign import ccall "static clipper.hpp polygon_new"
        freePolygon :: PolygonPtr -> IO ()

-- }
