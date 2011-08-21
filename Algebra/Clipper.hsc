{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
module Algebra.Clipper
(
 ClipType,ctIntersection,ctUnion,ctDifference,ctXor
,PolyType,ptSubject,ptClip
,PolyFillType,pftEvenOdd,pftNonZero
,IntPoint(..)
,Polygon(..), getPoints
,Polygons(..)
,execute
,intersection
,union
,difference
,Algebra.Clipper.xor
,polygonArea
,polygonIsClockwise
) where

import Foreign
import Foreign.C.Types
import Data.Int(Int64)
import Data.Word(Word64)
import Data.Monoid
import Control.Applicative((<$>), (<*>))

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

-- struct IntPoint {
--   long64 X;
--   long64 Y;
--   IntPoint(long64 x = 0, long64 y = 0): X(x), Y(y) {};
-- };

data IntPoint = IntPoint
    {
      pointX :: #{type long64}
    , pointY :: #{type long64}
    } deriving Show

-- typedef std::vector< IntPoint > Polygon;
-- typedef std::vector< Polygon > Polygons;

newtype Polygon = Polygon [IntPoint] deriving Show
type PolygonPtr = Ptr Polygon

getPoints (Polygon ps) = ps

instance Monoid Polygon where
    mempty = Polygon mempty
    mappend (Polygon x) (Polygon y) = Polygon (x `mappend` y)

instance Storable Polygon where
    sizeOf _ = #{size Polygon}
    alignment _ = alignment (undefined :: CInt)
    peek ptr = do
         numPts <- fromIntegral <$> polygonSize ptr
         Polygon <$> mapM (polygonGetPoint ptr) [0..(numPts-1)]
    poke ptr (Polygon ps) = do
         polygonClear ptr
         mapM_ addPt ps
        where addPt (IntPoint x y) = polygonAddPoint ptr x y

newtype Polygons = Polygons [Polygon] deriving Show
type PolygonsPtr = Ptr Polygons

size (Polygon ps) = fromIntegral $ length ps
sizes (Polygons ps) = fromIntegral $ length ps

instance Monoid Polygons where
    mempty = Polygons mempty
    mappend (Polygons x) (Polygons y) = Polygons (x `mappend` y)

instance Storable Polygons where
    sizeOf _ = #{size Polygons}
    alignment _ = alignment (undefined :: CInt)
    peek ptr = do
         numPolys <- fromIntegral <$> polygonsSize ptr
         polyPtrs <- mapM (polygonsGetPoly ptr) [0..(numPolys-1)]
         Polygons <$> mapM peek polyPtrs
    poke ptr (Polygons ps) = do
         polygonsClear ptr
         mapM_ addPoly ps
        where addPoly poly = polygonNew (size poly) >>= 
                             newForeignPtr polygonFree >>= 
                             flip withForeignPtr (setPoly poly)
              setPoly poly pptr = poke pptr poly >> polygonsAddPoly ptr pptr

-- struct ExPolygon {
--   Polygon  outer;
--   Polygons holes;
-- };
-- typedef std::vector< ExPolygon > ExPolygons;

data ExPolygon
type ExPolygonPtr = Ptr ExPolygon

data ExPolygons
type ExPolygonsPtr = Ptr ExPolygons

data Clipper
type ClipperPtr = Ptr Clipper

-- extern "C" {
-- typedef void * polygon;
-- typedef void * polygons;

polygonGetPoint :: PolygonPtr -> Int -> IO IntPoint
polygonGetPoint ptr i = IntPoint <$> polygonGetPointX ptr i' <*> polygonGetPointY ptr i'
    where i' = fromIntegral i

polygonArea :: Polygon -> IO Double
polygonArea poly = do
  fptr <- polygonNew (size poly) >>= 
          newForeignPtr polygonFree
  withForeignPtr fptr (flip poke poly)
  withForeignPtr fptr (flip polygonArea_ 0)

polygonIsClockwise :: Polygon -> IO Bool
polygonIsClockwise poly = do
  fptr <- polygonNew (size poly) >>= 
          newForeignPtr polygonFree
  withForeignPtr fptr (flip poke poly)
  ret <- withForeignPtr fptr (flip polygonIsClockwise_ 0)
  if ret == 0 then return False else return True

execute :: ClipType -> Polygons -> Polygons -> IO Polygons
execute cType sPolys cPolys = clipperNew >>= 
                              newForeignPtr clipperFree >>= 
                              flip withForeignPtr exec_
    where exec_ cPtr = do
            spPtr <- polygonsNew (sizes sPolys) >>= newForeignPtr polygonsFree
            withForeignPtr spPtr (\subptr -> poke subptr sPolys >> 
                                             clipperAddPolygons cPtr subptr ptSubject)
            cpPtr <- polygonsNew (sizes cPolys) >>= newForeignPtr polygonsFree
            withForeignPtr cpPtr (\clpptr -> poke clpptr cPolys >> 
                                             clipperAddPolygons cPtr clpptr ptClip)
            rPtr <- polygonsNew 0 >>= newForeignPtr polygonsFree
            withForeignPtr rPtr (\resPtr -> clipperExecutePolys cPtr cType resPtr)
            withForeignPtr rPtr peek

intersection = execute ctIntersection
union = execute ctUnion
difference = execute ctDifference
xor = execute ctXor

--   long64 polygon_getPointX(polygon poly, int i);
foreign import ccall "clipper.hpp polygon_getPointX"
        polygonGetPointX :: PolygonPtr -> #{type int} -> IO #{type long64}

--   long64 polygon_getPointY(polygon poly, int i);
foreign import ccall "clipper.hpp polygon_getPointY"
        polygonGetPointY :: PolygonPtr -> #{type int} -> IO #{type long64}

--   polygon polygon_new(int numPoints);
foreign import ccall "clipper.hpp polygon_new"
        polygonNew :: #{type int} -> IO PolygonPtr

--   void polygon_clear(polygon poly);
foreign import ccall "clipper.hpp polygon_clear"
        polygonClear :: PolygonPtr -> IO ()

--   void polygon_size(polygon poly);
foreign import ccall "clipper.hpp polygon_size"
        polygonSize :: PolygonPtr -> IO CInt

--   void polygon_addPoint(polygon poly, long64 x, long64 y)
foreign import ccall "clipper.hpp polygon_addPoint"
        polygonAddPoint :: PolygonPtr -> #{type long64} -> #{type long64} -> IO ()

--   void polygon_free(polygon poly);
foreign import ccall "clipper.hpp &polygon_free"
        polygonFree :: FunPtr (PolygonPtr -> IO ())

--   int polygon_isClockwise(polygon poly, int useFullInt64Range)
foreign import ccall "clipper.hpp polygon_isClockwise"
        polygonIsClockwise_ :: PolygonPtr -> #{type int} -> IO #{type int}

--   double polygon_getArea(polygon poly, int useFullInt64Range)
foreign import ccall "clipper.hpp polygon_getArea"
        polygonArea_ :: PolygonPtr -> #{type int} -> IO #{type double}

--   polygons polygons_new(int numPolys);
foreign import ccall "clipper.hpp polygons_new"
        polygonsNew :: #{type int}  -> IO PolygonsPtr

--   void polygons_clear(polygons poly);
foreign import ccall "clipper.hpp polygons_clear"
        polygonsClear :: PolygonsPtr -> IO ()

--   void polygons_size(polygons poly);
foreign import ccall "clipper.hpp polygons_size"
        polygonsSize :: PolygonsPtr -> IO CInt

--   void polygons_addPoly(polygons polys, polygon poly);
foreign import ccall "clipper.hpp polygons_addPoly"
        polygonsAddPoly :: PolygonsPtr -> PolygonPtr -> IO ()

--   polygon polygons_getPoly(polygons polys, int i);
foreign import ccall "clipper.hpp polygons_getPoly"
        polygonsGetPoly :: PolygonsPtr -> CInt -> IO PolygonPtr

--   void polygons_free(polygons poly);
foreign import ccall "clipper.hpp &polygons_free"
        polygonsFree :: FunPtr (PolygonsPtr -> IO ())

--   clipper clipper_new();
foreign import ccall "clipper.hpp clipper_new"
        clipperNew :: IO ClipperPtr

--   void clipper_addPolygon(clipper c, polygon poly, PolyType ptype);
foreign import ccall "clipper.hpp clipper_addPolygon"
        clipperAddPolygon :: ClipperPtr -> PolygonPtr -> PolyType -> IO ()

--   void clipper_addPolygons(clipper c, polygons poly, PolyType ptype);
foreign import ccall "clipper.hpp clipper_addPolygons"
        clipperAddPolygons :: ClipperPtr -> PolygonsPtr -> PolyType -> IO ()

--   void clipper_executePoly(clipper c, ClipType ctype, polygons soln);
foreign import ccall "clipper.hpp clipper_executePoly"
        clipperExecutePolys :: ClipperPtr -> ClipType -> PolygonsPtr -> IO ()

--   void clipper_free(clipper c);
foreign import ccall "clipper.hpp &clipper_free"
        clipperFree :: FunPtr (ClipperPtr -> IO ())

-- }
