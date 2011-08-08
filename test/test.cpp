#include <stdio.h>
#include <clipper.hpp>

int main(int argc, char * argv[])
{
  printf("hello\n");
  Polygon p1;
  Polygon p2;
  Polygons ps1;
  Polygons ps2;

  IntPoint i1(0, 0);
  IntPoint i2(0, 100);
  IntPoint i3(100, 100);
  IntPoint i4(100, 0);

  IntPoint j1(0, 0);
  IntPoint j2(50, 200);
  IntPoint j3(100, 0);

  p1.push_back(i1);
  p1.push_back(i2);
  p1.push_back(i3);
  p1.push_back(i4);

  p2.push_back(j1);
  p2.push_back(j2);
  p2.push_back(j3);

  ps1.push_back(p1);
  ps2.push_back(p2);

  Clipper clipper;

  clipper.AddPolygons(ps1, ptSubject);
  clipper.AddPolygons(ps2, ptClip);
  Polygons soln;
  clipper.Execute(ctIntersection, soln);

  for(int i = 0; i < soln.size(); i++)
  {
    Polygon& p = soln[i];
    for(int j = 0; j < p.size(); j++)
    {
      printf("Point:(%lld,%lld)\n", p[j].X, p[j].Y);
    }
  }

  printf("hehe\n");
  return 0;
}
