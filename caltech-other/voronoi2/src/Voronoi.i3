(* $Id: Voronoi.i3,v 1.2 2004/10/30 23:29:16 mika Exp $ *)
INTERFACE Voronoi;

TYPE
  Point = RECORD
    x, y : REAL;
  END;

  Triple = RECORD
    s1, s2, s3 : [-1..LAST(CARDINAL)];
  END;

<* EXTERNAL mod3_start *>
PROCEDURE Init();

<* EXTERNAL mod3_setup *>
PROCEDURE Setup();

<* EXTERNAL mod3_finish *>
PROCEDURE Finish();

<* EXTERNAL mod3_addsite *>
PROCEDURE AddSite(p : Point);

<* EXTERNAL mod3_addsite2 *>
PROCEDURE AddSite2(x, y : LONGREAL);

<* EXTERNAL mod3_delaunay *>
PROCEDURE Delaunay();

<* EXTERNAL mod3_gettriple *>
PROCEDURE NextTriple(VAR tri : Triple) : BOOLEAN;

TYPE CTriple = ADDRESS;

<*EXTERNAL mod3_getnext*>
PROCEDURE Next() : CTriple;

<*EXTERNAL mod3_isnull*>
PROCEDURE IsNull(tri : CTriple) : BOOLEAN;

<*EXTERNAL mod3_getidx*>
PROCEDURE GetIdx(tri : CTriple; idx : INTEGER (* 0..2 *)) : INTEGER;

<* EXTERNAL mod3_voronoi *>
PROCEDURE Voronoi();

(* how to use this code: 

   LOCK mu DO 
     Init();

     AddSite(p0);
     AddSite(p1);
     ...
     AddSite(pn-1);

     Setup();

     Delaunay();

     WHILE NextTriple(t) DO
       ...
     END;

     Finish()
   END
*)

VAR mu : MUTEX;

END Voronoi.
