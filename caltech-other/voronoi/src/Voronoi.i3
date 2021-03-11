(* $Id$ *)
INTERFACE Voronoi;

TYPE
  Point = RECORD
    x, y : REAL;
  END;

  Triple = RECORD
    s1, s2, s3 : CARDINAL;
  END;

<* EXTERNAL mod3_start *>
PROCEDURE Init();

<* EXTERNAL mod3_setup *>
PROCEDURE Setup();

<* EXTERNAL mod3_finish *>
PROCEDURE Finish();

<* EXTERNAL mod3_addsite *>
PROCEDURE AddSite(p : Point);

<* EXTERNAL mod3_delaunay *>
PROCEDURE Delaunay();

<* EXTERNAL mod3_gettriple *>
PROCEDURE NextTriple(VAR tri : Triple) : BOOLEAN;

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
