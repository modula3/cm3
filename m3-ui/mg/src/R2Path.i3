(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Stephen Harrison and Steve Glassman *)
(* *)
(* Last modified on Fri Aug 19 16:34:41 PDT 1994 by steveg   *)
(*      modified on Sun Jul 19 12:02:30 PDT 1992 by harrison *)

<*PRAGMA LL*>

(* Hacked up from Path.i3---see that file for authors. *)

(* A "R2Path.T" is a sequence of straight and curved line segments,
   suitable for converting to a "Path.T"---suitable for stroking or
   filling.

   A {\it segment} is a directed arc in the Cartesian plane determined by
   two cubic polynomials "h(t)", "v(t)", where "t" ranges over the interval
   of real numbers "[0, 1]".  The segment is said to {\it start} at "(h(0),
   v(0))" and {\it end} at "(h(1), v(1))".  If "h" and "v" are linear
   functions of "t", then the segment is {\it linear}: it consists of a
   line segment.  If "h" and "v" are constant functions of "t", then the
   segment is {\it degenerate}: it consists of a single point.

   The segments of a path are grouped into contiguous {\it subpaths}, which
   can be {\it open} or {\it closed}.  Within a subpath, each segment
   starts where the previous segment ends.  In a closed subpath, the last
   segment ends where the first segment starts.  (This may also happen for
   an open subpath, but this coincidence does not make the subpath closed.)

   The {\it current point} of a path is the endpoint of the last segment of
   its last subpath, assuming this subpath is open.  If the path is empty
   or if the last subpath is closed, the current point is undefined.

   The call "NEW(R2Path.T)" creates an empty path. *)

INTERFACE R2Path;

IMPORT Matrix2D, R2, R2Box;

TYPE
  T <: PublicT;

  PublicT =
    OBJECT
    METHODS
      init ();
      (* Set "self" to be empty. *)

      moveTo (READONLY p: R2.T);
      (* Extend "self" with a new degenerate segment that starts and ends
         at "p".  This begins a new subpath. *)

      lineTo (READONLY p: R2.T);
      (* Extend "self" with a linear segment that starts at its current
         point and ends at "p". *)

      arcTo (READONLY center: R2.T; READONLY radius, ang1, ang2: REAL);
      (* Extend "self" with an arc of a circle, possibly preceded by a
         straight line segment.  The arc is defined by a "radius" and two
         tangent lines---one drawn from the current point to "p", and the
         other drawn from "p" to "q".  A straight line segment is added to
         the path before the arc if "p" is not the same as the current
         point. *)

      curveTo (READONLY q, r, s: R2.T);
      (* Extend "self" with a curved segment that starts at its current
         point and ends at "s". *)

      (* "CurveTo" adds a curve that starts from the current point of
         "self" in the direction of "q", and ends at "s" coming from the
         direction of "r".  More precisely, let "p" be the current point of
         "self" and let "h(t)" and "v(t)" be the cubic polynomials such
         that

|        (h(0), v(0)) = p
|        (h(1), v(1)) = s
|        (h'(0), v'(0)) = 3 * (q - p)
|        (h'(1), v'(1)) = 3 * (s - r)

         (Where the primes denote differentiation with respect to "t".)
         Then "CurveTo" adds the segment "(h(t), v(t))" for "t" between
         zero and one.  (This is called the {\it Bezier} arc determined by
         "p", "q", "r", and "s".) *)

      close ();
      (* Add a linear segment to create a closed loop in "self". *)

      (* More precisely, let "p" be the current point of "self", and let
         "q" be last point of "self" that was added by a call to "MoveTo"
         (Thus "q" is the startpoint of the first segment of the last
         subpath of "self".) "Close" adds a linear segment from "p" to "q"
         and marks the sequence of segments from "q" to the end of the path
         as a closed subpath. *)

      isEmpty (): BOOLEAN;
      (* Returns "TRUE" if "self" is empty. *)

      isClosed (): BOOLEAN;
      (* Returns "TRUE" if path is empty or the last subpath of /self/ is
         closed. *)

      currentPoint (): R2.T;
      (* Returns the current point of /self/. *)

      (* "LineTo", "CurveTo", "Close", and "CurrentPoint" are checked
         runtime errors if the path has no current point. *)

      translate (READONLY delta: R2.T): T;
      (* The result of translating self by "delta". *)

      copy (): T;
      (* Returns a newly allocated path with the same contents as
         /self/. *)

      map (map: MapObject);
      (* Apply the appropriate method of "map" to each element of
         "self". *)

      bbox(READONLY matrix := Matrix2D.Identity): R2Box.T;
      (* Return the bounding box of "self" transformed by "matrix". *)
    END;

TYPE
  MapObject =
    OBJECT
    METHODS
      move (READONLY p: R2.T);
      line (READONLY p: R2.T);
      arc (READONLY center: R2.T; READONLY radius, ang1, ang2: REAL);
      close ();
      curve (READONLY p, q, r: R2.T)
    END;

END R2Path.
