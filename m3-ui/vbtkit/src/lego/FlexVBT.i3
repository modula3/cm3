(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jun 11 21:42:15 PDT 1993 by meehan                   *)
(*      modified on Sun Jan 31 23:10:25 PST 1993 by mhb                      *)
(*      modified on Tue Jun 16 13:08:50 PDT 1992 by muller                   *)
<* PRAGMA LL *>

(* The "FlexVBT.T" is a filter whose shape is based on a {\em natural}
   size with some {\em stretch} and {\em shrink}.  If a natural amount
   is left unspecified, the stretch and shrink are applied relative to
   the child's size.  If a stretch or shrink is left unspecified, 0 is
   assumed.  All units are specified in millimeters.  See
   Figure~\ref{flexvbt} for examples.

   This interface is similar to "RigidVBT", but more powerful in that
   one can specify a size based on a child's size and can dynamically
   change the size specification.  Also, it presents a slightly
   different model to the client: In "RigidVBT", one thinks in terms
   of the low and high bounds of some range.  Here, one thinks in
   terms of the amount thed natural size value can be stretched and
   shrunk. *)

INTERFACE FlexVBT;

IMPORT Axis, Filter, VBT;

CONST
  Large    = 99999.0;
  Missing  = -Large;
  Infinity = Large;

TYPE
  SizeRange = RECORD natural, shrink, stretch: REAL END;
  Shape     = ARRAY Axis.T OF SizeRange;

(* Some useful shapes are defined at the end of this interface. *)

TYPE
  T <: Public;
  Public = Filter.T OBJECT
           METHODS
             <* LL.sup <= VBT.mu *>
             init (ch: VBT.T; READONLY sh := Default): T
           END;

(* The call "v.init(ch, sh)" initializes "v" as a "FlexVBT" with child
   "ch" and shape specification "sh".  The default shape causes "v" to
   be a no-op: it will simply return the shape of its child as its
   own. *)

PROCEDURE FromAxis (         ch: VBT.T;
                             ax: Axis.T;
                    READONLY sh: SizeRange := DefaultRange): T;
<* LL.sup <= VBT.mu *>
(* Return a "FlexVBT" whose shape specification in the "ax" dimension
   is "sh" and whose shape in the other dimension is that of "ch". *)

PROCEDURE Set (v: T; READONLY sh: Shape);
<* LL.sup = VBT.mu.v *>
(* Change the shape of "v" to "sh", and notify "v"'s parent that
   "v"'s size has changed. *)

PROCEDURE Get (v: T): Shape;
<* LL.sup = VBT.mu.v *>
(* Get the shape of "v". *)

PROCEDURE SetRange (v: T; ax: Axis.T; READONLY sr: SizeRange);
<* LL.sup = VBT.mu.v *>
(* Change the shape of "v" to "sr" along the "ax" axis, and
   notify "v"'s parent that "v"'s size has changed. *)

(* The rest of this interface defines some useful shapes: "Default"
   uses child's size; "Fixed" uses child's preferred, removing all
   shrink and stretch; "Stretchy" uses child's preferred and shrink,
   giving infinite stretch; and "Rigid" is a procedure to set a shape
   to a specified natural size, with neither stretch nor shrink.  *)

CONST
  Default  = Shape{DefaultRange, DefaultRange};
  DefaultRange =
    SizeRange {natural := Missing,
               shrink  := Missing, 
               stretch := Missing};

  Fixed   = Shape{FixedRange, FixedRange};
  FixedRange =
    SizeRange {natural := Missing, 
               shrink  := 0.0, 
               stretch := 0.0};

  Stretchy = Shape{StretchyRange, StretchyRange};
  StretchyRange = 
    SizeRange {natural := Missing,
               shrink  := Missing, 
               stretch := Infinity};
               
PROCEDURE RigidRange (natural: REAL): SizeRange;
<* LL = arbitrary *>
(* Return a "SizeRange" with the specified natural amount and
   with no stretch or shrink.  Equivalent to
|  SizeRange {natural, 0.0, 0.0}
*)

PROCEDURE Rigid (hNat, vNat: REAL): Shape;
<* LL = arbitrary *>
(* Return a "Shape" with the specified natural amounts long
   the horizontal and vertical axes and
   with no stretch or shrink.  Equivalent to
|  Shape {SizeRange {hNat, 0.0, 0.0},
|         SizeRange {vNat, 0.0, 0.0}}
*)

END FlexVBT.















