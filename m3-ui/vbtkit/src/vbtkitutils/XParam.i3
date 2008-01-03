(* Copyright (C) 1992, Digital Equipment Corporation                       *)
(* All rights reserved.                                                    *)
(* See the file COPYRIGHT for a full description.                          *)
(*                                                                         *)
(* Last modified on Fri Jun 11 22:55:20 PDT 1993 by meehan                 *)
(*      modified on Tue Feb  2 00:38:05 PST 1993 by mhb                    *)
(*      modified on Tue Jun 16 21:55:35 PDT 1992 by muller                 *)
<* PRAGMA LL *>

(* The "XParam" interface provides utilities for handling X-style
   "-display" and "-geometry" command-line arguments.  If your
   application installs a single top-level window, the "XTrestle"
   interface may be more appropriate than this interface. *)

INTERFACE XParam;

IMPORT Point, Rect, Trestle, TrestleComm;

(* Here are routines for manipulating the "-display" argument: *)

TYPE
  Display = RECORD
              hostname: TEXT     := "";
              display : CARDINAL := 0;
              screen  : CARDINAL := 0;
              DECnet  : BOOLEAN  := FALSE
            END;

PROCEDURE ParseDisplay (spec: TEXT): Display RAISES {Error};
<* LL = arbitrary *>
(* Return a parsed version of the "-display" argument in "spec".*)

(* For example, if "spec" contains the string
   "myrtle.pa.dec.com:0.2", the record returned would be
|   Display{hostname := "myrtle.pa.dec.com",
|           display := 0, screen := 2, DECnet := FALSE}
   *)

PROCEDURE UnparseDisplay (READONLY d: Display): TEXT;
<* LL = arbitrary *>
(* Return the text-version of the "-display" argument "d". *)

(* Here are routines for manipulating the "-geometry" argument: *)

CONST Missing = Point.T{-1, -1};

TYPE
  Geometry =
    RECORD
      vertex := Rect.Vertex.NW;  (* corner for displacement *)
      dp     := Point.Origin;    (* displacement *)
      size   := Missing;         (* width, height *)
    END;

PROCEDURE ParseGeometry (spec: TEXT): Geometry RAISES {Error};
<* LL = arbitrary *>
(* Return a parsed version of the "-geometry" argument in "spec".  *)

(* For example, if "spec" contains the string
   "1024x800-0-10", the returned record would be
|     Geometry {Rect.Vertex.SE,
|               Point.T {0, 10},
|               Point.T {1024, 800}}
   The "size" field defaults to "Missing".  The horizontal and
   vertical displacements default to "Point.Origin" (no
   displacement).  The displacements are always positive values;
   use the "vertex" field to find out from which corner they are
   to be offset. *)

PROCEDURE UnparseGeometry (READONLY g: Geometry): TEXT;
<* LL = arbitrary *>
(* Return the text-version of the "-geometry" argument "g". *)

PROCEDURE Position (         trsl: Trestle.T;
                             id  : Trestle.ScreenID;
                    READONLY g   : Geometry          ): Point.T
  RAISES {TrestleComm.Failure};
<* LL.sup = VBT.mu *>
(* Return the position specified by "g" in the screen coordinates
   for the screenID "id" on the window system connected to "trsl"
   (cf.  "Trestle.GetScreens").  The value of "g.size" must not
   be "Missing", unless "g.vertex" is the northwest corner. *)

(* Here is the definition of the "Error" exception: *)

TYPE
  Info = OBJECT
           spec : TEXT;
           index: CARDINAL
         END;
  GeometryInfo = Info BRANDED OBJECT END;
  DisplayInfo  = Info BRANDED OBJECT END;

EXCEPTION Error(Info);
(* Parsing errors are reported with the text ("spec") and
   position ("index") of the first illegal character in the
   text. *)

END XParam.

(* \subsubsection{An example}

   Here is an example of how to use this interface to install a VBT
   "v" as a top level window, obeying the display and geometry
   arguments given to the application. It relies on the "Params"
   interface, which provides the number of arguments passed to the
   program, "Params.Count", and a procedure to retrieve the value of
   the "n"th argument, "Params.Get(n)".

|  EXCEPTION Error (TEXT);
|  VAR
|    display, geometry: TEXT := NIL;
|    d: XParam.DisplayRec;
|    g: XParam.Geometry;
|    i: CARDINAL := 1;
|  BEGIN
|    LOOP
|      IF i >= Params.Count - 1 THEN EXIT END;
|      WITH argument = Params.Get (i) DO
|        IF Text.Equal (argument, "-display") THEN
|          display := Params.Get (i + 1);
|          TRY d := XParam.ParseDisplay (display)
|          EXCEPT XParam.Error (info) =>
|            RAISE Error ("Illegal -display argument: "
|                          & info.spec)
|          END;
|          INC (i, 2)
|        ELSIF Text.Equal (argument, "-geometry") THEN
|          geometry := Params.Get (i + 1);
|          TRY
|            g := XParam.ParseGeometry (geometry);
|            IF g.size = XParam.Missing THEN
|              WITH shapes = VBTClass.GetShapes (v, FALSE) DO
|                g.size.h := shapes [Axis.T.Hor].pref;
|                g.size.v := shapes [Axis.T.Ver].pref
|              END
|            END
|          EXCEPT XParam.Error (info) =>
|            RAISE Error ("Illegal -geometry argument: "
|                          & info.spec);
|          END;
|          INC (i, 2)
|        ELSE INC (i)
|        END          (* IF *)
|      END            (* WITH *)
|    END;             (* LOOP *)


   At this point, if "display" is non-"NIL", then "d" contains the
   information from the "-display" argument.  Similarly, if "geometry"
   is non-"NIL", then "g" contains the information from the
   "-geometry" argument.  If the window-size specificiation was
   missing, the preferred shape of the window is used.

   Finally, we now process the "display" and "geometry" information:

|    VAR
|      trsl := Trestle.Connect (display);
|      screen: CARDINAL;
|    BEGIN
|      TrestleImpl.SetDefault (trsl);
|      Trestle.Attach (v, trsl);
|      Trestle.Decorate (v, ...);
|      IF geometry = NIL THEN
|        Trestle.MoveNear (v, NIL)
|      ELSE
|        StableVBT.SetShape (v, g.size.h, g.size.v)
|        IF d = NIL THEN
|          screen := Trestle.ScreenOf (v, Point.Origin).id
|        ELSE
|          screen := d.screen
|        END;
|        Trestle.Overlap (
|          v, screen, XParam.Position(trsl, screen, g))
|      END      (* IF *)
|    END        (* BEGIN *)
|  END;         (* BEGIN *)

   The call to "TrestleImpl.SetDefault" establishes the value of the
   "-display" argument as the default Trestle connection.  The call to
   "StableVBT.SetShape" is used to control the size of a top-level
   window. The "TrestleImpl" and "StableVBT" interfaces are part of
   Trestle.*)


