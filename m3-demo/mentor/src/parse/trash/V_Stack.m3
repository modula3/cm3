(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Sat Jul 25 00:56:10 1992 by kalsow     *)

MODULE V_Stack;

IMPORT HSV, PaintOp, PaintOpCache, RectsVBT, View, ZeusPanel;
IMPORT ParseViewClass, Parse;

TYPE
  T = ParseViewClass.T BRANDED OBJECT
        rects     : RectsVBT.T := NIL;
        max_stack : INTEGER    := 0;
        depth     : INTEGER    := 0;
      OVERRIDES
        oeSetup   := Setup;
        oePush    := Push;
        oePop     := Pop;
      END;

(*---------------------------------------------------------------- events ---*)

PROCEDURE Setup (t: T;  <*UNUSED*> s: Parse.State) =
  VAR r := t.rects;
  BEGIN
    ExpandStack (t);
    t.max_stack := 10;
    RectsVBT.SetN (r, t.max_stack);
    RectsVBT.SetWC (r, -0.1, -0.1, 1.1, FLOAT (t.max_stack) + 0.1);
  END Setup;

PROCEDURE Push (t: T;  <*UNUSED*>tag: TEXT) =
  VAR r := t.rects;  n := t.depth;
  BEGIN
    IF (t.depth >= t.max_stack) THEN ExpandStack (t) END;
    RectsVBT.Position (r, n, 0.0, FLOAT(n), 1.0, FLOAT (n+1));
    RectsVBT.Color (r, n, GetColor (n));
    RectsVBT.Draw (r, n);
    INC (t.depth);
  END Push;

PROCEDURE Pop  (t: T) =
  BEGIN
    DEC (t.depth);
    RectsVBT.Erase (t.rects, t.depth);
  END Pop;

(*-------------------------------------------------------------- internal ---*)

PROCEDURE ExpandStack (t: T;  new_size: INTEGER := 10) =
  VAR r := t.rects;
  BEGIN
    new_size := MAX (new_size, 2 * t.max_stack);
    t.max_stack := new_size;
    RectsVBT.SetN (r, new_size);
    RectsVBT.SetWC (r, -0.1, -0.1, 1.1, FLOAT (new_size) + 0.1);
  END ExpandStack;

PROCEDURE GetColor (n: INTEGER): PaintOp.T =
  VAR rgb := HSV.RGBFromHue (FLOAT (n MOD 4) / 4.0);
  BEGIN
    RETURN PaintOpCache.FromRGB(rgb);
  END GetColor;

(*------------------------------------------------------------------ init ---*)

PROCEDURE New (): View.T =
  VAR r := NEW(RectsVBT.T).init();
  BEGIN
    RETURN NEW(T, rects := r).init(r)
  END New;

BEGIN
  ZeusPanel.RegisterView (New, "parse stack", "Parse");
END V_Stack.
