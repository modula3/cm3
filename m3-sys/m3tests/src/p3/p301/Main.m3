MODULE Main;

IMPORT IO, Fmt;

TYPE
  Rec = RECORD a, b, c: INTEGER END;
  O   = OBJECT METHODS
          m (VAR n: INTEGER;  READONLY xs: ARRAY OF INTEGER): Rec;
        END;
  OI  = O OBJECT OVERRIDES m := M END;
  PV  = PROCEDURE (VAR n: INTEGER;  READONLY xs: ARRAY OF INTEGER): Rec;

PROCEDURE M (<*UNUSED*> self: OI;  VAR n: INTEGER;
             READONLY xs: ARRAY OF INTEGER): Rec =
  BEGIN
    INC (n);
    RETURN Rec {n, NUMBER (xs), xs[LAST (xs)]};
  END M;

PROCEDURE F (VAR n: INTEGER;  READONLY xs: ARRAY OF INTEGER): Rec =
  BEGIN
    INC (n, 100);
    RETURN Rec {n, NUMBER (xs), xs[FIRST (xs)]};
  END F;

PROCEDURE Show (tag: TEXT;  READONLY r: Rec) =
  BEGIN
    IO.Put (tag & ": a=" & Fmt.Int (r.a) & " b=" & Fmt.Int (r.b)
            & " c=" & Fmt.Int (r.c) & "\n");
  END Show;

VAR
  o   : O  := NEW (OI);
  pv  : PV := F;
  k   := 10;
  arr := ARRAY [0 .. 3] OF INTEGER {1, 2, 3, 4};

BEGIN
  Show ("o.m", o.m (k, arr));
  IO.Put ("k=" & Fmt.Int (k) & "\n");
  Show ("OI.m", OI.m (o, k, SUBARRAY (arr, 1, 2)));
  IO.Put ("k=" & Fmt.Int (k) & "\n");
  Show ("pv", pv (k, arr));
  IO.Put ("k=" & Fmt.Int (k) & "\n");
END Main.
