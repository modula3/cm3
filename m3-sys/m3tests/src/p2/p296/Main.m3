MODULE Main;

IMPORT IO, Fmt;

TYPE
  Small = [3 .. 9];
  Neg   = [-30 .. -12];
  R = RECORD
        a : BITS 3 FOR [0 .. 7]    := 5;
        b : BITS 5 FOR [0 .. 31]   := 17;
        c : BITS 8 FOR [0 .. 255]  := 200;
        s : BITS 4 FOR Small;            (* no default: FIRST = 3 *)
        n : BITS 8 FOR Neg;              (* no default: FIRST = -30 *)
        w : INTEGER                := -7;
      END;
  Few  = ARRAY [0 .. 2] OF R;            (* elements initialized inline *)
  Many = ARRAY [0 .. 9] OF R;            (* elements initialized in a loop *)
  Ranges = ARRAY [0 .. 5] OF [10 .. 20];
  Holder = RECORD
             x  : INTEGER;
             rs : Ranges;
             r  : R;
           END;
  O = OBJECT
        r  : R;                          (* no default, but R needs init *)
        p  : BITS 6 FOR [0 .. 63] := 42; (* packed field with a default *)
        q  : BITS 2 FOR [1 .. 3];        (* packed, zero excluded *)
        h  : Holder;
      END;

PROCEDURE PutR (tag: TEXT;  READONLY r: R) =
  BEGIN
    IO.Put (tag & " a=" & Fmt.Int (r.a) & " b=" & Fmt.Int (r.b)
            & " c=" & Fmt.Int (r.c) & " s=" & Fmt.Int (r.s)
            & " n=" & Fmt.Int (r.n) & " w=" & Fmt.Int (r.w) & "\n");
  END PutR;

PROCEDURE PutRanges (tag: TEXT;  READONLY a: Ranges) =
  BEGIN
    IO.Put (tag);
    FOR i := FIRST (a) TO LAST (a) DO IO.Put (" " & Fmt.Int (a[i])) END;
    IO.Put ("\n");
  END PutRanges;

VAR
  gR    : R;
  gMany : Many;

PROCEDURE Locals () =
  VAR
    r    : R;
    few  : Few;
    many : Many;
    h    : Holder;
  BEGIN
    PutR ("local", r);
    PutR ("few[2]", few[2]);
    PutR ("many[0]", many[0]);
    PutR ("many[9]", many[9]);
    PutRanges ("holder.rs", h.rs);
    PutR ("holder.r", h.r);
  END Locals;

PROCEDURE Heap () =
  VAR
    rr := NEW (REF R);
    ra := NEW (REF ARRAY OF R, 7);
    rm := NEW (REF ARRAY OF ARRAY OF R, 2, 3);
    o  := NEW (O);
  BEGIN
    PutR ("NEW R", rr^);
    PutR ("NEW ARRAY OF R [6]", ra[6]);
    PutR ("NEW ARRAY OF ARRAY OF R [1,2]", rm[1, 2]);
    PutR ("O.r", o.r);
    IO.Put ("O.p=" & Fmt.Int (o.p) & " O.q=" & Fmt.Int (o.q) & "\n");
    PutRanges ("O.h.rs", o.h.rs);
    PutR ("O.h.r", o.h.r);
  END Heap;

BEGIN
  PutR ("global", gR);
  PutR ("gMany[5]", gMany[5]);
  Locals ();
  Heap ();
END Main.
