(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Fri Nov 11 14:09:41 PST 1994 by kalsow     *)
(*      modified on Fri May  7 22:30:22 PDT 1993 by mjordan    *)

UNSAFE MODULE RTParams;

IMPORT RTLinker, RTArgs, M3toC, Text, TextF;

(* BEWARE!  Init is called before types are registered
   or main bodies are initialized. *)

VAR
  argv: UNTRACED REF ADDRESS;
  init_done := FALSE;

(* ------------------------------RTParams------------------------------------*)

PROCEDURE Nth (n: INTEGER): TEXT =
  VAR arg: UNTRACED REF ADDRESS;
  BEGIN
    IF (n < 0) OR (NumParameters <= n) THEN RETURN NIL END;
    arg := argv + n * ADRSIZE (ADDRESS);
    RETURN M3toC.CopyStoT (arg^);
  END Nth;

PROCEDURE IsPresent (n: TEXT): BOOLEAN =
  VAR  arg := argv;
  BEGIN
    FOR i := 0 TO NumParameters - 1 DO
      IF Match (n, arg^) THEN RETURN TRUE END;
      INC (arg, ADRSIZE (ADDRESS));
    END;
    RETURN FALSE;
  END IsPresent;

PROCEDURE Value (n: TEXT): TEXT =
  VAR p := RawValue (n);
  BEGIN
    IF (p = NIL) THEN RETURN NIL;
    ELSIF (p = ADR (zero)) THEN RETURN "";
    ELSE RETURN M3toC.CopyStoT (p);
    END;
  END Value;

VAR zero := 0;
PROCEDURE RawValue (n: TEXT): ADDRESS =
  VAR  arg := argv;  cp: UNTRACED REF CHAR;
  BEGIN
    FOR i := 0 TO NumParameters - 1 DO
      IF Match (n, arg^) THEN
        cp := arg^ + ADRSIZE (CHAR) * Text.Length (n);
        IF (cp^ = '=')
          THEN RETURN cp + ADRSIZE (CHAR);
          ELSE RETURN ADR (zero);
        END;
      END;
      INC (arg, ADRSIZE (ADDRESS));
    END;
    RETURN NIL;
  END RawValue;

PROCEDURE Match (name: TEXT;  arg: UNTRACED REF CHAR): BOOLEAN =
  BEGIN
    IF (name = NIL) OR (arg = NIL) THEN RETURN FALSE END;
    FOR i := 0 TO NUMBER (name^)-2 DO
      IF (name[i] # arg^) THEN RETURN FALSE END;
      INC (arg, ADRSIZE (CHAR));
    END;
    RETURN (arg^ = '\000') OR (arg^ = '=');
  END Match;

PROCEDURE Init () =
  CONST
    AWIDTH = ADRSIZE (ADDRESS);
  VAR
    info := RTLinker.info;
    n    := 0;
    cp   : UNTRACED REF ARRAY [0..3] OF CHAR;
    pp   : UNTRACED REF RECORD a, b: ADDRESS END;
    a, b : UNTRACED REF ADDRESS;
    c    : ADDRESS;
  BEGIN
    IF (init_done) THEN RETURN END;
    init_done := TRUE;
    EVAL RTArgs.ArgC ();  (* force its initialization *)

    (* extract the @M3 parameters *)
    NumParameters := 0;
    WHILE (n < info.argc) DO
      pp := info.argv + n * AWIDTH; 
      cp := pp.a;
      IF (cp # NIL) AND (cp[0] = '@') AND (cp[1] = 'M') AND (cp[2] = '3') THEN
        (* copy this guy to the end of the list *)
        FOR j := n TO info.argc-1 DO pp.a := pp.b;  INC (pp, AWIDTH) END;
        pp.a := ADR (cp[3]);
        INC (NumParameters);
        DEC (info.argc);
      ELSE (* not a runtime argument *)
        INC (n);
      END;
    END;
    argv := info.argv + (info.argc + 1) * AWIDTH;

    (* reverse the extracted parameters so they're in the right order *)
    a := argv;
    b := argv + (NumParameters - 1) * AWIDTH;
    FOR i := 0 TO NumParameters DIV 2 - 1 DO
      c := a^;  a^ := b^;  b^ := c;
      INC (a, AWIDTH);
      DEC (b, AWIDTH);
    END;

  END Init;

BEGIN
END RTParams.
