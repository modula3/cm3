(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Dec 12 15:56:41 PST 1994 by kalsow     *)

MODULE WebInfo;

IMPORT Word, Target, M3Buf, M3ID, Value, Scope, Module, Scanner;
IMPORT Expr, UserProc;

VAR
  info: M3Buf.T := NIL;

(*------------------------------------------------------ set up/tear down ---*)

PROCEDURE Reset () =
  CONST Tag = ARRAY BOOLEAN OF CHAR { 'A', 'B' };
  VAR file: TEXT;  line: INTEGER;
  BEGIN
    IF (info = NIL) THEN info := M3Buf.New (); END;

    Scanner.LocalHere (file, line);
    OutC ('@');
    OutT (file);
    NL ();

    OutC (Tag [Module.IsInterface ()]);
    OutN (Module.Name (NIL));
    NL ();
  END Reset;

PROCEDURE Finish (): TEXT =
  BEGIN
    RETURN M3Buf.ToText (info);
  END Finish;

(*----------------------------------------------------- compilation units ---*)

PROCEDURE Import_unit (n: Name) =
  BEGIN
    OutC ('C');
    OutN (n);
    NL ();
  END Import_unit;

PROCEDURE Export_unit (n: Name) =
  BEGIN
    OutC ('D');
    OutN (n);
    NL ();
  END Export_unit;

(*------------------------------------------- debugging type declarations ---*)

PROCEDURE Declare_typename (t: TypeUID;  x: Value.T) =
  BEGIN
    OutX (t, 'E');
    OutV (x);
    NL ();
  END Declare_typename;

PROCEDURE Declare_array (t: TypeUID;  index, elt: TypeUID;  s: Size) =
  BEGIN
    OutX (t, 'F');
    OutU (index);
    OutU (elt);
    OutI (s);
    NL ();
  END Declare_array;

PROCEDURE Declare_open_array (t: TypeUID;  elt: TypeUID;  s: Size) =
  BEGIN
    OutX (t, 'G');
    OutU (elt);
    OutI (s);
    NL ();
  END Declare_open_array;

PROCEDURE Declare_enum (t: TypeUID;  n_elts: INTEGER;  s: Size) =
  BEGIN
    OutX (t, 'H');
    OutI (n_elts);
    OutI (s);
    NL ();
  END Declare_enum;

PROCEDURE Declare_enum_elt (n: Name) =
  BEGIN
    OutC ('I');
    OutN (n);
    NL ();
  END Declare_enum_elt;

PROCEDURE Declare_packed  (t: TypeUID;  s: Size;  base: TypeUID) =
  BEGIN
    OutX (t, 'J');
    OutI (s);
    OutU (base);
    NL ();
  END Declare_packed ;

PROCEDURE Declare_record (t: TypeUID;  s: Size;  n_fields: INTEGER) =
  BEGIN
    OutX (t, 'K');
    OutI (s);
    OutI (n_fields);
    NL ();
  END Declare_record;

PROCEDURE Declare_field (n: Name;  o: Offset;  s: Size;  t: TypeUID) =
  BEGIN
    OutC ('L');
    OutN (n);
    OutI (o);
    OutI (s);
    OutU (t);
    NL ();
  END Declare_field;

PROCEDURE Declare_set (t, domain: TypeUID;  s: Size) =
  BEGIN
    OutX (t, 'M');
    OutU (domain);
    OutI (s);
    NL ();
  END Declare_set;

PROCEDURE Declare_subrange (t, domain: TypeUID;  READONLY min, max: Target.Int;
                   s: Size) =
  BEGIN
    OutX (t, 'N');
    OutU (domain);
    OutZ (min);
    OutZ (max);
    OutI (s);
    NL ();
  END Declare_subrange;

PROCEDURE Declare_pointer (t, target: TypeUID;  brand: TEXT; traced: BOOLEAN) =
  CONST Tag = ARRAY BOOLEAN OF CHAR { 'O', 'P' };
  BEGIN
    OutX (t, Tag [traced]);
    OutU (target);
    IF (brand # NIL) THEN
      OutC (' ');
      OutT (brand);
    END;
    NL ();
  END Declare_pointer;

PROCEDURE Declare_indirect (t, target: TypeUID) =
  BEGIN
    OutX (t, 'Q');
    OutU (target);
    NL ();
  END Declare_indirect;

PROCEDURE Declare_proctype (t: TypeUID; n_formals: INTEGER;
                            result: TypeUID;  n_raises: INTEGER) =
  BEGIN
    OutX (t, 'R');
    OutI (n_formals);
    OutU (result);
    OutI (n_raises);
    NL ();
  END Declare_proctype;

PROCEDURE Declare_formal (n: Name;  t: TypeUID) =
  BEGIN
    OutC ('S');
    OutN (n);
    OutU (t);
    NL ();
  END Declare_formal;

PROCEDURE Declare_raises (n: Name) =
  BEGIN
    OutC ('T');
    OutN (n);
    NL ();
  END Declare_raises;

PROCEDURE Declare_object (t, super: TypeUID;  brand: TEXT;  traced: BOOLEAN;
                n_fields, n_methods, n_overrides: INTEGER;  field_size: Size) =
  CONST Tag = ARRAY BOOLEAN OF CHAR { 'U', 'V' };
  BEGIN
    OutX (t, Tag[traced]);
    OutU (super);
    OutI (n_fields);
    OutI (n_methods);
    OutI (n_overrides);
    OutI (field_size);
    IF (brand # NIL) THEN
      OutC (' ');
      OutT (brand);
    END;
    NL ();
  END Declare_object;

PROCEDURE Declare_method (n: Name;  signature: TypeUID;  dfault: Expr.T) =
  VAR proc: Value.T;
  BEGIN
    OutC ('W');
    OutN (n);
    OutU (signature);
    IF (dfault = NIL) THEN
      (* skip *)
    ELSIF UserProc.IsProcedureLiteral (dfault, proc) THEN
      OutV (proc);
    ELSE
      OutT (" NIL");
    END;
    NL ();
  END Declare_method;

PROCEDURE Declare_override (n: Name;  dfault: Expr.T) =
  VAR proc: Value.T;
  BEGIN
    OutC ('X');
    OutN (n);
    IF (dfault = NIL) THEN
      OutT (" (??)");
    ELSIF UserProc.IsProcedureLiteral (dfault, proc) THEN
      OutV (proc);
    ELSE
      OutT (" NIL");
    END;
    NL ();
  END Declare_override;

PROCEDURE Declare_opaque (t, super: TypeUID) =
  BEGIN
    OutX (t, 'Y');
    OutU (super);
    NL ();
  END Declare_opaque;

PROCEDURE Reveal_opaque (lhs, rhs: TypeUID) =
  BEGIN
    OutX (lhs, 'Z');
    OutU (rhs);
    NL ();
  END Reveal_opaque;

(*--------------------------------------------------------- low-level I/O ---*)

PROCEDURE OutC (c: CHAR) =
  BEGIN
    M3Buf.PutChar (info, c);
  END OutC;

PROCEDURE OutI (i: INTEGER) =
  BEGIN
    M3Buf.PutChar (info, ' ');
    M3Buf.PutInt (info, i);
  END OutI;

PROCEDURE OutU (t: TypeUID) =
  BEGIN
    M3Buf.PutChar (info, ' ');
    PutHex (t);
  END OutU;

PROCEDURE OutN (n: Name) =
  BEGIN
    M3ID.Put (info, n);
  END OutN;

PROCEDURE OutX (t: TypeUID;  c: CHAR) =
  BEGIN
    M3Buf.PutChar (info, c);
    PutHex (t);
  END OutX;

PROCEDURE OutV (v: Value.T) =
  VAR s: Scope.IDStack;
  BEGIN
    M3Buf.PutChar (info, ' ');
    s.top := 0;
    Scope.NameToPrefix (v, s, dots := TRUE, with_module := TRUE);
    Scope.PutStack (info, s);
  END OutV;

PROCEDURE OutZ (READONLY i: Target.Int) =
  BEGIN
    M3Buf.PutChar (info, ' ');
    M3Buf.PutIntt (info, i);
  END OutZ;

PROCEDURE OutT (t: TEXT) =
  BEGIN
    M3Buf.PutText (info, t);
  END OutT;

CONST
  HexDigit = ARRAY [0..15] OF CHAR {
                 '0','1','2','3','4','5','6','7',
                 '8','9','a','b','c','d','e','f' };

PROCEDURE PutHex (i: INTEGER) =
  VAR buf: ARRAY [0..7] OF CHAR;
  BEGIN
    FOR j := 7 TO 0 BY -1 DO
      buf[j] := HexDigit [Word.Mod (i, 16)];
      i := Word.Divide (i, 16);
    END;
    M3Buf.PutSub (info, buf);
  END PutHex;

PROCEDURE NL () =
  BEGIN
    M3Buf.PutText (info, Target.EOL);
  END NL;

BEGIN
END WebInfo.
