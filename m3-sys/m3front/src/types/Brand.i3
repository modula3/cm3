(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE Brand;

IMPORT M3, Expr, Type;

TYPE
  T <: REFANY;

PROCEDURE Parse (): T;

PROCEDURE New (txt: TEXT): T;

PROCEDURE Check (t: T;  holder: Type.T;
                 VAR hash: INTEGER;  VAR cs: Expr.CheckState);  

PROCEDURE Compile (t: T): INTEGER;
(* Return the offset in the global constant pool of the
   initialized structure containing the brand. *)

PROCEDURE GenFPrint (t: T;  VAR x: M3.FPInfo);

PROCEDURE Equal (a, b: T): BOOLEAN;

PROCEDURE ToText (t: T): TEXT;

PROCEDURE Image (t: T;  VAR charBytes: INTEGER): REF ARRAY OF [0..255];
(* The brand's characters as Compile lays them out after the length word:
   target byte order, no terminating NUL.  charBytes is the size of one
   character.  NIL if t is NIL. *)

PROCEDURE Reset ();

END Brand.


