(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

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

PROCEDURE Reset ();

END Brand.


