(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Last modified on Fri Jan 28 10:55:23 PST 1994 by detlefs   *)

GENERIC MODULE Sort(Elem);

IMPORT Csort;

VAR
  cp_g: CompareProc;

PROCEDURE Array(
    VAR (*inout*) a: ARRAY OF Elem.T;
    p: CompareProc) RAISES {}=
  BEGIN
   WITH n = NUMBER(a) DO
     IF n # 0 THEN
       cp_g := p;
       Csort.qsort(ADR(a[0]), n, BYTESIZE(Elem.T),
         LOOPHOLE(RefCompare, Csort.CompareProc))
     END;
   END;
  END Array;

PROCEDURE RefCompare(rx, ry: UNTRACED REF Elem.T): INTEGER RAISES {}=
  BEGIN
    RETURN cp_g(rx^, ry^);
  END RefCompare;


BEGIN

END Sort.
