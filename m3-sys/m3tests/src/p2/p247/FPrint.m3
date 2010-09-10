(* reduced from m3-libs/m3core/Fingerprint.m3
 * This fails an assertion in some backend variants.
FPrint.m3: In function 'FPrint__xCombine':
FPrint.m3:22:0: internal compiler error: in referenced_var_lookup, at tree-dfa.c:519
 *)

MODULE FPrint;

IMPORT Word;

PROCEDURE xCombine () =
  BEGIN
    EVAL xFix32 (0);
  END xCombine;

PROCEDURE xFix32 (x: INTEGER): INTEGER =
  CONST a = 1;
  BEGIN
   IF Word.And (x, a) = 0 THEN
     RETURN 0;
   END;
   RETURN 0;
  END xFix32;

BEGIN
END FPrint.
