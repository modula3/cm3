(* MSIRType — translation of m3front Type.T into MSIR.T.

   Covers scalar types (INTEGER, LONGINT, BOOLEAN, CHAR, WIDECHAR,
   REAL, LONGREAL, EXTENDED, ADDRESS, REFANY), enumerations, record
   types (recursively), and opaque REF/traced-ref types.
   Returns NIL for any type not yet supported — callers treat that as
   "this proc is outside the MSIR-supported subset" and bail. *)

INTERFACE MSIRType;

IMPORT MSIR, Type;

PROCEDURE Translate(t: Type.T): MSIR.T;
(* Returns NIL if t is not yet supported. *)

PROCEDURE TranslateResult(t: Type.T): MSIR.T;
(* Like Translate, but maps NIL (no result) to MSIR.TVoid(). *)

END MSIRType.
