(* MSIRType — translation of m3front Type.T into MSIR.T.

   v0 covers only the scalars needed for the first vertical slice:
   INTEGER / CARDINAL / LONGINT (sized via Target), and BOOLEAN.
   Returns NIL for any type we cannot yet represent — callers treat
   that as "this proc is outside the MSIR-supported subset" and bail. *)

INTERFACE MSIRType;

IMPORT MSIR, Type;

PROCEDURE Translate(t: Type.T): MSIR.T;
(* Returns NIL if t is not yet supported. *)

PROCEDURE TranslateResult(t: Type.T): MSIR.T;
(* Like Translate, but maps NIL (no result) to MSIR.TVoid(). *)

END MSIRType.
