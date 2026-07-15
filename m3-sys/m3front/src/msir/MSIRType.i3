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

PROCEDURE ComputeType(t: Type.T): MSIR.T;
(* The ZType (computation width) of t: the width at which values of t live in
   registers, as opposed to Translate's MType (memory/storage width).  For any
   ordinal this is the machine INTEGER width (i64), matching the CG stack model
   where ordinals are computed at machine width and only trimmed in memory.
   For non-ordinals it equals Translate(t).  Used so that every ordinal VALUE
   produced by a memory read (deref/field/subscript/global) or literal is
   uniform machine width, eliminating per-operation operand-width coercions. *)

PROCEDURE Reset();
(* Clear the per-module type-translation cache.  Must be called at the
   start of each module so stale MSIR.T pointers from a prior module
   are not confused with new ones. *)

END MSIRType.
