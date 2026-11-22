(* MSIRBitLoc — the location of a designator that has no byte address.

   A sub-byte record or object field, or an element of a bit-packed array,
   cannot be named by a pointer.  Its location is the address of the storage
   that contains it plus a bit position.  Expr.BitLocMSIR computes that
   location, evaluating the designator's base exactly once; reads and writes
   then go through MSIRBuilder.ExtractBitLoc and MSIRBuilder.InsertBitLoc, so
   an update such as INC (a[f()].bits) calls f once, as the C backend does.

   This is a separate interface (not part of MSIRBuilder) because Expr's
   interface needs the type, and MSIRBuilder's interface imports interfaces
   that import Expr. *)

INTERFACE MSIRBitLoc;

IMPORT MSIR, Type;

TYPE
  T = RECORD
    base    : MSIR.Value := NIL;
      (* the containing storage, evaluated once *)
    bitOff  : INTEGER    := 0;
      (* static bit offset from base (idx = NIL) *)
    width   : INTEGER    := 0;
      (* the field's bits, or the packed array's element pack (idx # NIL) *)
    type    : Type.T     := NIL;
      (* the field or element type: decides sign- or zero-extension on reads *)
    idx     : MSIR.Value := NIL;
      (* non-NIL: element index into a bit-packed array at base *)
    containerBits : INTEGER := 0;
      (* idx # NIL: the packed array's total bits (0 = power-of-two pack) *)
    readOnly : BOOLEAN   := FALSE;
      (* base is a copy of a value with no address (e.g. a function result),
         so writes would be lost *)
  END;

END MSIRBitLoc.
