UNSAFE MODULE M3CG_MultiPass;

IMPORT M3CG, M3CG_Binary, RefSeq;
FROM M3CG IMPORT Type, MType, IType, RType, AType, ZType, Sign;
FROM M3CG IMPORT Name, Var, Proc, Alignment, TypeUID, Label;
FROM M3CG IMPORT Frequency, CallingConvention, CompareOp, ConvertOp, AtomicOp;
FROM M3CG IMPORT BitSize, ByteSize, BitOffset, ByteOffset, RuntimeError;
FROM M3CG IMPORT MemoryOrder;
IMPORT IntRefTbl;

REVEAL T = Public BRANDED "M3CG_MultiPass.T" OBJECT
  data: RefSeq.T := NIL;
  runtime       : IntRefTbl.T := NIL;  (* Name -> RuntimeHook *)
  next_label_id := 1;
  next_var      := 1;
  next_proc     := 1;
  next_scope    := 1;
 METHODS
  Add (a: REFANY) := Add;
OVERRIDES
  get_data := get_data;
  next_label := next_label;
  set_error_handler := set_error_handler;
END;

PROCEDURE New (): T =
BEGIN
  RETURN NIL;
END New;

PROCEDURE get_data(self: T): REF ARRAY OF REFANY =
VAR data := self.data;
    a := NEW(REF ARRAY OF REFANY, data.size());
BEGIN
  FOR b := FIRST(a^) TO LAST(a^) DO
    a[b] := data.get(b);
  END;
  RETURN a;
END get_data;

PROCEDURE Add (self: T; a: REFANY) =
BEGIN
  self.data.addhi(a);
END Add;

(*----------------------------------------------------------- ID counters ---*)

PROCEDURE next_label (self: T;  n: INTEGER := 1): Label =
  VAR label := self.next_label_id;
  BEGIN
    self.Add(NEW(next_label_t, n := n));
    RETURN label;
  END next_label;

(*------------------------------------------------ READONLY configuration ---*)

PROCEDURE set_error_handler (self: T;  p: PROCEDURE (msg: TEXT)) =
  BEGIN
    self.Add(NEW(set_error_handler_t, p := p));
  END set_error_handler;

BEGIN
END M3CG_MultiPass.

