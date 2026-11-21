(* Textual printer for MSIR. The output matches the conventions used in
   the design walkthroughs (see repo-root MSIR-design.md). *)

INTERFACE MSIRPrinter;

IMPORT MSIR, Wr;

PROCEDURE Module  (wr: Wr.T;  m: MSIR.Module);
PROCEDURE Proc    (wr: Wr.T;  p: MSIR.Proc);
PROCEDURE Type    (wr: Wr.T;  t: MSIR.T);
PROCEDURE Value   (wr: Wr.T;  v: MSIR.Value);
PROCEDURE Insn    (wr: Wr.T;  i: MSIR.Insn);
PROCEDURE Scopes  (wr: Wr.T;  p: MSIR.Proc);  (* EH scope table, D26 *)
PROCEDURE OpText  (op: MSIR.Op): TEXT;    (* e.g. "iadd", "gc.load" *)

END MSIRPrinter.
