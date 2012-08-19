UNSAFE INTERFACE M3CG_MultiPass;

(* This persists a run of M3CG calls to memory, so that multiple
 * passes can be run over it.
 *)

IMPORT M3CG, M3CG_Ops, M3CG_Binary;
FROM M3CG_Binary IMPORT Op;

TYPE T <: Public;

TYPE Public = M3CG_Ops.Public OBJECT
 METHODS
  get_data(): REF ARRAY OF REFANY;
END;

PROCEDURE New (): T;

(*----------------------------------------------------------- ID counters ---*)

TYPE next_label_t = REF RECORD
  n: INTEGER;
END;

(*------------------------------------------------ READONLY configuration ---*)

TYPE set_error_handler_t = REF RECORD
  p: PROCEDURE (msg: TEXT);
END;

END M3CG_MultiPass.
