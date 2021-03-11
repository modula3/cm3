(* $Id$ *)

INTERFACE SlowTextCompress;
IMPORT Rd, Wr, ProcUtils;

TYPE Mode = { Compress, Decompress };
CONST ModeNames = ARRAY Mode OF TEXT { "Compress", "Decompress" };

PROCEDURE Text(mode : Mode; in : TEXT) : TEXT RAISES { ProcUtils.ErrorExit };

PROCEDURE RdWr(mode : Mode; in : Rd.T; out : Wr.T) RAISES { ProcUtils.ErrorExit };
  (* read from reader, write to write, and close both in and out *)

END SlowTextCompress.

