(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan 27 16:55:04 PST 1997 by heydon                   *)
(*      modified on Sat Oct 17 18:03:10 PST 1992 by gnelson                  *)
(*      modified on Fri Aug  7 21:54:01 PDT 1992 by myers                    *)

INTERFACE JunoAssemble;

IMPORT JunoRT, JunoScope, JunoCompileRep;

TYPE CmdType = { Pred, Func, Proc };

PROCEDURE Cmd(
    cmd: JunoCompileRep.Result;
    scp: JunoScope.T;
    temp_cnt: CARDINAL;
    type: CmdType):
    JunoRT.ByteStream;
(* Returns the byte-code program produced by assembling the compilation result
   "cmd" under the scope "scp". The first instruction in the resulting stream
   increments the stack pointer by "temp_cnt" locations in order to make space
   for temporary (local) variables.

   The "type" reflects the kind of declaration in which this command is being
   compiled. Each type makes certain assumptions about the input command
   "cmd", and handles run-time errors and the state of the machine on return
   differently. In particular:

   "type = CmdType.Pred" OR "type = CmdType.Func" =>
     "Cmd" may be a partial command, but it is required not to abort.
     Execution of the resulting bytestream is guaranteed to terminate
     with the machine's condition bit set iff "Grd(cmd) # FALSE".

   "type = CmdType.Proc", =>
     Requires that "cmd" be a total command. If "cmd" aborts, the resulting
     bytestream will execute the run-time bytecode "JunoByteCode.ERROR" with
     the appropriate argument (encountered "ABORT", "IF..FI" failure, or
     undefined term).

   This procedure has the side-effect of setting the "start" and "end" fields
   of every AST node of the AST "cmd.cmd". Each node "n" is annotated such
   that the half-open interval "[n.start, n.end)" is the largest interval
   containing the PC locations corresponding the assembly instructions for
   "n". *)

END JunoAssemble.
