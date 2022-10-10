(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Oct 12 16:12:57 PDT 1994 by kalsow     *)

MODULE M3Backend;

IMPORT Text, Thread, Wr;

IMPORT M3C;
IMPORT LLGen; (* Could be a dummy.  See the m3makefile. *) 
IMPORT M3CG, Msg, Utils, NTObjFile, M3x86, M3ObjFile;
IMPORT M3CG_BinWr, M3CG_Ops, M3CG_Tee, M3ID;
IMPORT Target; 

VAR
  obj_file : M3ObjFile.T := NIL;
  obj_wr   : Wr.T        := NIL;
  obj_name : TEXT        := NIL;
VAR
  log_wr   : Wr.T        := NIL;
  log_name : TEXT        := NIL;
VAR
  ir_wr    : Wr.T        := NIL;
  ir_name  : TEXT        := NIL;

PROCEDURE TeeBinWr (cg: M3CG_Ops.Public; f_ir_name: TEXT): M3CG_Ops.Public =
  VAR result: M3CG_Ops.Public;
  BEGIN
    ir_name := f_ir_name;
    IF ir_name = NIL THEN RETURN cg END;
    IF Text.Equal (ir_name, "") THEN ir_name := NIL; RETURN cg END;
    ir_wr := Utils.OpenWriter (ir_name, fatal := FALSE);
    IF ir_wr = NIL THEN ir_name := NIL; RETURN cg END;
    result := M3CG_Tee.New (cg, M3CG_BinWr.New (ir_wr));
    RETURN result;
  END TeeBinWr;

(* EXPORTED: *)
PROCEDURE Open (library (* or program *): TEXT;
                source_base_name (* lacks .m3 or .i3 *): M3ID.T;
                target_wr: Wr.T;
                target_name (* Has suffix. *): TEXT;
                f_ir_name (* Has suffix .ic or .mc *): TEXT;
                backend_mode: Target.M3BackendMode_t
               ): M3CG.T =
  VAR cg: M3CG_Ops.Public := NIL;
  BEGIN

    (* C backend: *)
    IF backend_mode = Target.M3BackendMode_t.C THEN
      cg := M3C.New (library, source_base_name, target_wr, target_name);
      (* cg.comment would not appear at the top because
       * earlier passes ignore it
       *)
      TRY
        Wr.PutText(target_wr, "// library:");
        Wr.PutText(target_wr, library);
        Wr.PutText(target_wr, "\n// source_base_name:");
        Wr.PutText(target_wr, M3ID.ToText(source_base_name));
        Wr.PutText(target_wr, "\n// target_name:");
        Wr.PutText(target_wr, target_name);
        Wr.PutText(target_wr, "\n");
        RETURN TeeBinWr (cg, f_ir_name);
      EXCEPT
      | Wr.Failure (args) =>
          Msg.FatalError (args, "unable to write IR file: ", f_ir_name);
      | Thread.Alerted =>
          Msg.FatalError (NIL, "unable to write IR file: ", f_ir_name);
      END;
    END;

    (* LLVM backend: *)
    IF backend_mode IN Target.BackendLlvmSet THEN
      IF (Msg.level >= Msg.Level.Verbose) THEN
        log_name := target_name & "log";
        log_wr := Utils.OpenWriter (log_name, fatal := TRUE);
      END;
      RETURN TeeBinWr (LLGen.New (log_wr,backend_mode), f_ir_name);
    END;

    (* Integrated backend: *)
    IF backend_mode IN Target.BackendIntegratedSet THEN 
      <*ASSERT obj_file = NIL *>
      obj_file := NTObjFile.New ();
      obj_wr   := target_wr;
      obj_name := target_name;
      IF (Msg.level >= Msg.Level.Verbose) THEN
        log_name := target_name & "log";
        log_wr := Utils.OpenWriter (log_name, fatal := TRUE);
      END;
      RETURN TeeBinWr (M3x86.New (log_wr, obj_file), f_ir_name);
    END;

    (* m3cc backend: *)
    RETURN M3CG_BinWr.New (target_wr);
  END Open;

(* EXPORTED: *)
PROCEDURE Close (<*UNUSED*> cg: M3CG.T) =
  BEGIN
    IF obj_file # NIL THEN
      TRY
        NTObjFile.Dump (obj_file, obj_wr);
      EXCEPT Wr.Failure, Thread.Alerted =>
        Msg.FatalError (NIL, "problem writing object file: ", obj_name);
      END;
      obj_file := NIL;
      obj_wr   := NIL;
      obj_name := NIL;
    END;
    Utils.CloseWriter (log_wr, log_name);
    log_wr := NIL;
    log_name := NIL;
    Utils.CloseWriter (ir_wr, ir_name);
    ir_wr := NIL;
    ir_name := NIL;
  END Close;

BEGIN
END M3Backend.
