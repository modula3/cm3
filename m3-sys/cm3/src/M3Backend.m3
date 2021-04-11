(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Oct 12 16:12:57 PDT 1994 by kalsow     *)

MODULE M3Backend;

IMPORT Wr, Thread, M3C;
IMPORT LLGen; (* Could be a dummy.  See the m3makefile. *) 
IMPORT M3CG, Msg, Utils, NTObjFile, M3x86, M3ObjFile;
IMPORT M3CG_BinWr, M3CG_Ops, M3ID;
IMPORT Target; 

VAR
  obj_file : M3ObjFile.T := NIL;
  obj_wr   : Wr.T        := NIL;
  obj_name : TEXT        := NIL;
  log      : Wr.T        := NIL;
  log_name : TEXT        := NIL;

PROCEDURE Open (library (* or program *): TEXT; source: M3ID.T; target: Wr.T; target_name: TEXT; backend_mode: Target.M3BackendMode_t): M3CG.T =
  VAR cg: M3CG_Ops.Public := NIL;
  BEGIN
    IF backend_mode = Target.M3BackendMode_t.C THEN
      cg := M3C.New (library, source, target, target_name);
      (* cg.comment would not appear at the top because
       * earlier passes ignore it
       *)
      Wr.PutText(target, "// library:");
      Wr.PutText(target, library);
      Wr.PutText(target, "\n// source:");
      Wr.PutText(target, M3ID.ToText(source));
      Wr.PutText(target, "\n// target_name:");
      Wr.PutText(target, target_name);
      Wr.PutText(target, "\n");
      RETURN cg;
    END;
    IF backend_mode IN Target.BackendLlvmSet THEN
      IF (Msg.level >= Msg.Level.Verbose) THEN
        log_name := target_name & "log";
        log := Utils.OpenWriter (log_name, fatal := TRUE);
      END;
      RETURN LLGen.New (log,backend_mode);
    END;
    IF NOT backend_mode IN Target.BackendIntegratedSet THEN 
      RETURN M3CG_BinWr.New (target);
    END;
    <*ASSERT obj_file = NIL *>
    obj_file := NTObjFile.New ();
    obj_wr   := target;
    obj_name := target_name;
    IF (Msg.level >= Msg.Level.Verbose) THEN
      log_name := target_name & "log";
      log := Utils.OpenWriter (log_name, fatal := TRUE);
    END;
    RETURN M3x86.New (log, obj_file);
  END Open;

PROCEDURE Close (<*UNUSED*> cg: M3CG.T) =
  BEGIN
    IF obj_file # NIL THEN
      TRY
        NTObjFile.Dump (obj_file, obj_wr);
      EXCEPT Wr.Failure, Thread.Alerted =>
        Msg.FatalError (NIL, "problem writing object file: ", obj_name);
      END;
      Utils.CloseWriter (log, log_name);
      obj_file := NIL;
      obj_wr   := NIL;
      obj_name := NIL;
      log      := NIL;
      log_name := NIL;
    END;
  END Close;

BEGIN
END M3Backend.
