(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Apr 06 18:14:37 PDT 1992 by muller                   *)

UNSAFE MODULE Tcl;

IMPORT TclC, Text;

FROM Ctypes IMPORT int, char_star;
FROM M3toC IMPORT CopyTtoS, CopyStoT, TtoS, StoT;

REVEAL
  T = PublicT BRANDED OBJECT
        interp: TclC.Interp_star;
      OVERRIDES
        new                 := New;
        delete              := Delete;

        addErrorInfo        := AddErrorInfo;
        setErrorCode        := SetErrorCode;
        unixError           := UnixError;

        createCommand       := CreateCommand;
        deleteCommand       := DeleteCommand;

        createTrace         := CreateTrace;
        deleteTrace         := DeleteTrace;

        eval                := Eval;
        varEval             := VarEval;
        evalFile            := EvalFile;

        exprInt             := ExprInt;
        exprDouble          := ExprDouble;
        exprBoolean         := ExprBoolean;
        exprString          := ExprString;

        getInt              := GetInt;
        getDouble           := GetDouble;
        getBoolean          := GetBoolean;

        initHistory         := InitHistory;
        recordAndEval       := RecordAndEval;

        setResult           := SetResult;
        getResult           := GetResult;
        appendResult        := AppendResult;
        appendElement       := AppendElement;
        resetResult         := ResetResult;
        freeResult          := FreeResult;


        setVar              := SetVar;
        setVar2             := SetVar2;
        getVar              := GetVar;
        getVar2             := GetVar2;
        unsetVar            := UnsetVar;
        unsetVar2           := UnsetVar2;

        traceVar            := TraceVar;
        traceVar2           := TraceVar2;
        deleteTraceVar      := DeleteTraceVar; END;



(*---------------------------------------------------------------------------*)

PROCEDURE ConvertArgs (argc: int; argv: TclC.Argv): REF Args =
  VAR res: REF Args;
  BEGIN
    res := NEW (REF Args, argc);
    FOR i := 0 TO argc - 1 DO
      res [i] := CopyStoT (argv[i]); END;
    RETURN res;
  END ConvertArgs;

PROCEDURE RaiseError (res: int) RAISES {Error} =
  BEGIN
    CASE res OF
      | TclC.TCL_OK        => RETURN;
      | TclC.TCL_ERROR     => RAISE Error (ErrorCode.Error);
      | TclC.TCL_RETURN    => RAISE Error (ErrorCode.Return);
      | TclC.TCL_BREAK     => RAISE Error (ErrorCode.Break);
      | TclC.TCL_CONTINUE  => RAISE Error (ErrorCode.Continue);
      ELSE                    <*ASSERT FALSE*> END;
  END RaiseError;


(*---------------------------------------------------------------------------*)

PROCEDURE New (<*UNUSED*> self: T): T =
  BEGIN
    RETURN (NEW (T, interp := TclC.CreateInterp ()));
  END New;

PROCEDURE Delete (self: T) =
  BEGIN
    TclC.DeleteInterp (self.interp);
    self.interp := NIL;
  END Delete;

(*---------------------------------------------------------------------------*)

PROCEDURE AddErrorInfo (self: T; msg: Text.T) =
  BEGIN
    TclC.AddErrorInfo (self.interp, CopyTtoS (msg));
  END AddErrorInfo;

PROCEDURE SetErrorCode (self: T; s: ARRAY OF Text.T) =
  BEGIN
    CASE NUMBER (s) OF
      | 0 => TclC.SetErrorCode (self.interp, NIL);
      | 1 => TclC.SetErrorCode (self.interp, 
               CopyTtoS (s[0]), NIL);
      | 2 => TclC.SetErrorCode (self.interp,
               CopyTtoS (s[0]), CopyTtoS (s[1]), NIL);
      | 3 => TclC.SetErrorCode (self.interp,
               CopyTtoS (s[0]), CopyTtoS (s[1]), CopyTtoS (s[2]), NIL);
      | 4 => TclC.SetErrorCode (self.interp,
               CopyTtoS (s[0]), CopyTtoS (s[1]), CopyTtoS (s[2]),
               CopyTtoS (s[3]), NIL);
      ELSE   <* ASSERT FALSE *> END;
  END SetErrorCode;

PROCEDURE UnixError (self:T): Text.T =
  BEGIN
    RETURN (CopyStoT (TclC.UnixError (self.interp)));
  END UnixError;

(*---------------------------------------------------------------------------*)

TYPE
  CmdClientData = REF RECORD 
                    interp: T;
                    cl: CmdClosure; END;

PROCEDURE InvokeCmdClosure (clientData: TclC.ClientData; 
                            <*UNUSED*> interp: TclC.Interp_star;
                            argc: int; argv: TclC.Argv): int =
  VAR cd := LOOPHOLE (clientData, CmdClientData);
  BEGIN
    TRY
      cd.cl.apply (cd.interp, ConvertArgs (argc, argv)^);
      RETURN TclC.TCL_OK;
    EXCEPT
      | Error (code) =>
          CASE code OF 
            | ErrorCode.Error    => RETURN TclC.TCL_ERROR;
            | ErrorCode.Return   => RETURN TclC.TCL_RETURN;
            | ErrorCode.Break    => RETURN TclC.TCL_BREAK;
            | ErrorCode.Continue => RETURN TclC.TCL_CONTINUE; END; END;
  END InvokeCmdClosure;

PROCEDURE DeleteCmdClosure (clientData: TclC.ClientData) = 
  VAR cd := LOOPHOLE (clientData, CmdClientData);
  BEGIN
    cd.cl.delete ();
  END DeleteCmdClosure;

PROCEDURE CreateCommand (self: T; name: Text.T; cl: CmdClosure) =
  VAR clientData := NEW (CmdClientData, interp := self, cl := cl);
  BEGIN
    TclC.CreateCommand (self.interp, CopyTtoS (name),
                        InvokeCmdClosure, 
                        LOOPHOLE (clientData, TclC.ClientData), 
                        DeleteCmdClosure);
  END CreateCommand;


PROCEDURE DeleteCommand (self: T; name: Text.T): BOOLEAN =
  BEGIN
    RETURN (TclC.DeleteCommand (self.interp, CopyTtoS (name)) = 0);
  END DeleteCommand;

 
(*---------------------------------------------------------------------------*)

TYPE
  CmdTraceClientData = REF RECORD 
                         t: T;
                         cl: CmdTraceClosure; END;

REVEAL
  CmdTraceHandle = BRANDED OBJECT 
                     trace: TclC.Trace; END;

PROCEDURE InvokeCmdTraceClosure (clientData: TclC.ClientData; 
                                 <*UNUSED*> interp: TclC.Interp_star;
                                 level: int;
                                 command: char_star;
                                 <*UNUSED*> cmdProc: TclC.CmdProc;
                                 <*UNUSED*> cmdClientData: TclC.ClientData;
                                 argc: int; argv: TclC.Argv) =
  VAR cd := LOOPHOLE (clientData, CmdTraceClientData);
  BEGIN
    cd.cl.trace (cd.t, level, CopyStoT (command),
                 ConvertArgs (argc, argv)^);
  END InvokeCmdTraceClosure;

PROCEDURE CreateTrace (self: T; level: INTEGER; cl: CmdTraceClosure)
             : CmdTraceHandle =
  VAR clientData := NEW (CmdTraceClientData, t := self, cl := cl);
      trace: TclC.Trace;
  BEGIN
    trace := TclC.CreateTrace (self.interp, level, InvokeCmdTraceClosure,
                               LOOPHOLE (clientData, TclC.ClientData));
    RETURN (NEW (CmdTraceHandle, trace := trace));
  END CreateTrace;

PROCEDURE DeleteTrace (self: T; h: CmdTraceHandle) =
  BEGIN
    TclC.DeleteTrace (self.interp, h.trace);
  END DeleteTrace;



(*---------------------------------------------------------------------------*)

TYPE
  VarTraceClientData = REF RECORD 
                         t: T;
                         cl: VarTraceClosure; END;

REVEAL
  VarTraceHandle = REFANY;

PROCEDURE InvokeVarTraceClosure (clientData: TclC.ClientData;
                                 <*UNUSED*> interp: TclC.Interp_star;
                                 name1, name2: char_star;
                                 flags: int): char_star =
  VAR cd := LOOPHOLE (clientData, VarTraceClientData);
  BEGIN
    TRY
      RETURN CopyTtoS (cd.cl.trace (cd.t, CopyStoT (name1), 
                                    CopyStoT (name2),
                                    Int2VarTraceFlags (flags)));
    EXCEPT
      | Error (code) =>
          CASE code OF 
            | ErrorCode.Error    => RETURN CopyTtoS ("Error");
            | ErrorCode.Return   => RETURN CopyTtoS ("Return");
            | ErrorCode.Break    => RETURN CopyTtoS ("Break");
            | ErrorCode.Continue => RETURN CopyTtoS ("Continue"); END; END;
  END InvokeVarTraceClosure;

PROCEDURE TraceVar (self: T; name: Text.T; flags: VarTraceFlags;
                     cl: VarTraceClosure): VarTraceHandle RAISES {Error} =
  VAR clientData := NEW (VarTraceClientData, t := self, cl := cl);
  BEGIN
    RaiseError (TclC.TraceVar (self.interp, CopyTtoS (name),
                               VarTraceFlags2Int (flags), 
                               InvokeVarTraceClosure, 
                               LOOPHOLE (clientData, TclC.ClientData)));
  END TraceVar;

PROCEDURE TraceVar2 (self: T; name1, name2: Text.T; flags: VarTraceFlags;
                     cl: VarTraceClosure): VarTraceHandle RAISES {Error} =
  VAR clientData := NEW (VarTraceClientData, t := self, cl := cl);
  BEGIN
    RaiseError (TclC.TraceVar2(self.interp, CopyTtoS (name1), CopyTtoS (name2),
                               VarTraceFlags2Int (flags), 
                               InvokeVarTraceClosure,
                               LOOPHOLE (clientData, TclC.ClientData)));
  END TraceVar2;

PROCEDURE DeleteTraceVar (<*UNUSED*> self: T; <*UNUSED*> h: VarTraceHandle) =
  BEGIN
    <* ASSERT FALSE*>
  END DeleteTraceVar;

PROCEDURE VarTraceFlags2Int (flags: VarTraceFlags): int = 
  BEGIN
    RETURN LOOPHOLE (flags, int);
  END VarTraceFlags2Int;

PROCEDURE Int2VarTraceFlags (i: int): VarTraceFlags =
  BEGIN
    RETURN LOOPHOLE (i, VarTraceFlags);
  END Int2VarTraceFlags;

(*---------------------------------------------------------------------------*)

PROCEDURE Eval (self: T; cmd: Text.T; stopOnBracket: BOOLEAN) RAISES {Error} =
  VAR flag: int;
  BEGIN
    IF stopOnBracket THEN flag := TclC.BRACKET_TERM; ELSE flag := 0; END;
    RaiseError (TclC.Eval (self.interp, CopyTtoS (cmd), flag, NIL));
  END Eval;

PROCEDURE VarEval (self: T; s: ARRAY OF Text.T) RAISES {Error} =
  VAR cmd: Text.T;
  BEGIN
    FOR i := FIRST (s) TO LAST (s) DO
      cmd := cmd & s [i]; END;
    RaiseError (TclC.Eval (self.interp, CopyTtoS (cmd), 0, NIL));
  END VarEval;

PROCEDURE EvalFile (self: T; filename: Text.T) RAISES {Error} =
  BEGIN
    RaiseError (TclC.EvalFile (self.interp, CopyTtoS (filename)));
  END EvalFile;

(*---------------------------------------------------------------------------*)

PROCEDURE ExprInt (self: T; string: Text.T): INTEGER RAISES {Error} =
  VAR res: INTEGER;
  BEGIN
    RaiseError (TclC.ExprLong (self.interp, CopyTtoS (string), ADR (res)));
    RETURN (res);
  END ExprInt;

PROCEDURE ExprDouble (self: T; string: Text.T): LONGREAL RAISES {Error} =
  VAR res: LONGREAL;
  BEGIN
    RaiseError (TclC.ExprDouble (self.interp, CopyTtoS (string), ADR (res)));
    RETURN (res);
  END ExprDouble;

PROCEDURE ExprBoolean (self: T; string: Text.T): BOOLEAN RAISES {Error} =
  VAR res: INTEGER;
  BEGIN
    RaiseError (TclC.ExprLong (self.interp, CopyTtoS (string), ADR (res)));
    RETURN res = 1;
  END ExprBoolean;

PROCEDURE ExprString (self: T; string: Text.T) RAISES {Error} =
  BEGIN
    RaiseError (TclC.ExprString (self.interp, CopyTtoS (string)));
  END ExprString;

(*---------------------------------------------------------------------------*)

PROCEDURE GetInt (self: T; string: Text.T): INTEGER RAISES {Error} =
  VAR res: INTEGER;
  BEGIN
     RaiseError (TclC.GetInt (self.interp, CopyTtoS (string), ADR (res)));
     RETURN res;
  END GetInt;

PROCEDURE GetDouble (self: T; string: Text.T): LONGREAL RAISES {Error} =
  VAR res: LONGREAL;
  BEGIN
     RaiseError (TclC.GetDouble (self.interp, CopyTtoS (string), ADR (res)));
     RETURN res;
  END GetDouble;

PROCEDURE GetBoolean (self: T; string: Text.T): BOOLEAN RAISES {Error} =
  VAR res: INTEGER;
  BEGIN
     RaiseError (TclC.GetBoolean (self.interp, CopyTtoS (string), 
                 LOOPHOLE (ADR (res), UNTRACED REF INTEGER)));
     RETURN res = 1;
  END GetBoolean;


(*---------------------------------------------------------------------------*)

PROCEDURE InitHistory (self: T) =
  BEGIN
    TclC.InitHistory (self.interp);
  END InitHistory;

PROCEDURE RecordAndEval (self: T; cmd: Text.T;
                         stopOnBracket, recordOnly: BOOLEAN := FALSE) 
    RAISES {Error} =
  VAR flag: int;
  BEGIN
    IF recordOnly THEN
      flag := -1;
    ELSIF stopOnBracket THEN
      flag := TclC.BRACKET_TERM;
    ELSE
      flag := 0; END;
    RaiseError (TclC.RecordAndEval (self.interp, CopyTtoS (cmd), flag));
  END RecordAndEval;


(*---------------------------------------------------------------------------*)

PROCEDURE SetResult (self: T; string: Text.T) =
  BEGIN
    TclC.SetResult (self.interp, CopyTtoS (string), TclC.dynamic);
  END SetResult;

PROCEDURE GetResult (self: T): Text.T =
  BEGIN
    RETURN CopyStoT (self.interp.result);
  END GetResult;

PROCEDURE AppendResult (self: T; s: ARRAY OF Text.T) =
  BEGIN
    FOR i := FIRST (s) TO LAST (s) DO
      TclC.AppendResult (self.interp, CopyTtoS (s[i])); END;
  END AppendResult;

PROCEDURE AppendElement (self: T; string: Text.T; noSep: BOOLEAN) =
  VAR tclNoSep: INTEGER;
  BEGIN
    IF noSep THEN tclNoSep := 1 ELSE tclNoSep := 0; END;
    TclC.AppendElement (self.interp, CopyTtoS (string), tclNoSep);
  END AppendElement;

PROCEDURE ResetResult (self: T; ) =
  BEGIN
    TclC.ResetResult (self.interp);
  END ResetResult;

PROCEDURE FreeResult (self: T; ) =
  BEGIN
    TclC.FreeResult (self.interp);
  END FreeResult;


(*---------------------------------------------------------------------------*)

PROCEDURE SetVar (self: T; varName, newValue: Text.T; 
                  flags: VarSetFlags) RAISES {Error} =
  BEGIN
    EVAL TclC.SetVar (self.interp, CopyTtoS (varName), CopyTtoS (newValue),
                      VarSetFlagsToInt (flags));
  END SetVar;

PROCEDURE SetVar2 (self: T; name1, name2, newValue: Text.T;
                   flags: VarSetFlags) RAISES {Error} =
  BEGIN
    EVAL TclC.SetVar2 (self.interp, CopyTtoS (name1), CopyTtoS (name2),
                       CopyTtoS (newValue), VarSetFlagsToInt (flags));
  END SetVar2;

PROCEDURE GetVar (self: T; varName: Text.T;
                  flags: VarSetFlags): Text.T RAISES {Error} =
  BEGIN
    RETURN CopyStoT (TclC.GetVar (self.interp, CopyTtoS (varName),
                                  VarSetFlagsToInt (flags)))
  END GetVar;

PROCEDURE GetVar2 (self: T; name1, name2: Text.T;
                   flags: VarSetFlags): Text.T RAISES {Error} =
  BEGIN
    RETURN CopyStoT (TclC.GetVar2 (self.interp, CopyTtoS (name1), 
                                  CopyTtoS (name2), VarSetFlagsToInt (flags)));
  END GetVar2;

PROCEDURE UnsetVar (self: T; varName: Text.T;
                    flags: VarSetFlags) RAISES {Error} =
  BEGIN
    RaiseError (TclC.UnsetVar (self.interp, CopyTtoS (varName), 
                               VarSetFlagsToInt (flags)));
  END UnsetVar;

PROCEDURE UnsetVar2 (self: T; name1, name2: Text.T;
                     flags: VarSetFlags) RAISES {Error} =
  BEGIN
    RaiseError (TclC.UnsetVar2 (self.interp, CopyTtoS (name1), 
                                CopyTtoS (name2), VarSetFlagsToInt (flags)));
  END UnsetVar2;


PROCEDURE VarSetFlagsToInt (flags: VarSetFlags): int = 
  BEGIN
    RETURN LOOPHOLE (flags, int);
  END VarSetFlagsToInt;

(*---------------------------------------------------------------------------*)

REVEAL 
   CmdBuf = OBJECT METHODS
              new (): CmdBuf;
              delete ();
              assemble (s: Text.T): Text.T; END
            BRANDED OBJECT
              cmdBuf: TclC.CmdBuf
            OVERRIDES
              new := NewCmdBuf;
              delete := DeleteCmdBuf;
              assemble := Assemble; END;

PROCEDURE NewCmdBuf (self: CmdBuf): CmdBuf =
  BEGIN
    self := NEW (CmdBuf, cmdBuf := TclC.CreateCmdBuf ());
    RETURN self;
  END NewCmdBuf;

PROCEDURE DeleteCmdBuf (self: CmdBuf) =
  BEGIN
    TclC.DeleteCmdBuf (self.cmdBuf);
  END DeleteCmdBuf;

PROCEDURE Assemble (self: CmdBuf; s: Text.T): Text.T =
  VAR t: char_star;

  (* ASSERT:  s in the argument list is enough to prevent it from moving
              as long as AssembleCmd needs it;
              when non-NIL is returned, we need to copy it to make available
              forever to our caller *)
  BEGIN
    t := TclC.AssembleCmd (self.cmdBuf, TtoS (s));
    IF t = NIL THEN
      RETURN NIL;
    ELSE
      RETURN CopyStoT (t); END;
  END Assemble;

(*---------------------------------------------------------------------------*)

PROCEDURE SplitList (self: T; list: Text.T): REF Args RAISES {Error} =
  VAR argc: int; argv: TclC.Argv; res: REF Args;
  BEGIN
    RaiseError (TclC.SplitList (self.interp, CopyTtoS (list), 
                                ADR (argc), ADR (argv)));
    res := NEW (REF Args, argc);
    FOR i := 0 TO argc - 1 DO
      res [i] := CopyStoT (argv [i]); END;
    RETURN res;
  END SplitList;

PROCEDURE Merge (args: Args): Text.T =
  BEGIN
    RETURN CopyStoT (TclC.Merge (NUMBER (args), ADR (args[0])));
  END Merge;

(*---------------------------------------------------------------------------*)

PROCEDURE DoNothing (<*UNUSED*> self: CmdClosure) =
  BEGIN
  END DoNothing;

BEGIN
END Tcl.

