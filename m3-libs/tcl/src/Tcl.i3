(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Apr 07 17:31:12 PDT 1992 by muller                   *)

INTERFACE Tcl;

IMPORT Text;

TYPE
  ErrorCode = {Error, Return, Break, Continue};

EXCEPTION
  Error (ErrorCode);
  
TYPE
  Args = ARRAY OF Text.T;

  CmdClosure = OBJECT METHODS
                 apply (interp: T; args: Args) RAISES {Error};
                 delete () := DoNothing; END;


  CmdTraceClosure = OBJECT METHODS
                     trace (interp: T;
                            level: INTEGER;
                            command: Text.T;
                            args: Args); END;
  CmdTraceHandle <: REFANY;



  VarTraceClosure = OBJECT METHODS
                      trace (interp: T;
                             name1, name2: Text.T;
                             flags: VarTraceFlags): Text.T; END;
  VarTraceHandle <: REFANY;
  VarTraceFlag  = {GlobalOnly, Reads, Writes, Unsets};
  VarTraceFlags = SET OF VarTraceFlag;


  VarSetFlag  = {GlobalOnly, LeaveErrMsg, AppendValue, ListElement, NoSpace};
  VarSetFlags = SET OF VarSetFlag;



  T <: PublicT;

  PublicT = OBJECT METHODS
    new (): T;
    delete (); 

    addErrorInfo (msg: Text.T);
    setErrorCode (s: ARRAY OF Text.T);
    unixError (): Text.T;

    createCommand (name: Text.T; closure: CmdClosure);
    deleteCommand (name: Text.T): BOOLEAN;

    createTrace (level: INTEGER; closure: CmdTraceClosure): CmdTraceHandle;
    deleteTrace (h: CmdTraceHandle);

    traceVar  (name: Text.T; flags: VarTraceFlags;
               cl: VarTraceClosure): VarTraceHandle RAISES {Error};
    traceVar2 (name1, name2: Text.T; flags: VarTraceFlags;
               cl: VarTraceClosure): VarTraceHandle RAISES {Error};
    deleteTraceVar (h: VarTraceHandle);

    eval (cmd: Text.T; stopOnBracket: BOOLEAN) RAISES {Error};
    varEval (s: ARRAY OF Text.T) RAISES {Error};
    evalFile (fileName: Text.T) RAISES {Error};

    exprInt (string: Text.T): INTEGER RAISES {Error};
    exprDouble (string: Text.T): LONGREAL RAISES {Error};
    exprBoolean (string: Text.T): BOOLEAN RAISES {Error};
    exprString (string: Text.T) RAISES {Error};

    getInt (string: Text.T): INTEGER RAISES {Error};
    getDouble (string: Text.T): LONGREAL RAISES {Error};
    getBoolean (string: Text.T): BOOLEAN RAISES {Error};

    initHistory ();
    recordAndEval (cmd: Text.T;
                   stopOnBracket, recordOnly: BOOLEAN := FALSE) RAISES {Error};

    setResult (string: Text.T);
    getResult (): Text.T;
    appendResult (s: ARRAY OF Text.T);
    appendElement (string: Text.T; noSep: BOOLEAN);
    resetResult ();
    freeResult ();

    setVar  (varName, newValue: Text.T;
             flags: VarSetFlags) RAISES {Error};
    setVar2 (name1, name2, newValue: Text.T;
             flags: VarSetFlags) RAISES {Error};
    getVar  (varName: Text.T; 
             flags: VarSetFlags): Text.T RAISES {Error};
    getVar2 (name1, name2: Text.T;
             flags: VarSetFlags): Text.T RAISES {Error};
    unsetVar  (varName: Text.T; 
               flags: VarSetFlags) RAISES {Error};
    unsetVar2 (name1, name2: Text.T; 
               flags: VarSetFlags) RAISES {Error}; END;

PROCEDURE DoNothing (self: CmdClosure);

(*---------------------------------------------------------------------------*)

TYPE
  CmdBuf <: OBJECT METHODS
              new (): CmdBuf;
              delete ();
              assemble (s: Text.T): Text.T; END;

(*---------------------------------------------------------------------------*)

PROCEDURE SplitList (interp: T; list: Text.T): REF Args RAISES {Error};
PROCEDURE Merge (args: Args): Text.T;
             
END Tcl.
