(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri Feb 17 10:54:10 PST 1995 by kalsow     *)
(*      modified on Fri Apr  1 13:49:15 PST 1994 by harrison   *)

INTERFACE QValue;

IMPORT Quake, QCode;

CONST
  Brand = "QValue.T";

TYPE
  T = RECORD
    kind : Kind;
    int  : INTEGER;
    ref  : REFANY;
  END;

TYPE
  Kind = {    (*    int         ref     *)
    Var,      (*    Quake.ID    Binding *)
    Integer,  (*    value       ----    *)
    String,   (*    Quake.ID    ----    *)
    Table,    (*    ------      QVTbl.T *)
    Array,    (*    ------      QVSeq.T *)
    Proc      (*    ------      Proc    *)
  };


TYPE
  Proc = REF RECORD
    info   : QCode.ProcInfo := NIL;
    env    : Scope          := NIL;
  END;

TYPE
  Scope = REF RECORD
    id         : INTEGER;
    bindings   : Binding := NIL;
    parent     : Scope   := NIL;
  END;

TYPE
  Binding = REF RECORD
    next     : Binding  := NIL;
    readonly : BOOLEAN  := FALSE;
    name     : Quake.ID := Quake.NoID;
    value    : T;
  END;

END QValue.
