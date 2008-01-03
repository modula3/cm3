(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Feb 21 15:40:41 PST 1995 by kalsow     *)
(*      modified on Fri Apr  1 13:49:15 PST 1994 by harrison   *)

INTERFACE QCode;

IMPORT Quake, Thread;

TYPE (* the Quake virtual machine opcodes *)
  Op = {
    (*---------------------------------------------------- literal values ---*)
    Integer,
      (* push A *)
    String,
      (* push string "A" *)
    BuildArray,
      (* push an array containing the top A elements *)
    BuildTable,
      (* push a table with the top A/2 key-value pairs *)
    GetEnv,
      (* push value of environment variable named "A"*)
    PushProc,
      (* push proc[A] from the current code segment's procedure table *)

    (*--------------------------------------------- expression evaluation ---*)
    IsMember,
      (* elt := pop(); tbl := pop(); push(elt IN tbl) *)
    Concat,
      (* a := pop();  b := pop();  push (a & b); *)
    And,
      (* push (pop () AND pop ()) *)
    Or,
      (* push (pop () OR pop ()) *)
    Not,
      (* push (NOT pop ()) *)
    IndexTable,
      (* key := pop();  tbl := pop;  push (tbl{key}) *)
    SubscriptArray,
      (* key := pop();  arr := pop;  push (arr[key]) *)

    (*------------------------------------------------------ control flow ---*)
    InitForeach,
      (* elts := pop (); PushScope("id", UNDEFINED); PushForLoop (id, elts) *)
    NextForeach,
      (* z := TopForLoop;
         IF z.cnt >= NUM(z.elts)
           THEN PopForLoop;  PC := PC + A;
           ELSE Define (z.id, z.elts[z.cnt]);  INC (z.cnt);
         END; *)
    Goto,
      (* PC := PC + A *)
    IfFalse,
      (* x := pop();  IF NOT x, PC := PC + A *)
    Halt,
      (* stop execution *)

    (*--------------------------------------------------------- variables ---*)
    PushScope,
      (* enter a new, empty scope *)
    PopScope,
      (* pop the current scope *)
    DefineG,
      (* define a new global variable "A" := pop() *)
    DefineGR,
      (* define a new global, readonly variable "A" := pop() *)
    DefineL,
      (* define a new local variable "A" := pop() *)
    DefineLR,
      (* define a new local, readonly variable "A" := pop() *)
    LoadVar,
      (* push value of variable named "A" *)
    Assign,
      (* val := pop();  var(A) := val *)
    AssignTable,
      (* val := pop();  index := pop();  tbl := pop();  tbl{index} := val *)
    AssignArray,
      (* val := pop();  index := pop();  arr := pop();  arr[index] := val *)
    Append,
      (* val := pop();  arr := pop();  arr += val *)

    (*------------------------------------------------------ output files ---*)
    StartRedirect,
      (* fn := pop();  push (output);  output := Open (fn); *)
    StartAppendRedirect,
      (* fn := pop();  push (output);  output := OpenAppend (fn); *)
    EndRedirect,
      (* Close(output);  output := pop (); *)

    (*-------------------------------------------------------- procedures ---*)
    StartCall,
      (* push a marker on the stack to delimit the actual parameters *)
    CallProc,
      (* call procedure that's below the A arguments, expect no return value *)
    CallFunc,
      (* call function that's below the A arguments, expect a return value *)
    ReturnValue,
      (* pop the current frame, return TOS-0. *)
    Return,
      (* pop the current frame, return nothing *)
    SetLine
      (* set current_line := A *)
  };

CONST
  OpName = ARRAY Op OF TEXT {
    (*---------------------------------------------------- literal values ---*)
    "Integer", "String", "BuildArray", "BuildTable", "GetEnv", "PushProc",

    (*--------------------------------------------- expression evaluation ---*)
    "IsMember", "Concat", "And", "Or", "Not", "IndexTable", "SubscriptArray",

    (*------------------------------------------------------ control flow ---*)
    "InitForeach", "NextForeach", "Goto", "IfFalse", "Halt",

    (*--------------------------------------------------------- variables ---*)
    "PushScope", "PopScope", "DefineG", "DefineGR", "DefineL", "DefineLR",
    "LoadVar", "Assign", "AssignTable", "AssignArray", "Append",

    (*------------------------------------------------------ output files ---*)
    "StartRedirect", "StartAppendRedirect", "EndRedirect",

    (*-------------------------------------------------------- procedures ---*)
    "StartCall", "CallProc", "CallFunc", "ReturnValue", "Return", "SetLine"
  };

CONST (* 0=no arg, 1=integer, 2=ID, 3=pc-rel label *)
  OpFormat = ARRAY Op OF [0..3] {
    (*---------------------------------------------------- literal values ---*)
    1, 2, 1, 1, 2, 1,

    (*--------------------------------------------- expression evaluation ---*)
    0, 0, 0, 0, 0, 0, 0,

    (*------------------------------------------------------ control flow ---*)
    2, 3, 3, 3, 0,

    (*--------------------------------------------------------- variables ---*)
    0, 0, 2, 2, 2, 2,
    2, 2, 0, 0, 0,

    (*------------------------------------------------------ output files ---*)
    0, 0, 0,

    (*-------------------------------------------------------- procedures ---*)
    0, 1, 1, 0, 0, 1
  };

TYPE (* a machine instruction *)
  Instr = RECORD  op: Op;  a: INTEGER;  END;

REVEAL
  Stream <: Stream_;
TYPE
  Stream = Quake.CodeStream;
  Stream_ = OBJECT
    cursor      : INTEGER               := 0;
    instrs      : REF ARRAY OF Instr    := NIL;
    source_file : Quake.ID              := Quake.NoID;
    n_procs     : INTEGER               := 0;
    procs       : REF ARRAY OF ProcInfo := NIL;
  METHODS
    emit  (op: Op;  a: INTEGER);
    patch (pc: INTEGER;  op: Op;  a: INTEGER);
    add_proc (nm: Quake.ID): INTEGER;
  END;

TYPE
  ProcInfo = REF RECORD
    code    : Stream      := NIL;
    entry   : INTEGER     := 0;
    name    : Quake.ID    := Quake.NoID;
    n_args  : INTEGER     := 0;
    builtin : BOOLEAN     := FALSE;
    isFunc  : BOOLEAN     := FALSE; (* only valid if builtin *)
    handler : BuiltinProc := NIL;
  END;

TYPE
  BuiltinProc = PROCEDURE (m: Quake.Machine;  n_args: INTEGER)
                  RAISES {Quake.Error, Thread.Alerted};

END QCode.
