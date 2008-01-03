(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Dec 20 16:26:29 PST 1994 by kalsow     *)
(*      modified on Mon Sep 26 14:27:12 PDT 1994 by isard      *)
(*      modified on Mon Apr 13 09:55:12 PDT 1992 by muller     *)

INTERFACE M3x86Rep;

IMPORT M3CG, M3ID;

FROM M3CG IMPORT ByteOffset, ByteSize, Alignment;
FROM M3CG IMPORT Var, Proc, Name;
FROM M3CG IMPORT Type, MType, TypeUID;
FROM M3ObjFile IMPORT Seg;

TYPE U <: Public;
TYPE Public = M3CG.T OBJECT
      METHODS
        NewVar (t: Type; m3t: TypeUID; s: ByteSize; a: Alignment;
                name: Name := M3ID.NoID): x86Var;
      END;

TYPE VLoc = {global, temp};

TYPE OLoc = {mem, register, fstack, imm};

TYPE
  x86Var = Var OBJECT
    tag: INTEGER;
    type: Type;
    name: Name;
    s: ByteSize;
    a: Alignment;
    exported := FALSE;
    seg: Seg;
    symbol: INTEGER;
    offset: ByteOffset;
    loc: VLoc;
    stack_temp := FALSE;
    scope := 0;
    parent: x86Proc := NIL;
  END;

TYPE ProcList = REF RECORD
  loc: ByteOffset;
  link: ProcList;
END;

TYPE Temp = RECORD
  var: x86Var;
  free: BOOLEAN;
END;

TYPE
  x86Proc = Proc OBJECT
    tag: INTEGER;
    type: Type;
    name: Name;
    lev := 0;
    parent: x86Proc := NIL;
    n_params: INTEGER;
    framesize := 0;
    paramsize := 8;
    tempsize := 0;
    temparr: REF ARRAY OF Temp;
    templimit := 16;
    stdcall: BOOLEAN; (* => callee cleans *)
    symbol: INTEGER;
    exported := FALSE;
    import := FALSE;
    offset := 0;
    bound := FALSE;
    usage: ProcList := NIL;
  END;

TYPE
  MVar = RECORD
    var: x86Var;
    o: ByteOffset := 0;
    t: MType;
  END;

CONST NoStore = MVar {var := NIL, t := FIRST(MType)};

TYPE
  Operand = RECORD
    loc: OLoc;
    mvar: MVar := NoStore;
    reg: Regno := 0;
    imm: INTEGER := 0;
    stackp: INTEGER := 0;
    opcode := FALSE;
  END;

TYPE FlToInt = { Round, Floor, Ceiling, Truncate };

TYPE Force = {any, mem, anydword, anytemp, anyregimm, anyreg, regset};

CONST NRegs: INTEGER = 7;

TYPE Regno = [-1 .. NRegs];

CONST RegName = ARRAY Regno OF TEXT { "*NOREG*", "EAX", "ECX", "EDX",
                                      "EBX", "ESP", "EBP", "ESI", "EDI" };

TYPE RegSet = SET OF Regno;

END M3x86Rep.
