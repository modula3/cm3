(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Dec 20 16:26:29 PST 1994 by kalsow     *)
(*      modified on Mon Sep 26 14:27:12 PDT 1994 by isard      *)
(*      modified on Mon Apr 13 09:55:12 PDT 1992 by muller     *)

INTERFACE M3x86Rep;

IMPORT M3CG, M3ID, TIntN, Target;

FROM M3CG IMPORT ByteOffset, ByteSize, Alignment;
FROM M3CG IMPORT Var, Proc, Name, Label;
FROM M3CG IMPORT Type, MType, IType, TypeUID;
FROM M3ObjFile IMPORT Seg;

TYPE U <: Public;
TYPE Public = M3CG.T OBJECT
        proc_reguse := ARRAY Regno OF BOOLEAN{FALSE,..};
      METHODS
        NewVar (t: Type; m3t: TypeUID; s: ByteSize; a: Alignment;
                name: Name := M3ID.NoID): x86Var;
        debug_set_label (label: Label);
      END;

TYPE VLoc = {global, temp};

TYPE OLoc = {mem, register, fstack, imm};

TYPE
  x86Var = Var OBJECT
    tag: INTEGER;
    var_type: Type;
    name: Name;
    var_size: ByteSize;
    var_align: Alignment;
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
    proc_type: Type;
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
    fenceVar: x86Var := NIL;
    is_alloca := FALSE;
  END;

TYPE
  MVar = RECORD
    var: x86Var;
    mvar_offset: ByteOffset := 0;
    mvar_type: MType;
  END;

CONST NoStore = MVar {var := NIL, mvar_type := FIRST(MType)};

(* If an operand requires two registers, it has parts 0 and 1.
 * etc. This is used to implement multi precision integers,
 * and possibly enregistering structs. Likewise, if an
 * operand fits in one register, it has size 1, else 2.
 *)

TYPE OperandPart = [0..1];
TYPE OperandSize = [1..2];

TYPE
  Operand1 = RECORD
    loc: OLoc;
    mvar: MVar := NoStore;
    reg : Regno := 0;       (* seems like it should be -1 *)
    imm: INTEGER := 0;      (* This might change to TIntN.T. *)
    stackp: CARDINAL := 0;   (* this field might go away; seems like it should be -1 *)
    opcode := FALSE;
  END;

  Operand = RECORD
    loc: OLoc;
    mvar: MVar := NoStore;
    reg := ARRAY OperandPart OF Regno{0, ..}; (* seems like it should be -1 *)
    imm: TIntN.T := TIntN.Zero;
    optype: Type := Type.Void;
    stackp: CARDINAL := 0; (* seems like it should be -1 *)
    opcode := FALSE;
  END;

TYPE FlToInt = { Round, Floor, Ceiling, Truncate };

TYPE Force = {any, mem, anydword, anytemp, anyregimm, anyreg, regset};

CONST NRegs: INTEGER = 7;

TYPE Regno = [-1 .. NRegs];

(* These constants relate to how x86 instructions are encoded. Do not change them. *)

CONST                                             EAX=0; ECX=1; EDX=2; EBX=3; ESP=4; EBP=5; ESI=6; EDI=7;
CONST RegName = ARRAY Regno OF TEXT { "*NOREG*", "EAX", "ECX", "EDX", "EBX", "ESP", "EBP", "ESI", "EDI" };

TYPE RegSet = SET OF Regno;

(* ESP and EBP are "special" and excluded from these sets. *)

CONST NonVolatileRegisters = RegSet{EDI, ESI, (*EBP,*) EBX};
CONST    VolatileRegisters = RegSet{EAX, ECX, EDX (*,ESP*)};
CONST         AllRegisters = RegSet{EAX, ECX, EDX, EBX,
                                    (*ESP,*) (*EBP,*) ESI, EDI};
CONST RegistersForByteOperations = RegSet{EAX, EBX, ECX, EDX};

PROCEDURE TypeIsUnsigned (t: Type): BOOLEAN;
PROCEDURE TypeIsSigned (t: Type): BOOLEAN;
PROCEDURE TypeIs64 (t: Type): BOOLEAN;
PROCEDURE SplitMVar(READONLY mvar: MVar; VAR mvarA: ARRAY OperandPart OF MVar): OperandSize;
PROCEDURE SplitImm(type: Type; READONLY imm: TIntN.T; VAR immA: ARRAY OperandPart OF TIntN.T): OperandSize;
PROCEDURE SplitOperand(READONLY op: Operand; VAR opA: ARRAY OperandPart OF Operand): OperandSize;
PROCEDURE GetOperandSize(READONLY op: Operand): OperandSize;
PROCEDURE GetTypeSize(type: Type): OperandSize;

CONST TZero = TIntN.Zero;
CONST TOne = TIntN.One;

CONST UnsignedType = ARRAY IType OF IType { Type.Word32, Type.Word32,
                                            Type.Word64, Type.Word64 };

CONST MaximumShift = ARRAY IType OF TIntN.T { TIntN.ThirtyOne, TIntN.ThirtyOne,
                                              TIntN.SixtyThree, TIntN.SixtyThree };

CONST MinimumShift = ARRAY IType OF TIntN.T { TIntN.MThirtyOne, TIntN.MThirtyOne,
                                              TIntN.MSixtyThree, TIntN.MSixtyThree };

CONST BitCountMask = MaximumShift;
 
VAR(*CONST*) IntType: ARRAY M3CG.MType OF Target.Int_type;

END M3x86Rep.
