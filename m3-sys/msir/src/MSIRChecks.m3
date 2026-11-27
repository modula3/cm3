MODULE MSIRChecks;

IMPORT MSIR, Fmt, Target, Word, M3RT;
FROM MSIR IMPORT Op;

CONST
  LtPred = ARRAY BOOLEAN OF MSIR.CmpPred {MSIR.CmpPred.Slt, MSIR.CmpPred.Ult};
  GtPred = ARRAY BOOLEAN OF MSIR.CmpPred {MSIR.CmpPred.Sgt, MSIR.CmpPred.Ugt};

VAR
  seq := 0;                  (* for unique block labels *)
  slot: MSIR.Value := NIL;   (* the current proc's type-test answer *)

CONST NoArgs = ARRAY OF MSIR.Value {};

PROCEDURE IsCheck (op: Op): BOOLEAN =
  BEGIN
    RETURN op IN SET OF Op {Op.NilCheck, Op.RangeCheck, Op.SubscriptCheck,
                            Op.Fault, Op.ShapeCheck, Op.Narrow};
  END IsCheck;

(* Does Expand replace op? *)
PROCEDURE Expands (op: Op): BOOLEAN =
  BEGIN
    RETURN IsCheck (op)
        OR op IN SET OF Op {Op.Istype, Op.New, Op.OpenArrayNew, Op.Raise,
                            Op.Method};
  END Expands;

CONST AllocHook = ARRAY MSIR.AllocKind OF MSIR.RtHook {
  MSIR.RtHook.NewTracedRef, MSIR.RtHook.NewUntracedRef,
  MSIR.RtHook.NewTracedObj, MSIR.RtHook.NewUntracedObj,
  MSIR.RtHook.NewTracedArray, MSIR.RtHook.NewUntracedArray };

PROCEDURE Hooks (m: MSIR.Module): SET OF MSIR.RtHook =
  VAR
    s := SET OF MSIR.RtHook {};
    p: MSIR.Proc;  b: MSIR.Block;  i: MSIR.Insn;  op: Op;
  BEGIN
    FOR pi := 0 TO MSIR.ModuleProcCount (m) - 1 DO
      p := MSIR.ModuleProc (m, pi);
      FOR k := 0 TO MSIR.ProcBlockCount (p) - 1 DO
        b := MSIR.ProcBlock (p, k);
        FOR j := 0 TO MSIR.BlockInsnCount (b) - 1 DO
          i := MSIR.BlockInsn (b, j);
          op := MSIR.InsnOp (i);
          IF IsCheck (op) THEN
            s := s + SET OF MSIR.RtHook {MSIR.RtHook.ReportFault};
          END;
          IF op IN SET OF Op {Op.Istype, Op.Narrow}
             AND MSIR.InsnTypeTest (i) = MSIR.TypeTest.Subtype THEN
            s := s + SET OF MSIR.RtHook {MSIR.RtHook.CheckIsType};
          ELSIF op IN SET OF Op {Op.New, Op.OpenArrayNew} THEN
            s := s + SET OF MSIR.RtHook {AllocHook [MSIR.InsnAllocKind (i)]};
          ELSIF op = Op.Raise THEN
            s := s + SET OF MSIR.RtHook {MSIR.RtHook.RaiseEx};
            IF MSIR.InsnOperandCount (i) = 2 THEN
              s := s + SET OF MSIR.RtHook {MSIR.RtHook.NewTracedRef};
            END;
          END;
        END;
      END;
    END;
    RETURN s;
  END Hooks;

PROCEDURE Hook (m: MSIR.Module;  h: MSIR.RtHook): MSIR.Proc =
  VAR p := MSIR.ModuleRtHook (m, h);
  BEGIN
    <* ASSERT p # NIL *>  (* the front end binds Hooks (m) *)
    RETURN p;
  END Hook;

(* A new alloca of type t at the head of p's entry block. *)
PROCEDURE EntryAlloca (p: MSIR.Proc;  t: MSIR.T): MSIR.Value =
  VAR
    entry := MSIR.ProcEntry (p);
    insns := MSIR.BlockTakeInsns (entry);
    a := MSIR.BuildAlloca (entry, "", t);
  BEGIN
    FOR j := 0 TO LAST (insns^) DO MSIR.BlockAppendInsn (entry, insns[j]) END;
    RETURN a;
  END EntryAlloca;

(* The procedure that method i looks up, in block b: the suite pointer is
   the object's first word; the slot is at the op's offset, plus the
   holder's OTC_methodOffset if it has one (QualifyExpr's code before). *)
PROCEDURE Method (b: MSIR.Block;  i: MSIR.Insn): MSIR.Value =
  VAR
    ptrT  := MSIR.TPtr (MSIR.TVoid ());
    intT  := MSIR.TI (Target.Integer.size);
    off   := MSIR.InsnMethodOffset (i);
    suite := MSIR.BuildLoad (b, "", ptrT,
               MSIR.BuildConvert (b, "", MSIR.InsnOperand (i, 0),
                                  MSIR.TPtr (ptrT)));
    slot  : MSIR.Value;
  BEGIN
    IF MSIR.InsnOperandCount (i) = 2 THEN
      VAR cell := MSIR.InsnOperand (i, 1);
          base := MSIR.BuildLoad (b, "", intT,
                    MSIR.BuildPtrAdd (b, "", cell,
                                      M3RT.OTC_methodOffset DIV Target.Byte));
      BEGIN
        slot := MSIR.BuildGepByte (b, "", suite,
                  MSIR.BuildIAdd (b, "", base, MSIR.ConstInt (intT, off)));
      END;
    ELSIF off = 0 THEN
      slot := suite;
    ELSE
      slot := MSIR.BuildPtrAdd (b, "", suite, off);
    END;
    RETURN MSIR.BuildLoad (b, "", ptrT, slot);
  END Method;

(* Raise as i in block b, which it ends: RTHooks.RaiseEx (exc, arg,
   module, line), with arg the runtime's argument word.  As the C backend:
   a by-reference argument is copied to a new heap box (Exceptionz's
   raiser procedure); a wide set goes by the address of a copy. *)
PROCEDURE Raise (m: MSIR.Module;  p: MSIR.Proc;  b: MSIR.Block;
                 i: MSIR.Insn) =
  VAR
    exc  := MSIR.InsnExcDesc (i);
    ptrT := MSIR.TPtr (MSIR.TVoid ());
    intT := MSIR.TI (Target.Integer.size);
    arg  : MSIR.Value;
  BEGIN
    CASE MSIR.InsnOperandCount (i) OF
    | 0 =>
        arg := MSIR.ConstNil (ptrT);
    | 1 =>
        arg := MSIR.InsnOperand (i, 0);
        IF MSIR.Kind (MSIR.ValueType (arg)) = MSIR.TypeKind.IWide THEN
          VAR slot := EntryAlloca (p, MSIR.ValueType (arg));
          BEGIN
            MSIR.BuildStore (b, arg, slot);
            arg := slot;
          END;
        END;
        arg := MSIR.BuildConvert (b, "", arg, ptrT);
    | 2 =>
        VAR
          t   := MSIR.ExcDescArgType (exc);
          val := MSIR.BuildLoad (b, "", t, MSIR.InsnOperand (i, 0));
          box := MSIR.BuildCall (b, "", Hook (m, MSIR.RtHook.NewTracedRef),
                                 ARRAY OF MSIR.Value {MSIR.InsnOperand (i, 1)});
        BEGIN
          box := MSIR.BuildConvert (b, "", box, ptrT);
          MSIR.BuildStore (b, val, box);
          arg := box;
        END;
    ELSE <* ASSERT FALSE *>
    END;
    EVAL MSIR.BuildCall (b, "", Hook (m, MSIR.RtHook.RaiseEx),
           ARRAY OF MSIR.Value {
             MSIR.ExcDescValue (exc), arg,
             MSIR.StructFieldRef (MSIR.ModuleInfoName (m), 0, ptrT),
             MSIR.ConstInt (intT, MSIR.InsnSrcLine (i))});
    MSIR.BuildUnreachable (b);
  END Raise;

(* Allocate as i (new or openarray.new) in block b: the allocator's
   result, as i's type. *)
PROCEDURE Allocate (m: MSIR.Module;  p: MSIR.Proc;  b: MSIR.Block;
                    i: MSIR.Insn): MSIR.Value =
  VAR
    cell := MSIR.InsnOperand (i, 0);
    hook := Hook (m, AllocHook [MSIR.InsnAllocKind (i)]);
    raw: MSIR.Value;
  BEGIN
    IF MSIR.InsnOp (i) = Op.New THEN
      raw := MSIR.BuildCall (b, "", hook, ARRAY OF MSIR.Value {cell});
    ELSE
      (* the open-array shape the allocator takes: elt_ptr, then the
         number of dimensions, then the sizes, at M3RT.OA_elt_ptr and on *)
      VAR
        ndims  := MSIR.InsnOperandCount (i) - 1;
        ptrT   := MSIR.TPtr (MSIR.TVoid ());
        intT   := MSIR.TI (Target.Integer.size);
        ipB    := Target.Integer.size DIV Target.Byte;
        shapeB := M3RT.OA_sizes DIV Target.Byte;
        flds   := NEW (REF ARRAY OF MSIR.Field, ndims + 2);
        shape  : MSIR.Value;
      BEGIN
        flds[0] := MSIR.Field {name := "", type := ptrT};
        flds[1] := MSIR.Field {name := "", type := intT};
        FOR k := 0 TO ndims - 1 DO
          flds[2 + k] := MSIR.Field {name := "", type := intT};
        END;
        shape := EntryAlloca (p, MSIR.TStruct ("__oa_shape", flds^));
        MSIR.BuildStore (b, MSIR.BuildPtrAdd (b, "", shape, shapeB + ipB),
                         MSIR.BuildPtrAdd (b, "", shape, 0));
        MSIR.BuildStore (b, MSIR.ConstInt (intT, ndims),
                         MSIR.BuildPtrAdd (b, "", shape, shapeB));
        FOR k := 1 TO ndims DO
          MSIR.BuildStore (b, MSIR.BuildConvert (b, "", MSIR.InsnOperand (i, k), intT),
                           MSIR.BuildPtrAdd (b, "", shape, shapeB + ipB * k));
        END;
        raw := MSIR.BuildCall (b, "", hook, ARRAY OF MSIR.Value {cell, shape});
      END;
    END;
    RETURN MSIR.BuildConvert (b, "", raw, MSIR.ValueType (MSIR.InsnResult (i)));
  END Allocate;

PROCEDURE HasCheck (b: MSIR.Block): BOOLEAN =
  BEGIN
    FOR j := 0 TO MSIR.BlockInsnCount (b) - 1 DO
      IF Expands (MSIR.InsnOp (MSIR.BlockInsn (b, j))) THEN RETURN TRUE END;
    END;
    RETURN FALSE;
  END HasCheck;

PROCEDURE NewBlk (p: MSIR.Proc;  like: MSIR.Block;  tag: TEXT): MSIR.Block =
  VAR b := MSIR.NewBlock ("chk." & tag & "." & Fmt.Int (seq),
                          ARRAY OF MSIR.BlockParam {});
  BEGIN
    INC (seq);
    MSIR.ProcAddBlock (p, b);
    MSIR.BlockSetScope (b, MSIR.BlockScope (like));
    RETURN b;
  END NewBlk;

(* Is bound c the extreme (lowest if lo, else highest) of the w-bit
   integers, so that comparing against it can never fail?  A non-constant
   bound, or one represented otherwise, just costs a compare. *)
PROCEDURE Extreme (c: MSIR.Value;  lo, unsigned: BOOLEAN;  w: INTEGER): BOOLEAN =
  VAR x, e: INTEGER;
  BEGIN
    IF MSIR.GetValueKind (c) # MSIR.ValueKind.ConstInt
       OR w <= 0 OR w > BITSIZE (INTEGER) THEN RETURN FALSE END;
    x := MSIR.GetIntVal (c);
    IF unsigned THEN
      IF lo THEN e := 0
      ELSIF w = BITSIZE (INTEGER) THEN e := -1
      ELSE e := Word.Minus (Word.LeftShift (1, w), 1)
      END;
    ELSIF w = BITSIZE (INTEGER) THEN
      IF lo THEN e := FIRST (INTEGER) ELSE e := LAST (INTEGER) END;
    ELSE
      e := Word.LeftShift (1, w - 1);
      IF lo THEN e := -e ELSE e := e - 1 END;
    END;
    RETURN x = e;
  END Extreme;

(* Is the reference v of the type tested by i (istype or narrow)?  Built
   from block cur, which may end; cur is then where the answer is known.
   The code the C backend emits for NARROW (Narrow.EmitCore). *)
PROCEDURE TypeTest (m: MSIR.Module;  p: MSIR.Proc;  like: MSIR.Block;
                    VAR cur: MSIR.Block;  i: MSIR.Insn): MSIR.Value =
  VAR
    v    := MSIR.InsnOperand (i, 0);
    intT := MSIR.TI (Target.Integer.size);
    i1   := MSIR.TI1 ();
    cell, tc, word, hdr, rtc, raw: MSIR.Value;
    yes, no, join, notNil, tagged, untagged, other: MSIR.Block;
  BEGIN
    IF MSIR.InsnTypeTest (i) = MSIR.TypeTest.Null THEN
      RETURN MSIR.BuildICmp (cur, "", MSIR.CmpPred.Eq, v,
                             MSIR.ConstNil (MSIR.ValueType (v)));
    END;
    cell := MSIR.InsnOperand (i, 1);
    yes  := NewBlk (p, like, "tt.yes");
    no   := NewBlk (p, like, "tt.no");
    join := NewBlk (p, like, "tt.join");
    notNil   := NewBlk (p, like, "tt.notnil");
    tagged   := NewBlk (p, like, "tt.tagged");
    untagged := NewBlk (p, like, "tt.untagged");
    tc := MSIR.BuildLoad (cur, "", intT, cell);  (* TC_typecode, at 0 *)

    (* NIL is of every reference type *)
    MSIR.BuildCondBr (cur, MSIR.BuildICmp (cur, "", MSIR.CmpPred.Eq, v,
                                           MSIR.ConstNil (MSIR.ValueType (v))),
                      yes, NoArgs, notNil, NoArgs);

    (* a tagged reference (low bit set) has REFANY's typecode *)
    word := MSIR.BuildConvert (notNil, "", v, intT);
    MSIR.BuildCondBr (notNil,
      MSIR.BuildICmp (notNil, "", MSIR.CmpPred.Ne,
                      MSIR.BuildIAnd (notNil, "", word, MSIR.ConstInt (intT, 1)),
                      MSIR.ConstInt (intT, 0)),
      tagged, NoArgs, untagged, NoArgs);
    MSIR.BuildCondBr (tagged,
      MSIR.BuildICmp (tagged, "", MSIR.CmpPred.Eq,
                      MSIR.ConstInt (intT, M3RT.REFANY_typecode), tc),
      yes, NoArgs, no, NoArgs);

    (* the typecode in the header, one word before the referent *)
    hdr := MSIR.BuildLoad (untagged, "", intT,
             MSIR.BuildPtrAdd (untagged, "", v,
                               -(Target.Address.size DIV Target.Byte)));
    rtc := MSIR.BuildIAnd (untagged, "",
             MSIR.BuildILShr (untagged, "", hdr,
                              MSIR.ConstInt (intT, M3RT.RH_typecode_offset)),
             MSIR.ConstInt (intT, Word.LeftShift (1, M3RT.RH_typecode_size) - 1));
    IF MSIR.InsnTypeTest (i) = MSIR.TypeTest.Subtype THEN
      other := NewBlk (p, like, "tt.subtype");
    ELSE
      other := no;
    END;
    MSIR.BuildCondBr (untagged,
      MSIR.BuildICmp (untagged, "", MSIR.CmpPred.Eq, rtc, tc),
      yes, NoArgs, other, NoArgs);

    (* an object or opaque type: the runtime decides *)
    IF other # no THEN
      raw := MSIR.BuildCall (other, "", Hook (m, MSIR.RtHook.CheckIsType),
                             ARRAY OF MSIR.Value {v, cell});
      MSIR.BuildCondBr (other,
        MSIR.BuildICmp (other, "", MSIR.CmpPred.Ne, raw,
                        MSIR.ConstInt (MSIR.ValueType (raw), 0)),
        yes, NoArgs, no, NoArgs);
    END;

    (* the answer goes through the proc's slot: the lowerings do not take
       block arguments *)
    MSIR.BuildStore (yes, MSIR.ConstInt (i1, 1), slot);
    MSIR.BuildBr (yes, join, NoArgs);
    MSIR.BuildStore (no, MSIR.ConstInt (i1, 0), slot);
    MSIR.BuildBr (no, join, NoArgs);
    cur := join;
    RETURN MSIR.BuildLoad (join, "", i1, slot);
  END TypeTest;

(* The condition under which check i fails, built from block cur (which
   a narrow's type test moves on). *)
PROCEDURE FailCond (m: MSIR.Module;  p: MSIR.Proc;  like: MSIR.Block;
                    VAR cur: MSIR.Block;  i: MSIR.Insn): MSIR.Value =
  VAR v := MSIR.InsnOperand (i, 0);  b := cur;
  BEGIN
    CASE MSIR.InsnOp (i) OF
    | Op.Narrow =>
        VAR is := TypeTest (m, p, like, cur, i);
        BEGIN
          RETURN MSIR.BuildICmp (cur, "", MSIR.CmpPred.Eq, is,
                                 MSIR.ConstInt (MSIR.TI1 (), 0));
        END;
    | Op.NilCheck =>
        RETURN MSIR.BuildICmp (b, "", MSIR.CmpPred.Eq, v,
                               MSIR.ConstNil (MSIR.ValueType (v)));
    | Op.ShapeCheck =>
        RETURN MSIR.BuildICmp (b, "", MSIR.CmpPred.Ne, v,
                               MSIR.InsnOperand (i, 1));
    | Op.SubscriptCheck =>
        RETURN MSIR.BuildICmp (b, "", MSIR.CmpPred.Uge, v,
                               MSIR.InsnOperand (i, 1));
    | Op.RangeCheck =>
        VAR
          lo := MSIR.InsnOperand (i, 1);
          hi := MSIR.InsnOperand (i, 2);
          u  := MSIR.InsnCheckUnsigned (i);
          w  := MSIR.BitWidth (MSIR.ValueType (v));
          below, above: MSIR.Value := NIL;
        BEGIN
          IF NOT Extreme (lo, TRUE, u, w) THEN
            below := MSIR.BuildICmp (b, "", LtPred[u], v, lo);
          END;
          IF NOT Extreme (hi, FALSE, u, w) THEN
            above := MSIR.BuildICmp (b, "", GtPred[u], v, hi);
          END;
          IF below = NIL AND above = NIL THEN
            RETURN MSIR.ConstInt (MSIR.TI1 (), 0);
          ELSIF below = NIL THEN RETURN above
          ELSIF above = NIL THEN RETURN below
          ELSE RETURN MSIR.BuildIOr (b, "", below, above)
          END;
        END;
    ELSE <* ASSERT FALSE *>
    END;
  END FailCond;

(* In b, report check i's failure: RTHooks.ReportFault (module,
   line * 32 + code), which does not return. *)
PROCEDURE CallHook (m: MSIR.Module;  b: MSIR.Block;  i: MSIR.Insn) =
  VAR
    ptrT := MSIR.TPtr (MSIR.TVoid ());
    intT := MSIR.TI (Target.Integer.size);
  BEGIN
    EVAL MSIR.BuildCall (b, "", Hook (m, MSIR.RtHook.ReportFault),
           ARRAY OF MSIR.Value {
             MSIR.StructFieldRef (MSIR.ModuleInfoName (m), 0, ptrT),
             MSIR.ConstInt (intT, MSIR.InsnSrcLine (i) * 32
                                  + MSIR.InsnCheckCode (i))});
    MSIR.BuildUnreachable (b);
  END CallHook;

(* Does p test a type by its typecell (needing the answer slot)? *)
PROCEDURE NeedsSlot (p: MSIR.Proc): BOOLEAN =
  VAR b: MSIR.Block;  i: MSIR.Insn;
  BEGIN
    FOR k := 0 TO MSIR.ProcBlockCount (p) - 1 DO
      b := MSIR.ProcBlock (p, k);
      FOR j := 0 TO MSIR.BlockInsnCount (b) - 1 DO
        i := MSIR.BlockInsn (b, j);
        IF MSIR.InsnOp (i) IN SET OF Op {Op.Istype, Op.Narrow}
           AND MSIR.InsnTypeTest (i) # MSIR.TypeTest.Null THEN
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END NeedsSlot;

PROCEDURE ExpandProc (m: MSIR.Module;  p: MSIR.Proc) =
  VAR
    k := 0;
  BEGIN
    slot := NIL;
    IF NeedsSlot (p) THEN
      slot := EntryAlloca (p, MSIR.TI1 ());
    END;
    (* New continuation blocks are appended and examined in turn. *)
    WHILE k < MSIR.ProcBlockCount (p) DO
      VAR b := MSIR.ProcBlock (p, k);
      BEGIN
        IF HasCheck (b) THEN
          VAR insns := MSIR.BlockTakeInsns (b);  cur := b;
          BEGIN
            FOR j := 0 TO LAST (insns^) DO
              VAR i := insns[j];
              BEGIN
                IF MSIR.InsnOp (i) = Op.Fault THEN
                  (* a terminator: the call and unreachable end the block *)
                  CallHook (m, cur, i);
                ELSIF MSIR.InsnOp (i) = Op.Raise THEN
                  Raise (m, p, cur, i);
                ELSIF MSIR.InsnOp (i) = Op.Istype THEN
                  (* the test's answer replaces i's result *)
                  MSIR.InsnMakeCopy (i, TypeTest (m, p, b, cur, i));
                  MSIR.BlockAppendInsn (cur, i);
                ELSIF MSIR.InsnOp (i) = Op.Method THEN
                  MSIR.InsnMakeCopy (i, Method (cur, i));
                  MSIR.BlockAppendInsn (cur, i);
                ELSIF MSIR.InsnOp (i) IN SET OF Op {Op.New, Op.OpenArrayNew} THEN
                  MSIR.InsnMakeCopy (i, Allocate (m, p, cur, i));
                  MSIR.BlockAppendInsn (cur, i);
                ELSIF IsCheck (MSIR.InsnOp (i)) THEN
                  VAR fail  := FailCond (m, p, b, cur, i);
                      fault := NewBlk (p, b, "fault");
                      cont  := NewBlk (p, b, "cont");
                  BEGIN
                    MSIR.BuildCondBr (cur, fail,
                                      fault, ARRAY OF MSIR.Value {},
                                      cont,  ARRAY OF MSIR.Value {});
                    CallHook (m, fault, i);
                    cur := cont;
                  END;
                ELSE
                  MSIR.BlockAppendInsn (cur, i);
                END;
              END;
            END;
          END;
        END;
        INC (k);
      END;
    END;
  END ExpandProc;

PROCEDURE Expand (m: MSIR.Module) =
  BEGIN
    FOR i := 0 TO MSIR.ModuleProcCount (m) - 1 DO
      ExpandProc (m, MSIR.ModuleProc (m, i));
    END;
  END Expand;

BEGIN
END MSIRChecks.
