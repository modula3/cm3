(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu Jun 23 17:05:28 PDT 1994 by kalsow     *)

UNSAFE MODULE RTTypeMap;

IMPORT RT0, RTMapOp, RTType;

TYPE
  Byte = RTMapOp.Byte;
  BP   = RTMapOp.BP;
  IP   = RTMapOp.IP;
  AP   = RTMapOp.AP;
  PC   = RTMapOp.PC;
  Op   = RTMapOp.T;

REVEAL
  Visitor = V_ BRANDED "RTTypeMap.Visitor" OBJECT
    stack: RTMapOp.Stack;
  END;

PROCEDURE WalkRef (r: REFANY;  m: Mask;  v: Visitor) RAISES ANY =
  VAR
    tc: RT0.Typecode;
    t: RT0.TypeDefn;
  BEGIN
    IF (r = NIL) THEN RETURN END;
    tc := TYPECODE (r);
    t := RTType.Get (tc);
    DoWalkRef (t, LOOPHOLE (r, ADDRESS), m, v);
  END WalkRef;

PROCEDURE DoWalkRef (t: RT0.TypeDefn; a: ADDRESS; m: Mask; v: Visitor)
  RAISES ANY =
  BEGIN
    IF (t.parent # NIL) THEN
      DoWalkRef (t.parent, a, m, v);
      INC (a, t.dataOffset);
    END;
    Walk (a, t.type_map, m, v);
  END DoWalkRef;

PROCEDURE Walk (x, pc: ADDRESS;  mask: Mask;  v: Visitor) RAISES ANY =
  VAR op: Op;  n, m, z: INTEGER;   bottom := v.stack.top;
  BEGIN
    IF (x = NIL) OR (pc = NIL) THEN RETURN END;
    LOOP
      op := LOOPHOLE (pc, PC)^;
      INC (pc, ADRSIZE (Op));
      CASE op OF
      | Op.Mark =>
          RTMapOp.Push (v.stack, pc, -1);

      | Op.Stop =>
          LOOP
            IF (v.stack.top <= bottom) THEN RETURN END;
            (* otherwise, resume a suspended walk *)
            WITH tos = v.stack.data [v.stack.top-1] DO
              pc := tos.pc;
              DEC (tos.count);
              IF (tos.count >= 0) THEN EXIT END;
              DEC (v.stack.top);
            END;
          END;

      | Op.Array_1, Op.Array_2, Op.Array_3, Op.Array_4,
        Op.Array_5, Op.Array_6, Op.Array_7, Op.Array_8 =>
          z := RTMapOp.OpArgBytes [op];
          n := RTMapOp.GetInt (pc, z); (* number of elements *)
          WITH tos = v.stack.data [v.stack.top-1] DO
            IF tos.count = -1 THEN (* first time *) tos.count := n END;
            DEC (tos.count);
            IF (tos.count > 0) THEN  pc := tos.pc  ELSE  DEC (v.stack.top) END;
          END;

      | Op.OpenArray_1, Op.OpenArray_2 =>
          z := RTMapOp.OpArgBytes [op];
          n := RTMapOp.GetInt (pc, z); (* open array depth *)
          VAR elts := LOOPHOLE (x, AP)^;  (* array elements *) BEGIN
            INC (x, ADRSIZE (ADDRESS));
 
            (* compute the total number of elements to visit *)
            m := 1;
            FOR j := 1 TO n DO
              m := m * LOOPHOLE (x, IP)^;
              INC (x, ADRSIZE (INTEGER));
            END;

            (* push the remainder of the map with the element count *)
            IF (m <= 0) THEN RETURN END;
            RTMapOp.Push (v.stack, pc, m-1);
            x := elts;
          END;

      | Op.Set_1, Op.Set_2, Op.Set_3, Op.Set_4 =>
          z := RTMapOp.OpArgBytes [op];
          IF Kind.Set IN mask THEN v.apply (x, Kind.Set) END;
          n := RTMapOp.GetInt (pc, z);
          INC (x, n * ADRSIZE (Byte));

      | Op.Ref, Op.UntracedRef, Op.Proc,
        Op.Real, Op.Longreal, Op.Extended,
        Op.Int_1, Op.Int_2, Op.Int_4, Op.Int_8,
        Op.Word_1, Op.Word_2, Op.Word_4, Op.Word_8 =>
          VAR kind := RTMapOp.OpKind [op]; BEGIN
            IF kind IN mask THEN v.apply (x, kind) END;
            INC (x, RTMapOp.OpSize [op]);
          END;

      | Op.Int_Field, Op.Word_Field =>
          VAR kind := RTMapOp.OpKind [op]; BEGIN
            IF kind IN mask THEN v.apply (x, kind) END;
          END;
          (* n := LOOPHOLE (pc, BP)^; *)
          (* m := LOOPHOLE (pc+1, BP)^; *)
          INC (pc, 2 * ADRSIZE (Byte));

      | Op.PushPtr =>
          RTMapOp.Push (v.stack, x + ADRSIZE (AP), -1);
          x := LOOPHOLE (x, AP)^;

      | Op.Return =>
          DEC (v.stack.top);
          x := v.stack.data[v.stack.top].pc;

      | Op.Skip_1, Op.Skip_2, Op.Skip_3, Op.Skip_4,
        Op.Skip_5, Op.Skip_6, Op.Skip_7, Op.Skip_8 =>
          INC (x, RTMapOp.OpSize [op]);

      | Op.SkipF_1 =>
          INC (x, LOOPHOLE (pc, BP)^);
          INC (pc, ADRSIZE (Byte));

      | Op.SkipF_2, Op.SkipF_3, Op.SkipF_4,
        Op.SkipF_5, Op.SkipF_6, Op.SkipF_7, Op.SkipF_8 =>
          z := RTMapOp.OpArgBytes [op];
          n := RTMapOp.GetInt (pc, z);
          INC (x, n);

      | Op.SkipB_1 =>
          DEC (x, LOOPHOLE (pc, BP)^);
          INC (pc, ADRSIZE (Byte));

      | Op.SkipB_2, Op.SkipB_3, Op.SkipB_4,
        Op.SkipB_5, Op.SkipB_6, Op.SkipB_7, Op.SkipB_8 =>
          z := RTMapOp.OpArgBytes [op];
          n := RTMapOp.GetInt (pc, z);
          DEC (x, n);
      END;
    END;
  END Walk;

BEGIN 
END RTTypeMap.


