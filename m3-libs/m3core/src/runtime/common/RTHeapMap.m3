(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Nov 18 17:45:58 PST 1994 by kalsow     *)
(*      modified on Mon Apr 26 19:00:42 PDT 1993 by muller     *)

UNSAFE MODULE RTHeapMap;

IMPORT RT0, RTType, RTHeapRep, RTModule, RTMapOp, RTIO;

VAR DEBUG := FALSE;

TYPE
  Byte = RTMapOp.Byte;
  BP   = RTMapOp.BP;
  IP   = RTMapOp.IP;
  AP   = RTMapOp.AP;
  PC   = RTMapOp.PC;
  Op   = RTMapOp.T;

REVEAL
  Visitor = V_ BRANDED "RTHeapMap.Visitor" OBJECT
    stack: RTMapOp.Stack;
  END;

(*------------------------------------------------ walking of single refs ---*)

PROCEDURE WalkRef (h: ObjectPtr;  v: Visitor) =
  VAR
    tc: RT0.Typecode := h.typecode;
    t: RT0.TypeDefn;
  BEGIN
    IF (tc # RTHeapRep.Fill_1_type) AND (tc # RTHeapRep.Fill_N_type) THEN
      <* ASSERT tc # 0 *>
      t := RTType.Get (tc);
      IF DEBUG THEN
        RTIO.PutText ("Ref: tc=");
        RTIO.PutInt  (tc);
        RTIO.PutText (" cell=");
        RTIO.PutAddr (t);
        RTIO.PutText (" ref=");
        RTIO.PutAddr (h + ADRSIZE (h^));
        RTIO.PutText("\n");
        RTIO.Flush ();
      END;
      DoWalkRef (t, h+ADRSIZE (h^), v);
    END;
  END WalkRef;

PROCEDURE DoWalkRef (t: RT0.TypeDefn;  a: ADDRESS;  v: Visitor) =
  BEGIN
    IF (t.parent # NIL) THEN
      DoWalkRef (t.parent, a, v);
      INC (a, t.dataOffset);
    END;
    Walk (a, t.gc_map, v);
  END DoWalkRef;
  
(*------------------------------------------------ walking of the globals ---*)

TYPE ModuleMap = UNTRACED REF ARRAY OF RT0.ModulePtr;
VAR global_map: ModuleMap := NIL;

PROCEDURE WalkGlobals (v: Visitor) =
  VAR m: RT0.ModulePtr;
  BEGIN
    IF (global_map = NIL) THEN BuildGlobalMap () END;
    FOR i := 0 TO LAST (global_map^) DO
      m := global_map[i];
      IF DEBUG THEN
        RTIO.PutText ("Module: ");
        RTIO.PutInt  (i);
        RTIO.PutText ("  ");
        RTIO.PutAddr (m);
        RTIO.PutText ("\n");
        RTIO.Flush ();
      END;
      Walk (m, m.gc_map, v);
    END;
  END WalkGlobals;

PROCEDURE BuildGlobalMap () =
  VAR n: INTEGER;  m: RT0.ModulePtr;  max := RTModule.Count()-1;
  BEGIN
    (* first, count the modules that qualify *)
    n := 0;
    FOR i := 0 TO max DO
      m := RTModule.Get (i);
      IF (m # NIL) AND (m.gc_map # NIL) THEN INC (n); END;
    END;

    (* allocate the space *)
    global_map := NEW (ModuleMap, n);

    (* and fill it in *)
    n := 0;
    FOR i := 0 TO max DO
      m := RTModule.Get (i);
      IF (m # NIL) AND (m.gc_map # NIL) THEN
        global_map[n] := m;
        INC (n);
      END;
    END;
  END BuildGlobalMap;

PROCEDURE WalkModuleGlobals (v: Visitor;  mod: CARDINAL) =
  VAR m := RTModule.Get (mod);
  BEGIN
    IF (m # NIL) AND (m.gc_map # NIL) THEN
      Walk (m, m.gc_map, v);
    END;
  END WalkModuleGlobals;

(*------------------------------------------------------- internal walker ---*)

PROCEDURE Walk (x, pc: ADDRESS;  v: Visitor) =
  VAR op: Op;  n, m, z: INTEGER;  bottom := v.stack.top;
  BEGIN
    IF (x = NIL) OR (pc = NIL) THEN RETURN END;
    IF DEBUG THEN
      RTIO.PutText ("Walk: map=");
      RTIO.PutAddr (pc);
      RTIO.PutText (" val=");
      RTIO.PutAddr (x);
      RTIO.PutText (" visitor=");
      RTIO.PutAddr (LOOPHOLE (v, ADDRESS));
      RTIO.PutText ("\n");
      RTIO.Flush ();
    END;
    LOOP
      op := LOOPHOLE (pc, PC)^;
      IF DEBUG THEN
        RTIO.PutText ("  pc=");
        RTIO.PutAddr (pc);
        RTIO.PutText ("  op=");
        RTIO.PutInt  (ORD (op));
        RTIO.PutText ("\n");
        RTIO.Flush ();
      END;
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
            
      | Op.Ref =>
          v.apply (x);
          INC (x, ADRSIZE (ADDRESS));

      | Op.PushPtr =>
          RTMapOp.Push (v.stack, x + ADRSIZE (AP), -1);
          x := LOOPHOLE (x, AP)^;

      | Op.Return =>
          DEC (v.stack.top);
          x := v.stack.data [v.stack.top].pc;

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

      ELSE
          <*ASSERT FALSE*>
      END;
    END;
  END Walk;

BEGIN
END RTHeapMap.

