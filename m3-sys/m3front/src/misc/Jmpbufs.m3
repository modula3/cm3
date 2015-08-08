(* File: Jmpbufs.m3 *)

MODULE Jmpbufs;

IMPORT CG, Module, Target, M3ID, Fmt, Error;

PROCEDURE DebugPrint (t: TEXT) =
BEGIN
    (* Error.Warn (2, t); *)
END DebugPrint;

PROCEDURE CheckProcPush (VAR cs: CheckState; name: M3ID.T): Proc =
VAR proc := NEW (Proc);
    save: REF CheckState := NIL;
BEGIN
  DebugPrint ("CheckProcPush " & IdToText(name));
  IF cs # CheckState { } THEN
    save := NEW (REF CheckState);
    save^ := cs;
  END;
  cs.save := save;
  cs.proc := proc;
  cs.try_count := 0;
  proc.name := name;
  RETURN proc;
END CheckProcPush;

PROCEDURE CheckProcPop (VAR cs: CheckState; proc: Proc) =
BEGIN
  DebugPrint ("CheckProcPop 1 " & IdToText(proc.name) & " try_count:" & Fmt.Int(cs.try_count));
  proc.try_count := cs.try_count;
  IF cs.save # NIL THEN
    cs := cs.save^;
  ELSE
    cs := CheckState { };
  END;
  IF cs.proc # NIL THEN
    DebugPrint ("CheckProcPop 2 " & IdToText(cs.proc.name) & " try_count:" & Fmt.Int(cs.try_count));
  END;
END CheckProcPop;

PROCEDURE CheckTry (VAR cs: CheckState; VAR try: Try) =
BEGIN
  DebugPrint ("CheckTry " & IdToText(cs.proc.name) & " try_index:"
    & Fmt.Int(cs.try_count));
  try.proc := cs.proc;
  try.try_index:= cs.try_count;
  INC(cs.try_count);
END CheckTry;

PROCEDURE CompileTryGetJmpbuf (VAR try: Try): CG.Var =
VAR try_index := try.try_index;
BEGIN
  DebugPrint("CompileTryGetJmpbuf " & IdToText(try.proc.name));
  IF try.proc = NIL THEN
    Error.Msg ("CompileTryGetJmpbuf try.proc = NIL");
  END;
  <* ASSERT try.proc # NIL *>
  IF try.proc.jmpbufs = NIL THEN
    Error.Msg ("CompileTryGetJmpbuf try.proc.jmpbufs = NIL");
  END;
  <* ASSERT try.proc.jmpbufs # NIL *>
  IF try_index >= NUMBER(try.proc.jmpbufs^) THEN
    Error.Msg ("CompileTryGetJmpbuf try_index:"
      & Fmt.Int(try_index)
      & " out of bounds:"
      & Fmt.Int(NUMBER(try.proc.jmpbufs^)));
    END;
    <* ASSERT try_index < NUMBER(try.proc.jmpbufs^) *>
  RETURN try.proc.jmpbufs[try.try_index];
END CompileTryGetJmpbuf;

PROCEDURE IdToText (id: M3ID.T): TEXT =
VAR text := M3ID.ToText (id);
BEGIN
  IF text = NIL THEN text := "" END;
  RETURN text;
END IdToText;

PROCEDURE CompileProcAllocateJmpbufs (p: Proc) =
VAR module: Module.T;
    alloca: CG.Proc;
    size: CG.Var;
    try_count: INTEGER;
BEGIN
    IF p = NIL THEN RETURN END;
    module := Module.Current ();
    alloca := Module.GetAlloca (module);
    size := Module.GetJmpbufSize (module);
    try_count := p.try_count;
    <* ASSERT try_count >= 0 *>
    CG.Comment(-1, FALSE, "AllocateJmpbufs "
               & IdToText(p.name)
               & " try_count:"
               & Fmt.Int(try_count));
    IF try_count = 0 THEN
      RETURN;
    END;
    
    (* We are going to alloca(size * n), where n
       is the number of TRYs; then each TRY will
       use its jmp_buf. As the jmp_buf size is not
       a constant here, multiplication n * jmp_buf
       is not efficient. Therefore, use addition
       to precompute the n pointers (TRY can occur
       in a loop -- making it more valuable to store
       the individual pointers.) *)

    p.jmpbufs := NEW(REF ARRAY OF CG.Var, try_count);
    FOR i := 0 TO try_count - 1 DO
      p.jmpbufs[i] := CG.Declare_local (M3ID.NoID,
                                        Target.Address.size,
                                        Target.Address.align,
                                        CG.Type.Addr,
                                        0,
                                        in_memory := FALSE,
                                        up_level := FALSE,
                                        f := CG.Likely);
    END;

    (* If referencing the size more than once, store
       the global in a local. *)

    IF try_count > 1 THEN
      CG.Load_int (Target.Word.cg_type, size);
      size := CG.Declare_temp (Target.Address.size,
                               Target.Address.align,
                               Target.Word.cg_type,
                               in_memory := FALSE);
      CG.Store_int (Target.Word.cg_type, size);
    END;

    (* jmpbufs[0] = alloca (size * try_count)
       unless try_count == 1 then
         jmpbufs[0] = alloca (size) *)

    CG.Start_call_direct (alloca, 0, Target.Address.cg_type);
    CG.Load_int (Target.Word.cg_type, size);
    IF try_count # 1 THEN
      CG.Load_intt (try_count);
      CG.Multiply (Target.Word.cg_type);
    ELSE
      (* x * 1 == x *)
    END;
    CG.Pop_param (Target.Word.cg_type);
    CG.Call_direct (alloca, Target.Address.cg_type);

    CG.Store_addr (p.jmpbufs[0]);
    
    (* loop: jmpbufs[n] = jmpbufs[n - 1] + size *)

    IF try_count > 1 THEN
      FOR i := 1 TO try_count - 1 DO
        CG.Load_addr (p.jmpbufs[i - 1]);
        CG.Loophole (Target.Address.cg_type, Target.Word.cg_type);
        CG.Load_int (Target.Word.cg_type, size);
        CG.Add (Target.Word.cg_type);
        CG.Loophole (Target.Word.cg_type, Target.Address.cg_type);
        CG.Store_addr (p.jmpbufs[i]);
      END;
    END;
END CompileProcAllocateJmpbufs;

BEGIN
END Jmpbufs.
