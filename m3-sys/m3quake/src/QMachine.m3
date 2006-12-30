(* Copyright (C) 1995, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Feb 28 15:59:51 PST 1995 by kalsow     *)

MODULE QMachine;

IMPORT Atom, AtomList, IntRefTbl, Env, Fmt, Text, FileWr;
IMPORT Wr, Thread, Stdio, OSError, TextSeq;
IMPORT Pathname, Process, File, FS, RTParams;
IMPORT M3Buf, M3File, M3Process, CoffTime;
IMPORT QIdent, QValue, QVal, QCode, QCompiler, QVTbl, QVSeq, QScanner;
FROM Quake IMPORT Error, ID, IDMap, NoID;
(* IMPORT IO; *)

CONST
  OnUnix = (CoffTime.EpochAdjust = 0.0d0);

TYPE
  QK = QValue.Kind;
  Op = QCode.Op;

REVEAL
  T = T_ BRANDED "QMachine.T" OBJECT
    reg       : Registers;
    scopes    : ScopeStack     := NIL;
    stack     : ValueStack     := NIL;
    loops     : LoopStack      := NIL;
    output    : OutputStack    := NIL;
    frames    : FrameStack     := NIL;
    includes  : IncludeStack   := NIL;
    globals   : IntRefTbl.T    := NIL;  (* ID -> QValue.Binding *)
    tmp_files : TextSeq.T      := NIL;
    tracing   : BOOLEAN        := FALSE;
    do_echo   : BOOLEAN        := FALSE;
    last_cp   : QCode.Stream   := NIL;
    bindings  : QValue.Binding := NIL;
    buffers   : BufStack;
    default_wr: Wr.T;
    shell     : TEXT         := NIL;
    sh_option : TEXT         := NIL;
    tmp_dir   : TEXT         := NIL;
  OVERRIDES
    init      := Init;
    evaluate  := Evaluate;
    get       := Get;
    put       := Put;
    lookup    := LookUp;
    push      := Push;
    pop       := Pop;
    error     := Err;
    cleanup   := CleanUp;
    include   := Include;
    normalize := Normalize;
    start_call:= StartCall;
    call_proc := CallProc;
    cp_if     := CopyIfNew;
    make_dir  := MakeDir;
    cur_file  := CurFile;
    cur_path  := CurPath;
    cur_wr    := CurWr;
    set_wr    := SetWr;
    exec_echo := ExecEcho;
  END;

TYPE
  Registers = RECORD
    cp : QCode.Stream   := NIL; (* code pointer *)
    pc : INTEGER        := 0;   (* program counter *)
    xp : INTEGER        := 0;   (* scope stack pointer *)
    sp : INTEGER        := 0;   (* value stack pointer *)
    lp : INTEGER        := 0;   (* loop stack pointer *)
    op : INTEGER        := 0;   (* output stack pointer *)
    fp : INTEGER        := 0;   (* frame pointer *)
    ln : INTEGER        := 0;   (* line number *)
    ip : INTEGER        := 0;   (* include stack pointer *)
    pi : QCode.ProcInfo := NIL; (* procedure info *)
    fn : BOOLEAN        := FALSE; (* => expect return result *)
  END;

TYPE
  ScopeStack   = REF ARRAY OF QValue.Scope;
  ValueStack   = REF ARRAY OF QValue.T;
  LoopStack    = REF ARRAY OF LoopInfo;
  OutputStack  = REF ARRAY OF OutputInfo;
  FrameStack   = REF ARRAY OF FrameInfo;
  IncludeStack = REF ARRAY OF IncludeInfo;

TYPE
  LoopInfo = RECORD
    iter     : QVTbl.Iterator  := NIL;
    array    : QVSeq.T         := NIL;
    next_elt : INTEGER         := 0;
    variable : QValue.Binding  := NIL;
  END;

TYPE
  OutputInfo = RECORD
    name : TEXT := NIL;
    wr   : Wr.T := NIL;
  END;

TYPE
  FrameInfo = RECORD
    proc   : QValue.Proc := NIL;
    saved  : Registers;
    outer  : BOOLEAN;  (* TRUE => exit eval loop when the frame is popped *)
  END;

TYPE
  IncludeInfo = RECORD
    file   : QCode.Stream;
    old_cp : QCode.Stream;
    old_pc : INTEGER;
  END;

TYPE
  BufStack = RECORD
    tos  : INTEGER := 0;
    bufs : ARRAY [0..9] OF M3Buf.T;
  END;

(*-------------------------------------------------------- initialization ---*)

PROCEDURE Init (t: T;  map: IDMap): T =
  BEGIN
    t.map        := map;
    t.scopes     := NEW (ScopeStack,  40);
    t.stack      := NEW (ValueStack,  100);
    t.loops      := NEW (LoopStack,   20);
    t.output     := NEW (OutputStack, 10);
    t.frames     := NEW (FrameStack,  40);
    t.includes   := NEW (IncludeStack, 10);
    t.globals    := NEW (IntRefTbl.Default).init ();
    t.default_wr := Stdio.stdout;

    InitOSEnv (t);
    InitBuiltins (t);

    EVAL PushScope (t);  (* so that "local" variables have a place to go *)
    RETURN t;
  END Init;

(*------------------------------------------------------------ evaluation ---*)

PROCEDURE Evaluate (t: T;  s: QCode.Stream)
  RAISES {Error, Thread.Alerted} =
  BEGIN
    PushInclude (t, s, t.reg);
    Eval (t);
  END Evaluate;

PROCEDURE Eval (t: T)
  RAISES {Error, Thread.Alerted} =
  VAR
    op   : QCode.Op;
    arg  : INTEGER;
    val  : QValue.T;
    val2 : QValue.T;
    arr  : QVSeq.T;
    tbl  : QVTbl.T;
    int  : INTEGER;
    bind : QValue.Binding;
    txt  : TEXT;
    buf  : M3Buf.T;
    done : BOOLEAN;
  BEGIN
    LOOP
      IF (t.tracing) THEN TraceInstruction (t) END;
      WITH z = t.reg.cp.instrs [t.reg.pc] DO op := z.op;  arg := z.a; END;
      INC (t.reg.pc);

      CASE op OF

      | Op.Integer =>
          val.kind := QK.Integer;
          val.int  := arg;
          val.ref  := NIL;
          Push (t, val);

      | Op.String =>
          val.kind := QK.String;
          val.int  := arg;
          val.ref  := NIL;
          Push (t, val);

      | Op.BuildArray =>
          arr := NEW (QVSeq.T).init (arg);
          FOR i := 0 TO arg-1 DO  Pop (t, val);  arr.addlo (val);  END;
          val.kind := QK.Array;
          val.int  := 0;
          val.ref  := arr;
          Push (t, val);
          arr := NIL;
          val.ref := NIL;

      | Op.BuildTable =>
          tbl := NEW (QVTbl.Default).init();
          FOR i := arg-1 TO 0 BY -2 DO
            Pop (t, val2);
            Pop (t, val);
            EVAL tbl.put (QVal.ToID (t, val), val2);
          END;
          val.kind := QK.Table;
          val.int  := 0;
          val.ref  := tbl;
          Push (t, val);
          tbl := NIL;
          val.ref := NIL;
          val2.ref := NIL;

      | Op.GetEnv =>
          PushText (t, Env.Get (t.map.id2txt (arg)));

      | Op.PushProc =>
          val.kind := QK.Proc;
          val.int  := 0;
          val.ref  := NEW (QValue.Proc, info := t.reg.cp.procs [arg],
                           env  := t.scopes [0]);
                         (*env  := t.scopes [t.reg.xp-1]);*)
          (* In quake all procedures are global, and we don't want
             dynamic scoping... *)
          Push (t, val);
          val.ref := NIL;

      | Op.IsMember =>
          Pop (t, val);  int := QVal.ToID (t, val);
          Pop (t, val);  tbl := QVal.ToTable (t, val);
          PushBool (t, tbl.get (int, val));
          tbl := NIL;
          val.ref := NIL;

      | Op.Concat =>
          Pop (t, val2);
          Pop (t, val);
          buf := GetBuf (t);
          QVal.ToBuf (t, val, buf);
          QVal.ToBuf (t, val2, buf);
          PushText (t, M3Buf.ToText (buf));
          FreeBuf (t, buf);
          buf := NIL;
          val2.ref := NIL;
          val.ref := NIL;

      | Op.And =>
          Pop (t, val2);
          Pop (t, val);
          PushBool (t, QVal.ToBool (t, val) AND QVal.ToBool (t, val2));
          val2.ref := NIL;
          val.ref := NIL;

      | Op.Or =>
          Pop (t, val2);
          Pop (t, val);
          PushBool (t, QVal.ToBool (t, val) OR QVal.ToBool (t, val2));
          val2.ref := NIL;
          val.ref := NIL;

      | Op.Not =>
          Pop (t, val);
          PushBool (t, NOT QVal.ToBool (t, val));
          val.ref := NIL;
          
      | Op.IndexTable =>
          Pop (t, val);  int := QVal.ToID (t, val);
          Pop (t, val);  tbl := QVal.ToTable (t, val);
          IF NOT tbl.get (int, val) THEN
            Err (t, Fmt.F ("table does not contain entry for: \"%s\"",
                           t.map.id2txt (int)));
          END;
          Push (t, val);
          tbl := NIL;
          val.ref := NIL;

      | Op.SubscriptArray =>
          Pop (t, val);  int := QVal.ToInt (t, val);
          Pop (t, val);  arr := QVal.ToArray (t, val);
          IF (int < 0) OR (arr.size() <= int) THEN
            Err (t, "array subscript out of bounds: " & Fmt.Int (int));
          END;
          Push (t, arr.get(int));
          arr := NIL;
          val.ref := NIL;

      | Op.InitForeach =>
          Pop (t, val);
          PushLoop (t, arg, val);
          val.ref := NIL;

      | Op.NextForeach =>
          IF NOT IterateLoop (t) THEN
            PopLoop (t);
            INC (t.reg.pc, arg);
          END;
        
      | Op.Goto =>
          INC (t.reg.pc, arg);

      | Op.IfFalse =>
          Pop (t, val);
          IF NOT QVal.ToBool (t, val) THEN INC (t.reg.pc, arg); END;
          val.ref := NIL;

      | Op.Halt =>
          PopInclude (t);
          (**
          IF (t.reg.ip <= 0) THEN EXIT; END;
          **)
          EXIT;

      | Op.PushScope =>
          EVAL PushScope (t);

      | Op.PopScope =>
          PopScope (t);

      | Op.DefineG =>
          bind := DefineGlobal (t, arg, readonly := FALSE);
          Pop (t, bind.value);

      | Op.DefineGR =>
          bind := DefineGlobal (t, arg, readonly := TRUE);
          Pop (t, bind.value);

      | Op.DefineL =>
          bind := Define (t, arg, readonly := FALSE);
          Pop (t, bind.value);

      | Op.DefineLR =>
          bind := Define (t, arg, readonly := TRUE);
          Pop (t, bind.value);

      | Op.LoadVar =>
          bind := LookUp (t, arg);
          IF (bind # NIL) THEN
            Push (t, bind.value);
          ELSIF strict_variables THEN
            Err (t, "undefined variable: " & t.map.id2txt (arg));
          ELSE
            PushText (t, t.map.id2txt (arg));
          END;

      | Op.Assign =>
          bind := LookUp (t, arg);
          IF (bind = NIL) THEN
            bind := DefineGlobal (t, arg, readonly := FALSE);
          ELSIF bind.readonly THEN
            Err (t, "cannot assign to readonly variable: " & t.map.id2txt(arg));
          END;
          Pop (t, val);
          bind.value := val;

      | Op.AssignTable =>
          Pop (t, val);
          Pop (t, val2);  int := QVal.ToID (t, val2);
          Pop (t, val2);  tbl := QVal.ToTable (t, val2);
          EVAL tbl.put (int, val);
          tbl := NIL;
          val.ref := NIL;
          val2.ref := NIL;

      | Op.AssignArray =>
          Pop (t, val);
          Pop (t, val2);  int := QVal.ToInt (t, val2);
          Pop (t, val2);  arr := QVal.ToArray (t, val2);
          IF (int < 0) OR (arr.size() <= int) THEN
            Err (t, "array subscript out of bounds: " & Fmt.Int (int));
          END;
          arr.put (int, val);
          arr := NIL;
          val.ref := NIL;
          val2.ref := NIL;

      | Op.Append =>
          Pop (t, val);
          Pop (t, val2);  arr := QVal.ToArray (t, val2);
          arr.addhi (val);
          arr := NIL;
          val.ref := NIL;
          val2.ref := NIL;

      | Op.StartRedirect =>
          Pop (t, val);  txt := QVal.ToText (t, val);
          PushOutput (t, txt, append := FALSE);
          txt := NIL;
          val.ref := NIL;

      | Op.StartAppendRedirect =>
          Pop (t, val);  txt := QVal.ToText (t, val);
          PushOutput (t, txt, append := TRUE);
          txt := NIL;
          val.ref := NIL;

      | Op.EndRedirect =>
          PopOutput (t);

      | Op.StartCall =>
          PushFrame (t);

      | Op.CallProc =>
          DoCall (t, arg, FALSE, FALSE);

      | Op.CallFunc =>
          DoCall (t, arg, TRUE, FALSE);

      | Op.SetLine =>
          t.reg.ln := arg;

      | Op.ReturnValue =>
          CheckReturn (t, TRUE);
          Pop (t, val);
          done := PopFrame (t);
          Push (t, val);
          IF done THEN EXIT; END;

      | Op.Return =>
          CheckReturn (t, FALSE);
          IF PopFrame (t) THEN EXIT END;

      END; (* case *)
    END; (* loop *)
  END Eval;

PROCEDURE TraceInstruction (t: T)
  RAISES {Thread.Alerted} =
  BEGIN
    TRY
      PrintTrace (t);
    EXCEPT Wr.Failure =>
      t.tracing := FALSE;
    END;
  END TraceInstruction;

PROCEDURE PrintTrace (t: T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR op: QCode.Op;  arg: INTEGER;
  BEGIN
    IF (t.last_cp # t.reg.cp) THEN
      Print ("****** ");
      IF (t.reg.cp # NIL) THEN
        Print (t.map.id2txt (t.reg.cp.source_file));
      END;
      Print (" ******", Wr.EOL);
      t.last_cp := t.reg.cp;
    END;

    WITH z = t.reg.cp.instrs [t.reg.pc] DO op := z.op;  arg := z.a; END;

    FOR i := 1 TO t.reg.xp DO  Print ("."); END;
    Print (Fmt.Pad (Fmt.Int (t.reg.pc),4,' ',Fmt.Align.Left));
    Print (" ", QCode.OpName[op]);
    CASE QCode.OpFormat [op] OF
    | 0 => (*done*)
    | 1 => Print ("  ", Fmt.Int (arg));
    | 2 => Print ("  (", Fmt.Int (arg), ") \"");
           Print (t.map.id2txt (arg), "\"");
    | 3 => Print ("  pc+(", Fmt.Int (arg), ") => ",
                  Fmt.Int (t.reg.pc + 1 + arg));
    END;
    Print (Wr.EOL);
    FlushIO ();
  END PrintTrace;

(*------------------------------------------------------- procedure calls ---*)

PROCEDURE PushFrame (t: T)
  RAISES {Error} =
  VAR val: QValue.T;
  BEGIN
    Pop (t, val);  (* the procedure value *)
    StartCall (t, val);
  END PushFrame;

PROCEDURE StartCall (t: T;  READONLY proc: QValue.T)
  RAISES {Error} =
  BEGIN
    IF (t.reg.fp >= NUMBER (t.frames^)) THEN ExpandFrames (t); END;
    WITH f = t.frames[t.reg.fp] DO
      f.proc  := QVal.ToProc (t, proc);
      f.saved := t.reg;
    END;
    INC (t.reg.fp);
  END StartCall;

PROCEDURE ExpandFrames (t: T) =
  VAR n := NUMBER (t.frames^);  new := NEW (FrameStack, n+n);
  BEGIN
    SUBARRAY (new^, 0, n) := t.frames^;
    t.frames := new;
  END ExpandFrames;

PROCEDURE CallProc (t: T;  n_args: INTEGER;  isFunc: BOOLEAN)
  RAISES {Error, Thread.Alerted} =
  BEGIN
    DoCall (t, n_args, isFunc, TRUE);
    Eval (t);
  END CallProc;

PROCEDURE DoCall (t: T;  n_args: INTEGER;  isFunc, outer: BOOLEAN)
  RAISES {Error, Thread.Alerted} =
  VAR p: QValue.Proc;  s: QValue.Scope;  val: QValue.T;
  BEGIN
    WITH f = t.frames[t.reg.fp-1] DO
      f.outer := outer;
      p := f.proc;
      IF (p.info.n_args # n_args) AND (p.info.n_args >= 0) THEN
        Err (t, Fmt.F ("%s to procedure %s (expected %s, received %s)",
                       "wrong number of parameters passed",
                       t.map.id2txt (p.info.name),
                       Fmt.Int (p.info.n_args), Fmt.Int (n_args)));
      END;
      IF (p.info.builtin) THEN
        (* we save and restore the registers in case a builtin
           procedure is on the stack when an error is raised *)
        f.saved.pi := t.reg.pi;
        f.saved.pc := t.reg.pc;
        f.saved.cp := t.reg.cp;
        f.saved.ln := t.reg.ln;
        f.saved.fn := t.reg.fn;
        t.reg.pi := p.info;
        t.reg.pc := 0;
        t.reg.cp := NIL;
        t.reg.ln := 0;
        t.reg.fn := isFunc;
        p.info.handler (t, n_args);
        IF (p.info.handler = DoInclude) THEN
          (* the builtin include() function pops its own frame! *)
        ELSE
          CheckReturn (t, p.info.isFunc);
          IF p.info.isFunc THEN
            Pop (t, val);
            EVAL PopFrame (t);
            Push (t, val);
          ELSE
            EVAL PopFrame (t);
          END;
        END;
      ELSE
        f.saved.pi := t.reg.pi;
        f.saved.pc := t.reg.pc;
        f.saved.cp := t.reg.cp;
        f.saved.ln := t.reg.ln;
        t.reg.pi := p.info;
        t.reg.pc := p.info.entry;
        t.reg.cp := p.info.code;
        t.reg.fn := isFunc;
        s := PushScope (t);
        s.parent := p.env;  (* use procedure's static link *)
        (* scope debugging
        VAR m := "\n"; BEGIN
          FOR i := 0 TO t.reg.xp -1 DO
            m := m & " s: " & Fmt.Int(t.scopes[i].id);
            IF t.scopes[i].parent = NIL THEN
              m := m & " p: NIL\n";
            ELSE
              m := m & " p: " & Fmt.Int(t.scopes[i].parent.id) & "\n";
            END;
          END;
          IO.Put("scopes (DoCall):" & m);
        END;
        *)
        <*ASSERT s.parent # s*>
      END;
    END;
  END DoCall;

PROCEDURE PopFrame (t: T): BOOLEAN
  RAISES {Error, Thread.Alerted} =
  VAR val: QValue.T;
  BEGIN
    DEC (t.reg.fp);
    WITH f = t.frames[t.reg.fp] DO
      f.proc := NIL;
      WHILE (t.reg.ip > f.saved.ip) DO PopInclude (t); END;
      t.reg.pi := f.saved.pi;  f.saved.pi := NIL;
      t.reg.cp := f.saved.cp;  f.saved.cp := NIL;
      t.reg.pc := f.saved.pc;
      t.reg.ln := f.saved.ln;
      t.reg.fn := f.saved.fn;
      WHILE (t.reg.xp > f.saved.xp) DO PopScope (t);   END;
      WHILE (t.reg.sp > f.saved.sp) DO Pop (t, val);   END;
      WHILE (t.reg.lp > f.saved.lp) DO PopLoop (t);    END;
      WHILE (t.reg.op > f.saved.op) DO PopOutput (t);  END;
      RETURN f.outer;
    END;
  END PopFrame;

PROCEDURE CheckReturn (t: T;  with_value: BOOLEAN)
  RAISES {Error} =
  BEGIN
    IF (t.reg.fp < 1) THEN
      Err (t, "return not in a function or procedure");
    END;
    IF (t.reg.fn = with_value) THEN
      (* ok *)
    ELSIF (t.reg.fn) THEN
      Err (t, "expected return value is missing");
    ELSE
      Err (t, "unexpected return value");
    END;
  END CheckReturn;

(*------------------------------------------------------- global bindings ---*)

PROCEDURE Get (t: T;  name: ID;  VAR(*OUT*) value: QValue.T): BOOLEAN =
  VAR ref: REFANY;
  BEGIN
    IF t.globals.get (name, ref) THEN
      value := NARROW (ref, QValue.Binding).value;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END Get;

PROCEDURE Put (t: T;  name: ID;  READONLY value: QValue.T)
  RAISES {Error} =
  VAR bind := DefineGlobal (t, name, readonly := FALSE);
  BEGIN
    bind.value := value;
  END Put;

(*----------------------------------------------- scopes & local bindings ---*)

PROCEDURE PushScope (t: T): QValue.Scope =
  VAR m := "\n";
  BEGIN
    IF (t.reg.xp >= NUMBER (t.scopes^)) THEN ExpandScopes (t); END;
    WITH s = t.scopes [t.reg.xp] DO
      IF (s = NIL) THEN 
        s := NEW (QValue.Scope); 
        s.id := nextScopeId;
        INC (nextScopeId);
      END;
      IF (t.reg.xp > 0)
        THEN s.parent := t.scopes[t.reg.xp-1];
        ELSE s.parent := NIL;
      END;
      <*ASSERT s.parent # s*>
      (* scope debugging
      FOR i := 0 TO t.reg.xp DO
        m := m & " s: " & Fmt.Int(t.scopes[i].id);
        IF t.scopes[i].parent = NIL THEN
          m := m & " p: NIL\n";
        ELSE
          m := m & " p: " & Fmt.Int(t.scopes[i].parent.id) & "\n";
        END;
      END;
      IO.Put("scopes:" & m);
      *)
      IF s.parent # NIL THEN 
        IF s.parent.parent = s THEN
          TRY
            Err (t, "loop in scopes, t.reg.xp = " & Fmt.Int(t.reg.xp) & m);
          EXCEPT
            Error(e) => 
            TRY
              Wr.PutText(Stdio.stderr, e);
            EXCEPT ELSE END;
          END;
          s.parent.parent := NIL;
        END;
      END;
      INC (t.reg.xp);
      RETURN s;
    END;
  END PushScope;

VAR nextScopeId := 0;

PROCEDURE ExpandScopes (t: T) =
  VAR n := NUMBER (t.scopes^);  new := NEW (ScopeStack, n+n);
  BEGIN
    SUBARRAY (new^, 0, n) := t.scopes^;
    t.scopes := new;
  END ExpandScopes;

PROCEDURE PopScope (t: T) =
  VAR b, last_b: QValue.Binding;
  BEGIN
    DEC (t.reg.xp);
    WITH s = t.scopes [t.reg.xp] DO
      b := s.bindings;
      IF (b # NIL) THEN
        (* recycle the bindings *)
        WHILE (b # NIL) DO
          b.readonly  := FALSE;
          b.name      := NoID;
          b.value.ref := NIL;
          last_b := b;
          b := b.next;
        END;
        last_b.next := t.bindings;
        t.bindings := s.bindings;
        s.bindings := NIL;
      END;
      s.parent := NIL;
    END;
  END PopScope;

PROCEDURE Define (t: T;  id: ID;  readonly: BOOLEAN): QValue.Binding
  RAISES {Error} =
  VAR old, new: QValue.Binding;
  BEGIN
    (* IO.Put("Define(" & Fmt.Int(id) & ")\n"); *)
    WITH s = t.scopes [t.reg.xp-1] DO
      old := s.bindings;
      new := NewBinding (t);
      new.next     := old;
      new.name     := id;
      new.readonly := readonly;
      WHILE (old # NIL) DO
        IF (old.name = id) THEN
          Err (t, "duplicate symbol defined: " & t.map.id2txt (id));
        END;
        old := old.next;
      END;
      s.bindings := new;
    END;
    RETURN new;
  END Define;

PROCEDURE DefineGlobal (t: T;  id: ID;  readonly: BOOLEAN): QValue.Binding
  RAISES {Error} =
  VAR ref: REFANY;  bind: QValue.Binding;
  BEGIN
    IF t.globals.get (id, ref) THEN
      bind := ref;
    ELSE
      bind := NewBinding (t);
      bind.name := id;
      bind.readonly := FALSE;
      EVAL t.globals.put (id, bind);
    END;
    IF (bind.readonly) THEN
      Err (t, "cannot redefine readonly global symbol: " & t.map.id2txt (id));
    END;
    bind.readonly := readonly;
    RETURN bind;
  END DefineGlobal;

PROCEDURE LookUp (t: T;  id: ID): QValue.Binding =
  VAR s: QValue.Scope;  b: QValue.Binding;  ref: REFANY;
  BEGIN
    (* IO.Put("LookUp(" & Fmt.Int(id) & ")\n"); *)
    (* try the local scopes first *)
    IF (t.reg.xp > 0) THEN
      s := t.scopes [t.reg.xp-1];
      WHILE (s # NIL) DO
        b := s.bindings;
        WHILE (b # NIL) DO
          IF (b.name = id) THEN RETURN b; END;
          b := b.next;
        END;
        s := s.parent;
      END;
    END;
    
    (* finally try the globals *)
    IF t.globals.get (id, ref)
      THEN RETURN ref;
      ELSE RETURN NIL;
    END;
  END LookUp;

PROCEDURE NewBinding (t: T): QValue.Binding =
  VAR b := t.bindings;
  BEGIN
    IF (b # NIL) THEN
      t.bindings := b.next;
      b.next := NIL;
    ELSE
      b := NEW (QValue.Binding);
    END;
    RETURN b;
  END NewBinding;

(*------------------------------------------------------------ data stack ---*)

PROCEDURE Push (t: T;  READONLY value: QValue.T) =
  BEGIN
    IF (t.reg.sp >= NUMBER (t.stack^)) THEN ExpandStack (t); END;
    t.stack [t.reg.sp] := value;
    INC (t.reg.sp);
  END Push;

PROCEDURE ExpandStack (t: T) =
  VAR n := NUMBER (t.stack^);  new := NEW (ValueStack, n+n);
  BEGIN
    SUBARRAY (new^, 0, n) := t.stack^;
    t.stack := new;
  END ExpandStack;

PROCEDURE Pop (t: T;  VAR(*OUT*) value: QValue.T) RAISES {Error} =
  BEGIN
    IF (t.reg.sp <= 0) THEN Err (t, "empty stack"); END;
    DEC (t.reg.sp);
    WITH z = t.stack [t.reg.sp] DO
      value := z;
      z.ref := NIL;
    END;
  END Pop;

PROCEDURE PushText (t: T;  s: TEXT) =
  VAR v: QValue.T;
  BEGIN
    IF (s = NIL) THEN s := ""; END;
    v.kind := QK.String;
    v.int  := t.map.txt2id (s);
    v.ref  := NIL;
    Push (t, v);
  END PushText;

PROCEDURE PushBool (t: T;  b: BOOLEAN) =
  VAR v: QValue.T;
  BEGIN
    v.kind := QK.String;
    v.int  := t.map.boolean [b];
    v.ref  := NIL;
    Push (t, v);
  END PushBool;

PROCEDURE PushInt (t: T;  i: INTEGER) =
  VAR v: QValue.T;
  BEGIN
    v.kind := QK.Integer;
    v.int  := i;
    v.ref  := NIL;
    Push (t, v);
  END PushInt; 

PROCEDURE PushID (t: T;  nm: ID) =
  VAR v: QValue.T;
  BEGIN
    v.kind := QK.String;
    v.int  := nm;
    v.ref  := NIL;
    Push (t, v);
  END PushID; 

PROCEDURE PopText (t: T): TEXT  RAISES {Error} =
  VAR v: QValue.T;
  BEGIN
    Pop (t, v);
    RETURN QVal.ToText (t, v);
  END PopText;

PROCEDURE PopBool (t: T): BOOLEAN  RAISES {Error} =
  VAR v: QValue.T;
  BEGIN
    Pop (t, v);
    RETURN QVal.ToBool (t, v);
  END PopBool;

PROCEDURE PopInt (t: T): INTEGER  RAISES {Error} =
  VAR v: QValue.T;
  BEGIN
    Pop (t, v);
    RETURN QVal.ToInt (t, v);
  END PopInt; 

PROCEDURE PopID (t: T): ID  RAISES {Error} =
  VAR v: QValue.T;
  BEGIN
    Pop (t, v);
    RETURN QVal.ToID (t, v);
  END PopID; 

(*---------------------------------------------------------- output stack ---*)

PROCEDURE PushOutput (t: T;  nm: TEXT;  append: BOOLEAN)
  RAISES {Error} =
  BEGIN
    IF (t.reg.op >= NUMBER (t.output^)) THEN ExpandOutput (t); END;
    WITH o = t.output [t.reg.op] DO
      o.name := nm;
      TRY
        IF (append)
          THEN o.wr := FileWr.OpenAppend (nm);
          ELSE o.wr := FileWr.Open (nm);
        END;
      EXCEPT OSError.E(ec) =>
        Err (t, Fmt.F ("unable to open \"%s\" for writing%s", nm, OSErr(ec)));
      END;
    END;
    INC (t.reg.op);
  END PushOutput;

PROCEDURE ExpandOutput (t: T) =
  VAR n := NUMBER (t.output^);  new := NEW (OutputStack, n+n);
  BEGIN
    SUBARRAY (new^, 0, n) := t.output^;
    t.output := new;
  END ExpandOutput;

PROCEDURE PopOutput (t: T)
  RAISES {Error, Thread.Alerted} =
  BEGIN
    DEC (t.reg.op);
    WITH o = t.output [t.reg.op] DO
      TRY
        Wr.Close (o.wr);
        o.wr := NIL;
        o.name := NIL;
      EXCEPT Wr.Failure(ec) =>
        Err (t, Fmt.F ("unable to close \"%s\"%s", o.name, OSErr(ec)));
      END;
    END;
  END PopOutput;

PROCEDURE CurWr (t: T): Wr.T =
  BEGIN
    IF (t.reg.op <= 0)
      THEN RETURN t.default_wr;
      ELSE RETURN t.output [t.reg.op-1].wr;
    END;
  END CurWr;

PROCEDURE SetWr (t: T;  wr: Wr.T) =
  BEGIN
    IF (t.reg.op <= 0)
      THEN t.default_wr := wr;
      ELSE t.output [t.reg.op-1].wr := wr;
    END;
  END SetWr;

(*------------------------------------------------------------ loop stack ---*)

PROCEDURE PushLoop (t: T;  nm: ID;  READONLY elts: QValue.T)
  RAISES {Error} =
  VAR tbl: QVTbl.T;  arr: QVSeq.T;
  BEGIN
    IF (t.reg.lp >= NUMBER (t.loops^)) THEN ExpandLoops (t); END;
    WITH x = t.loops [t.reg.lp] DO
      IF (elts.kind = QK.Table) THEN
        tbl := elts.ref;
        x.iter := tbl.iterate ();
      ELSIF (elts.kind = QK.Array) THEN
        arr := elts.ref;
        x.array := arr;
        x.next_elt := 0;
      ELSE
        Err (t, "\"foreach\" not applied to an array or table");
      END;
      EVAL PushScope (t);
      x.variable := Define (t, nm, readonly := TRUE);
    END;
    INC (t.reg.lp);
  END PushLoop;

PROCEDURE ExpandLoops (t: T) =
  VAR n := NUMBER (t.loops^);  new := NEW (LoopStack, n+n);
  BEGIN
    SUBARRAY (new^, 0, n) := t.loops^;
    t.loops := new;
  END ExpandLoops;

PROCEDURE IterateLoop (t: T): BOOLEAN =
  VAR int: INTEGER;  val: QValue.T;
  BEGIN
    WITH x = t.loops [t.reg.lp-1] DO
      IF (x.iter # NIL) THEN (* we're iterating over a table *)
        IF NOT x.iter.next (int, val) THEN RETURN FALSE; END;
        WITH z = x.variable.value DO
          z.kind := QK.String;
          z.int  := int;
          z.ref  := NIL;
        END;
      ELSE (* we're iterating over an array *)
        IF (x.next_elt >= x.array.size()) THEN RETURN FALSE; END;
        x.variable.value := x.array.get (x.next_elt);
        INC (x.next_elt);
      END;
    END;
    RETURN TRUE;
  END IterateLoop;

PROCEDURE PopLoop (t: T) =
  BEGIN
    PopScope (t);
    DEC (t.reg.lp);
    WITH x = t.loops [t.reg.lp] DO
      x.iter     := NIL;
      x.array    := NIL;
      x.variable := NIL;
    END;
  END PopLoop;

(*--------------------------------------------------------- include stack ---*)

PROCEDURE PushInclude (t: T;  s: QCode.Stream;  VAR reg: Registers) =
  BEGIN
    IF (reg.ip >= NUMBER (t.includes^)) THEN ExpandIncludes (t); END;
    WITH x = t.includes [reg.ip] DO
      x.file   := s;
      x.old_cp := reg.cp;
      x.old_pc := reg.pc;
    END;
    reg.cp := s;
    reg.pc := 0;
    INC (reg.ip);
  END PushInclude;

PROCEDURE ExpandIncludes (t: T) =
  VAR n := NUMBER (t.includes^);  new := NEW (IncludeStack, n+n);
  BEGIN
    SUBARRAY (new^, 0, n) := t.includes^;
    t.includes := new;
  END ExpandIncludes;

PROCEDURE PopInclude (t: T) =
  BEGIN
    DEC (t.reg.ip);
    WITH x = t.includes [t.reg.ip] DO
      t.reg.cp := x.old_cp;
      t.reg.pc := x.old_pc;
      x.file   := NIL;
      x.old_cp := NIL;
    END;
  END PopInclude;

(*----------------------------------------------------- OS dependent goo! ---*)

PROCEDURE InitOSEnv (t: T) =
  BEGIN
    t.shell := GetEnv(NIL, "QUAKE_SHELL");
    t.sh_option := GetEnv(NIL, "QUAKE_SHELL_OPTION");
    t.tmp_dir := GetEnv(NIL, "QUAKE_TMPDIR");
    IF OnUnix THEN
      IF t.shell = NIL THEN t.shell := "/bin/sh" END;
      IF t.sh_option = NIL THEN t.sh_option := "-c" END;
      IF t.tmp_dir = NIL THEN 
        t.tmp_dir := GetEnv ("/tmp", "TMPDIR", "TMP", "TEMP");
      END;
    ELSE
      IF t.shell = NIL THEN t.shell := GetEnv ("COMMAND.COM", "COMSPEC") END;
      IF t.sh_option = NIL THEN t.sh_option := "/c" END;
      IF t.tmp_dir = NIL THEN
        t.tmp_dir   := GetEnv ("C:\\TEMP", "TMPDIR", "TMP", "TEMP");
      END;
    END;
  END InitOSEnv;

PROCEDURE GetEnv (default, v0, v1, v2, v3, v4: TEXT := NIL): TEXT =
  VAR val := Env.Get (v0);
  BEGIN
    IF val = NIL AND v1 # NIL THEN val := Env.Get(v1) END;
    IF val = NIL AND v2 # NIL THEN val := Env.Get(v2) END;
    IF val = NIL AND v3 # NIL THEN val := Env.Get(v3) END;
    IF val = NIL AND v4 # NIL THEN val := Env.Get(v4) END;
    IF val = NIL THEN val := default; END;
    RETURN val;
  END GetEnv;

(*---------------------------------------------------- builtin procedures ---*)

TYPE
  Builtin = RECORD
    name    : TEXT;
    proc    : QCode.BuiltinProc;
    n_args  : INTEGER;
    is_func : BOOLEAN;
  END;

CONST
  Builtins = ARRAY OF Builtin {
    Builtin {"arglist",     DoArgList,   2, TRUE},
    Builtin {"cp_if",       DoCopyIfNew, 2, FALSE},
    Builtin {"defined",     DoDefined,   1, TRUE},
    Builtin {"empty",       DoEmpty,     1, TRUE},
    Builtin {"equal",       DoEqual,     2, TRUE},
    Builtin {"error",       DoError,     1, FALSE},
    Builtin {"escape",      DoEscape,    1, TRUE},
    Builtin {"exec",        DoExec,     -1, FALSE},
    Builtin {"file",        DoFile,      0, TRUE},
    Builtin {"format",      DoFormat,   -1, TRUE},
    Builtin {"include",     DoInclude,   1, FALSE},
    Builtin {"make_dir",    DoMakeDir,   1, FALSE},
    Builtin {"normalize",   DoNormalize, 2, TRUE},
    Builtin {"path",        DoPath,      0, TRUE},
    Builtin {"stale",       DoStale,     2, TRUE},
    Builtin {"try_exec",    DoTryExec,  -1, TRUE},
    Builtin {"unlink_file", DoUnlink,    1, TRUE},
    Builtin {"write",       DoWrite,    -1, FALSE},
    Builtin {"TRACE_INSTR", DoTrace,     0, FALSE}
  };

PROCEDURE InitBuiltins (t: T) =
  VAR b: QValue.Binding;
  BEGIN
    FOR i := FIRST (Builtins) TO LAST (Builtins) DO
      WITH z = Builtins [i] DO
        b := NewBuiltin (t, z.name, z.proc, z.n_args, z.is_func);
        EVAL t.globals.put (b.name, b);
      END;
    END;
    b := NewConst (t, "TRUE", t.map.boolean [TRUE]);
    EVAL t.globals.put (b.name, b);
    b := NewConst (t, "FALSE", t.map.boolean [FALSE]);
    EVAL t.globals.put (b.name, b);
  END InitBuiltins;

PROCEDURE NewBuiltin (t       : T;
                      nm      : TEXT;
                      handler : QCode.BuiltinProc;
                      n_args  : INTEGER;
                      isFunc  : BOOLEAN): QValue.Binding =
  VAR
    id   := t.map.txt2id (nm);
    info := NEW (QCode.ProcInfo, name := id, isFunc := isFunc,
                   n_args := n_args, builtin := TRUE, handler := handler);
    proc := NEW (QValue.Proc, info := info, env := NIL);
    bind := NEW (QValue.Binding, name := info.name, readonly := TRUE);
  BEGIN
    bind.value.kind := QValue.Kind.Proc;
    bind.value.int  := 0;
    bind.value.ref  := proc;
    RETURN bind;
  END NewBuiltin;

PROCEDURE NewConst (t: T;  nm: TEXT;  val: ID): QValue.Binding =
  VAR
    id   := t.map.txt2id (nm);
    bind := NEW (QValue.Binding, name := id, readonly := TRUE);
  BEGIN
    bind.value.kind := QValue.Kind.String;
    bind.value.int  := val;
    bind.value.ref  := NIL;
    RETURN bind;
  END NewConst;

PROCEDURE DoArgList (t: T;  n_args: INTEGER)
  RAISES {Error, Thread.Alerted} =
  CONST
    Max_args       = 10;
    Max_arg_length = 1024;
  VAR
    prefix, args : TEXT;
    split        : TextSeq.T;
    val0, val1   : QValue.T;
    file         : TEXT;
    wr           : Wr.T;
    buf          : M3Buf.T;
  BEGIN
    <*ASSERT n_args = 2 *>
    buf := GetBuf (t);
    Pop (t, val1);
    QVal.ToBuf (t, val1, buf);
    args  := M3Buf.ToText (buf);
    split := SplitArgs (args);
    FreeBuf (t, buf);

    Pop (t, val0);
    prefix := QVal.ToText (t, val0);

    (* check for the easy case *)
    IF (split.size () <= Max_args)
      AND (Text.Length (args) <= Max_arg_length) THEN
      Push (t, val1);
      RETURN;
    END;

    TRY
      file := UniqueTempFile (t);
      wr := FileWr.Open (file);
      TRY
        FOR i := 0 TO split.size()-1  DO
          Wr.PutText (wr, split.get(i));
          Wr.PutText (wr, Wr.EOL);
        END;
      FINALLY
        Wr.Close (wr);
      END;

      PushText (t, prefix & file);
    EXCEPT
    | Wr.Failure(ec) =>
        Err (t, "unable to write on \"" & file & "\"" & OSErr (ec));
    | OSError.E(ec) =>
        Err (t, "unable to write on \"" & file & "\"" & OSErr (ec));
    END;
  END DoArgList;

PROCEDURE DoCopyIfNew (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR val: QValue.T;  src, dest: TEXT;
  BEGIN
    <*ASSERT n_args = 2 *>
    Pop (t, val);  dest := QVal.ToText (t, val);
    Pop (t, val);  src  := QVal.ToText (t, val);
    CopyIfNew (t, src, dest);
  END DoCopyIfNew;

PROCEDURE FileExists(fn : Pathname.T) : BOOLEAN =
  VAR s : File.Status;
  BEGIN
    TRY
      s := FS.Status(fn);
    EXCEPT ELSE
      RETURN FALSE;
    END;
    RETURN TRUE;
  END FileExists;

PROCEDURE CopyIfExists (t: T; Source, Destination: TEXT) RAISES {Error} =
BEGIN
    IF FileExists(Source)
    THEN
        TRY
            M3File.Copy(Source, Destination);
        EXCEPT OSError.E(ec) =>
            Err(t, Fmt.F("unable to copy \"%s\" to \"%s\"%s", Source, Destination, OSErr(ec)));
        END;
    END
END CopyIfExists;

PROCEDURE CopyIfNew (t: T;  src, dest: TEXT) RAISES {Error} =
VAR
    equal := FALSE;
    Extension: TEXT;
    Source: TEXT;
    Destination: TEXT;
BEGIN

    IF M3File.IsDirectory (dest) THEN
        dest := Pathname.Join (dest, Pathname.Last (src), NIL);
    END;

    Source := src;
    Destination := dest;

    IF NOT FileExists(Source) THEN
        Err(t, Fmt.F("unable to copy \"%s\" to \"%s\", \"%s\" does not exist", Source, Destination, Source));
        RETURN;
    END;

    IF FileExists(Destination) THEN
        equal := M3File.IsEqual (Source, Destination);
    END;

    IF NOT equal
    THEN
        TRY
            M3File.Copy(Source, Destination);

            (* Sometimes copy along .pdb and .manifest files.
            Consider: Only Win32, and at a higher level where more decisions can be made.
            This could occur for arbitrary extensions, but should not all.
            In particular, ..exes, .dlls, .libs can all have arbitrary extensions.
            It is the file format that matters, and still the "extra" files are optional.
            *)
            Extension := Pathname.LastExt(src);
            IF (Text.Length(Extension) = 3
                AND ((CIEqual(Extension, "exe")
                    OR CIEqual(Extension, "dll")
                    OR CIEqual(Extension, "lib")
                    )))
            THEN
                Source := (src & ".manifest");
                Destination := (dest & ".manifest"); (* append, not change extension -- foo.exe => foo.exe.manifest, not foo.manifest *)
                CopyIfExists(t, Source, Destination);

                Source := Pathname.ReplaceExt(src, "pdb"); (* change extension -- foo.exe => foo.pdb *)
                Destination := Pathname.ReplaceExt(dest, "pdb");
                CopyIfExists(t, Source, Destination);
            END;
        EXCEPT OSError.E(ec) =>
            Err(t, Fmt.F("unable to copy \"%s\" to \"%s\"%s", Source, Destination, OSErr(ec)));
        END;
    END;
END CopyIfNew;

PROCEDURE DoDefined (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR val: QValue.T;
  BEGIN
    <*ASSERT n_args = 1 *>
    Pop (t, val);
    PushBool (t, LookUp (t, QVal.ToID (t, val)) # NIL);
  END DoDefined;

PROCEDURE DoEmpty (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR empty := FALSE;  val: QValue.T;
  BEGIN
    <*ASSERT n_args = 1 *>
    Pop (t, val);
    CASE val.kind OF
    | QK.Integer => empty := FALSE;
    | QK.String  => empty := (val.int = t.map.boolean[FALSE]);
    | QK.Array   => empty := NARROW (val.ref, QVSeq.T).size() = 0;
    | QK.Table   => empty := NARROW (val.ref, QVTbl.T).size() = 0;
    ELSE
      Err (t, "\"empty\" not applied to a string, table, or array");
    END;
    PushBool (t, empty);
  END DoEmpty;

PROCEDURE DoEqual (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR v1, v2: QValue.T;  eq := FALSE;
  BEGIN
    <*ASSERT n_args = 2 *>
    Pop (t, v1);
    Pop (t, v2);
    IF (v1.kind = v2.kind) THEN
      CASE v1.kind OF
      | QK.Var     => eq := (v1.int = v2.int) AND (v1.ref = v2.ref);
      | QK.Integer => eq := (v1.int = v2.int);
      | QK.String  => eq := (v1.int = v2.int);
      | QK.Table   => eq := (v1.ref = v2.ref);
      | QK.Array   => eq := (v1.ref = v2.ref);
      | QK.Proc    => eq := (v1.ref = v2.ref);
      END;
    END;
    PushBool (t, eq);
  END DoEqual;

PROCEDURE DoError (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR val: QValue.T;
  BEGIN
    <*ASSERT n_args = 1 *>
    Pop (t, val);
    Err (t, QVal.ToText (t, val));
  END DoError;

PROCEDURE DoEscape (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR
    val     : QValue.T;
    txt     : TEXT;
    buf     : M3Buf.T;
    ch      : CHAR;
    len     : INTEGER;
    out_len : INTEGER;
    out_buf : ARRAY [0..199] OF CHAR;
    in_buf  : ARRAY [0..199] OF CHAR;
    new_ch  : BOOLEAN := FALSE;
  BEGIN
    <*ASSERT n_args = 1 *>
    Pop (t, val);  txt := QVal.ToText (t, val);
    len := Text.Length (txt);
    IF (len+len <= NUMBER (out_buf)) THEN
      out_len := 0;
      Text.SetChars (in_buf, txt);
      FOR i := 0 TO len-1 DO
        ch := Text.GetChar (txt, i);
        IF (ch = '\134') THEN
          out_buf[out_len] := ch; INC (out_len);
          new_ch := TRUE;
        END;
        out_buf [out_len] := ch;  INC (out_len);
      END;
      IF (new_ch)
        THEN PushText (t, Text.FromChars (SUBARRAY (out_buf, 0, out_len)));
        ELSE Push (t, val);
      END;
    ELSE
      buf := GetBuf (t);
      FOR i := 0 TO len - 1 DO
        ch := Text.GetChar (txt, i);
        IF (ch = '\134') THEN M3Buf.PutChar (buf, ch); new_ch := TRUE; END;
        M3Buf.PutChar (buf, ch);
      END;
      txt := M3Buf.ToText (buf);
      FreeBuf (t, buf);
      IF (new_ch)
        THEN PushText (t, txt);
        ELSE Push (t, val);
      END;
    END;
  END DoEscape;

PROCEDURE ExecEcho (t: T;  b: BOOLEAN): BOOLEAN =
  VAR old := t.do_echo;
  BEGIN
    t.do_echo := b;
    RETURN old;
  END ExecEcho;

TYPE
  ExecInfo = RECORD
    command       : TEXT;
    exit_code     : INTEGER;
    ignore_errors : BOOLEAN;
  END;

PROCEDURE DoExec (t: T;  n_args: INTEGER)
  RAISES {Error, Thread.Alerted} =
  VAR info := ExecCommand (t, n_args);
  BEGIN
    IF (info.exit_code # 0) AND NOT info.ignore_errors THEN
      Err (t, Fmt.F("exit %s: %s", Fmt.Int(info.exit_code), info.command));
    END;
  END DoExec;

PROCEDURE DoTryExec (t: T;  n_args: INTEGER)
  RAISES {Error, Thread.Alerted} =
  VAR info := ExecCommand (t, n_args);
  BEGIN
    IF (info.ignore_errors) THEN info.exit_code := 0; END;
    PushInt (t, info.exit_code);
  END DoTryExec;

PROCEDURE ExecCommand (t: T;  n_args: INTEGER): ExecInfo
  RAISES {Error, Thread.Alerted} =
  VAR
    info         : ExecInfo;
    echo         := TRUE;
    first        := TRUE;
    n            : INTEGER;
    handle       : Process.T;
    stdin, stdout, stderr: File.T;
    args         : ARRAY [0..1] OF TEXT;
    buf          : M3Buf.T;
    n_shell_args : INTEGER;
    wr           : Wr.T := CurWr (t);
  BEGIN
    info.command := "";
    info.exit_code := 0;
    info.ignore_errors := FALSE;
    IF (n_args <= 0) THEN RETURN info; END;

    (* pack the arguments into a single string & pop the stack *)
    buf   := GetBuf (t);
    FOR i := t.reg.sp - n_args TO t.reg.sp - 1 DO
      IF (first) THEN first := FALSE;  ELSE  M3Buf.PutChar (buf, ' ');  END;
      QVal.ToBuf (t, t.stack[i], buf);
      t.stack[i].ref := NIL;
    END;
    t.reg.sp := t.reg.sp - n_args;
    info.command := M3Buf.ToText (buf);
    FreeBuf (t,  buf);

    (* strip the leading magic characters *)
    n := 0;
    WHILE n < Text.Length (info.command) DO
      CASE Text.GetChar (info.command, n) OF
      | '@' => echo := FALSE;
      | '-' => info.ignore_errors := TRUE;
      ELSE EXIT;
      END;
      INC (n);
    END;
    info.command := Text.Sub (info.command, n);

    (* echo the command & flush any pending output *)
    TRY
      IF echo OR t.do_echo THEN
        Wr.PutText (wr, info.command);
        Wr.PutText (wr, Wr.EOL);
      END;
      FlushIO ();
    EXCEPT Wr.Failure (ec) =>
      Err (t, "write failed" & OSErr (ec));
    END;

    args [0] := t.sh_option;
    args [1] := info.command;
    n_shell_args := 2;

    (* finally, execute the command *)
    TRY
      FlushIO ();
      Process.GetStandardFileHandles (stdin, stdout, stderr);
      handle := Process.Create (t.shell, SUBARRAY (args, 0, n_shell_args),
                                stdin := stdin, stdout := stdout,
                                stderr := stderr);
    EXCEPT
    | Thread.Alerted =>
        KillProcess (handle);
        RAISE Thread.Alerted;
    | Wr.Failure (ec) =>
        KillProcess (handle);
        Err (t, "write failed" & OSErr (ec));
    | OSError.E (ec) =>
        KillProcess (handle);
        Err (t, Fmt.F ("exec failed%s *** %s", OSErr (ec), info.command));
    END;

    (* wait for everything to shutdown... *)
    info.exit_code := Process.Wait (handle);
    RETURN info;
  END ExecCommand;

PROCEDURE KillProcess (handle: Process.T) =
  BEGIN
    IF (handle # NIL) THEN
      TRY
        M3Process.Interrupt (handle);
      EXCEPT OSError.E =>
        (* ignore *)
      END;
    END;
  END KillProcess;

PROCEDURE DoFile (t: T;  n_args: INTEGER) =
  BEGIN
    <*ASSERT n_args = 0 *>
    PushText (t, CurFile (t));
  END DoFile;

PROCEDURE CurFile (t: T): TEXT =
  BEGIN
    RETURN t.map.id2txt (t.includes[t.reg.ip-1].file.source_file);
  END CurFile;

PROCEDURE DoFormat (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR
    val     : QValue.T;
    n       : INTEGER;
    format  : TEXT;
    strings := NEW (REF ARRAY OF TEXT, n_args - 1);
  BEGIN
    <*ASSERT n_args > 0 *>
    n := 0;
    FOR i := t.reg.sp - n_args + 1 TO t.reg.sp - 1 DO
      strings [n] := QVal.ToText (t, t.stack[i]);  INC (n);
      t.stack[i].ref := NIL;
    END;
    DEC (t.reg.sp, n_args - 1);
    Pop (t, val);
    format := QVal.ToText (t, val);
    PushText (t, Fmt.FN (format, strings^));
  END DoFormat;

PROCEDURE DoInclude (t: T;  n_args: INTEGER) RAISES {Error, Thread.Alerted} =
  VAR path: TEXT;  val: QValue.T;
  BEGIN
    <*ASSERT n_args = 1 *>
    Pop (t, val);  path := QVal.ToText (t, val);
    IncludeFile (t, path, from_code := TRUE);
  END DoInclude;

PROCEDURE Include (t: T;  path: TEXT) RAISES {Error, Thread.Alerted} =
  BEGIN
    IncludeFile (t, path, from_code := FALSE);
  END Include;

PROCEDURE IncludeFile (t: T;  path: TEXT;  from_code: BOOLEAN)
  RAISES {Error, Thread.Alerted} =
  VAR old_path: TEXT;  code: QCode.Stream;
  BEGIN
    IF NOT Pathname.Absolute (path) THEN
      old_path := CurFile (t);
      path := Pathname.Join (Pathname.Prefix (old_path), path, NIL);
    END;

    TRY
      code := QCompiler.CompileFile (path, t.map);
    EXCEPT Error(msg) =>
      Err (t, msg);
    END;

    (****
    WITH f = t.frames [t.reg.fp-1] DO
      PushInclude (t, code, f.saved);
      t.reg.ip := f.saved.ip;
    END;
    ****)

    IF (from_code) THEN
      EVAL PopFrame (t); (* pop the call to "include()" *)
    END;
    PushInclude (t, code, t.reg);
    Eval (t);
  END IncludeFile;

PROCEDURE DoMakeDir (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR val: QValue.T;  dir: TEXT;
  BEGIN
    <*ASSERT n_args = 1 *>
    Pop (t, val);  dir := QVal.ToText (t, val);
    MakeDir (t, dir);
  END DoMakeDir;

PROCEDURE MakeDir (t: T;  dir: TEXT)  RAISES {Error} =
  VAR parent: TEXT;
  BEGIN
    IF dir = NIL THEN RETURN END;
    IF M3File.IsDirectory (dir) THEN RETURN END;

    parent := Pathname.Prefix (dir);
    IF (parent # NIL) AND NOT PathEqual (parent, dir) THEN
      MakeDir (t, parent);
    END;

    TRY
      FS.CreateDirectory (dir);
    EXCEPT OSError.E (ec) =>
      Err (t, Fmt.F ("unable to create directory \"%s\"%s", dir, OSErr (ec)));
    END;
  END MakeDir;

PROCEDURE DoNormalize (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR val: QValue.T;  unfixed, prefix: TEXT;
  BEGIN
    <*ASSERT n_args = 2 *>
    Pop (t, val);  unfixed := QVal.ToText (t, val);
    Pop (t, val);  prefix  := QVal.ToText (t, val);
    PushText (t, Normalize (t, prefix, unfixed));
  END DoNormalize;

PROCEDURE Normalize (t: T;  prefix, unfixed: TEXT): TEXT  RAISES {Error} =
  VAR unfixedArcs, prefixArcs: Pathname.Arcs;
  BEGIN
    TRY
      unfixedArcs := Pathname.Decompose(unfixed);
    EXCEPT Pathname.Invalid =>
      Err (t, Fmt.F ("invalid path (\"%s\") in normalize", unfixed));
    END;

    TRY
      prefixArcs := Pathname.Decompose(prefix);
    EXCEPT Pathname.Invalid =>
      Err (t, Fmt.F ("invalid path (\"%s\") in normalize", prefix));
    END;

    TRY
      RETURN Pathname.Compose (StripPrefix (t, prefixArcs,
                                   CanonicalizePath (unfixedArcs)));
    EXCEPT Pathname.Invalid =>
      Err (t, Fmt.F ("invalid path in normalize(\"%s\", \"%s\")",
                     prefix, unfixed));
    END;
    RETURN NIL;
  END Normalize;

PROCEDURE DoPath (t: T;  n_args: INTEGER) =
  BEGIN
    <*ASSERT n_args = 0 *>
    PushText (t, CurPath (t));
  END DoPath;

PROCEDURE CurPath (t: T): TEXT =
  BEGIN
    RETURN Pathname.Prefix (CurFile (t));
  END CurPath;

PROCEDURE DoStale (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR
    val, val2 : QValue.T;
    arr       : QVSeq.T;
    dep       : TEXT;
    target    : TEXT;
    t_status  : File.Status;
    d_status  : File.Status;
  BEGIN
    <*ASSERT n_args = 2 *>
    Pop (t, val2);  (* dependencies *)
    Pop (t, val);   target := QVal.ToText (t, val);

    TRY
      t_status := FS.Status (target);
      IF (val2.kind = QK.Array) THEN
        arr := val2.ref;
        FOR i := 0 TO arr.size() - 1 DO
          dep := QVal.ToText (t, arr.get (i));
          d_status := FS.Status (dep);
          IF t_status.modificationTime < d_status.modificationTime THEN
            PushBool (t, TRUE);
            RETURN;
          END;
        END;
      ELSE
        dep := QVal.ToText (t, val2);
        d_status := FS.Status (dep);
        IF t_status.modificationTime < d_status.modificationTime THEN
          PushBool (t, TRUE);
          RETURN;
        END;
      END;
    EXCEPT OSError.E =>
      PushBool (t, TRUE);
      RETURN;
    END;

    PushBool (t, FALSE);
  END DoStale;

PROCEDURE DoUnlink (t: T;  n_args: INTEGER) RAISES {Error} =
  VAR val: QValue.T;  ok := FALSE;
  BEGIN
    <*ASSERT n_args = 1 *>
    Pop (t, val);
    TRY
      FS.DeleteFile (QVal.ToText (t, val));
      ok := TRUE;
    EXCEPT OSError.E =>
      ok := FALSE;
    END;
    PushBool (t, ok);
  END DoUnlink;

PROCEDURE DoWrite (t: T;  n_args: INTEGER)
  RAISES {Error, Thread.Alerted} =
  VAR wr := CurWr (t);  buf := GetBuf (t);  txt: TEXT;
  BEGIN
    (* write the arguments to an internal buffer & pop the stack *)
    FOR i := t.reg.sp - n_args TO t.reg.sp - 1 DO
      QVal.ToBuf (t, t.stack[i], buf);
      t.stack[i].ref := NIL;
    END;
    t.reg.sp := t.reg.sp - n_args;
    txt := M3Buf.ToText (buf);
    FreeBuf (t, buf);

    TRY Wr.PutText (wr, txt);
    EXCEPT Wr.Failure (ec) => Err (t, "write failed" & OSErr (ec));
    END;
  END DoWrite;

PROCEDURE DoTrace (t: T;  n_args: INTEGER) =
  BEGIN
    <*ASSERT n_args = 0*>
    t.tracing := NOT t.tracing;
  END DoTrace;

(*-------------------------------------------------------- memory buffers ---*)
(* We don't use TRY/FINALLY or worry about buffers that aren't freed.
   In the rare cases when they're not returned, the collector get them. *)

PROCEDURE GetBuf (t: T): M3Buf.T =
  VAR buf: M3Buf.T;
  BEGIN
    IF (t.buffers.tos > 0) THEN
      DEC (t.buffers.tos);
      WITH z = t.buffers.bufs [t.buffers.tos] DO  buf := z; z := NIL;  END;
    ELSE
      buf := M3Buf.New ();
    END;
    RETURN buf;
  END GetBuf;

PROCEDURE FreeBuf (t: T;  buf: M3Buf.T) =
  BEGIN
    IF (t.buffers.tos < NUMBER (t.buffers.bufs)) THEN
      t.buffers.bufs [t.buffers.tos] := buf;
      INC (t.buffers.tos);
    END;
  END FreeBuf;

(*------------------------------------------------------------ temp files ---*)

PROCEDURE CleanUp (t: T) =
  VAR n: INTEGER;  path: TEXT;
  BEGIN
    IF (t.tmp_files # NIL) THEN
      n := t.tmp_files.size ();
      WHILE (n > 0) DO
        path := t.tmp_files.remlo ();
        TRY
          FS.DeleteFile (path);
        EXCEPT OSError.E =>
          (* ignore *)
        END;
        DEC (n);
      END;
    END;
  END CleanUp;

PROCEDURE UniqueTempFile (t: T): TEXT =
  VAR root, file: TEXT;  seq := 0;
  BEGIN
    root := Pathname.Join (t.tmp_dir, "qk", NIL);
    file := root;
    LOOP
      TRY
        EVAL FS.Status (file);
      EXCEPT OSError.E =>
        EXIT;
      END;
      INC (seq);
      file := root & "_" & Fmt.Int (seq);
    END;
    IF (t.tmp_files = NIL) THEN t.tmp_files := NEW (TextSeq.T).init(); END;
    t.tmp_files.addhi (file);
    RETURN file;
  END UniqueTempFile;

(*------------------------------------------------------------------ misc ---*)

PROCEDURE Err (t: T;  msg: TEXT) RAISES {Error} =
  VAR buf := GetBuf (t);  txt: TEXT;  line: INTEGER;
  BEGIN
    IF FindErrorFile (t, txt, line) THEN
      M3Buf.PutText (buf, "\"");
      M3Buf.PutText (buf, txt);
      M3Buf.PutText (buf, "\", line ");
      M3Buf.PutInt  (buf, line);
      M3Buf.PutText (buf, ": ");
    END;
    M3Buf.PutText (buf, "quake runtime error: ");
    M3Buf.PutText (buf, msg);
    M3Buf.PutText (buf, Wr.EOL);
    M3Buf.PutText (buf, Wr.EOL);
    M3Buf.PutText (buf, "--procedure--  -line-  -file---");
    M3Buf.PutText (buf, Wr.EOL);
    DumpFrame (t, buf, t.reg);
    FOR i := t.reg.fp-1 TO 0 BY -1 DO
      DumpFrame (t, buf, t.frames[i].saved);
    END;
    txt := M3Buf.ToText (buf);
    FreeBuf (t, buf);
    RAISE Error (txt);
  END Err;

PROCEDURE FindErrorFile (t: T;  VAR(*OUT*) file: TEXT;
                                VAR(*OUT*) line: INTEGER): BOOLEAN =
  BEGIN
    IF FindErrorFrame (t, t.reg, file, line) THEN RETURN TRUE; END;
    FOR i := t.reg.fp-1 TO 0 BY -1 DO
      IF FindErrorFrame (t, t.frames[i].saved, file, line) THEN RETURN TRUE; END;
    END;
    RETURN FALSE;
  END FindErrorFile;

PROCEDURE FindErrorFrame (t    : T;
                 READONLY reg  : Registers;
               VAR(*OUT*) file : TEXT;
               VAR(*OUT*) line : INTEGER): BOOLEAN =
  BEGIN
    IF reg.cp = NIL THEN RETURN FALSE; END;
    (* else, we're executing in user-written quake code *)
    file := t.map.id2txt (reg.cp.source_file);
    line := MAX (1, reg.ln);
    RETURN TRUE;
  END FindErrorFrame;

PROCEDURE DumpFrame (t: T;  buf: M3Buf.T;  READONLY reg: Registers) =
  BEGIN
    IF (reg.pi = NIL)
      THEN Out (buf, "", 13);
      ELSE Out (buf, t.map.id2txt (reg.pi.name), 13);
    END;
    M3Buf.PutText (buf, "  ");
    IF (reg.ln > 0)
      THEN Out (buf, Fmt.Int (reg.ln), -6);
      ELSE Out (buf, "--", -6);
    END;
    M3Buf.PutText (buf, "  ");
    IF (reg.cp = NIL)
      THEN M3Buf.PutText (buf, "<builtin>");
      ELSE M3Buf.PutText (buf, t.map.id2txt (reg.cp.source_file));
    END;
    M3Buf.PutText (buf, Wr.EOL);
  END DumpFrame;

PROCEDURE Out (buf: M3Buf.T;  txt: TEXT;  width: INTEGER) =
  VAR len := Text.Length (txt);
  BEGIN
    IF (width < 0) THEN
      width := -width;
      WHILE (len < width) DO M3Buf.PutChar (buf, ' '); INC (len); END;
      M3Buf.PutText (buf, txt);
    ELSE
      M3Buf.PutText (buf, txt);
      WHILE (len < width) DO M3Buf.PutChar (buf, ' '); INC (len); END;
    END;
  END Out;

PROCEDURE OSErr (args: AtomList.T): TEXT =
  VAR msg : TEXT := NIL;
  BEGIN
    WHILE (args # NIL) DO
      IF (msg = NIL) THEN  msg := ": ";  ELSE  msg := msg & "  ***  ";  END;
      msg  := msg & Atom.ToText (args.head);
      args := args.tail;
    END;
    RETURN msg;
  END OSErr;

PROCEDURE CanonicalizePath (path: Pathname.Arcs): Pathname.Arcs =
  (* Remove '..' and '.' components from "path". *)
  VAR found := FALSE;  arc: TEXT;  new: Pathname.Arcs;  pending: INTEGER;
  BEGIN
    FOR i := 0 TO path.size () - 1 DO
      arc := path.get (i);
      IF (arc # NIL) AND
        (PathEqual (arc, Pathname.Current)
          OR PathEqual (arc, Pathname.Parent)) THEN
        found := TRUE;
        EXIT;
      END;
    END;

    IF NOT found THEN RETURN path; END;

    new := NEW(Pathname.Arcs).init();
    pending := 0;
    FOR i := 0 TO path.size() - 1 DO
      arc := path.get(i);
      IF (arc = NIL) THEN
        new.addhi (arc);  (* leave the NIL arcs in place (?) *)
      ELSIF PathEqual(arc, Pathname.Current) THEN
        (* skip it *)
      ELSIF PathEqual(arc, Pathname.Parent) THEN
        INC(pending);
      ELSIF pending > 0 THEN
        DEC(pending);
      ELSE
        new.addhi(arc);
      END;
    END;

    WHILE pending > 0 DO new.addhi(Pathname.Parent); DEC(pending); END;
    RETURN new;
  END CanonicalizePath;

PROCEDURE StripPrefix (t: T;  prefix, path: Pathname.Arcs): Pathname.Arcs
  RAISES {Error} =
  VAR
    path_sz, prefix_sz: INTEGER;
    path_txt, prefix_txt: TEXT;
    result: Pathname.Arcs;
  BEGIN
    TRY
      path_txt   := Pathname.Compose (path);
      prefix_txt := Pathname.Compose (prefix);
      path_sz    := path.size ();
      prefix_sz  := prefix.size ();

      IF PathEqual (path_txt, prefix_txt) THEN
        result := NEW (Pathname.Arcs).init (1);
        result.addhi (NIL);
        RETURN result;
      END;

      IF NOT Pathname.Absolute(path_txt)
      OR NOT Pathname.Absolute(prefix_txt) THEN
        RETURN path;
      END;
    EXCEPT Pathname.Invalid =>
      Err (t, "internal error: invalid pathname in StripPrefix");
    END;

    (* make sure "prefix" really is a prefix of "path" *)
    IF (prefix_sz > path_sz) THEN RETURN path; END;
    FOR i := 0 TO prefix_sz - 1 DO
      IF NOT PathEqual (prefix.get(i), path.get(i)) THEN
        RETURN path;
      END;
    END;

    result := TextSeq.Sub (path, prefix_sz);
    result.addlo (NIL);  (* make it a relative path *)
    RETURN result;
  END StripPrefix;

PROCEDURE PathEqual (a, b: TEXT): BOOLEAN =
  BEGIN
    IF Text.Equal (a, b) THEN RETURN TRUE; END;
    IF OnUnix THEN RETURN FALSE; END;
    RETURN CIEqual (a, b);
  
  END PathEqual;

PROCEDURE CIEqual (a, b: TEXT): BOOLEAN =
  (* on Win32, try a case-insensitive match... *)
  VAR len, nxt: INTEGER;  buf_a, buf_b: ARRAY [0..127] OF CHAR;
  BEGIN
    len := Text.Length (a);
    IF (len # Text.Length (b)) THEN RETURN FALSE; END;
    nxt := 0;
    WHILE (nxt < len) DO
      Text.SetChars (buf_a, a, nxt);
      Text.SetChars (buf_b, b, nxt);
      FOR i := 0 TO MIN (NUMBER (buf_a), len-nxt) - 1 DO
        IF lcase[buf_a[i]] # lcase[buf_b[i]] THEN RETURN FALSE; END;
      END;
      INC (nxt, NUMBER (buf_a));
    END;
    RETURN TRUE;
  END CIEqual;

PROCEDURE SplitArgs (txt: TEXT): TextSeq.T =
  VAR
    seq   := NEW (TextSeq.T).init ();
    i     := 0;
    len   := Text.Length (txt);
    start : INTEGER;
  BEGIN
    WHILE i < len DO
      WHILE i < len AND QScanner.WhiteSpace [Text.GetChar (txt, i)] DO INC(i); END;
      start := i;
      WHILE i < len AND NOT QScanner.WhiteSpace [Text.GetChar (txt, i)] DO INC(i); END;
      IF i > start THEN seq.addhi (Text.Sub (txt, start, i - start)); END;
    END;
    RETURN seq;
  END SplitArgs;

PROCEDURE Print (a, b, c, d: TEXT := NIL)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR wr := Stdio.stdout;
  BEGIN
    IF (wr = NIL) THEN (*try...*) wr := Stdio.stderr; END;
    IF (a # NIL) THEN Wr.PutText (wr, a); END;
    IF (b # NIL) THEN Wr.PutText (wr, b); END;
    IF (c # NIL) THEN Wr.PutText (wr, c); END;
    IF (d # NIL) THEN Wr.PutText (wr, d); END;
  END Print;

PROCEDURE FlushIO ()
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF (Stdio.stdout # NIL) THEN Wr.Flush (Stdio.stdout); END;
    IF (Stdio.stderr # NIL) THEN Wr.Flush (Stdio.stderr); END;
  END FlushIO;

VAR
  strict_variables := NOT RTParams.IsPresent ("oldquake");
  lcase : ARRAY CHAR OF CHAR;
BEGIN
  FOR c := FIRST (lcase) TO LAST (lcase) DO lcase[c] := c; END;
  FOR c := 'A' TO 'Z' DO
    lcase[c] := VAL (ORD (c) - ORD ('A') + ORD ('a'), CHAR);
  END;
END QMachine.
