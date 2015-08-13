(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: TryStmt.m3                                            *)
(* Last modified on Tue May 23 15:36:40 PDT 1995 by kalsow     *)
(*      modified on Fri Oct  5 06:40:21 1990 by muller         *)

MODULE TryStmt;

IMPORT M3, M3ID, CG, Variable, Scope, Exceptionz, Value, Error, Marker;
IMPORT Type, Stmt, StmtRep, TryFinStmt, Token;
IMPORT Scanner, ESet, Target, M3RT, Tracer, Jmpbufs;
FROM Scanner IMPORT Match, MatchID, GetToken, Fail, cur;

TYPE
  P = Stmt.T OBJECT
        scope     : Scope.T;
        body      : Stmt.T;
        h_origin  : INTEGER;
        handles   : Handler;
        hasElse   : BOOLEAN;
        elseBody  : Stmt.T;
        handled   : ESet.T;
        jmpbufs   : Jmpbufs.Try;
      OVERRIDES
        check       := Check;
        compile     := Compile;
        outcomes    := GetOutcome;
      END;

TYPE
  Handler = REF RECORD
              next   : Handler;
              tags   : Except;
              type   : Type.T;
              var    : Variable.T;
              scope  : Scope.T;
              body   : Stmt.T;
              origin : INTEGER;
            END;

TYPE
  Except = REF RECORD
             next : Except;
             name : M3.QID;
             obj  : Value.T;
           END;

PROCEDURE Parse (): Stmt.T =
  TYPE TK = Token.T;
  VAR s: Stmt.T;  p: P;  bar: BOOLEAN;  here := Scanner.offset;
  BEGIN
    Match (TK.tTRY);
    s := Stmt.Parse ();
    IF (cur.token = TK.tFINALLY) THEN
      s := TryFinStmt.Parse (s);
      s.origin := here;
      RETURN s;
    END;

    p := NEW (P);
    StmtRep.Init (p);
    p.origin   := here;
    p.scope    := Scope.Top ();
    p.body     := s;
    p.hasElse  := FALSE;
    p.elseBody := NIL;
    p.handled  := NIL;
    p.handles  := NIL;

    Match (TK.tEXCEPT);
    p.h_origin := Scanner.offset;
    bar := (cur.token = TK.tBAR);
    IF (bar) THEN GetToken (); (* | *)  END;
    LOOP
      IF (cur.token = TK.tELSE) THEN EXIT END;
      IF (cur.token = TK.tEND) THEN EXIT END;
      bar := FALSE;
      ParseHandler (p);
      IF (cur.token # TK.tBAR) THEN EXIT END;
      bar := TRUE; GetToken (); (* | *)
    END;

    ReverseHandlers (p);
    IF (bar) THEN
      Fail ("missing handler");
    END;

    IF (cur.token = TK.tELSE) THEN
      GetToken (); (* ELSE *)
      p.hasElse := TRUE;
      p.elseBody := Stmt.Parse ();
    END;

    Match (TK.tEND);
    RETURN p;
  END Parse;

PROCEDURE ParseHandler (p: P) =
  TYPE TK = Token.T;
  VAR h: Handler;  e: Except;  id: M3ID.T;  trace: Tracer.T;
  VAR u: BOOLEAN := FALSE; (* Exception argument Id has UNUSED pragma. *) 
  BEGIN
    h := NEW (Handler);
    h.next   := p.handles;  p.handles := h;
    h.tags   := NIL;
    h.type   := NIL;
    h.var    := NIL;
    h.scope  := NIL;
    h.body   := NIL;
    h.origin := Scanner.offset;
    LOOP
      e := NEW (Except);
      e.next := h.tags;  h.tags := e;
      e.obj  := NIL;
      e.name.module := M3ID.NoID;
      e.name.item   := MatchID ();
      IF (cur.token = TK.tDOT) THEN
        GetToken (); (* . *)
        e.name.module := e.name.item;
        e.name.item   := MatchID ();
      END;
      IF (cur.token # TK.tCOMMA) THEN EXIT END;
      GetToken (); (* , *)
    END;
    IF (cur.token = TK.tLPAREN) THEN
      GetToken (); (* ( *)
      IF cur.token = TK.tUNUSED THEN 
        u := TRUE; 
        GetToken (); (* UNUSED *) 
        Match (TK.tENDPRAGMA);
      END; 
      id := MatchID ();
      trace := Variable.ParseTrace ();
      Match (TK.tRPAREN);
      h.var := Variable.New (id, u);
      h.scope := Scope.New1 (h.var);
      Variable.BindTrace (h.var, trace);
      Match (TK.tIMPLIES);
      h.body := Stmt.Parse ();
      Scope.PopNew ();
    ELSE
      Match (TK.tIMPLIES);
      h.body := Stmt.Parse ();
    END;
  END ParseHandler;

PROCEDURE ReverseHandlers (p: P) =
  VAR h1, h2, h3: Handler;
  BEGIN
    h1 := p.handles;
    h3 := NIL;
    WHILE (h1 # NIL) DO
      h2 := h1.next;
      h1.next := h3;
      h3 := h1;
      h1 := h2;
    END;
    p.handles := h3;
  END ReverseHandlers;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  VAR h: Handler;  handled: ESet.T;
  BEGIN
    Jmpbufs.CheckTry (cs.jmpbufs, p.jmpbufs);
    h := p.handles;
    WHILE (h # NIL) DO CheckLabels (h, p.scope, cs); h := h.next; END;

    IF (p.hasElse) THEN
      Marker.PushTryElse (CG.No_label, CG.No_label, NIL);
      handled := ESet.NewAny ();
    ELSE
      handled := ESet.NewEmpty (p.scope);
      h := p.handles;
      WHILE (h # NIL) DO PushRaises (h, handled); h := h.next; END;
      p.handled := handled;
      Marker.PushTry (CG.No_label, CG.No_label, NIL, handled);
    END;
    ESet.TypeCheck (handled);

    ESet.Push (cs, handled, NIL, stop := FALSE);
    Stmt.TypeCheck (p.body, cs);
    ESet.Pop  (cs, handled, NIL, stop := FALSE);
    Marker.Pop ();

    (* check each handler *)
    PushHandler (NIL, 0, FALSE);
      h := p.handles;
      WHILE (h # NIL) DO CheckHandler (h, cs); h := h.next; END;
      Stmt.TypeCheck (p.elseBody, cs);
    PopHandler ();
  END Check;

PROCEDURE CheckLabels (h: Handler;  scope: Scope.T;  VAR cs: Stmt.CheckState) =
  VAR e: Except;  obj: Value.T;  t: Type.T;
  BEGIN
    Scanner.offset := h.origin;
    e := h.tags;
    WHILE (e # NIL) DO
      obj := Scope.LookUpQID (scope, e.name);
      IF (obj = NIL) THEN Error.QID (e.name, "undefined") END;
      e.obj := obj;
      Value.TypeCheck (obj, cs);
      IF (Value.ClassOf (obj) # Value.Class.Exception) THEN
        Error.QID (e.name, "is not an exception");
      ELSE
        IF (h.scope # NIL) THEN
          t := Exceptionz.ArgType (obj);
          IF (e = h.tags) THEN (* first one *)
            h.type := t;
          ELSIF NOT Type.IsEqual (t, h.type, NIL) THEN
            Error.Msg ("exceptions have incompatible types");
          END;
        END;
      END;
      e := e.next;
    END;
  END CheckLabels;

PROCEDURE PushRaises (h: Handler;  handled: ESet.T) =
  VAR e: Except;
  BEGIN
    Scanner.offset := h.origin;
    e := h.tags;
    WHILE (e # NIL) DO
      ESet.Add (handled, e.name, e.obj);
      e := e.next;
    END;
  END PushRaises;

PROCEDURE CheckHandler (h: Handler;  VAR cs: Stmt.CheckState) =
  VAR zz: Scope.T;
  BEGIN
    Scanner.offset := h.origin;
    IF (h.scope # NIL) THEN
      IF (h.type = NIL) THEN
        Error.Msg ("exception(s) don\'t have a return argument");
      END;
      Variable.BindType (h.var, h.type, indirect := FALSE, readonly := FALSE,
                         needs_init := FALSE, open_array_ok := FALSE);
      IF Exceptionz.ArgByReference (h.type) THEN
        Variable.NeedsAddress (h.var);
      END;
      Scope.TypeCheck (h.scope, cs);
      zz :=Scope.Push (h.scope);
        Stmt.TypeCheck (h.body, cs);
        Scope.WarnUnused (h.scope);
      Scope.Pop (zz);
    ELSE
      Stmt.TypeCheck (h.body, cs);
    END;
  END CheckHandler;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  BEGIN
    IF (p.handles = NIL) AND (NOT p.hasElse) THEN
      RETURN Stmt.Compile (p.body);
    END;
    IF Target.Has_stack_walker
      THEN RETURN Compile1 (p);
      ELSE RETURN Compile2 (p);
    END;
  END Compile;

PROCEDURE Compile1 (p: P): Stmt.Outcomes =
  VAR
    oc: Stmt.Outcomes;
    h: Handler;
    l: CG.Label;
    info: CG.Var;
    next_handler: CG.Label;
    another: BOOLEAN;
    next_info: CG.Var;
  BEGIN
    (* declare and initialize the info record *)
    info := CG.Declare_local (M3ID.NoID, M3RT.EA_SIZE, Target.Address.align,
                              CG.Type.Struct, 0, in_memory := TRUE,
                              up_level := FALSE, f := CG.Never);
    CG.Load_nil ();
    CG.Store_addr (info, M3RT.EA_exception);

    (* compile the body *)
    l := CG.Next_label (3);
    CG.Set_label (l, barrier := TRUE);
    IF (p.hasElse)
      THEN Marker.PushTryElse (l, l+1, info);
      ELSE Marker.PushTry (l, l+1, info, p.handled);
    END;
    Marker.SaveFrame ();
      oc := Stmt.Compile (p.body);
    Marker.Pop ();
    CG.Jump (l+2);

    IF (p.hasElse) THEN
      (* EXITs and RETURNs from the body are caught by the ELSE clause *)
      oc := oc - Stmt.Outcomes {Stmt.Outcome.Returns, Stmt.Outcome.Exits};
    END;

    (* check for enclosing handlers *)
    another := Marker.NextHandler (next_handler, next_info);

    (* set the "Compiler.ThisException()" globals *)
    PushHandler (info, 0, direct := TRUE);

    (* compile each of the handlers *)
    CG.Set_label (l+1, barrier := TRUE);
    Scanner.offset := p.h_origin;
    CG.Gen_location (p.h_origin);
    h := p.handles;
    WHILE (h # NIL) DO
      oc := oc + CompileHandler1 (h, info, l+2,
                       (NOT p.hasElse) AND (h.next = NIL) AND (NOT another));
      h := h.next;
    END;

    IF (p.hasElse) THEN
      oc := oc + Stmt.Compile (p.elseBody);
    ELSIF another THEN
      (* we didn't eat this exception => mark and invoke the next handler *)
      CG.Load_addr_of (next_info, 0, Target.Address.align);
      CG.Load_addr_of (info, 0, Target.Address.align);
      CG.Copy (M3RT.EA_SIZE, overlap := FALSE);
      CG.Jump (next_handler);
    END;

    (* restore the "Compiler.ThisException()" globals *)
    PopHandler ();

    CG.Set_label (l+2);

    RETURN oc;
  END Compile1;

PROCEDURE CompileHandler1 (h: Handler;  info: CG.Var;
                         resume: CG.Label;  last: BOOLEAN): Stmt.Outcomes =
  VAR
    e: Except;
    oc: Stmt.Outcomes;
    zz: Scope.T;
    top: CG.Label;
    need_top := FALSE;
    t_info: Type.Info;
  BEGIN
    top := CG.Next_label (2);

    CG.Gen_location (h.origin);

    IF (NOT last) THEN
      (* check for a match *)
      e := h.tags;
      <*ASSERT e # NIL*>
      WHILE (e # NIL) DO
        CG.Load_addr (info, M3RT.EA_exception);
        CG.Boost_alignment (Target.Integer.align);
        CG.Load_indirect (Target.Integer.cg_type, 0, Target.Integer.size);
        CG.Load_intt (Exceptionz.UID (e.obj));  (** Value.Load (e.obj);  **)
        e := e.next;
        IF (e # NIL)
          THEN CG.If_compare (Target.Integer.cg_type, CG.Cmp.EQ, top, CG.Maybe);  need_top := TRUE;
          ELSE CG.If_compare (Target.Integer.cg_type, CG.Cmp.NE, top+1, CG.Maybe);
        END;
      END;
      (*** CG.Jump (top+1); ***)
    END;

    IF (need_top) THEN CG.Set_label (top) END;

    IF (h.scope # NIL) THEN
      zz := Scope.Push (h.scope);
        Scope.Enter (h.scope);
        Scope.InitValues (h.scope);
        Variable.LoadLValue (h.var);
        EVAL Type.CheckInfo (h.type, t_info);
        IF Exceptionz.ArgByReference (h.type) THEN
          CG.Load_addr (info, M3RT.EA_arg);
          CG.Boost_alignment (t_info.alignment);
          CG.Copy (t_info.size, overlap := FALSE);
        ELSE
          CG.Load_addr (info, M3RT.EA_arg);
          CG.Loophole (CG.Type.Addr, t_info.stk_type);
          CG.Store_indirect (t_info.stk_type, 0, t_info.size);
        END;
        Variable.ScheduleTrace (h.var);
        oc := Stmt.Compile (h.body);
        IF (Stmt.Outcome.FallThrough IN oc) THEN CG.Jump (resume) END;
        (* for the debugger's sake, this Jump should be inside the scope *)
        Scope.Exit (h.scope);
      Scope.Pop (zz);
    ELSE
      oc := Stmt.Compile (h.body);
      IF (Stmt.Outcome.FallThrough IN oc) THEN CG.Jump (resume) END;
    END;


    CG.Set_label (top+1);
    RETURN oc;
  END CompileHandler1;

PROCEDURE Compile2 (p: P): Stmt.Outcomes =
  VAR
    oc: Stmt.Outcomes;
    h: Handler;
    l: CG.Label;
    frame, eset: CG.Var;
    eoffset: INTEGER;
  BEGIN
    l := CG.Next_label (3);
    CG.Set_label (l, barrier := TRUE);

    (* declare and initialize the info record *)
    frame := CG.Declare_local (M3ID.NoID, M3RT.EF1_SIZE, Target.Address.align,
                               CG.Type.Struct, 0, in_memory := TRUE,
                               up_level := FALSE, f := CG.Never);

    (**********************************************
    (* mark the frame: no exception happened yet *)
    CG.Load_nil ();
    CG.Store_addr (frame, M3RT.EF1_exception);
    ***********************************************)

    IF (p.hasElse) THEN
      Marker.PushTryElse (l, l+1, frame);
      Marker.PushFrame (frame, M3RT.HandlerClass.ExceptElse);
    ELSE
      Marker.PushTry (l, l+1, frame, p.handled);
      ESet.GetAddress (p.handled, eset, eoffset);
      CG.Load_addr_of (eset, eoffset, Target.Address.align);
      CG.Store_addr (frame, M3RT.EF1_handles);
      Marker.PushFrame (frame, M3RT.HandlerClass.Except);
    END;

    (* capture the machine state *)
    Marker.CaptureState (frame, Jmpbufs.CompileTryGetJmpbuf (p.jmpbufs), l+1);

    (* compile the body *)
    oc := Stmt.Compile (p.body);
    Marker.Pop ();
    IF (Stmt.Outcome.FallThrough IN oc) THEN
      Marker.PopFrame (frame);
      CG.Jump (l+2);
    END;

    IF (p.hasElse) THEN
      (* EXITs and RETURNs from the body are caught by the ELSE clause *)
      oc := oc - Stmt.Outcomes {Stmt.Outcome.Returns, Stmt.Outcome.Exits};
    END;

    (* set the "Compiler.ThisException()" globals *)
    PushHandler (frame, M3RT.EF1_info, direct := TRUE);

    (* compile each of the handlers *)
    CG.Set_label (l+1, barrier := TRUE);
    Scanner.offset := p.h_origin;
    CG.Gen_location (p.h_origin);
    h := p.handles;
    WHILE (h # NIL) DO
      oc := oc + CompileHandler2 (h, frame, l+2,
                                  (NOT p.hasElse) AND (h.next = NIL));
      h := h.next;
    END;

    IF (p.hasElse) THEN
      oc := oc + Stmt.Compile (p.elseBody);
    END;

    (* restore the "Compiler.ThisException()" globals *)
    PopHandler ();

    CG.Set_label (l+2);

    RETURN oc;
  END Compile2;

PROCEDURE CompileHandler2 (h: Handler;  frame: CG.Var;
                         resume: CG.Label;  last: BOOLEAN): Stmt.Outcomes =
  VAR
    e: Except;
    oc: Stmt.Outcomes;
    zz: Scope.T;
    top: CG.Label;
    need_top := FALSE;
    t_info: Type.Info;
  BEGIN
    top := CG.Next_label (2);

    CG.Gen_location (h.origin);

    IF (NOT last) THEN
      (* check for a match *)
      e := h.tags;
      <*ASSERT e # NIL*>
      WHILE (e # NIL) DO
        CG.Load_addr (frame, M3RT.EF1_info + M3RT.EA_exception);
        CG.Boost_alignment (Target.Integer.align);
        CG.Load_indirect (Target.Integer.cg_type, 0, Target.Integer.size);
        CG.Load_intt (Exceptionz.UID (e.obj));  (** Value.Load (e.obj);  **)
        e := e.next;
        IF (e # NIL)
          THEN CG.If_compare (Target.Integer.cg_type, CG.Cmp.EQ, top, CG.Maybe);  need_top := TRUE;
          ELSE CG.If_compare (Target.Integer.cg_type, CG.Cmp.NE, top+1, CG.Maybe);
        END;
      END;
      (*** CG.Jump (top+1); ***)
    END;

    IF (need_top) THEN CG.Set_label (top) END;

    IF (h.scope # NIL) THEN
      zz := Scope.Push (h.scope);
        Scope.Enter (h.scope);
        Scope.InitValues (h.scope);
        Variable.LoadLValue (h.var);
        EVAL Type.CheckInfo (h.type, t_info);
        IF Exceptionz.ArgByReference (h.type) THEN
          CG.Load_addr (frame, M3RT.EF1_info + M3RT.EA_arg);
          CG.Boost_alignment (t_info.alignment);
          CG.Copy (t_info.size, overlap := FALSE);
        ELSE
          CG.Load_addr (frame, M3RT.EF1_info + M3RT.EA_arg);
          CG.Loophole (CG.Type.Addr, t_info.stk_type);
          CG.Store_indirect (t_info.stk_type, 0, t_info.size);
        END;
        Variable.ScheduleTrace (h.var);
        oc := Stmt.Compile (h.body);
        IF (Stmt.Outcome.FallThrough IN oc) THEN CG.Jump (resume) END;
        (* for the debugger's sake, this Jump should be inside the scope *)
        Scope.Exit (h.scope);
      Scope.Pop (zz);
    ELSE
      oc := Stmt.Compile (h.body);
      IF (Stmt.Outcome.FallThrough IN oc) THEN CG.Jump (resume) END;
    END;

    CG.Set_label (top+1);
    RETURN oc;
  END CompileHandler2;

PROCEDURE GetOutcome (p: P): Stmt.Outcomes =
  VAR h: Handler;  oc := Stmt.GetOutcome (p.body);
  BEGIN
    IF (p.hasElse) THEN
      (* EXITs and RETURNs from the body are caught by the ELSE clause *)
      oc := oc - Stmt.Outcomes {Stmt.Outcome.Returns, Stmt.Outcome.Exits};
    END;

    h := p.handles;
    WHILE (h # NIL) DO
      oc := oc + Stmt.GetOutcome (h.body);
      h := h.next;
    END;

    IF (p.hasElse) THEN
      oc := oc + Stmt.GetOutcome (p.elseBody)
    END;

    RETURN oc;
  END GetOutcome;

(*------------------------------- handler info for Compiler.ThisException ---*)

TYPE
  HandlerInfo = RECORD
    info   : CG.Var;
    offset : INTEGER;
    direct : BOOLEAN;
  END;

VAR (* for "Compiler.ThisException()" *)
  tos   : INTEGER := 0;
  stack : ARRAY [0..50] OF HandlerInfo;

PROCEDURE PushHandler (info: CG.Var;  offset: INTEGER;   direct: BOOLEAN) =
  BEGIN
    WITH z = stack [tos] DO
      z.info   := info;
      z.offset := offset;
      z.direct := direct;
    END;
    INC (tos);
  END PushHandler;

PROCEDURE PopHandler () =
  BEGIN
    DEC (tos);
    stack[tos].info := NIL;
  END PopHandler;

PROCEDURE InHandler (): BOOLEAN =
  BEGIN
    RETURN (tos > 0);
  END InHandler;

PROCEDURE LoadInfoPtr () =
  BEGIN
    IF (tos <= 0) THEN  CG.Load_nil ();  RETURN;  END;
    WITH z = stack[tos-1] DO
      IF z.direct THEN
        CG.Load_addr_of (z.info, z.offset, Target.Address.align);
      ELSE
        CG.Load_addr (z.info, z.offset);
      END;
    END;
  END LoadInfoPtr;

BEGIN
END TryStmt.
