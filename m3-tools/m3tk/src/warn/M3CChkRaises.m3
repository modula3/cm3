(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)
(**)
(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3CChkRaises;

IMPORT ASCII, Text, Fmt, Rd, RdExtras, TextRd, Thread;

IMPORT AST, M3AST_LX, M3AST_AS;
IMPORT M3ASTNext;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;

IMPORT SeqM3AST_AS_Qual_used_id;
IMPORT ASTWalk;

IMPORT M3CStdProcs, M3Error, M3CDef, M3CId, M3CPragma;
IMPORT M3ASTScope;

<*FATAL Rd.Failure, Thread.Alerted *>


TYPE
  ExcArray = REF ARRAY OF M3AST_AS.Exc_id;

  CatchStack = BRANDED REF RECORD
    next: CatchStack := NIL;
    fatal := FALSE;            (* TRUE if this represents FATAL pragma *)
    node: AST.NODE := NIL;
    catches: ExcArray := NIL ; (* 'catches = NIL' is set of all exceptions *)
  END;

<*INLINE*> PROCEDURE DoWarning(n: M3Error.ERROR_NODE; m: TEXT) RAISES {}=
  BEGIN
    M3Error.Warn(n, m);
  END DoWarning;

<*INLINE*> PROCEDURE DoWarningWithId(h: Handle; n: M3Error.ERROR_NODE;
                                     excId: M3AST_AS.Exc_id)=
  VAR id1, id2: M3AST_LX.Symbol_rep := NIL;
      t := "potentially unhandled exception ";
  BEGIN
    IF excId.tmp_unit_id # h.cu.as_root.as_id THEN
      id1 := excId.tmp_unit_id.lx_symrep;
      id2 := excId.lx_symrep;
      t := t & "'%s.%s'";
    ELSE
      id1 := excId.lx_symrep;
      t := t & "'%s'";
    END;
    M3Error.WarnWithId(n, t, id1, id2);
  END DoWarningWithId;


<*INLINE*> PROCEDURE InitNull(): ExcArray RAISES {}=
  BEGIN
    RETURN NEW(ExcArray, 0);
  END InitNull;


VAR
  null_g := InitNull();


PROCEDURE Push(n: AST.NODE; catches: ExcArray; VAR s: CatchStack) RAISES {}=
  BEGIN
    s := NEW(CatchStack, next := s, node := n, catches := catches);
  END Push;


REVEAL
  Handle = M3ASTScope.Closure BRANDED OBJECT
    cu: M3AST_AS.Compilation_Unit;
    pragmas: M3CPragma.Store;
    stack: CatchStack := NIL;
    fatal: CatchStack := NIL;
    used_id: M3AST_AS.USED_ID := NIL;
  OVERRIDES callback := Node;
  END; (* record *)


TYPE
  Phase = {Count, Add};


PROCEDURE PushProc(p: M3AST_AS.Proc_decl; VAR stack: CatchStack) RAISES {}=
  VAR
    catches: ExcArray := NIL;
  BEGIN
    TYPECASE p.as_type.as_raises OF <*NOWARN*>
    | NULL => (* implied RAISES {} *)
        catches := null_g;

    | M3AST_AS.Raisees_any =>

    | M3AST_AS.Raisees_some(raises) =>
        FOR phase := Phase.Count TO Phase.Add DO
          VAR
            iter := SeqM3AST_AS_Qual_used_id.NewIter(raises.as_raisees_s);
            qualUsedId: M3AST_AS.Qual_used_id;
            count := 0;
          BEGIN
            WHILE SeqM3AST_AS_Qual_used_id.Next(iter, qualUsedId) DO
              TYPECASE qualUsedId.as_id.sm_def OF
              | NULL =>
              | M3AST_AS.Exc_id(excId) =>
                  IF phase = Phase.Add THEN catches[count] := excId END;
                  INC(count);
              ELSE
              END;
            END;
            IF phase = Phase.Count THEN
              IF count = 0 THEN catches := null_g; EXIT END;
              catches := NEW(ExcArray, count);
            END;
          END;
        END;
    END;
    Push(p, catches, stack);
  END PushProc;


PROCEDURE PushTry(
    try: M3AST_AS.Try_st;
    VAR stack: CatchStack)
    RAISES {}=
  BEGIN
    TYPECASE try.as_try_tail OF
    | M3AST_AS.Try_except(except) =>
        VAR
          catches: ExcArray := NIL;
        BEGIN
          IF except.as_else = NIL THEN
            FOR phase := Phase.Count TO Phase.Add DO
              VAR
                iter := M3ASTNext.NewIterHandlerLabel(except.as_handler_s);
                handler: M3AST_AS.Handler;
                qualUsedId: M3AST_AS.Qual_used_id;
                count := 0;
              BEGIN
                WHILE M3ASTNext.HandlerLabel(iter, handler, qualUsedId) DO
                  TYPECASE qualUsedId.as_id.sm_def OF
                  | NULL =>
                  | M3AST_AS.Exc_id(excId) =>
                      IF phase = Phase.Add THEN catches[count] := excId END;
                      INC(count);
                  ELSE
                  END;
                END;
                IF phase = Phase.Count THEN
                  IF count = 0 THEN catches := null_g; EXIT END;
                  catches := NEW(ExcArray, count);
                END;
              END;
            END;
          END;
          Push(except, catches, stack);
        END;
    ELSE
    END;
  END PushTry;


PROCEDURE DealtWith(
    excId: M3AST_AS.Exc_id;
    stack: CatchStack;
    raiseSt: M3AST_AS.Raise_st := NIL)
    : BOOLEAN
    RAISES {}=
(* Find if the given exception is dealt with by an enclosing TRY EXCEPT,
   PROCEDURE with RAISES clause or a FATAL pragma.
   Also set the 'tmp_needs_raises' attribute on the enclosing procedure if it
   has a RAISES clause and 'excId' is not dealt with by the RAISES clause or a
   TRY EXCEPT.
   If 'excId' is being raised by 'raiseSt' and it is not dealt with by a RAISES
   or enclosing TRY EXCEPT the 'tmp_fatal' attribute of 'raiseSt' is set 
*)
  VAR
    result := FALSE;
  BEGIN
    LOOP
      (* If 'stack' is NIL any uncaught exception will be fatal *)
      IF stack = NIL THEN EXIT END;

      (* If we are in a TRY EXCEPT ELSE or a procedure with no RAISES clause
       then anything goes. If there is an FATAL ANY we don't complain (we will
        return TRUE) but we continue so we can set the 'tmp_needs_raises' and 
        'tmp_fatal' attributes if necessary 
      *)
      IF stack.catches = NIL THEN
        IF stack.fatal THEN result := TRUE ELSE RETURN TRUE END;
      END;

      IF excId # NIL AND stack.catches # NIL THEN
        FOR i := 0 TO LAST(stack.catches^) DO
          IF excId = stack.catches[i] THEN
            (* This exception is mentioned; if it is mentioned in a RAISES or
             TRY EXCEPT all is well. If it is mentioned in an FATAL
             pragma we need to continue in order to set the 'tmp_needs_raises'
             and 'tmp_fatal' attributes if necessary *)
            IF stack.fatal THEN result := TRUE ELSE RETURN TRUE END;
          END;
        END;
      END;

      (* If we have reached a RAISES clause (i.e.procedure declaration) we
         set 'tmp_needs_raises' as 'excId' has not been handled *)
      TYPECASE stack.node OF
      | M3AST_AS.Proc_decl(procDecl) =>
          IF raiseSt = NIL THEN procDecl.tmp_needs_raises := TRUE END;
      ELSE
      END;

      stack := stack.next;
    END;

    IF raiseSt # NIL THEN raiseSt.tmp_fatal := TRUE END;
    RETURN result;
  END DealtWith;


EXCEPTION
  BadPragmaFormat;


<*INLINE*> PROCEDURE CheckAtAlpha(
    s: Rd.T)
    RAISES {Rd.Failure, Rd.EndOfFile, BadPragmaFormat}=
  BEGIN
    IF Rd.GetChar(s) IN ASCII.Letters THEN
      Rd.UnGetChar(s);
    ELSE
      RAISE BadPragmaFormat;
    END;
  END CheckAtAlpha;


PROCEDURE FindDefId(
    h: Handle;
    t: Text.T)
    : M3AST_AS.DEF_ID=
  BEGIN
    h.used_id.lx_symrep := M3CId.Enter(t);
    RETURN M3ASTScope.Lookup(h.scope, h.used_id);
  END FindDefId;


PROCEDURE FindInInterface(
    h: Handle;
    defId: M3AST_AS.DEF_ID;
    name: Text.T)
    : M3AST_AS.DEF_ID=
  BEGIN
    h.used_id.lx_symrep := M3CId.Enter(name);
    h.used_id.sm_def := NIL;
    M3CDef.ResolveInterfaceId(defId, h.used_id);
    RETURN h.used_id.sm_def
  END FindInInterface;

PROCEDURE BlockOf(handle: Handle; pragma: M3CPragma.T): M3AST_AS.Block=
  VAR follow := M3CPragma.FollowingNode(pragma);
      pre := M3CPragma.PrecedingStmOrDecl(pragma);
  BEGIN
    (* Attempt to find the Block that this pragma is part of, or we ought to 
       report an error. If the following node is a Block or a Declaration/
       Revelation, result is it or Block it is contained in, respectively.
       Otherwise, if the PrecedingStmOrDecl is a Declaration/Revelation,
       then the Block that is contained in, else an error. 
    *)
    TYPECASE follow OF
    | NULL =>
    | M3AST_AS.Block(b) => RETURN b
    | M3AST_AS.DECL_REVL(d) => RETURN BlockOfNode(handle, d);
    ELSE
    END;
    TYPECASE pre OF
    | NULL =>
    | M3AST_AS.Proc_decl, M3AST_AS.Const_decl, M3AST_AS.TYPE_DECL,
      M3AST_AS.Var_decl, M3AST_AS.Exc_decl, M3AST_AS.REVELATION,
      M3AST_AS.DECL_REVL =>
        RETURN BlockOfNode(handle, pre);
    ELSE
    END;
    RETURN NIL;
  END BlockOf;

PROCEDURE LookingForNode(n: AST.NODE; lookingFor: AST.NODE;
    block: M3AST_AS.Block): M3AST_AS.Block=
  BEGIN
    TYPECASE n OF
    | M3AST_AS.Block(b) =>
        block := b;
    ELSE 
      IF lookingFor = n THEN RETURN block END;
    END;
    VAR iter := n.newIter();
        child: AST.NODE;
    BEGIN
      WHILE iter.next(child) DO
        IF child # NIL THEN
          WITH b = LookingForNode(child, lookingFor, block) DO
            IF b # NIL THEN RETURN b END;
          END
        END;
      END;
    END;
    RETURN NIL;
  END LookingForNode;

PROCEDURE BlockOfNode(h: Handle; n: M3AST_AS.SRC_NODE): M3AST_AS.Block=
  BEGIN
    RETURN LookingForNode(h.cu, n, NIL);
  END BlockOfNode;

PROCEDURE Fatal(
    handle: Handle;
    pragma: M3CPragma.T;
    args: Text.T)
    : CatchStack
    RAISES {}=

  PROCEDURE NotFound(first, second: TEXT)=
    VAR
      notFound: Text.T;
    BEGIN
      IF second # NIL THEN
        notFound := first & "." & second;
      ELSE
        notFound := first;
      END;
      M3Error.ReportAtPos(M3CPragma.Position(pragma),
          Fmt.F("Identifier '%s' not declared", notFound));
    END NotFound;

  VAR
    new := NEW(CatchStack, fatal := TRUE, 
               node := BlockOf(handle, pragma));
  BEGIN
    IF new.node = NIL THEN
      M3Error.ReportAtPos(M3CPragma.Position(pragma),
                "FATAL can only occur where a declaration would be legal");
      RETURN NIL;
    END;
    IF args = NIL THEN
      M3Error.ReportAtPos(M3CPragma.Position(pragma),
                "exception names or ANY expected after FATAL");
    ELSE
      CONST
        NotAlphaNumeric = ASCII.All - ASCII.AlphaNumerics;
      TYPE
        Save = REF RECORD next: Save; excId: M3AST_AS.Exc_id END;
      VAR
        s := TextRd.New(args);
        length := Rd.Length(s);
        count := 0;
        save: Save := NIL;
        first, second: Text.T := NIL;
        defId: M3AST_AS.DEF_ID;
      BEGIN
        LOOP
          TRY
            CheckAtAlpha(s);
            first := RdExtras.GetText(s, terminate := NotAlphaNumeric);
            IF Text.Equal(first, "ANY") THEN count := -1; EXIT END;
            second := NIL;
            defId := FindDefId(handle, first);
            IF Rd.Index(s) # length AND Rd.GetChar(s) = '.' THEN
              CheckAtAlpha(s);
              second := RdExtras.GetText(s, terminate := NotAlphaNumeric);
              IF defId # NIL THEN
                defId := FindInInterface(handle, defId, second);
              END;
            END;
            TYPECASE defId OF
            | NULL => NotFound(first, second);
            | M3AST_AS.Exc_id(excId) => 
                save := NEW(Save, next := save, excId := excId);
                INC(count);
            ELSE
              NotFound(first, second);
            END;
            IF Rd.Index(s) = length THEN EXIT END;
            EVAL RdExtras.Skip(s, ASCII.Set{' ', ','});
          EXCEPT
          | Rd.EndOfFile, BadPragmaFormat =>
              M3Error.ReportAtPos(M3CPragma.Position(pragma),
                  "Bad pragma format");
              EXIT;
          END;
        END;
        (* count < 0 => ANY; count = 0 => error; count > 0 ok *)
        IF count = 0 THEN RETURN NIL
        ELSIF count < 0 THEN RETURN new
        ELSE
          new.catches := NEW(ExcArray, count);
          FOR i := count - 1 TO 0 BY -1 DO
            new.catches[i] := save.excId;
            save := save.next;
          END;
        END;
      END;
    END;
    RETURN new;
  END Fatal;


PROCEDURE NewHandle(
    cu: M3AST_AS.Compilation_Unit)
    : Handle
    RAISES {}=
  VAR
    iter := M3CPragma.NewIter(cu.lx_pragmas);
    pragma: M3CPragma.T;
    args: Text.T;
    last: CatchStack := NIL;
    new := NEW(Handle, cu := cu, pragmas := cu.lx_pragmas,
               used_id := NEW(M3AST_AS.USED_ID).init());
  BEGIN
    (* Conveniently, exceptions can only be declared at the outermost scope
       in a unit, so we can set the scope now and process the pragmas
       before the walk of the AST occurs. 
    *)
    M3ASTScope.Set(new, NARROW(cu.as_root, M3AST_AS.UNIT_WITH_BODY).as_block,
                   ASTWalk.VisitMode.Entry);
    WHILE M3CPragma.Next(iter, pragma) DO
      IF M3CPragma.Match(pragma, "FATAL", args) THEN
        WITH fatal = Fatal(new, pragma, args) DO
          IF fatal # NIL THEN
            IF last = NIL THEN
              new.fatal := fatal;
            ELSE
              last.next := fatal;
            END;
            last := fatal;
          END;
        END;
      END;
    END;
    RETURN new;
  END NewHandle;


PROCEDURE Node(h: Handle; n: AST.NODE; vm: ASTWalk.VisitMode) RAISES {}=
  BEGIN
    IF vm = ASTWalk.VisitMode.Exit THEN
      WHILE h.stack # NIL AND h.stack.node = n DO h.stack := h.stack.next END;
    ELSE
      VAR
        notCallOrRaise := FALSE;
      BEGIN
        TYPECASE n OF
        | M3AST_AS.Proc_decl(procDecl) =>
            PushProc(procDecl, h.stack);
            notCallOrRaise := TRUE;
        | M3AST_AS.Try_st(trySt) =>
            PushTry(trySt, h.stack);
            notCallOrRaise := TRUE;
        ELSE
        END;

        WHILE h.fatal # NIL AND h.fatal.node = n DO
          VAR
            save := h.fatal;
          BEGIN
            h.fatal := save.next;
            save.next := h.stack;
            h.stack := save;
          END;
        END;

        IF notCallOrRaise THEN RETURN END;

      END;
      
      (* If enclosed by RAISES ANY (i.e. no raises clause), EXCEPT ELSE or
         FATAL ANY then there is no point in doing any checking *)
      IF h.stack # NIL AND h.stack.catches = NIL THEN RETURN END;

      TYPECASE n OF
      | M3AST_AS.Call(call) =>
          VAR
            pf: M3CStdProcs.T;
          BEGIN
            IF M3CStdProcs.IsStandardCall(call, pf) THEN RETURN END;
          END;
          TYPECASE call.as_callexp.sm_exp_type_spec OF
          | NULL =>
          | M3AST_AS.Procedure_type(procType) =>
              TYPECASE procType.as_raises OF
              | NULL => (* implied RAISES {} *)
              | M3AST_AS.Raisees_some(raises) =>
                VAR
                  iter := SeqM3AST_AS_Qual_used_id.NewIter(
                      raises.as_raisees_s);
                  qualUsedId: M3AST_AS.Qual_used_id;
                BEGIN
                  WHILE SeqM3AST_AS_Qual_used_id.Next(iter, qualUsedId) DO
                    TYPECASE qualUsedId.as_id.sm_def OF
                    | NULL =>
                    | M3AST_AS.Exc_id(excId) =>
                        IF NOT DealtWith(excId, h.stack) THEN
                          DoWarningWithId(h, call, excId);
                        END;
                    ELSE
                    END;
                  END;
                END;

              | M3AST_AS.Raisees_any =>
                (* Could raise anything *)
                IF NOT DealtWith(NIL, h.stack) THEN
                  DoWarning(call, "procedure call may raise any exception");
                END;
              ELSE
              END; 
          ELSE
          END;

      | M3AST_AS.Raise_st(raiseSt) =>
          TYPECASE raiseSt.as_qual_id.as_id.sm_def OF
          | NULL =>
          | M3AST_AS.Exc_id(excId) =>
              IF NOT DealtWith(excId, h.stack, raiseSt) THEN
                DoWarningWithId(h, raiseSt, excId);
              END;
          ELSE
          END;
      ELSE
      END;
    END;
  END Node;


BEGIN

END M3CChkRaises.
