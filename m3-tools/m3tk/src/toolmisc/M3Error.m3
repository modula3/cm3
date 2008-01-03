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
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)


MODULE M3Error EXPORTS M3Error, M3ErrorStream;

IMPORT TextExtras, Fmt, Wr, Stdio, Thread, RefList, TextRefTbl, Atom;
IMPORT AST, M3AST_LX, M3AST_AS;
IMPORT M3AST_FE_priv;

IMPORT M3AST_LX_F, (*M3AST_AS_F,*) M3AST_SM_F, M3AST_TM_F, M3AST_FE_priv_F;

IMPORT M3CSrcPos, M3CId, M3CUnit, M3ASTWalk;
IMPORT M3Assert;

(* This version records error message strings in a hash table, and records
   nodes with errors on a list, plus the args if any. 
   The errors can then be displayed in various ways, including a 
   sequence of text messages, sorted by line number.
*)

VAR
  messages_g := NEW(TextRefTbl.Default).init();
  cu_g: M3AST_AS.Compilation_Unit := NIL;
  stream_g: Wr.T := Stdio.stderr;
  warn_g := TRUE;

REVEAL Notification = Notification_public BRANDED OBJECT END;

TYPE NotificationElem = REF RECORD e: Notification END;

<* FATAL Thread.Alerted, Wr.Failure *>

VAR notifications_g: RefList.T := NIL;
    notificationsMutex_g := NEW(MUTEX);

PROCEDURE AddNotification(e: Notification) RAISES {}=
  BEGIN
    LOCK notificationsMutex_g DO
      notifications_g := RefList.AppendD(notifications_g,
                             RefList.List1(NEW(NotificationElem, e := e)));
    END; (* lock *)
  END AddNotification;

PROCEDURE RemoveNotification(e: Notification) RAISES {}=
  VAR l: RefList.T := notifications_g; prev: RefList.T := NIL;
  BEGIN
    LOCK notificationsMutex_g DO
      WHILE l # NIL DO
        IF NARROW(l.head, NotificationElem).e = e THEN
          IF prev = NIL THEN notifications_g := l.tail
          ELSE prev.tail := l.tail;
          END;
          RETURN
        END;
        prev := l; l := l.tail;
      END;
    END; (* lock *)
  END RemoveNotification; 

PROCEDURE Notify(cu: M3AST_AS.Compilation_Unit; warn: BOOLEAN)=
  BEGIN
    LOCK notificationsMutex_g DO
      VAR list: RefList.T := notifications_g;
      BEGIN
        WHILE list # NIL DO
          NARROW(list.head, NotificationElem).e.notify(cu, warn);
          list := list.tail;
        END;
      END;
    END;
  END Notify;

TYPE
  NodeElem = REF RECORD
    node: ERROR_NODE;
    errors: RefList.T (* of ErrorElem *);
  END;
  ErrorElem = REF RECORD
    messageKey: Atom.T;
    a1, a2, a3, a4: M3AST_LX.Symbol_rep
  END;

REVEAL
  M3AST_FE_priv.Unit_errors = BRANDED REF RefList.T;

PROCEDURE SetCu(cu: M3AST_AS.Compilation_Unit) RAISES {} =
  BEGIN
    cu_g := cu;
    IF cu # NIL AND cu.fe_priv_errors = NIL THEN 
      cu.fe_priv_errors := NEW(M3AST_FE_priv.Unit_errors);
    END;
  END SetCu;

PROCEDURE ReportAtPos(pos: M3CSrcPos.T; message: TEXT) RAISES {} =
  BEGIN
    M3Assert.Check(cu_g # NIL);
    PutError(M3CUnit.TextName(cu_g.fe_uid), pos, message);
  END ReportAtPos;

PROCEDURE Report(n: ERROR_NODE; message: TEXT) RAISES {}=
  BEGIN
    RecordError(n, message, TRUE);
  END Report;

PROCEDURE ReportWithId(n: ERROR_NODE; message: TEXT; 
    id1, id2, id3, id4: M3AST_LX.Symbol_rep := NIL) RAISES {} =
  BEGIN
    RecordError(n, message, TRUE, id1, id2, id3, id4);
  END ReportWithId;

PROCEDURE Warn(pos: ERROR_NODE; message: TEXT) RAISES {} =
  BEGIN
    IF warn_g THEN RecordError(pos, "warning: " & message, FALSE); END;
  END Warn;

PROCEDURE WarnWithId(pos: ERROR_NODE; message: TEXT;
    id1, id2, id3, id4: M3AST_LX.Symbol_rep := NIL) RAISES {} =
  BEGIN
    IF warn_g THEN 
      RecordError(pos, "warning: " & message, FALSE, id1, id2, id3, id4);
    END;
  END WarnWithId;

PROCEDURE SuppressWarnings(b := TRUE) RAISES {}=
  BEGIN
    warn_g := b;
  END SuppressWarnings;

PROCEDURE RecordError(
    pos: ERROR_NODE;
    message: TEXT;
    serious: BOOLEAN;
    arg1, arg2, arg3, arg4: M3AST_LX.Symbol_rep := NIL)
    RAISES {} =
  VAR
    errors: RefList.T;
    error: ErrorElem;
    cu: M3AST_AS.Compilation_Unit;
    key: Atom.T;
    nodeElem: NodeElem;
  BEGIN
    (* Redeclaration errors may be in units other than current *)
    TYPECASE pos OF
    | M3AST_AS.DEF_ID(def_id) =>
        WITH unitId = def_id.tmp_unit_id DO
          IF unitId = NIL THEN
            (* We are at an early stage of processing; the 'tmp_unit_id' has
             not been set up yet. This implies 'def_id' is declared in the
             current unit as imported units are fully semantically analysed
             (and hence have their 'tmp_unit_id' fields set up) before the
             current unit is analysed *)
            cu := cu_g;
          ELSE
            cu := unitId.sm_spec.sm_comp_unit;
          END;
        END;
    ELSE
      cu := cu_g;
    END;

    key := Atom.FromText(message);
    EVAL messages_g.put(message, key);
    IF CarefulNodeLookup(cu, pos, nodeElem) THEN
      (* not the first on this node *)
    ELSE
      nodeElem := NEW(NodeElem, node := pos, errors := NIL);
      cu.fe_priv_errors^ := RefList.AppendD(cu.fe_priv_errors^,
                                            RefList.List1(nodeElem));
    END; (* if *)
    errors := nodeElem.errors;
    WHILE errors # NIL DO
      error := NARROW(errors.head, ErrorElem);
      IF (error.messageKey = key) AND (error.a1 = arg1) AND 
         (error.a2 = arg2) AND (error.a3 = arg3) AND (error.a4 = arg4) THEN
        RETURN  (* duplicate *)
      END;
      errors := errors.tail;
    END; (* while *)

    error := NEW(ErrorElem, a1 := arg1, a2 := arg2, a3 := arg3, a4 := arg4, 
        messageKey := key);
    nodeElem.errors := RefList.AppendD(nodeElem.errors, RefList.List1(error));
    Notify(cu, serious);
  END RecordError;

PROCEDURE CarefulNodeLookup(
    cu: M3AST_AS.Compilation_Unit;
    n: ERROR_NODE;
    VAR (*out*) nodeElem: NodeElem)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF cu.fe_priv_errors = NIL THEN 
      cu.fe_priv_errors := NEW(M3AST_FE_priv.Unit_errors);
    END;
    RETURN NodeLookup(cu.fe_priv_errors^, n, nodeElem);
  END CarefulNodeLookup;

PROCEDURE NodeLookup(
    nodes: RefList.T;
    n: ERROR_NODE;
    VAR (*out*) nodeElem: NodeElem)
    : BOOLEAN
    RAISES {}=
  BEGIN
    WHILE nodes # NIL DO
      IF NARROW(nodes.head, NodeElem).node = n THEN
        nodeElem := nodes.head;
        RETURN TRUE
      ELSE
        nodes := nodes.tail;
      END;
    END; (* while *)
    RETURN FALSE;
  END NodeLookup;


PROCEDURE Show(n: ERROR_NODE; forget := TRUE) RAISES {} =
  VAR
    nodeElem: NodeElem;
    errors: RefList.T;
    error: ErrorElem;
  BEGIN
    IF NodeLookup(cu_g.fe_priv_errors^, n, nodeElem) THEN
      errors := nodeElem.errors;
      WHILE errors # NIL DO
        error := NARROW(errors.head, ErrorElem);
        PutError(M3CUnit.TextName(cu_g.fe_uid), 
                 n.lx_srcpos, 
                 Atom.ToText(error.messageKey),
                 error.a1, error.a2, error.a3, error.a4);
        errors := errors.tail;
      END; (* while *)
      IF forget THEN
        cu_g.fe_priv_errors^ := RefList_DeleteD(cu_g.fe_priv_errors^, nodeElem);
      END;
    END; (* if *)

    VAR
      usedId: M3AST_AS.USED_ID;
    BEGIN
      IF n.IsA_USED_ID(usedId) AND usedId # n THEN
        Show(usedId)
      END;
    END;
  END Show;

TYPE Closure = M3ASTWalk.Closure OBJECT forget: BOOLEAN END;

PROCEDURE ShowAll(n: ERROR_NODE; forget := TRUE) RAISES {}=
  BEGIN
    (* nothing to do if cu.fe_priv_errors empty *)
    IF cu_g.fe_priv_errors^ # NIL THEN
      <* FATAL ANY *>
      BEGIN
        M3ASTWalk.VisitNodes(n,
            NEW(Closure, forget := forget, callback := DoShow));
      END;
    END; (* if *)
  END ShowAll;

PROCEDURE DoShow(cl: Closure; n: AST.NODE; 
    <*UNUSED*> vm: M3ASTWalk.VisitMode) RAISES {}=
  BEGIN
    Show(n, cl.forget);
  END DoShow;

PROCEDURE PutError(name: TEXT; pos: M3CSrcPos.T;
    message: TEXT; id1, id2, id3, id4: M3AST_LX.Symbol_rep := NIL;
    ) RAISES {} =
  VAR
    line, linePos: CARDINAL;
    t, messageWithIds: TEXT;
  BEGIN
    t := Fmt.F("\"%s\"", name);
    IF pos # M3CSrcPos.Null THEN
      line := M3CSrcPos.Unpack(pos, linePos);
      t := Fmt.F("%s, line %s,%s", t, Fmt.Int(line), Fmt.Int(linePos));
    END; (* if *)
    IF id1 = NIL THEN
      (* This is not just an optimisation it is important. Some syntax errors
       may have an embedded %s in 'message' but do not want it substituted by
       an id (such messages must never contain a %s which does need
       substitution or chaos will ensue) *)
      messageWithIds := message;
    ELSE
      messageWithIds := Fmt.F(message, 
                          SafeIdToText(id1),
                          SafeIdToText(id2),
                          SafeIdToText(id3),
                          SafeIdToText(id4)); 
    END;
    t := TextExtras.Join(t, ": ", messageWithIds);
    WITH err = stream_g DO
      Wr.PutText(err, t); Wr.PutChar(err, '\n');
    END;
  END PutError;

PROCEDURE SafeIdToText(id: M3AST_LX.Symbol_rep): TEXT RAISES {}=
  BEGIN
    IF id # NIL THEN RETURN id.toText() ELSE RETURN NIL; END;
  END SafeIdToText;

PROCEDURE Set(s: Wr.T): Wr.T RAISES {}=
  VAR
    old: Wr.T;
  BEGIN
    old := stream_g; stream_g := s; RETURN old;
  END Set;

PROCEDURE RefList_DeleteD(list: RefList.T;
                                 x: REFANY): RefList.T=
  VAR result, t: RefList.T;
  BEGIN
    IF list.head = x THEN result := list.tail
    ELSE
      result := list; t := list; list := list.tail;
      WHILE list # NIL DO
        IF list.head = x THEN t.tail := list.tail; EXIT
        ELSE t := list; list := list.tail;
        END;
      END;
    END;
    RETURN result;
  END RefList_DeleteD;


BEGIN

END M3Error.
