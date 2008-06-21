(* Copyright (C) 1992, Digital Equipment Corporation             *)
(* All rights reserved.                                          *)
(* See the file COPYRIGHT for a full description.                *)
(*                                                               *)
(* Last modified on Tue Mar  7 14:38:20 PST 1995 by kalsow       *)
(*                                                               *)
(* Enhanced by Peter Klein (pk@i3.informatik.rwth-aachen.de) to  *)
(* parse procedure signatures and connect procedure declarations *)
(* in interfaces with their implmentations. -  Mar 7, 1995       *)

MODULE M3MarkUp;

IMPORT Buf, Text, Text2, M3Sym, Marker;

CONST
  Start_Impls = "<A HREF=\"" & Intf_to_Impl_Mark & "/";
  Start_Intfs = "<A HREF=\"" & Impl_to_Intf_Mark & "/";
  Start_Defn  = "<A NAME=\"" & ThisDecl & "\">";
  Start_Use   = "<A HREF=\"";
  Start_Type  = "<A HREF=\"/type/";
  Start_EM    = "<B><EM><FONT SIZE=+1>";
  End_EM      = "</FONT></EM></B>";
  End_Ref     = "\">";
  End_DeclRef = "#_DECL_\">";
  End_Anchor  = "</A>";

CONST
  HasDecl = ARRAY M3Sym.Kind OF BOOLEAN {
    FALSE, (* IntfName *)
    FALSE, (* ImplName *)
    FALSE, (* GIntfName *)
    FALSE, (* GImplName *)
    FALSE, (* GIntfUse *)
    FALSE, (* GImplUse *)
    TRUE,  (* GFormal *)
    FALSE, (* GActual *)
    FALSE, (* Export *)
    FALSE, (* Import *)
    FALSE, (* FromImport *)
    TRUE,  (* SymImport *)
    FALSE, (* ImportXX *)
    FALSE, (* ImportAs *)
    TRUE,  (* ConstDecl *)
    TRUE,  (* VarDecl *)
    TRUE,  (* ExceptDecl *)
    TRUE,  (* ProcDecl *)
    TRUE,  (* TypeDecl *)

    TRUE,  (* TypeUse *)
    TRUE,  (* ExceptUse *)
    TRUE,  (* ProcUse *)
    TRUE,  (* MiscUse *)
    
    FALSE, (* Keyword *)
    FALSE, (* BuiltinOp *)
    FALSE, (* BuiltinType *)
    FALSE  (* BuiltinConst *)
  };

CONST
  IgnoreSyms = M3Sym.KindSet { M3Sym.Kind.BuiltinOp, M3Sym.Kind.BuiltinConst };

CONST
  EndRef     = ARRAY BOOLEAN(*has_decl*) OF TEXT { End_Ref,     End_DeclRef };
  StartOther = ARRAY BOOLEAN(*is_intf*)  OF TEXT { Start_Intfs, Start_Impls };

TYPE
  Insertion = Marker.CharInsertion;

TYPE
  State = M3Sym.CallBack OBJECT
    buf          : Buf.T;
    target       : TEXT;
    target_len   : INTEGER;
    ins          : Insertion;
    is_interface : BOOLEAN;
    is_generic   : BOOLEAN;
    unit_name    : M3Sym.Id;
  OVERRIDES
    note_sym     := NoteSym;
    note_qid     := NoteQID;
  END;

PROCEDURE Get (buf: Buf.T;  target: TEXT): Insertion =
  VAR s := NEW (State);  head := NEW (Insertion, next := NIL);
  BEGIN
    s.buf          := buf;
    s.target       := target;
    s.target_len   := 0;
    s.ins          := head;
    s.is_interface := TRUE;
    s.is_generic   := FALSE;
    IF (target # NIL) THEN s.target_len := Text.Length (target); END;
    M3Sym.Scan (buf, s, IgnoreSyms);
    RETURN head;
  END Get;

PROCEDURE NoteSym (s    : State;
          READONLY sym  : M3Sym.Id;
                   kind : M3Sym.Kind;
        <*UNUSED*> intf : TEXT): BOOLEAN =
  VAR decl_hit := FALSE;
  BEGIN
    CASE kind OF

    | M3Sym.Kind.IntfName =>
        s.is_interface := TRUE;
        s.is_generic := FALSE;
        s.unit_name := sym;
        Add (s, sym.start, Start_Impls);
        Add (s, sym.start, End_Ref);
        Add (s, sym.start + sym.len, End_Anchor);

    | M3Sym.Kind.ImplName =>
        s.is_interface := FALSE;
        s.is_generic := FALSE;
        s.unit_name := sym;
        (* don't mark the name, it might be the link to the exported interface *)

    | M3Sym.Kind.GIntfName =>
        s.is_interface := TRUE;
        s.is_generic := TRUE;
        s.unit_name := sym;
        NoteUse (s, sym, kind);

    | M3Sym.Kind.GImplName =>
        s.is_interface := FALSE;
        s.is_generic := TRUE;
        s.unit_name := sym;
        NoteUse (s, sym, kind);

    | M3Sym.Kind.GFormal,
      M3Sym.Kind.ConstDecl,
      M3Sym.Kind.VarDecl,
      M3Sym.Kind.ExceptDecl =>
        (* mark this definition, if it matches the one we're looking for *)
        IF TargetMatch (s, sym) THEN
          Add (s, sym.start, Start_Defn);
          Add (s, sym.start, Start_EM);
          Add (s, sym.start + sym.len, End_EM);
          Add (s, sym.start + sym.len, End_Anchor);
        END;

    | M3Sym.Kind.ProcDecl =>
        IF TargetMatch (s, sym) THEN
          (* we want to find this procedure => mark it *)
          Add (s, sym.start-1, Start_Defn);
          Add (s, sym.start, End_Anchor);
          decl_hit := TRUE;
        END;
        (* now, make the procedure ID a link to the corresponding impl/defn *)
        Add (s, sym.start, StartOther [s.is_interface]);
        AddToken (s, sym);
        Add (s, sym.start, End_DeclRef);
        IF decl_hit THEN
          Add (s, sym.start, Start_EM);
          Add (s, sym.start + sym.len, End_EM);
        END;
        Add (s, sym.start + sym.len, End_Anchor);

    | M3Sym.Kind.TypeDecl =>
        IF TargetMatch (s, sym) THEN
          (* we want to find this type declaration => mark it *)
          Add (s, sym.start-1, Start_Defn);
          Add (s, sym.start, End_Anchor);
          decl_hit := TRUE;
        END;
        IF NOT s.is_generic THEN
          (* build a link to the "/type" part of the name space *)
          Add (s, sym.start, Start_Type);
          AddToken (s, s.unit_name);
          Add (s, sym.start, ".");
          AddToken (s, sym);
          Add (s, sym.start, End_Ref);
          IF decl_hit THEN
            Add (s, sym.start, Start_EM);
            Add (s, sym.start + sym.len, End_EM);
          END;
          Add (s, sym.start + sym.len, End_Anchor);
        ELSE (* generic types can still be decl hits... *)
          IF decl_hit THEN
            Add (s, sym.start, Start_EM);
            Add (s, sym.start + sym.len, End_EM);
          END;
        END;

    | M3Sym.Kind.GIntfUse,
      M3Sym.Kind.GImplUse,
      M3Sym.Kind.GActual,
      M3Sym.Kind.Export,
      M3Sym.Kind.Import,
      M3Sym.Kind.FromImport,
      M3Sym.Kind.SymImport,
      M3Sym.Kind.ImportXX,
      M3Sym.Kind.ImportAs,
      M3Sym.Kind.TypeUse,
      M3Sym.Kind.ExceptUse,
      M3Sym.Kind.ProcUse,
      M3Sym.Kind.MiscUse =>
        NoteUse (s, sym, kind);

    | M3Sym.Kind.Keyword =>
        Add (s, sym.start, "<B>");
        Add (s, sym.start + sym.len, "</B>");

    | M3Sym.Kind.BuiltinType =>
        Add (s, sym.start, Start_Type);
        AddToken (s, sym);
        Add (s, sym.start, End_Ref);
        Add (s, sym.start + sym.len, End_Anchor);

    | M3Sym.Kind.BuiltinOp,
      M3Sym.Kind.BuiltinConst =>
        (* ignore *)

    END; (* CASE *)
    RETURN FALSE;
  END NoteSym;

PROCEDURE NoteUse (s: State;  READONLY sym: M3Sym.Id;  kind: M3Sym.Kind) =
  BEGIN
    Add (s, sym.start, Start_Use);
    AddToken (s, sym);
    Add (s, sym.start, EndRef [HasDecl [kind]]);
    Add (s, sym.start + sym.len, End_Anchor);
  END NoteUse;

PROCEDURE NoteQID (s: State;  READONLY qid: M3Sym.QId;  kind: M3Sym.Kind): BOOLEAN =
  VAR markup := Start_Use;  start := qid[0].start;
  BEGIN
    FOR i := FIRST (qid) TO LAST (qid) DO
      Add (s, start, markup);
      AddToken (s, qid[i]);
      markup := "/";
    END;
    Add (s, start, EndRef [HasDecl [kind]]);
    WITH  z = qid [LAST (qid)] DO  Add (s, z.start + z.len, End_Anchor);  END;
    RETURN FALSE;
  END NoteQID;

PROCEDURE TargetMatch (s: State;  READONLY sym: M3Sym.Id): BOOLEAN =
  BEGIN
    IF (s.target # NIL) AND (sym.len = s.target_len) AND
      Text2.EqualSub (s.target, SUBARRAY (s.buf^, sym.start, sym.len)) THEN
      s.target := NIL; (* prevent further matches. *)
      RETURN TRUE;
    END;
    RETURN FALSE;
  END TargetMatch;

PROCEDURE AddToken (s: State;  READONLY sym: M3Sym.Id) =
  BEGIN
    WITH z = s.ins.insert [s.ins.count-1] DO
      z.start  := sym.start;
      z.length := sym.len;
    END;
  END AddToken;

PROCEDURE Add (s: State;  offs: INTEGER;  txt: TEXT) =
  BEGIN
    IF (s.ins.count >= NUMBER (s.ins.insert)) THEN
      s.ins.next := NEW (Insertion, next := NIL, count := 0);
      s.ins := s.ins.next;
    END;
    WITH z = s.ins.insert [s.ins.count] DO
      z.offset := offs;
      z.txt    := txt;
    END;
    INC (s.ins.count);
  END Add;

BEGIN
END M3MarkUp.
