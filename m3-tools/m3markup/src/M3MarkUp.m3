(* Copyright (C) 1992, Digital Equipment Corporation             *)
(* All rights reserved.                                          *)
(* See the file COPYRIGHT for a full description.                *)
(*                                                               *)
(* Last modified on Tue Apr  2 11:30:41 PST 1996 by heydon       *)
(*      modified on Tue Mar  7 14:38:20 PST 1995 by kalsow       *)
(*                                                               *)
(* Enhanced by Peter Klein (pk@i3.informatik.rwth-aachen.de) to  *)
(* parse procedure signatures and connect procedure declarations *)
(* in interfaces with their implmentations. -  Mar 7, 1995       *)

MODULE M3MarkUp;

IMPORT Buf, Text, M3Scanner, M3Token;

CONST
  End_Anchor      = "</A>";
  End_I3          = ".i3\">";
  End_IG          = ".ig\">";
  End_MG          = ".mg\">";
  End_Ref         = "\">";

VAR
  Start_Exporters := "<A HREF=\"/4";
  Start_Interface := "<A HREF=\"/3";
  Start_Exporter  := "<A HREF=\"/S";
  Start_Type      := "<A HREF=\"/L";
  Start_Header    := "<A NAME=\"";

TYPE
  TK = M3Token.T;

TYPE
  State = { Idle, GrabUnit, GrabExports, GrabGeneric, GrabImports,
            GrabFromUnit, SkipFromImports, SkipRename, GrabProc, GrabType
          };

TYPE
  PragmaCleanScanner = M3Scanner.Default OBJECT OVERRIDES
    next := SkipPragmaNext;
  END;

PROCEDURE SetHrefRoot(prefix: TEXT) =
  BEGIN
    Start_Exporters := "<A HREF=\"" & prefix & "/4";
    Start_Interface := "<A HREF=\"" & prefix & "/3";
    Start_Exporter  := "<A HREF=\"" & prefix & "/S";
    Start_Type      := "<A HREF=\"" & prefix & "/L";
    Start_Header    := "<A NAME=\"" & prefix & "";
  END SetHrefRoot;

PROCEDURE Get (buf: Buf.T): Insertion =
  VAR n_exports := 0;
  VAR state := State.GrabUnit;
  VAR is_interface := TRUE;
  VAR is_generic := FALSE;
  VAR id: TEXT;
  VAR id_offs: INTEGER;
  VAR unit := "";
  VAR unit_offs: INTEGER;
  VAR head := NEW (Insertion, next := NIL);
  VAR ins := head;
  VAR lex := NEW (PragmaCleanScanner).initFromBuf (buf, skip_comments := TRUE,
                                                   split_pragmas := FALSE);
  BEGIN
    (* build a list of insertions *)
    LOOP
      CASE lex.token OF
      | TK.Module    => is_interface := FALSE;
      | TK.Interface => is_interface := TRUE;
      | TK.Generic   => is_generic := TRUE;
      | TK.Procedure => state := State.GrabProc;
      | TK.Ident =>
          CASE state OF
          | State.Idle =>
              (* skip it *)
          | State.GrabUnit =>
              GetID (lex, unit, unit_offs);
              IF is_interface AND NOT is_generic THEN
                Add (ins, unit_offs, Start_Exporters);
                Add (ins, unit_offs, unit);
                Add (ins, unit_offs, End_I3);
                Add (ins, unit_offs + lex.length, End_Anchor);
              END;
              IF is_generic
                THEN state := State.SkipFromImports;
                ELSE state := State.GrabImports;
              END;
          | State.GrabExports =>
              GetID (lex, id, id_offs);
              Add (ins, id_offs, Start_Interface);
              Add (ins, id_offs, id);
              Add (ins, id_offs, End_I3);
              Add (ins, id_offs + lex.length, End_Anchor);
              INC (n_exports);
          | State.GrabGeneric =>
              GetID (lex, id, id_offs);
              Add (ins, id_offs, Start_Interface);
              Add (ins, id_offs, id);
              IF is_interface
                THEN Add (ins, id_offs, End_IG);
                ELSE Add (ins, id_offs, End_MG);
              END;
              Add (ins, id_offs + lex.length, End_Anchor);
              state := State.GrabImports;
          | State.GrabImports =>
              GetID (lex, id, id_offs);
              Add (ins, id_offs, Start_Interface);
              Add (ins, id_offs, id);
              Add (ins, id_offs, End_I3);
              Add (ins, id_offs + lex.length, End_Anchor);
          | State.GrabFromUnit =>
              GetID (lex, id, id_offs);
              Add (ins, id_offs, Start_Interface);
              Add (ins, id_offs, id);
              Add (ins, id_offs, End_I3);
              Add (ins, id_offs + lex.length, End_Anchor);
          | State.SkipRename =>
              (* skip this one *)
              state := State.GrabImports;
          | State.SkipFromImports =>
              (* skip this one *)
          | State.GrabProc =>
              IF (is_interface) THEN
                GetID (lex, id, id_offs);
                Add (ins, id_offs, Start_Exporter);
                Add (ins, id_offs, unit & ".i3." & id & "#" & id);
                Add (ins, id_offs, End_Ref);
                Add (ins, id_offs + lex.length, End_Anchor);
              ELSE
                GetID (lex, id, id_offs);
                Add (ins, id_offs, Start_Header);
                Add (ins, id_offs, id);
                Add (ins, id_offs, End_Ref);
                Add (ins, id_offs + lex.length, End_Anchor);
              END;
              lex.next (); (* id *)
              MarkUpSignature (lex, ins, unit, is_interface);
              state := State.Idle;
          | State.GrabType =>
              MarkUpQualTypeIdent (lex, ins, unit);
              lex.next (); (* skip = or <: *)
              MarkUpType (lex, ins, unit, is_interface);
          END;
      | TK.Exports =>
          state := State.GrabExports;
      | TK.Semi =>
          IF (state = State.GrabExports)
            OR (state = State.GrabUnit)
            OR (state = State.GrabFromUnit)
            OR (state = State.SkipRename)
            OR (state = State.SkipFromImports) THEN
            state := State.GrabImports;
          ELSIF (n_exports = 0) AND (state = State.GrabImports)
            AND NOT is_generic AND NOT is_interface THEN
            Add (ins, unit_offs, Start_Interface);
            Add (ins, unit_offs, unit);
            Add (ins, unit_offs, End_I3);
            Add (ins, unit_offs + Text.Length (unit), End_Anchor);
            INC (n_exports);
          ELSIF (state = State.SkipRename) THEN
            state := State.GrabImports;
          END;
      | TK.Equal =>
          IF (state = State.GrabExports) OR (state = State.GrabUnit) THEN
            state := State.GrabGeneric;
          ELSIF (state = State.GrabImports) THEN
            state := State.GrabGeneric;
            IF (n_exports = 0) AND NOT is_generic AND NOT is_interface THEN
              Add (ins, unit_offs, Start_Interface);
              Add (ins, unit_offs, unit);
              Add (ins, unit_offs, End_I3);
              Add (ins, unit_offs + Text.Length (unit), End_Anchor);
              INC (n_exports);
            END;
          END;
      | TK.From =>
          state := State.GrabFromUnit;
      | TK.Import =>
          IF (state = State.GrabFromUnit) THEN
            state := State.SkipFromImports;
          ELSE
            state := State.GrabImports;
          END;
      | TK.As =>
          state := State.SkipRename;
      | TK.Comma =>
          IF (state = State.SkipRename) THEN
            state := State.GrabImports;
          END;
      | TK.Type, TK.Reveal =>
          state := State.GrabType;
      | TK.Const, TK.Exception, TK.Var, TK.Raises,
        TK.Value, TK.End, TK.Readonly, TK.Begin, TK.Case, TK.Exit,
        TK.Eval, TK.For, TK.If, TK.Lock, TK.Loop, TK.Raise, TK.Repeat,
        TK.Until, TK.Return, TK.Typecase, TK.Try, TK.Finally, TK.Except,
        TK.While, TK.Do, TK.With, TK.L_paren =>
          state := State.Idle;
      | TK.EOF, TK.Error =>
          EXIT;
      ELSE
        (* skip it *)
      END;
      lex.next ();
    END;

    RETURN head.next;
  END Get;

PROCEDURE MarkUpType (lex: M3Scanner.T;  VAR ins: Insertion;
                      unit: TEXT;  is_interface: BOOLEAN) =
  (******VAR id: TEXT;  offset: INTEGER;*******)
  BEGIN
    CASE lex.token OF
    | TK.Ident =>
      (*******************
        (* working around an error in M3Token:  ROOT is not
           recognized as a token *)
        GetID (lex, id, offset);
        IF (Text.Equal (id, "ROOT")) THEN
          lex.next (); (* ROOT *)
          IF (lex.token # TK.Semi) AND (lex.token # TK.R_paren) 
            AND (lex.token # TK.Assign) AND (lex.token # TK.Equal) THEN
            MarkUpType (lex, ins, unit, is_interface);
          END;
        ELSE
       ****************)
          MarkUpQualTypeIdent (lex, ins, unit);
          IF (lex.token = TK.Object) OR (lex.token = TK.Branded) THEN
            MarkUpType (lex, ins, unit, is_interface);
          END;
      (*************************
        END;
       *************************)
    | TK.Array =>
        WHILE (lex.token # TK.Of) DO
          lex.next (); (* ARRAY or COMMA *)
          IF (lex.token # TK.Of) THEN
            MarkUpType (lex, ins, unit, is_interface);
          END;
        END;
        lex.next (); (* OF *)
        MarkUpType (lex, ins, unit, is_interface);
    | TK.Bits =>
        SkipTo (lex, TK.For);
        lex.next (); (* FOR *)
        MarkUpType (lex, ins, unit, is_interface);
    | TK.L_brace =>
        SkipTo (lex, TK.R_brace);
        lex.next (); (* enum types are boring *)
    | TK.L_bracket =>
        SkipTo (lex, TK.R_bracket);
        lex.next (); (* so are subranges *)
    | TK.Procedure =>
        lex.next (); (* PROCEDURE *)
        MarkUpSignature (lex, ins, unit, is_interface);
    | TK.Record =>
        lex.next (); (* RECORD *)
        MarkUpFields (lex, ins, unit, is_interface);
        lex.next (); (* END *)
    | TK.Object => 
        lex.next (); (* OBJECT *)
        MarkUpFields (lex, ins, unit, is_interface);
        IF (lex.token = TK.Methods) THEN
          lex.next (); (* METHODS *)
          MarkUpMethods (lex, ins, unit, is_interface);
        END;
        IF (lex.token = TK.Overrides) THEN
          lex.next (); (* OVERRIDES *)
          MarkUpOverrides (lex, ins, unit, is_interface);
        END;
        lex.next (); (* END *)
        IF (lex.token = TK.Branded) OR (lex.token = TK.Object) THEN
          MarkUpType (lex, ins, unit, is_interface);
        END;
    | TK.Untraced =>
        lex.next (); (* UNTRACED *)
        MarkUpType (lex, ins, unit, is_interface);
    | TK.Branded =>
        WHILE (lex.token # TK.Ref) AND (lex.token # TK.Object) DO
          lex.next (); (* skip the brand expression *)
        END;
        MarkUpType (lex, ins, unit, is_interface);
    | TK.Ref =>
        lex.next (); (* REF *)
        MarkUpType (lex, ins, unit, is_interface);
    | TK.Set =>
        lex.next (); (* SET *)
        lex.next (); (* OF *)
        MarkUpType (lex, ins, unit, is_interface);
    | TK.L_paren =>
        lex.next (); (* L_paren *)
        MarkUpType (lex, ins, unit, is_interface);
        lex.next (); (* R_paren *)
    ELSE
      (*  <* ASSERT FALSE *>  *)
      (* just ignore in every-day use *)
    END;
  END MarkUpType;

PROCEDURE MarkUpSignature (lex: M3Scanner.T;  VAR ins: Insertion;
                           unit: TEXT;  is_interface: BOOLEAN) =
  BEGIN
    lex.next (); (* L_paren *)
    WHILE (lex.token # TK.R_paren) DO
      CASE lex.token OF
      | TK.Var, TK.Readonly, TK.Value, TK.Semi =>
          (* skip *)
      | TK.Ident =>
          WHILE (lex.token # TK.Colon) AND (lex.token # TK.Assign) DO
            lex.next (); (* formal names and commas *)
          END;
          IF lex.token = TK.Colon THEN
            lex.next ();
            MarkUpType (lex, ins, unit, is_interface);
          END;
          WHILE (lex.token # TK.Semi) AND (lex.token # TK.R_paren) DO
            lex.next ();
          END;
      ELSE
        (*  <* ASSERT FALSE *>  *)
        (* just ignore in every-day use *)
      END;
      IF (lex.token # TK.R_paren) THEN lex.next (); END;
    END;
    lex.next (); (* R_paren *)
    IF (lex.token = TK.Colon) THEN
      lex.next (); (* colon *)
      MarkUpType (lex, ins, unit, is_interface);
    END;
    WHILE (lex.token # TK.Semi) AND (lex.token # TK.Equal)
      AND (lex.token # TK.R_paren) AND (lex.token # TK.Assign)
      AND (lex.token # TK.End) DO
      lex.next ();
    END;
  END MarkUpSignature;

PROCEDURE MarkUpFields (lex: M3Scanner.T;  VAR ins: Insertion;
                        unit: TEXT;  is_interface: BOOLEAN) =
  BEGIN
    WHILE (lex.token # TK.Methods)
      AND (lex.token # TK.Overrides)
      AND (lex.token # TK.End) DO
      WHILE (lex.token # TK.Colon) AND (lex.token # TK.Assign) DO
        lex.next ();
      END;
      IF lex.token = TK.Colon THEN
        lex.next ();
        MarkUpType (lex, ins, unit, is_interface);
      ELSE
        WHILE (lex.token # TK.Semi) AND (lex.token # TK.Methods) AND
          (lex.token # TK.Overrides) AND (lex.token # TK.End) DO
          lex.next ();
        END;
      END;
      IF (lex.token = TK.Semi) THEN lex.next (); END;
    END;
  END MarkUpFields;

PROCEDURE MarkUpMethods (lex: M3Scanner.T;  VAR ins: Insertion;
                         unit: TEXT;  is_interface: BOOLEAN) =
  BEGIN
    WHILE (lex.token # TK.Overrides) AND (lex.token # TK.End) DO
      lex.next ();  (* skip ident *)
      MarkUpSignature (lex, ins, unit, is_interface);
      IF (lex.token = TK.Assign) THEN
        lex.next ();
        MarkUpProc (lex, ins, unit, is_interface);
      END;
      IF (lex.token = TK.Semi) THEN lex.next (); END;
    END;
  END MarkUpMethods;

PROCEDURE MarkUpOverrides (lex: M3Scanner.T;  VAR ins: Insertion;
                          unit: TEXT;  is_interface: BOOLEAN) =
  BEGIN
    WHILE (lex.token # TK.End) DO
      lex.next ();  (* skip ident *)
      lex.next ();  (* skip := *)
      MarkUpProc (lex, ins, unit, is_interface);
      IF (lex.token = TK.Semi) THEN lex.next (); END;
    END;
  END MarkUpOverrides; 
  
PROCEDURE MarkUpQualTypeIdent (lex: M3Scanner.T;  VAR ins: Insertion;
                              currentUnit: TEXT) =
  VAR id: TEXT;
      id_offs, tmp_id_offs: INTEGER;
      anchorPos: INTEGER;
      unit := currentUnit;
  BEGIN
    GetID (lex, id, id_offs);
    anchorPos := id_offs + lex.length;
    lex.next ();
    IF (lex.token = TK.Dot) THEN
      (* qualified identifier *)
      unit := id;
      lex.next ();
      GetID (lex, id, tmp_id_offs);
      anchorPos := tmp_id_offs + lex.length;
      lex.next ();
    END;
    Add (ins, id_offs, Start_Type);
    IF Text.Equal (id, "ADDRESS") OR Text.Equal (id, "BOOLEAN") OR
      Text.Equal (id, "CARDINAL") OR Text.Equal (id, "CHAR") OR
      Text.Equal (id, "EXTENDED") OR Text.Equal (id, "INTEGER") OR
      Text.Equal (id, "LONGREAL") OR Text.Equal (id, "MUTEX") OR
      Text.Equal (id, "NULL") OR Text.Equal (id, "REAL") OR
      Text.Equal (id, "REFANY") OR Text.Equal (id, "TEXT") THEN
      Add (ins, id_offs, id);
    ELSE
      Add (ins, id_offs, unit & "." & id);
    END;
    Add (ins, id_offs, End_Ref);
    Add (ins, anchorPos, End_Anchor);
  END MarkUpQualTypeIdent;
  
PROCEDURE MarkUpProc (lex: M3Scanner.T;  VAR ins: Insertion;
                     currentUnit: TEXT;  is_interface: BOOLEAN) =
  VAR id: TEXT;
      id_offs, tmp_id_offs: INTEGER;
      anchorPos: INTEGER;
      unit := currentUnit;
  BEGIN
    GetID (lex, id, id_offs);
    IF Text.Equal (id, "NIL") THEN lex.next(); RETURN; END;
    anchorPos := id_offs + lex.length;
    lex.next ();
    IF (lex.token = TK.Dot) THEN
      (* qualified identifier *)
      unit := id;
      lex.next ();
      GetID (lex, id, tmp_id_offs);
      anchorPos := tmp_id_offs + lex.length;
      lex.next ();
    END;
    IF (currentUnit # unit) OR is_interface THEN
      Add (ins, id_offs, Start_Exporter);
      Add (ins, id_offs, unit & ".i3." & id & "#" & id);
    ELSE
      Add (ins, id_offs, Start_Interface);
      Add (ins, id_offs, unit & ".m3#" & id);
    END;
    Add (ins, id_offs, End_Ref);
    Add (ins, anchorPos, End_Anchor);
  END MarkUpProc; 
  
PROCEDURE SkipTo (lex: M3Scanner.T;  token: TK) =
  BEGIN
    WHILE (lex.token # token) AND (lex.token # TK.EOF) DO
      lex.next ();
    END;
  END SkipTo;

PROCEDURE GetID (lex: M3Scanner.T;  VAR id: TEXT;  VAR offset: INTEGER) =
  BEGIN
    offset := lex.offset;
    id := Text.FromChars (SUBARRAY (lex.buffer^, lex.offset, lex.length));
  END GetID;

PROCEDURE Add (VAR x: Insertion;  offs: INTEGER;  txt: TEXT) =
  BEGIN
    x.next := NEW (Insertion, next := NIL, offset := offs, insert := txt);
    x := x.next;
  END Add;

PROCEDURE SkipPragmaNext (lex: PragmaCleanScanner) =
  BEGIN
    REPEAT
      M3Scanner.Default.next (lex);
    UNTIL (lex.token # TK.Begin_pragma);
  END SkipPragmaNext;

BEGIN
END M3MarkUp.
