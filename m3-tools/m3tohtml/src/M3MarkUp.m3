(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun  1 13:04:48 PDT 1995 by kalsow                   *)

MODULE M3MarkUp;

IMPORT Rd, Text, Fmt, M3Scanner, M3Token;
IMPORT TextList, TextListSort, TextTextTbl;
IMPORT M3DB, FilePath;

CONST
  Begin_ref       = "<A HREF=\"";
  End_file        = ".html";
  End_ref         = "\">";
  Goto_tag        = "#";

  Begin_tag       = "<A NAME=\"";
  End_tag         = "\">";

  End_anchor      = "</A>";

  Begin_interface = "<interface>";
  End_interface   = "</interface>";

  Begin_module    = "<module>";
  End_module      = "</module>";

  Begin_impl      = "<implements>";
  End_impl        = "</implements>";

  Begin_gen_intf  = "<genericInterface>";
  End_gen_intf    = "</genericInterface>";

  Begin_gen_impl  = "<genericModule>";
  End_gen_impl    = "</genericModule>";

  Begin_proc      = "<procedure>";
  End_proc        = "</procedure>";
  
TYPE
  TK = M3Token.T;

(*******************
CONST
  Suffix = ARRAY BOOLEAN, BOOLEAN OF TEXT {(* standard  generic *)
    (*module*)        ARRAY BOOLEAN OF TEXT { ".m3",     ".mg" },
    (*interface*)     ARRAY BOOLEAN OF TEXT { ".i3",     ".ig" }
  };
*******************)

CONST
  BeginBracket = ARRAY BOOLEAN OF TEXT { "<inModule>\n",  "<inInterface>\n" };
  EndBracket   = ARRAY BOOLEAN OF TEXT { "</inModule>\n", "</inInterface>\n" };

TYPE
  InsList = RECORD  head, tail : Insertion := NIL;  cnt: INTEGER := 0; END;

TYPE
  Info = RECORD
    path         : TEXT;
    key          : TEXT;
    lex          : M3Scanner.T;
    unit         : TEXT;
    id           : TEXT;
    id_offset    : INTEGER;
    id_length    : INTEGER;
    is_interface : BOOLEAN;
    is_generic   : BOOLEAN;
    ins          : InsList;
    choice       : InsList;
    bracket      : Insertion;
  END;

PROCEDURE Get (rd: Rd.T;  path: TEXT): Insertion =
  VAR z: Info;
  BEGIN
    ResetCache ();

    z.path      := path;
    z.key       := NIL;
    z.lex       := NEW (M3Scanner.Default).initFromRd (rd,
                                                 skip_comments := TRUE,
                                                 split_pragmas := FALSE);
    z.unit      := NIL;
    z.id        := NIL;
    z.id_offset := -1;
    z.id_length := 0;

    z.is_interface := TRUE;
    z.is_generic   := FALSE;

    (* build a list of insertions for the header *)
    AddH (z, "<HTML>\n<HEAD>\n<TITLE>SRC Modula-3: ");
    AddH (z, path);
    AddH (z, "</TITLE>\n</HEAD>\n<BODY>\n");
    AddH (z, "<A NAME=\"0TOP0\">\n<H2>");
    AddH (z, path);
    AddH (z, "</H2></A><HR>\n");
    AddH (z, "");   z.bracket := z.ins.tail;

    NextToken (z); (* skip the initial comment *)
    MarkUnit (z);

    (* close the unit's opening bracket *)
    z.id_offset := z.lex.offset+z.lex.length + 10000;
    AddH (z, EndBracket [z.is_interface]);

    (* add the choices *)
    Append (z.ins, z.choice);

    (* and finish (leave blank lines for xmosaic scrolling) *)
    AddH (z, "<PRE>\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n</PRE>\n");
    AddH (z, "</BODY>\n</HTML>\n");

    RETURN SortInsertions (z);
  END Get;

PROCEDURE MarkUnit (VAR z: Info) =
  BEGIN
    MarkCopyright (z);
    IF z.lex.token = TK.Unsafe THEN NextToken (z); END;

    IF z.lex.token = TK.Generic THEN
      NextToken (z); (*GENERIC*)

      IF z.lex.token = TK.Interface THEN
        NextToken (z); (*INTERFACE*)
        z.is_generic   := TRUE;
        z.is_interface := TRUE;

        IF NOT GetUnitID (z) THEN RETURN; END;
        AddH (z, Begin_gen_intf);
        z.key := "generic module " & z.unit;
        IF UnknownRef (z) THEN GenRef (z, NIL, M3DB.GenericMod (z.unit)); END;
        AddT (z, End_gen_intf);

        SkipToSemi (z);
        MarkImports (z);
        MarkDecls (z);

      ELSIF z.lex.token = TK.Module THEN
        NextToken (z); (*MODULE*)
        z.is_generic   := TRUE;
        z.is_interface := FALSE;

        IF NOT GetUnitID (z) THEN RETURN; END;
        AddH (z, Begin_gen_impl);
        z.key := "generic interface " & z.unit;
        IF UnknownRef (z) THEN GenRef (z, NIL, M3DB.GenericIntf (z.unit)); END;
        AddT (z, End_gen_impl);

        SkipToSemi (z);
        MarkImports (z);
        MarkDecls (z);

      ELSE (* error *)
        RETURN;
      END;

    ELSIF z.lex.token = TK.Interface THEN
      NextToken (z); (*INTERFACE*)
      z.is_generic   := FALSE;
      z.is_interface := TRUE;

      IF NOT GetUnitID (z) THEN RETURN; END;
      AddH (z, Begin_interface);
      z.key := z.unit & "'s implementation ";
      IF UnknownRef (z) THEN GenRef (z, NIL, M3DB.Exports (z.unit)); END;
      AddT (z, End_interface);

      IF z.lex.token = TK.Semi THEN
        NextToken (z); (* ; *)
        MarkImports (z);
        MarkDecls (z);
      ELSIF z.lex.token = TK.Equal THEN
        NextToken (z); (* = *)
        MarkGenericInstance (z);
      ELSE RETURN;
      END;

    ELSIF z.lex.token = TK.Module THEN
      NextToken (z); (*MODULE*)
      z.is_generic   := FALSE;
      z.is_interface := FALSE;

      IF NOT GetUnitID (z) THEN RETURN; END;
      AddH (z, Begin_module);
      IF z.lex.token # TK.Exports THEN
        AddH (z, Begin_impl);
        z.key := "interface " & z.unit;
        IF UnknownRef (z) THEN GenRef (z, NIL, M3DB.Interface (z.unit)); END;
        AddT (z, End_impl);
      END;
      AddT (z, End_module);

      MarkExports (z);
      IF z.lex.token = TK.Semi THEN
        NextToken (z); (* ; *)
        MarkImports (z);
        MarkDecls (z);
      ELSIF z.lex.token = TK.Equal THEN
        NextToken (z); (* = *)
        MarkGenericInstance (z);
      ELSE RETURN;
      END;

    ELSE (* error *)
      RETURN;
    END;
  END MarkUnit;

PROCEDURE GetUnitID (VAR z: Info): BOOLEAN =
  BEGIN
    IF NOT GetID (z) THEN RETURN FALSE; END;
    z.unit           := z.id;
    z.bracket.insert := BeginBracket [z.is_interface];
    (***
    z.title.insert   := z.unit & Suffix [z.is_interface, z.is_generic];
    z.header.insert  := z.title.insert;
    ***)
    RETURN TRUE;
  END GetUnitID;

PROCEDURE MarkExports (VAR z: Info) =
  BEGIN
    IF z.lex.token = TK.Exports THEN
      NextToken (z); (*EXPORTS*)
      WHILE GetIntfID (z) DO
        AddH (z, Begin_impl);
        AddT (z, End_impl);
        IF z.lex.token # TK.Comma THEN EXIT END;
        NextToken (z); (* , *)
      END;
    END;
  END MarkExports;

PROCEDURE MarkImports (VAR z: Info) =
  BEGIN
    LOOP
      IF z.lex.token = TK.Import THEN
        NextToken (z); (*IMPORT*)
        WHILE GetIntfID (z) DO
          IF z.lex.token = TK.As THEN
            NextToken (z); (*AS*)
            IF z.lex.token = TK.Ident THEN
              NextToken (z); (*ID*)
            END;
          END;
          IF z.lex.token # TK.Comma THEN EXIT END;
          NextToken (z); (* , *)
        END;

      ELSIF z.lex.token = TK.From THEN
        NextToken (z); (*FROM*)
        EVAL GetIntfID (z);

      ELSE EXIT;
      END;
      SkipToSemi (z);
    END;
  END MarkImports;

PROCEDURE GetIntfID (VAR z: Info): BOOLEAN =
  BEGIN
    IF NOT GetID (z) THEN RETURN FALSE; END;
    z.key := "interface " & z.id;
    IF UnknownRef (z) THEN GenRef (z, NIL, M3DB.Interface (z.id)); END;
    RETURN TRUE;
  END GetIntfID;

PROCEDURE MarkGenericInstance (VAR z: Info) =
  BEGIN
    IF NOT GetID (z) THEN RETURN END;
    IF z.is_interface THEN
      z.key := "generic interface " & z.id;
      IF UnknownRef (z) THEN GenRef (z, NIL, M3DB.GenericIntf (z.id)); END;
    ELSE
      z.key := "generic module " & z.id;
      IF UnknownRef (z) THEN GenRef (z, NIL, M3DB.GenericMod (z.id)); END;
    END;

    IF z.lex.token # TK.L_paren THEN RETURN END;
    NextToken (z); (* ( *)

    WHILE GetIntfID (z) DO
      IF z.lex.token # TK.Comma THEN EXIT END;
      NextToken (z); (* , *)
    END;

    IF z.lex.token # TK.R_paren THEN RETURN END;
    NextToken (z); (* ) *)
  END MarkGenericInstance;

PROCEDURE MarkDecls (VAR z: Info) =
  VAR id, unit: TEXT;  eq: BOOLEAN;
  BEGIN
    LOOP
      CASE z.lex.token OF
      | TK.Type =>
          NextToken (z); (*TYPE*)
          WHILE FindTypeID (z, id, unit, eq) DO
            IF NOT eq THEN
              z.key := Fmt.F ("opaque type %s.%s", z.unit, id);
              IF UnknownRef (z) THEN
                VAR
                  types := M3DB.RevealsType (id);
                  units : TextList.T;
                BEGIN
                  IF z.is_generic
                    THEN units := M3DB.GenericMod (z.unit);
                    ELSE units := M3DB.Exports (z.unit);
                  END;
                  units := And (units, types);
                  IF (units = NIL) THEN
                    units := And (M3DB.RevealsTo (z.unit), types);
                  END;
                  GenRef (z, id, units);
                END;
              END;
            END;
          END;

      | TK.Procedure =>
          NextToken (z); (*PROCEDURE*)
          IF GetID (z) THEN
            IF z.is_interface THEN
              z.key := Fmt.F ("procedure %s.%s", z.unit, z.id);
              IF UnknownRef (z) THEN
                IF z.is_generic THEN
                  GenRef (z, z.id, And (M3DB.GenericMod (z.unit),
                                        M3DB.DefinesProc (z.id)));
                ELSE
                  GenRef (z, z.id, And (M3DB.Exports (z.unit),
                                        M3DB.DefinesProc (z.id)));
                END;
              END;
              SkipToSemi (z);
            ELSE
              AddH (z, Begin_tag);
              AddH (z, z.id);
              AddH (z, End_tag);
              AddH (z, Begin_proc);
              AddT (z, End_proc);
              AddT (z, End_anchor);
              SkipProc (z, z.id);
            END;
          END;

      | TK.Reveal =>
          NextToken (z); (*REVEALS*)
          WHILE FindTypeID (z, id, unit, eq) DO
            IF eq THEN
              AddH (z, Begin_tag);
              AddH (z, z.id);
              AddH (z, End_tag);
              AddT (z, End_anchor);
            END;
          END;

      | TK.EOF, TK.Error => EXIT;
      ELSE NextToken (z);
      END;
    END;
  END MarkDecls;

PROCEDURE MarkCopyright (VAR z: Info) =
  VAR id: TEXT;
  BEGIN
    IF (z.lex.token = TK.Ident) AND (z.lex.length = 9) THEN
      id := Text.FromChars (SUBARRAY (z.lex.buffer^, z.lex.offset,
                                     z.lex.length));
      IF Text.Equal ("Copyright", id) THEN
        z.id_offset := z.lex.offset;
        z.id_length := z.lex.length;
        AddH (z, Begin_ref);
        AddH (z, FilePath.Normalize ("COPYRIGHT.html", z.path));
        AddH (z, End_ref);
        AddT (z, " (C) 1994, Digital Equipment Corp.");
        AddT (z, End_anchor);
        NextToken (z);
      END;
    END;
  END MarkCopyright;

PROCEDURE FindTypeID (VAR z: Info;  VAR id, unit: TEXT;
                      VAR eq: BOOLEAN): BOOLEAN =
  BEGIN
    LOOP
      CASE z.lex.token OF
      | TK.Ident =>
          EVAL GetID (z);
          id := z.id;
          IF z.lex.token = TK.Dot THEN
            NextToken (z); (* . *)
            IF z.lex.token = TK.Ident THEN
              unit := z.id;
              EVAL GetID (z);
            END;
          END;
          IF z.lex.token = TK.Equal THEN
            NextToken (z); (* = *)
            eq := TRUE;
            RETURN TRUE;
          ELSIF z.lex.token = TK.Subtype THEN
            NextToken (z); (* <: *)
            eq := FALSE;
            RETURN TRUE;
          ELSE (* skip *)
          END;

      | TK.L_paren =>
          SkipParens (z);

      | TK.Const, TK.Type, TK.Exception, TK.Var, TK.Procedure, TK.Reveal,
        TK.Begin, TK.EOF, TK.Error =>
          RETURN FALSE;
 
      ELSE
          NextToken (z);
      END; (*CASE*)
    END; (*LOOP*)
  END FindTypeID;

PROCEDURE SkipParens (VAR z: Info) =
  VAR depth: INTEGER := 0;
  BEGIN
    LOOP
      IF z.lex.token = TK.L_paren THEN
        INC (depth);
      ELSIF z.lex.token = TK.R_paren THEN
        DEC (depth);
        IF (depth <= 0) THEN NextToken (z); RETURN END;
      ELSIF z.lex.token = TK.EOF THEN
        RETURN;
      ELSIF z.lex.token = TK.Error THEN
        RETURN;
      END;
      NextToken (z);
    END;
  END SkipParens;

PROCEDURE SkipProc (VAR z: Info;  proc_id: TEXT) =
  BEGIN
    LOOP
      IF z.lex.token = TK.End THEN
        NextToken (z); (*END*)
        IF GetID (z) AND Text.Equal (proc_id, z.id) THEN EXIT; END;
      ELSIF z.lex.token = TK.EOF OR z.lex.token = TK.Error THEN
        EXIT;
      ELSE
        NextToken (z);
      END;
    END;
  END SkipProc;

PROCEDURE SkipToSemi (VAR z: Info) =
  BEGIN
    WHILE (z.lex.token # TK.Semi)
      AND (z.lex.token # TK.EOF)
      AND (z.lex.token # TK.Error) DO
      NextToken (z);
    END;
    IF (z.lex.token = TK.Semi) THEN NextToken (z); END;
  END SkipToSemi;

PROCEDURE GetID (VAR z: Info): BOOLEAN =
  BEGIN
    IF z.lex.token # TK.Ident THEN  z.id := NIL; RETURN FALSE;  END;
    z.id_offset := z.lex.offset;
    z.id_length := z.lex.length;
    z.id := Text.FromChars (SUBARRAY(z.lex.buffer^, z.id_offset, z.id_length));
    NextToken (z);
    RETURN TRUE;
  END GetID;

PROCEDURE NextToken (VAR z: Info) =
  BEGIN
    REPEAT
      z.lex.next ();
    UNTIL (z.lex.token # TK.Begin_pragma);
  END NextToken;

(*------------------------------------------------------- insertion lists ---*)

PROCEDURE AddH (VAR x: Info;  txt: TEXT) =
  BEGIN
    AddI (x.ins, x.id_offset, txt);
  END AddH;

PROCEDURE AddT (VAR x: Info;  txt: TEXT) =
  BEGIN
    AddI (x.ins, x.id_offset + x.id_length, txt);
  END AddT;

PROCEDURE AddC (VAR x: Info;  txt: TEXT) =
  BEGIN
    AddI (x.choice, 0, txt);
  END AddC;

PROCEDURE AddI (VAR z: InsList;  offs: INTEGER;  txt: TEXT) =
  VAR i := NEW (Insertion, next := NIL, offset := offs, insert := txt);
  BEGIN
    IF (z.head = NIL)
      THEN  z.head := i;
      ELSE  z.tail.next := i;
    END;
    z.tail := i;
    INC (z.cnt);
  END AddI;

PROCEDURE Append (VAR a, b: InsList) =
  VAR x := b.head;  offs := a.tail.offset;
  BEGIN
    IF (b.cnt <= 0) THEN RETURN END;

    (* splice the two lists *)
    INC (a.cnt, b.cnt);
    a.tail.next := x;
    a.tail := b.tail;

    (* fix the offsets *)
    WHILE (x # NIL) DO
      x.offset := offs;
      x := x.next;
    END;

    (* empty the old list *)
    b.head := NIL;
    b.tail := NIL;
    b.cnt  := 0;
  END Append;

(*------------------------------------------------------- HREF generation ---*)

VAR href_cache := NEW (TextTextTbl.Default);
VAR next_multi := 1;

PROCEDURE ResetCache () =
  BEGIN
    EVAL href_cache.init ();
    next_multi := 1;
  END ResetCache;

PROCEDURE UnknownRef (VAR x: Info): BOOLEAN =
  VAR dest: TEXT;
  BEGIN
    IF NOT href_cache.get (x.key, dest) THEN RETURN TRUE; END;
    EmitRef (x, dest);
    RETURN FALSE;
  END UnknownRef;

PROCEDURE GenRef (VAR x: Info;  tag: TEXT;  targets: TextList.T) =
  VAR ref: TEXT;
  BEGIN
    targets := NormalizeList (targets);

    IF targets = NIL THEN
      (* no hits *)
    ELSIF targets.tail = NIL THEN
      (* direct hit *)
      ref := targets.head & End_file;
      IF (tag # NIL) THEN ref := ref & Goto_tag & tag; END;
      EVAL href_cache.put (x.key, ref);
      EmitRef (x, ref);
    ELSE
      (* a set of hits *)
      ref := GenMultiRef (x, tag, targets);
      IF (ref # NIL) THEN
        EVAL href_cache.put (x.key, ref);
        EmitRef (x, ref);
      END;
    END;
  END GenRef;

PROCEDURE EmitRef (VAR x: Info;  dest: TEXT) =
  BEGIN
    AddH (x, Begin_ref);
    IF (Text.GetChar (dest, 0) = '#')
      THEN AddH (x, dest);
      ELSE AddH (x, FilePath.Normalize (dest, x.path));
    END;
    AddH (x, End_ref);
    AddT (x, End_anchor);
  END EmitRef;

PROCEDURE GenMultiRef (VAR x: Info;  tag: TEXT;  targets: TextList.T): TEXT =
  VAR label := "x" & Fmt.Int (next_multi);
  BEGIN
    IF (tag = NIL) THEN
      (* HACK work around xmosaic bug... *)
      tag := "0TOP0";
    END;
    INC (next_multi);
    AddC (x, "<HR>\n");
    AddC (x, Begin_tag);
    AddC (x, label);
    AddC (x, End_tag);
    AddC (x, x.key);
    AddC (x, " is in:\n");
    AddC (x, End_anchor);
    AddC (x, "<UL>\n");
    WHILE (targets # NIL) DO
      AddC (x, "<LI>");
      AddC (x, Begin_ref);
      AddC (x, FilePath.Normalize (targets.head, x.path));
      AddC (x, End_file);
      IF (tag # NIL) THEN
        AddC (x, Goto_tag);
        AddC (x, tag);
      END;
      AddC (x, End_ref);
      AddC (x, targets.head);
      AddC (x, End_anchor);
      AddC (x, "\n");
      targets := targets.tail;
    END;
    AddC (x, "</UL>\n<P>\n");
    RETURN Goto_tag & label;
  END GenMultiRef;

PROCEDURE NormalizeList (x: TextList.T): TextList.T =
  VAR cur, prev: TextList.T;
  BEGIN
    x := TextListSort.SortD (x);
    cur := x;  prev := NIL;
    WHILE (cur # NIL) DO
      IF (prev # NIL) AND Text.Equal (prev.head, cur.head)
        THEN prev.tail := cur.tail;  (* delete cur *)
        ELSE prev := cur;
      END;
      cur := cur.tail;
    END;
    RETURN x;
  END NormalizeList;

PROCEDURE And (a, b: TextList.T): TextList.T =
  (* destroys 'a', and normalizes 'b' *)
  VAR c, res: TextList.T;
  BEGIN
    a := NormalizeList (a);
    b := NormalizeList (b);
    res := NIL;
    WHILE (a # NIL) AND (b # NIL) DO
      CASE Text.Compare (a.head, b.head) OF
      | -1 => (* a < b *)  a := a.tail;
      | +1 => (* a > b *)  b := b.tail;
      |  0 => (* a = b *)  c := a;  a := a.tail; b := b.tail;
                           c.tail := res;  res := c;
      END;
    END;
    RETURN res;
  END And;

(*--------------------------------------------------------------- sorting ---*)

PROCEDURE SortInsertions (VAR z: Info): Insertion =
(* Do a simple insertion sort since the list is already nearly sorted *)
  VAR a, b, c, d, e: Insertion;
  BEGIN
    a := z.ins.head;
    b := a.next;  a.next := NIL;
    WHILE (b # NIL) DO
      (* insert 'b' *)
      c := b.next;
      d := a;  e := NIL;
      WHILE (d # NIL) AND (b.offset < d.offset) DO
        e := d;
        d := d.next;
      END;
      IF (e # NIL)
        THEN e.next := b;
        ELSE a := b;
      END;
      b.next := d;
      b := c;
    END;

    (* reverse the list *)
    b := NIL;
    WHILE (a # NIL) DO
      c := a.next;
      a.next := b;
      b := a;
      a := c;
    END;

    RETURN b;
  END SortInsertions;


(*---------- debug ---*)
(**********************
PROCEDURE Out (a, b: TEXT := NIL) =
  <*FATAL ANY*>
  BEGIN
    IF (a  # NIL) THEN Wr.PutText (Stdio.stdout, a); END;
    IF (b  # NIL) THEN Wr.PutText (Stdio.stdout, b); END;
    Wr.PutText (Stdio.stdout, "\n");
  END Out;

PROCEDURE OutL (a: TEXT := NIL;  l: TextList.T := NIL) =
  <*FATAL ANY*>
  BEGIN
    IF (a  # NIL) THEN Wr.PutText (Stdio.stdout, a); END;
    WHILE (l # NIL) DO
      Wr.PutText (Stdio.stdout, l.head);
      Wr.PutText (Stdio.stdout, " ");
      l := l.tail;
    END;
    Wr.PutText (Stdio.stdout, "\n");
  END OutL;
**************)
BEGIN
END M3MarkUp.
