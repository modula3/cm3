(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3Header.m3                                           *)
(* Last modified on Mon Jul 11 11:55:37 PDT 1994 by kalsow     *)

MODULE M3Header;

IMPORT File, Token, Host, M3ID, Scanner;
FROM Scanner IMPORT GetToken, cur;

TYPE
  TK = Token.T;

TYPE
  State = RECORD
    imports   : IDList  := NIL;
    generic   : File.T  := NIL;
    interface : BOOLEAN := FALSE;
    failed    : BOOLEAN := FALSE;
  END;

PROCEDURE Parse (): IDList =
  VAR
    s  : State;
    id : M3ID.T;
  BEGIN
    PushID (s, M3ID.Add ("RTHooks")); (* compiler magic *)

    IF (cur.token = TK.tEXTERNAL) THEN SkipExternalPragma (); END;
    IF (cur.token = TK.tUNSAFE) THEN GetToken (); END;

    s.interface := (cur.token = TK.tINTERFACE);
    IF (cur.token #TK.tINTERFACE) AND (cur.token # TK.tMODULE) THEN
      RETURN s.imports;
    END;
    GetToken (); (* INTERFACE/MODULE *)

    IF NOT MatchID (s, id) THEN RETURN s.imports END;

    IF (cur.token = TK.tEXPORTS) THEN
      GetToken (); (*EXPORTS*)
      MatchIDList (s, save := TRUE);
    ELSIF (NOT s.interface) THEN
      PushID (s, id);
    END;

    IF (cur.token = TK.tSEMI) THEN
      (* this is a simple module/interface, just fall through *)
      GetToken (); (* ; *)
    ELSIF (cur.token = TK.tEQUAL) THEN
      (* this is an instantiation of a generic module/interface *)
      GetToken (); (* = *)
      PushGeneric (s);
    ELSE (* bail out *)
      RETURN s.imports;
    END;

    ParseImports (s);

    IF (s.generic # NIL) THEN
      Scanner.Pop ();
      Host.CloseFile (s.generic);
    END;

    RETURN s.imports;
  END Parse;

PROCEDURE SkipExternalPragma () =
  BEGIN
    GetToken (); (* EXTERNAL *)
    IF (cur.token = TK.tIDENT) OR (cur.token = TK.tTEXTCONST) THEN
      GetToken (); (* IDENT, TEXTCONST *)
      IF (cur.token = TK.tCOLON) THEN
        GetToken (); (* : *)
        IF (cur.token = TK.tIDENT) OR (cur.token = TK.tTEXTCONST) THEN
          GetToken (); (* language *)
        END;
      END;
    END;
    IF (cur.token = TK.tENDPRAGMA) THEN
      GetToken (); (* "*>" *)
    END;
  END SkipExternalPragma;

PROCEDURE PushGeneric (VAR s: State) =
  (* instantiate a call on a generic interface or module *)
  VAR
    genericName  : M3ID.T;
    filename     : TEXT;
    old_filename : TEXT;
    old_line     : INTEGER;
  BEGIN
    Scanner.Here (old_filename, old_line);

    IF NOT MatchID (s, genericName) THEN RETURN END;

    (* parse the list of actuals *)
    ParseGenericArgs (s, save := TRUE);
 
    (* open the external file *)
    s.generic := Host.OpenUnit (genericName, s.interface, TRUE, filename);
    IF (s.generic = NIL) THEN s.failed := TRUE;  RETURN; END;

    (* build a synthetic file name & start reading *)
    filename := Host.FileTail(old_filename) & " => " & filename;
    Scanner.Push (filename, s.generic, is_main := Scanner.in_main);

    (* make sure we got what we wanted *)
    IF NOT Match (s, TK.tGENERIC) THEN RETURN END;
    IF (cur.token # TK.tINTERFACE) AND (cur.token #TK.tMODULE) THEN RETURN END;
    GetToken ();

    (* get the generic's name *)
    IF NOT Match (s, TK.tIDENT) THEN RETURN END;

    (* skip the list of formals *)
    ParseGenericArgs (s, save := FALSE);

    EVAL Match (s, TK.tSEMI);
  END PushGeneric;

PROCEDURE ParseGenericArgs (VAR s: State;  save: BOOLEAN) =
  BEGIN
    IF NOT Match (s, TK.tLPAREN) THEN RETURN END;
    MatchIDList (s, save);
    IF NOT Match (s, TK.tRPAREN) THEN RETURN END;
  END ParseGenericArgs;

PROCEDURE ParseImports (VAR s: State) =
  BEGIN
    LOOP
      IF    (cur.token = TK.tIMPORT) THEN ParseImport (s);
      ELSIF (cur.token = TK.tFROM)   THEN ParseFromImport (s);
      ELSE  EXIT;
      END;
    END;
  END ParseImports;

PROCEDURE ParseImport (VAR s: State) =
  VAR id: M3ID.T;
  BEGIN
    GetToken (); (* IMPORT *)
    LOOP
      IF NOT MatchID (s, id) THEN RETURN END;
      PushID (s, id);

      IF (cur.token = TK.tAS) THEN
        GetToken (); (* AS *)
        IF NOT MatchID (s, id) THEN RETURN END;
      END;

      IF (cur.token # TK.tCOMMA) THEN EXIT END;
      GetToken (); (* , *)
    END;
    EVAL Match (s, TK.tSEMI);
  END ParseImport;

PROCEDURE ParseFromImport (s: State) =
  VAR id: M3ID.T;
  BEGIN
    GetToken (); (* FROM *)
    IF NOT MatchID (s, id) THEN RETURN END;
    PushID (s, id);
    IF NOT Match (s, TK.tIMPORT) THEN RETURN END;
    MatchIDList (s, save := FALSE);
    EVAL Match (s, TK.tSEMI);
  END ParseFromImport;

PROCEDURE MatchIDList (VAR s: State;  save: BOOLEAN) =
  VAR id: M3ID.T;
  BEGIN
    LOOP
      IF NOT MatchID (s, id) THEN RETURN END;
      IF save THEN PushID (s, id); END;
      IF (cur.token # TK.tCOMMA) THEN EXIT; END;
      GetToken (); (* , *)
    END;
  END MatchIDList;

PROCEDURE Match (VAR s: State;  t: TK): BOOLEAN =
  BEGIN
    IF (s.failed) OR (cur.token # t) THEN
      s.failed := TRUE;
      RETURN FALSE;
    END;
    GetToken ();
    RETURN TRUE;
  END Match;

PROCEDURE MatchID (VAR s: State;  VAR(*OUT*) id: M3ID.T): BOOLEAN =
  BEGIN
    IF (s.failed) OR (cur.token # TK.tIDENT) THEN
      s.failed := TRUE;
      RETURN FALSE;
    END;
    id := cur.id;
    GetToken ();
    RETURN TRUE;
  END MatchID;

PROCEDURE PushID (VAR s: State;  id: M3ID.T) =
  BEGIN
    s.imports := NEW (IDList, interface := id, next := s.imports);
  END PushID;

BEGIN
END M3Header.
