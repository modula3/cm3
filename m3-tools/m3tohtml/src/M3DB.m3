(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Dec 15 09:44:34 PST 1994 by kalsow                   *)

MODULE M3DB;

IMPORT Rd, TextList, TextDB, M3Scanner, Text;

VAR
  db := NEW (TextDB.T).init ();
  imports     : TextDB.Relation;
  exports     : TextDB.Relation;
  revealsTo   : TextDB.Relation;
  revealsType : TextDB.Relation;
  definesType : TextDB.Relation;
  definesProc : TextDB.Relation;
  interface   : TextDB.Relation;
  module      : TextDB.Relation;
  genericIntf : TextDB.Relation;
  genericMod  : TextDB.Relation;

PROCEDURE Open (path: TEXT) =
  BEGIN
    db.load (path);
    imports     := db.create_relation ("imports");
    exports     := db.create_relation ("exports");
    revealsTo   := db.create_relation ("revealsTo");
    revealsType := db.create_relation ("revealsType");
    definesType := db.create_relation ("type");
    definesProc := db.create_relation ("procedure");
    interface   := db.create_relation ("interface");
    module      := db.create_relation ("module");
    genericIntf := db.create_relation ("genericInterface");
    genericMod  := db.create_relation ("genericModule");
  END Open;

PROCEDURE Dump (path: TEXT) =
  BEGIN
    db.dump (path);
  END Dump;

(*--------------------------------------------------------------- queries ---*)

PROCEDURE Imports (interface: TEXT): TextList.T =
  BEGIN
    RETURN imports.getValue (interface);
  END Imports;

PROCEDURE Exports (interface: TEXT): TextList.T =
  BEGIN
    RETURN exports.getValue (interface);
  END Exports;

PROCEDURE RevealsTo (interface: TEXT): TextList.T =
  BEGIN
    RETURN revealsTo.getValue (interface);
  END RevealsTo;

PROCEDURE RevealsType (type: TEXT): TextList.T =
  BEGIN
    RETURN revealsType.getValue (type);
  END RevealsType;

PROCEDURE DefinesType (type: TEXT): TextList.T =
  BEGIN
    RETURN definesType.getValue (type);
  END DefinesType;

PROCEDURE DefinesProc (proc: TEXT): TextList.T =
  BEGIN
    RETURN definesProc.getValue (proc);
  END DefinesProc;

PROCEDURE Interface (nm: TEXT): TextList.T =
  BEGIN
    RETURN interface.getValue (nm);
  END Interface;

PROCEDURE Module (nm: TEXT): TextList.T =
  BEGIN
    RETURN module.getValue (nm);
  END Module;

PROCEDURE GenericIntf (nm: TEXT): TextList.T =
  BEGIN
    RETURN genericIntf.getValue (nm);
  END GenericIntf;

PROCEDURE GenericMod (nm: TEXT): TextList.T =
  BEGIN
    RETURN genericMod.getValue (nm);
  END GenericMod;

(*---------------------------------------------------------------- update ---*)

PROCEDURE DeleteUnit (path: TEXT) =
  VAR x := db.all_relations ();
  BEGIN
    WHILE (x # NIL) DO
      NARROW (x.head, TextDB.Relation).deleteValue (path);
      x := x.tail;
    END;
  END DeleteUnit;

PROCEDURE AddUnit (rd: Rd.T;  path: TEXT) =
  VAR lex := NEW (M3Scanner.Default).initFromRd (rd,
                                           skip_comments := TRUE,
                                           split_pragmas := FALSE);
  BEGIN
    NextToken (lex); (* skip the initial comment *)
    UpdateUnit (lex, path);
  END AddUnit;

PROCEDURE UpdateUnit (lex: M3Scanner.T;  path: TEXT) =
  VAR unit: TEXT;
  BEGIN
    IF lex.token = M3Scanner.TK_Unsafe THEN NextToken (lex); END;

    IF lex.token = M3Scanner.TK_Generic THEN
      NextToken (lex); (*GENERIC*)
      IF lex.token = M3Scanner.TK_Interface THEN
        NextToken (lex); (*INTERFACE*)
        IF NOT GetID (lex, unit) THEN RETURN; END;
        genericIntf.insert (unit, path);
        SkipToSemi (lex);
        UpdateImports (lex, path);
        UpdateDecls (lex, path, TRUE);

      ELSIF lex.token = M3Scanner.TK_Module THEN
        NextToken (lex); (*MODULE*)
        IF NOT GetID (lex, unit) THEN RETURN; END;
        genericMod.insert (unit, path);
        SkipToSemi (lex);
        UpdateImports (lex, path);
        UpdateDecls (lex, path, FALSE);

      ELSE (* error *)
        RETURN;
      END;

    ELSIF lex.token = M3Scanner.TK_Interface THEN
      NextToken (lex); (*INTERFACE*)
      IF NOT GetID (lex, unit) THEN RETURN; END;
      interface.insert (unit, path);
      IF lex.token = M3Scanner.TK_Semi THEN
        NextToken (lex); (* ; *)
        UpdateImports (lex, path);
        UpdateDecls (lex, path, TRUE);
      ELSIF lex.token = M3Scanner.TK_Equal THEN
        NextToken (lex); (* = *)
        UpdateGenericInstance (lex, path);
      ELSE RETURN;
      END;

    ELSIF lex.token = M3Scanner.TK_Module THEN
      NextToken (lex); (*MODULE*)
      IF NOT GetID (lex, unit) THEN RETURN; END;
      module.insert (unit, path);
      UpdateExports (lex, unit, path);
      IF lex.token = M3Scanner.TK_Semi THEN
        NextToken (lex); (* ; *)
        UpdateImports (lex, path);
        UpdateDecls (lex, path, FALSE);
      ELSIF lex.token = M3Scanner.TK_Equal THEN
        NextToken (lex); (* = *)
        UpdateGenericInstance (lex, path);
      ELSE RETURN;
      END;

    ELSE (* error *)
      RETURN;
    END;
  END UpdateUnit;

PROCEDURE UpdateExports (lex: M3Scanner.T;  unit, path: TEXT) =
  VAR id: TEXT;
  BEGIN
    IF lex.token # M3Scanner.TK_Exports THEN
      exports.insert (unit, path);
    ELSE
      NextToken (lex); (*EXPORTS*)
      LOOP
        IF NOT GetID (lex, id) THEN EXIT END;
        exports.insert (id, path);
        IF lex.token # M3Scanner.TK_Comma THEN EXIT END;
        NextToken (lex); (* , *)
      END;
    END;
  END UpdateExports;

PROCEDURE UpdateImports (lex: M3Scanner.T;  path: TEXT) =
  VAR id: TEXT;
  BEGIN
    LOOP
      IF lex.token = M3Scanner.TK_Import THEN
        NextToken (lex); (*IMPORT*)
        LOOP
          IF NOT GetID (lex, id) THEN EXIT END;
          imports.insert (id, path);
          IF lex.token = M3Scanner.TK_As THEN
            NextToken (lex); (*IMPORT*)
            IF lex.token = M3Scanner.TK_Ident THEN
              NextToken (lex); (*ID*)
            END;
          END;
          IF lex.token # M3Scanner.TK_Comma THEN EXIT END;
          NextToken (lex); (* , *)
        END;
      ELSIF lex.token = M3Scanner.TK_From THEN
        NextToken (lex); (*FROM*)
        IF GetID (lex, id) THEN imports.insert (id, path); END;
      ELSE EXIT;
      END;
      SkipToSemi (lex);
    END;
  END UpdateImports;

PROCEDURE UpdateGenericInstance (lex: M3Scanner.T;  path: TEXT) =
  VAR id: TEXT;
  BEGIN
    IF NOT GetID (lex, id) THEN RETURN END;
    imports.insert (id, path);
    IF lex.token # M3Scanner.TK_L_paren THEN RETURN END;
    NextToken (lex); (* ( *)
    LOOP
      IF NOT GetID (lex, id) THEN EXIT END;
      imports.insert (id, path);
      IF lex.token # M3Scanner.TK_Comma THEN EXIT END;
      NextToken (lex); (* , *)
    END;
  END UpdateGenericInstance;

PROCEDURE UpdateDecls (lex: M3Scanner.T;  path: TEXT;  is_intf: BOOLEAN) =
  VAR id, unit: TEXT;  eq: BOOLEAN;
  BEGIN
    LOOP
      CASE lex.token OF
      | M3Scanner.TK_Type =>
          NextToken (lex); (*TYPE*)
          WHILE FindTypeID (lex, id, unit, eq) DO
            definesType.insert (id, path);
          END;

      | M3Scanner.TK_Procedure =>
          NextToken (lex); (*PROCEDURE*)
          IF is_intf THEN
            SkipToSemi (lex);
          ELSE
            IF GetID (lex, id) THEN
              definesProc.insert (id, path);
              SkipProc (lex, id);
            END;
          END;

      | M3Scanner.TK_Reveal =>
          NextToken (lex); (*REVEALS*)
          WHILE FindTypeID (lex, id, unit, eq) DO
            IF eq THEN
              IF (unit # NIL) THEN revealsTo.insert (unit, path); END;
              revealsType.insert (id, path);
            END;
          END;

      | M3Scanner.TK_EOF, M3Scanner.TK_Error => EXIT;
      ELSE NextToken (lex);
      END;
    END;
  END UpdateDecls;

PROCEDURE FindTypeID (lex: M3Scanner.T;  VAR id, unit: TEXT;
                      VAR eq: BOOLEAN): BOOLEAN =
  BEGIN
    LOOP
      id := NIL;
      unit := NIL;
      CASE lex.token OF
      | M3Scanner.TK_Ident =>
          EVAL GetID (lex, id);
          IF lex.token = M3Scanner.TK_Dot THEN
            NextToken (lex); (* . *)
            IF lex.token = M3Scanner.TK_Ident THEN
              unit := id;
              EVAL GetID (lex, id);
            END;
          END;
          IF lex.token = M3Scanner.TK_Equal THEN
            NextToken (lex); (* = *)
            eq := TRUE;
            RETURN TRUE;
          ELSIF lex.token = M3Scanner.TK_Subtype THEN
            NextToken (lex); (* <: *)
            eq := FALSE;
            RETURN TRUE;
          ELSE (* skip *)
          END;

      | M3Scanner.TK_L_paren =>
          SkipParens (lex);

      | M3Scanner.TK_Const, M3Scanner.TK_Type, M3Scanner.TK_Exception, M3Scanner.TK_Var, M3Scanner.TK_Procedure, M3Scanner.TK_Reveal,
        M3Scanner.TK_Begin, M3Scanner.TK_EOF, M3Scanner.TK_Error =>
          RETURN FALSE;
 
      ELSE
          NextToken (lex); 
      END; (*CASE*)
    END; (*LOOP*)
  END FindTypeID;

PROCEDURE SkipParens (lex: M3Scanner.T) =
  VAR depth: INTEGER := 0;
  BEGIN
    LOOP
      IF lex.token = M3Scanner.TK_L_paren THEN
        INC (depth);
      ELSIF lex.token = M3Scanner.TK_R_paren THEN
        DEC (depth);
        IF (depth <= 0) THEN NextToken (lex); RETURN END;
      ELSIF lex.token = M3Scanner.TK_EOF THEN
        RETURN;
      ELSIF lex.token = M3Scanner.TK_Error THEN
        RETURN;
      END;
      NextToken (lex);
    END;
  END SkipParens;

PROCEDURE SkipProc (lex: M3Scanner.T;  proc_id: TEXT) =
  VAR id: TEXT;
  BEGIN
    LOOP
      IF lex.token = M3Scanner.TK_End THEN
        NextToken (lex); (*END*)
        IF GetID (lex, id) AND Text.Equal (proc_id, id) THEN EXIT; END;
      ELSIF lex.token = M3Scanner.TK_EOF OR lex.token = M3Scanner.TK_Error THEN
        EXIT;
      ELSE
        NextToken (lex);
      END;
    END;
  END SkipProc;

PROCEDURE GetID (lex: M3Scanner.T;  VAR id: TEXT): BOOLEAN =
  BEGIN
    IF lex.token # M3Scanner.TK_Ident THEN  id := NIL; RETURN FALSE;  END;
    id := Text.FromChars (SUBARRAY(lex.buffer^, lex.offset,lex.length));
    NextToken (lex);
    RETURN TRUE;
  END GetID;

PROCEDURE SkipToSemi (lex: M3Scanner.T) =
  BEGIN
    WHILE (lex.token # M3Scanner.TK_Semi)
      AND (lex.token # M3Scanner.TK_EOF)
      AND (lex.token # M3Scanner.TK_Error) DO
      NextToken (lex);
    END;
    IF (lex.token = M3Scanner.TK_Semi) THEN NextToken (lex); END;
  END SkipToSemi;

PROCEDURE NextToken (lex: M3Scanner.T) =
  BEGIN
    REPEAT
      lex.next ();
    UNTIL (lex.token # M3Scanner.TK_Begin_pragma);
  END NextToken;

BEGIN
END M3DB.


