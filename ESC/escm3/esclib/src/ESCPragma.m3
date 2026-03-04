(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

MODULE ESCPragma;

IMPORT M3AST_AS, M3CPragma, M3CSrcPos;
IMPORT RefList, Atom, Text;
IMPORT ESCSpec;
IMPORT M3AST_all; (* must be imported to reveal Compilation_Unit fields *)

(* Sentinel value set as hook on processed pragmas *)
TYPE HookMarker = BRANDED "ESCPragma.Hook" REF INTEGER;
VAR hookMarker: REFANY;

PROCEDURE Extract(cu: M3AST_AS.Compilation_Unit;
                  unitName: Atom.T): RefList.T =
  VAR
    iter := M3CPragma.NewIter(cu.lx_pragmas);
    pragma: M3CPragma.T;
    args: TEXT;
    result: RefList.T := NIL;
    last: RefList.T := NIL;
    raw: ESCSpec.RawPragma;
    pos: M3CSrcPos.T;
    kind: ESCSpec.PragmaKind;
    matched: BOOLEAN;
  BEGIN
    WHILE M3CPragma.Next(iter, pragma) DO
      matched := FALSE;
      IF M3CPragma.Match(pragma, "SPEC", args) THEN
        kind := ESCSpec.PragmaKind.Spec;
        matched := TRUE;
      ELSIF M3CPragma.Match(pragma, "LOOPINV", args) THEN
        kind := ESCSpec.PragmaKind.LoopInv;
        matched := TRUE;
      ELSIF M3CPragma.Match(pragma, "PRAGMA", args) THEN
        (* <*PRAGMA SPEC*> -- the directive that enables SPEC pragmas.
           Mark as processed but don't produce a RawPragma. *)
        IF args # NIL AND TextStartsWith(args, "SPEC") THEN
          M3CPragma.SetHook(pragma, hookMarker);
        END;
      END;

      IF matched THEN
        pos := M3CPragma.Position(pragma);
        raw := NEW(ESCSpec.RawPragma,
                   kind := kind,
                   text := args,
                   line := pos.line,
                   col := pos.col,
                   unitName := unitName);

        (* Append to result list in source order *)
        WITH cell = RefList.List1(raw) DO
          IF last = NIL THEN
            result := cell;
          ELSE
            last.tail := cell;
          END;
          last := cell;
        END;

        (* Mark pragma as processed *)
        M3CPragma.SetHook(pragma, hookMarker);
      END;
    END;

    RETURN result;
  END Extract;

PROCEDURE HasSpecs(cu: M3AST_AS.Compilation_Unit): BOOLEAN =
  VAR
    iter := M3CPragma.NewIter(cu.lx_pragmas);
    pragma: M3CPragma.T;
    args: TEXT;
  BEGIN
    WHILE M3CPragma.Next(iter, pragma) DO
      IF M3CPragma.Match(pragma, "SPEC", args) OR
         M3CPragma.Match(pragma, "LOOPINV", args) THEN
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END HasSpecs;

PROCEDURE TextStartsWith(t, prefix: TEXT): BOOLEAN =
  BEGIN
    IF Text.Length(t) < Text.Length(prefix) THEN RETURN FALSE END;
    RETURN Text.Equal(Text.Sub(t, 0, Text.Length(prefix)), prefix);
  END TextStartsWith;

BEGIN
  hookMarker := NEW(HookMarker);
END ESCPragma.
