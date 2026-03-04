(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

MODULE ESCNameMap;

IMPORT Atom;

PROCEDURE FieldName(module, type, field: Atom.T): Atom.T =
  BEGIN
    RETURN Atom.FromText(Atom.ToText(module) & "." &
                         Atom.ToText(type) & "." &
                         Atom.ToText(field));
  END FieldName;

PROCEDURE TypePred(typeName: Atom.T): Atom.T =
  BEGIN
    RETURN Atom.FromText("Is$" & Atom.ToText(typeName));
  END TypePred;

PROCEDURE FuncName(module, var: Atom.T): Atom.T =
  BEGIN
    RETURN Atom.FromText("FUNC." & Atom.ToText(module) & "." &
                         Atom.ToText(var));
  END FuncName;

PROCEDURE ResidueName(module, var: Atom.T): Atom.T =
  BEGIN
    RETURN Atom.FromText("RESIDUE." & Atom.ToText(module) & "." &
                         Atom.ToText(var));
  END ResidueName;

PROCEDURE TypeCode(typeName: Atom.T): Atom.T =
  BEGIN
    RETURN Atom.FromText(Atom.ToText(typeName) & ".TYPECODE");
  END TypeCode;

PROCEDURE QualName(module, name: Atom.T): Atom.T =
  BEGIN
    RETURN Atom.FromText(Atom.ToText(module) & "." &
                         Atom.ToText(name));
  END QualName;

PROCEDURE PipeName(name: TEXT): Atom.T =
  BEGIN
    RETURN Atom.FromText(name);
  END PipeName;

BEGIN
  Nil := Atom.FromText("$NIL");
  AtTrue := Atom.FromText("@true");
  AtFalse := Atom.FromText("@false");
  IntFirst := Atom.FromText("INTEGER.FIRST");
  IntLast := Atom.FromText("INTEGER.LAST");
  Return := Atom.FromText("RETURN");
  Exit := Atom.FromText("EXIT");
END ESCNameMap.
