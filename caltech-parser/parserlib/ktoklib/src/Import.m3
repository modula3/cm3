(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE Import;
IMPORT Text;
IMPORT TextIntTbl;
REVEAL
  T = Public BRANDED OBJECT
    modules: TextIntTbl.T;
  OVERRIDES
    init := Init;
    addModule := AddModule;
    addType := AddType;
    toDeclaration := ToDeclaration;
  END;

PROCEDURE Init(self: T): T =
  BEGIN
    self.modules := NEW(TextIntTbl.Default).init();
    RETURN self;
  END Init;

PROCEDURE AddModule(self: T; MN: TEXT) =
  BEGIN
    EVAL self.modules.put(MN, 0);
  END AddModule;

PROCEDURE AddType(self: T; m3typeName: TEXT) =
  VAR
    pos := Text.FindChar(m3typeName, '.');
  BEGIN
    IF pos # -1 THEN
      self.addModule(Text.Sub(m3typeName, 0, pos));
    END;
  END AddType;

PROCEDURE ToDeclaration(self: T): TEXT =
  VAR
    iterate := self.modules.iterate();
    MN: TEXT;
    dummy: INTEGER;
    result: TEXT := "";
  BEGIN
    WHILE iterate.next(MN, dummy) DO
      result := result & ("IMPORT " & MN & ";\n");
    END;
    RETURN result;
  END ToDeclaration;

BEGIN
END Import.
