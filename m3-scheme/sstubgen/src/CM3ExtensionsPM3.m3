(* $Id$ *)

MODULE CM3ExtensionsPM3 EXPORTS CM3Extensions;
IMPORT RefRefTbl;
IMPORT M3CStdTypes;
IMPORT Type;
FROM AstToType IMPORT Handle;
IMPORT M3AST_AS;
IMPORT SchemeObject, SchemeLongReal;

PROCEDURE InitAstTable(astTable : RefRefTbl.T) =
  BEGIN
    (* skip *)
  END InitAstTable;

PROCEDURE  ProcessTypeSpec(h: Handle; ts: M3AST_AS.TYPE_SPEC) : Type.T =
  BEGIN
    RETURN NIL
  END ProcessTypeSpec;

PROCEDURE TranslateLongintRef(l : REFANY) : SchemeObject.T =
  BEGIN 
    <*ASSERT FALSE *>
  END TranslateLongintRef;

BEGIN END CM3ExtensionsPM3.

