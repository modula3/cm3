(* $Id$ *)

MODULE CM3Extensions;
IMPORT RefRefTbl;
IMPORT M3CStdTypes;
IMPORT Type;
FROM AstToType IMPORT Handle;
IMPORT M3AST_AS;
IMPORT SchemeObject, SchemeLongReal;

PROCEDURE InitAstTable(astTable : RefRefTbl.T) =
  BEGIN
    EVAL astTable.put(M3CStdTypes.WideChar(), Type.widechar);
    EVAL astTable.put(M3CStdTypes.Longint(), Type.longint);
  END InitAstTable;

PROCEDURE  ProcessTypeSpec(h: Handle; ts: M3AST_AS.TYPE_SPEC) : Type.T =
  BEGIN
    TYPECASE ts OF
      M3AST_AS.WideChar_type => RETURN Type.widechar
    |
      M3AST_AS.Longint_type => RETURN Type.longint
    ELSE
      RETURN NIL
    END
  END ProcessTypeSpec;

PROCEDURE TranslateLongintRef(l : REFANY) : SchemeObject.T =
  BEGIN 
    RETURN SchemeLongReal.FromLR(FLOAT(NARROW(l,REF LONGINT)^,
                                       LONGREAL)) 
  END TranslateLongintRef;



BEGIN END CM3Extensions.

