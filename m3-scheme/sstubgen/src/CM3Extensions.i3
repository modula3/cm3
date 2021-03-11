(* $Id$ *)

INTERFACE CM3Extensions;
IMPORT RefRefTbl;
FROM AstToType IMPORT Handle;
IMPORT Type, M3AST_AS;
IMPORT SchemeObject;

PROCEDURE InitAstTable(astTable : RefRefTbl.T);

PROCEDURE ProcessTypeSpec(h: Handle; ts: M3AST_AS.TYPE_SPEC) : Type.T;

PROCEDURE TranslateLongintRef(val : REFANY (* REF LONGINT *)) : SchemeObject.T;

END CM3Extensions.
