MODULE ASTNodeArray;

(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

IMPORT AST, AST_Iter; 

PROCEDURE Number(n: AST.NODE): CARDINAL RAISES {}=
  VAR i := 0; iter := n.newIter(); child: AST.NODE;
  BEGIN
    WHILE iter.next(child) DO INC (i) END; (* while *)
    RETURN i;
  END Number;

PROCEDURE High(n: AST.NODE): INTEGER RAISES {}=
  BEGIN
    RETURN Number(n)-1;
  END High;

EXCEPTION BadIndex;

PROCEDURE Ith(n: AST.NODE; i: CARDINAL): AST.NODE RAISES {}=
  <*FATAL BadIndex*>
  VAR iter := n.newIter(); child: AST.NODE;
  BEGIN
    FOR j := 0 TO i DO
      IF NOT iter.next(child) THEN RAISE BadIndex END;
    END; (* for *)
    RETURN child;
  END Ith;


BEGIN

END ASTNodeArray.
