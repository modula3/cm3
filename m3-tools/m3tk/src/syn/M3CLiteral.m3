MODULE M3CLiteral;

(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)

IMPORT Text;
IMPORT M3AST_LX, M3CHash;


REVEAL
  M3AST_LX.Number_rep = T BRANDED OBJECT END;
  M3AST_LX.Text_rep = T BRANDED OBJECT END;


PROCEDURE NewLiteral(<*UNUSED*> c: M3CHash.IdCreator;
                     text: Text.T): M3CHash.Id RAISES {}=
  BEGIN
    WITH ch = Text.GetChar(text, 0) DO
      IF ch = '\"' OR ch = '\''  THEN
        RETURN NEW(M3AST_LX.Text_rep);
      ELSE
        RETURN NEW(M3AST_LX.Number_rep);
      END;
    END; 
  END NewLiteral;


PROCEDURE NewLiteralCreator(): M3CHash.IdCreator RAISES {}=
  TYPE
    LiteralCreator = M3CHash.IdCreator OBJECT OVERRIDES new := NewLiteral END;
  BEGIN
    RETURN NEW(LiteralCreator);
  END NewLiteralCreator;


VAR
  table_g := NEW(M3CHash.Table).init(10240, NewLiteralCreator());


<*INLINE*> PROCEDURE Table(): M3CHash.Table RAISES {}=
  BEGIN
    RETURN table_g;
  END Table;


<*INLINE*> PROCEDURE ToText(id: T): Text.T RAISES {}=
  BEGIN
    RETURN id.toText();
  END ToText;


<*INLINE*> PROCEDURE Enter(text: Text.T): T RAISES {}=
  BEGIN
    RETURN table_g.enter(text);
  END Enter;


BEGIN

END M3CLiteral.
