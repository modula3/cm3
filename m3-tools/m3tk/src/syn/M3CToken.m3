MODULE M3CToken;

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

IMPORT M3AST_LX;

(* An "M3AST_LX.Token_rep" is represented as a reference to the
corresponding "T" value. *)

REVEAL
  M3AST_LX.Token_rep = BRANDED REF T;

VAR
  table_g: ARRAY T OF M3AST_LX.Token_rep;

PROCEDURE Token_rep(t: T): M3AST_LX.Token_rep=
  BEGIN
    RETURN table_g[t];
  END Token_rep;

PROCEDURE Token_repToText(tr: M3AST_LX.Token_rep): TEXT=
  BEGIN
    WITH t = tr^, e = VAL(t, E) DO
      IF t IN ReservedWords THEN RETURN ReservedWordTexts[e]
      ELSIF t IN ReservedTokens THEN RETURN ReservedTokenTexts[e]
      ELSE <*ASSERT FALSE*>
      END
    END
  END Token_repToText;

BEGIN
  FOR i := FIRST(T) TO LAST(T) DO
    table_g[i] := NEW(M3AST_LX.Token_rep);
    table_g[i]^ := i;
  END;
END M3CToken.
