(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Tue Sep 27 14:10:09 PDT 1994 by weich      *)
(*      modified on Thu Dec  3 16:27:43 PST 1992 by owicki     *)

(* Walk through a "M3Context.T" and add a property to each
   node that represents a type specification that is not
   a named type. The property added is the name of the type
   specified (precicly one name of the type).

   This typename is later used instead of the type specification
   (see "AstToType.AddToTable").
*)

INTERFACE TypeNames;

IMPORT M3Context;

PROCEDURE Preprocess(c: M3Context.T);

END TypeNames.
