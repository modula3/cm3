(* Copyright (C) 1989, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(* Created by Susan Owicki                                  *)
(* Last modified on Wed Sep 21 09:46:56 PDT 1994 by weich   *)
(*      modified On Fri Feb 18 17:30:06 PST 1994 by kalsow  *)
(*      Modified On Mon May 17 13:26:35 PDT 1993 by mjordan *)
(*      Modified On Thu Apr 22 11:43:51 PDT 1993 by owicki  *)

(* Provide a procedure to produce Modula-3 code out of type
   structures. Provide a procedure to generate procedure
   header Modula-3 code. *)

INTERFACE GenTypeCode;

IMPORT Formatter, Type;

PROCEDURE ToText (t: Type.T; byName: BOOLEAN := TRUE): TEXT;
(* Return a textual representation of the type t.  If byName
   = TRUE, and type T is named, then return the name.
   Otherwise return text for the delcaration of T.  Note
   that even with byName = FALSE, typeNames will be used
   within the text for t's declaration *)

PROCEDURE ProcHeader (f         : Formatter.T;
                      procName  : TEXT;
                      sig       : Type.Signature;
                      suffix                        := "";
                      argPragmas: REF ARRAY OF TEXT := NIL );
(* Output on "f" a header for a procedure with name
   "procName" and a signature derived from "sig".  The
   signature differs from "sig" in that it's first argument
   is a self argument ("self: T"), and its formal
   parameters' names are suffixed by "suffix". 
   "pragmas" is a list of text for pragmas to precede arguments;
   "pragma[i]" preceeds the ith parameter in "sig.formals[i]". *)

END GenTypeCode.
