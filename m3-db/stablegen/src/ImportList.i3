(* Copyright (C) 1989, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(* Created by Susan Owicki                                  *)
(* Last modified on Sat Sep  3 14:53:26 PDT 1994 by weich   *)
(* modified On Fri Feb 18 17:30:06 PST 1994 by kalsow       *)
(* Modified On Mon May 17 13:26:35 PDT 1993 by mjordan      *)
(* Modified On Thu Apr 22 11:43:51 PDT 1993 by owicki       *)

(* A "ImportList.T" is a set of interface names. The procedure
   "Get" generates such a list. It takes a "Type.Object" and looks
   at all its components to construct a list of all necessary
   imports to represent the type in a Modula-3 program.

   A procedure "Add" can be used to add other interfaces to the
   list. "ToText" produces a comma separated list of all
   interface names (useful for "IMPORT" statements).
*)

INTERFACE ImportList;

IMPORT Atom, Type;

TYPE
  T <: REFANY;
  MethodList = REF ARRAY OF Method;
  Method = RECORD
             name: Atom.T;
             sig : Type.Signature
           END;

PROCEDURE FromType (type: Type.Object; methods: MethodList): T;
(* Generate a new import list.  Look for fields in "type" and
   for signature of methods in "methods". *)


PROCEDURE Add (importList: T; intf: Atom.T);
(* Add the interface name "intf" to "importList".  This is a
   no-op if "intf" is already in the list *)

PROCEDURE ToText (imports: T): TEXT;
(* Generate a comma separated list of interface names out of
   "imports".  The output is suitable for an "IMPORT"
   statement. *)

END ImportList.
