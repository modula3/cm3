(*| Copyright (C) 1993, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)
(*| Last modified on Thu Jul 14 11:33:02 PDT 1994 by kalsow     *)
(*|      modified on Tue Apr 27 11:42:41 PDT 1993 by mcjones    *)
(*|      modified on Sun Feb 21 14:27:08 PST 1993 by jdd        *)
(*|      modified on Tue Sep 25 00:38:09 1990 by muller         *)

(*
  "RTTypeSRC" provides SRC Modula-3 extensions to RTType and RTTypeFP.
*)

INTERFACE RTTypeSRC;

IMPORT RT0;

PROCEDURE TypeName (ref: REFANY): TEXT;
(* = TypecodeName (TYPECODE (ref)) *)

PROCEDURE TypecodeName (tc: RT0.Typecode): TEXT;
(* Returns a text describing the ref whose typecode = "tc".  Its
   heuristic is to consider, in order, "named typecells", brands, arrays,
   object-subtypes, objects, and procedures.  If all else fails, it returns
   "<?>". *)

PROCEDURE FindType (id: INTEGER): RT0.TypeDefn;
(* Returns the type descriptor with UID id.  If no such type exists,
   returns NIL. *)

(*------------------------------------------------------------------ setup --*)

PROCEDURE AddTypecell (tc: RT0.TypeDefn;  m: RT0.ModulePtr);
(* add "tc" to the list of registered typecells *)

PROCEDURE NoteFullRevelation (r: RT0.RevPtr;  m: RT0.ModulePtr);
(* Declare the opaque-concrete type binding in "r". *)

PROCEDURE ResolveTypeLink (uid: INTEGER;  t: RT0.TypeLinkPtr;  m: RT0.ModulePtr);
(* Fixes "t" to refer to the type corresponding to "uid".  It is a
   checked runtime error if the type is not known. *)

PROCEDURE FinishObjectTypes ();
(* Finish initializing any OBJECT typecells.  It is a checked runtime
   error if any of the object typecells refers to an unknown supertype. *)

PROCEDURE VerifyPartialRevelation (r: RT0.RevPtr;  m: RT0.ModulePtr);
(* Verify that revelation "r" from module "m" is consistent with
   the rest of the registered types. *)

END RTTypeSRC.
