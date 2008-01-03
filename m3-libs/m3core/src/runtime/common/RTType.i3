(*| Copyright (C) 1994, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)
(*|                                                             *)
(*| Last modified on Wed May 25 15:22:06 PDT 1994 by kalsow     *)
(*|      modified on Tue Nov  9 12:35:01 PST 1993 by mcjones    *)
(*|      modified on Wed Jun  2 14:58:06 PDT 1993 by muller     *)
(*|      modified on Sun Feb 21 14:26:52 PST 1993 by jdd        *)

(*
  "RTType" provides access to the runtime type system.
  \index{runtime type}
  \index{type!runtime}

  Each reference type is assigned a unique typecode.  A typecode is
  ``proper'' if it lies in the range "[0..MaxTypecode()]".  The proper
  typecodes include all those that correspond to actual types in the
  running Modula-3 program.  Other typecodes, proper and improper, may
  be used internally by the runtime system and garbage collector.

  Although the language requires that typecodes exist only for object
  types and for traced reference types (including "NULL"), the
  implementation of "RTType" also provides typecodes for untraced
  reference types.

  The values returned by the builtin operation "TYPECODE" correspond
  to (a subset of) the proper typecodes.
*)

INTERFACE RTType;

IMPORT RT0;

TYPE Typecode = RT0.Typecode;

CONST NoSuchType: Typecode = LAST(Typecode);
(* A reserved typecode that represents unknown types. *)

PROCEDURE MaxTypecode(): Typecode;
(* Return the largest proper typecode. *)

PROCEDURE IsSubtype(a, b: Typecode): BOOLEAN;
(* Return "TRUE" iff the type corresponding to "a" is a subtype of the
   type corresponding to "b".  It is a checked runtime error if either
   "a" or "b" is not a proper typecode. *)

PROCEDURE Supertype(tc: Typecode): Typecode;
(* Return the typecode of the declared supertype of the object type
   corresponding to "tc".  If "tc" corresponds to "ROOT", "UNTRACED
   ROOT" or a non-object reference type, return "NoSuchType".  It is a
   checked runtime error if "tc" is not a proper typecode. *)

PROCEDURE IsTraced(tc: Typecode): BOOLEAN;
(* Return "TRUE" iff the type corresponding to "tc" is traced. *)

PROCEDURE Get(tc: Typecode): RT0.TypeDefn;
(* Return a pointer to the typecell with typecode "tc".  It is a
   checked runtime error to pass an improper typecode. *)

PROCEDURE GetNDimensions(tc: Typecode): CARDINAL;
(* Return the number of open dimensions of the open array type that
   corresponds to "tc"'s referent.  If "tc"'s referent is not an open
   array, return "0". *)

END RTType.
