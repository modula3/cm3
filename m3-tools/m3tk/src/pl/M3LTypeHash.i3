INTERFACE M3LTypeHash;

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

IMPORT M3AST_AS;


(* This module provides a quick way to distinguish types by using a type class
and a hash value. These have the property that if two types have the same class
and the same hash value they may be identical; if they do not they are
guaranteed to be non identical.

  The hash function used depends on names and default expressions within the
type, if present. In the case of constructor types it can also depend on the
hash values of the component types (e.g. when calculating the hash value for a
packed type the hash value of the unpacked type may be used). To prevent too
much recursion the hash values of component types are only used when the class
of the component type is less than the class of the constructor type.
  So, for example, the hash value of a packed array will not contain the hash
value of the array type because array types appear after packed types in the
'Class' ordering. This property is relied upon by the type equivalence
algorithm which uses the hash functions provided in this module.
  Note that 'Branded' covers branded objects, branded refs and opaque types -
because these are globally unique types they can be assigned a unique hash
value immediately. 
  The "Any" and "Type" cases cover the "Any_type" and "Type_type"
  types used in the standard interface (only). *)


(* Type classification *)

TYPE
  Class = {Null, Integer, Real, LongReal, Extended,
      RefAny, Address, Root, UntracedRoot,
      Enumeration, IntegerSubrange, EnumerationSubrange, Set, Packed,
      Array, OpenArray, Procedure, Function, Record,
      Object, SubClassObject, Ref, UntracedRef,
      Branded, Any, Type};

  ClassSet = SET OF Class;

  BasicType = [Class.Null..Class.UntracedRoot];
  ConstructorType = [Class.Enumeration..Class.UntracedRef];
  NonRecursiveType = [Class.Enumeration..Class.Set];


CONST
  SetOfAllBasicTypes = ClassSet{FIRST(BasicType)..LAST(BasicType)};
  SetOfAllConstructorTypes =
      ClassSet{FIRST(ConstructorType)..LAST(ConstructorType)};
  SetOfAllNonRecursiveTypes =
      ClassSet{FIRST(NonRecursiveType)..LAST(NonRecursiveType)};


PROCEDURE Classify(t: M3AST_AS.TYPE_SPEC): Class RAISES {};
(* classify the given type *)


(* Hash values *)

PROCEDURE Value(
    t: M3AST_AS.TYPE_SPEC;
    class: ConstructorType)
    : INTEGER
    RAISES {};
(* hash value of type with known class. 'class' must match 't' and 't' must
not be an opaque type with a known concrete type or chaos will ensue  *)

END M3LTypeHash.
