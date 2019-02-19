(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Type.i3                                             *)
(* Last Modified On Mon Feb 28 16:44:35 PST 1994 by wobber     *)
(*      Modified On Wed Apr 21 23:21:35 PDT 1993 by owicki     *)
(*      Modified On Fri Feb  2 10:35:09 PST 1990 by gnelson    *)
(*      Modified On Wed Dec 20 18:07:50 1989 by kalsow         *)

INTERFACE Type;

(* A Type.T is a representation for a Modula-3 type, together with
information about the name of the type and its constituent types
in some scope, called the global scope.  *)

IMPORT Atom, Text, Value;

TYPE
  T <: OBJECT name: Qid; visited := FALSE; brandsOK := TRUE  END;
  (* If name is not NIL, it is the name of the type in the global scope.
     If the type has multiple names in the global scope, one of them
     is chosen arbitrarily.
     If visited is TRUE, brandsOK is TRUE iff any branded type on which
     this type depends has an explicit brand *)

  (* Ordinal | Real | LongReal | Extended | Reference  | Array 
  | Packed | Record | Set | Procedure  *)

  Ordinal =  T BRANDED OBJECT END; (* Enumeration | Subrange *)

  Enumeration = Ordinal BRANDED OBJECT END; (* Char | WideChar | UserDefined *)

  Char = Enumeration BRANDED OBJECT END;
  WideChar = Enumeration BRANDED OBJECT END;

  UserDefined = Enumeration OBJECT
    elts : REF ARRAY OF Atom.T;
  END;

  Subrange = Ordinal OBJECT
    base : Ordinal;
    min  : Value.T;
    max  : Value.T;
  END;

  (* INTEGER is represented by a Subrange p with p.base = p,
     p.min = FIRST(INTEGER) and p.max = LAST(INTEGER). *)

  (* LONGINT is represented by a Subrange p with p.base = p,
     p.min = FIRST(LONGINT) and p.max = LAST(LONGINT). *)

  Real = T BRANDED OBJECT END;

  LongReal = T BRANDED OBJECT END;

  Extended = T BRANDED OBJECT END;

  Reference = T OBJECT
    traced : BOOLEAN;
    branded: BOOLEAN;  
    brand  : Atom.T;  (* IF branded AND brand=NIL then the type was
                         declared as branded, with no brand given *)
    revIntf : Atom.T;
    (* If this type was declared opaquely but revealed in
       a module included in stube generation, revIntf is the
       name of the interface containing the revelation *)
  END;

  (* Object | Ref | Opaque *)
  (* Note that a type declared as opaque is represented by an object
     of type Object if it is fully revealed in an interface
     given to the stub generator *)

  Object = Reference OBJECT
    super   : Reference;  (* Opaque or Object *)
    fields  : REF ARRAY OF Field;
    methods : REF ARRAY OF Method;
    (* Note that fields and methods contain only the identifiers 
       declared for this type, and not those of supertypes *)
    overrides : REF ARRAY OF Override;
  END;

  Ref = Reference OBJECT
    target : T
  END;

  Opaque = Reference OBJECT
    revealedSuperType: Reference;
  END;

  Array = T OBJECT 
    index: Ordinal; 
    element : T 
  END;
  (* index = NIL for open array types. *)

  OpenArray = Array OBJECT
    refArray: Ref;
    openDimensions: INTEGER; (* Depth of open arrays, including this one *)
  END;

  Packed = T OBJECT
    size : CARDINAL;
    base : T;
  END;

  Record = T OBJECT
    fields : REF ARRAY OF Field;
  END;

  Set = T OBJECT
    range : Ordinal;
  END;

  Procedure = T OBJECT
    sig      : Signature;
  END;

  Field = REF RECORD
    name    : Atom.T;
    type    : T;
    default : Value.T;
      (* default = NIL if there is no default.
         default = Value.Nil if the default is NIL. *)
      (* It is assumed that fields of type PROCEDURE do not have
         default values.  *)
  END; 

  Method = REF RECORD
    name    : Atom.T;
    sig     : Signature; (* Not including the Self argument *)
    default : MethodDefault;
  END;

  Override = REF RECORD
    name : Atom.T;
    default : MethodDefault;
  END;

  MethodDefault = BRANDED OBJECT END;

  MethodDefault1 = MethodDefault OBJECT qid: Qid  END;
  (* represents the top level procedure named qid *)

  MethodDefault2 = MethodDefault OBJECT 
    obType: Object;
    method: Atom.T
  END;
  (*  represents obType.method *)


  Signature = RECORD
    formals : REF ARRAY OF Formal;
    result  : T;
    raises  : REF ARRAY OF Exception
    (* raises = NIL corresponds to RAISES ANY; 
       NUMBER(raises^)=0 corresponds to RAISES{}  *)
  END;

  Exception = OBJECT 
    qid: Qid; 
      (* The Qid's name exceptions in the global scope *)
    arg: T;
      (* The type of the exceptions argument, if any *)
  END;

  Formal = REF RECORD
    mode    : Mode;
    outOnly : BOOLEAN;
    name    : Atom.T;
    type    : T;
    default : Value.T;
  END;

  Mode = { Value, Var, Readonly };

  Qid = REF RECORD intf: Atom.T;
               item: Atom.T
        END;

VAR (* READONLY *)
  integer       : Subrange;
  longint       : Subrange;
  cardinal      : Subrange;
  boolean       : UserDefined;
  char          : Char;
  widechar      : WideChar;
  real          : Real;
  longreal      : LongReal;
  extended      : Extended;
  refany        : Reference;
  address       : Reference;
  root          : Object;
  untracedRoot  : Object;
  null          : Reference;
  text          : Opaque;
  mutex         : Opaque;

PROCEDURE ToText(t: T; byName: BOOLEAN := TRUE): Text.T;
(* Return a textual representation of the type.  If 
   byName AND t.name # NIL, the text for t.name is returned.  
   Otherwise the returned value is composed from the type of T and its 
   component values.  Note that even if byName = TRUE, names
   within the definition of T will not be expanded.  An opqaue
   type can only be represented by name; if T is an opaque type and
   byName = FALSE, the returned text is meaningless (it contains
   a warning). *)

PROCEDURE MayBeRefAny(t: T): BOOLEAN;
(* Returns "TRUE" if t is or may be revealed to be REFANY *)
 
PROCEDURE NamedType(t: T): BOOLEAN;
(* Returns "TRUE" if t has a name, i.e. t.name # NIL *)

END Type.

