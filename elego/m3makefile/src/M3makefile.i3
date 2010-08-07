(*--------------------------------------------------------------------------*)
INTERFACE M3makefile;

IMPORT Pathname, TextSeq, OSError;

EXCEPTION ParseError(TEXT);

(*--------------------------------------------------------------------------*)
TYPE
  TargetType = {Library, Program};

  (* 
     Consider a quake procedure call of the form
|      x_module("foo", "bar")
     Here, `x-module' is called the `type' of the declaration,
     `foo' is the name and `bar' is an additional parameter.
     The elements of the corresponding object type below would be
|      type = "x_module"
|      name = "foo"
|      args = ("foo", "bar")
   *)
  M3DeclarationList = OBJECT
    type : TEXT;
    name : TEXT;
    args : TextSeq.T;
    next : M3DeclarationList;
  METHODS
    copy(m : M3DeclarationList) : M3DeclarationList := Copy;
  END;

(*--------------------------------------------------------------------------*)
TYPE
  Public = OBJECT
  METHODS
    init(fn : Pathname.T) : T RAISES {OSError.E, ParseError};
    (* 
       Parse the m3makefile `fn' and build up an internal representation.
       Raise OSError.E in case of any system failures or file not found,
       raise ParseError if any unknown quake declaration is encountered. 
    *)

    imports() : TextSeq.T;
    (* 
       Returns the list of imported packages. 
    *)

    elements() : M3DeclarationList;
    (* 
       Returns the complete list of quake declarations for interfaces,
       implementation, modules, header-files etc., including imports
       and targets.
    *)

    declList(type : TEXT) : M3DeclarationList;
    (* 
       Returns the list of all quake declarations that are of type
       `type'. 
    *)

    decls(type : TEXT) : TextSeq.T;
    (* 
       Returns a sequence of all names of declarations of type
       `type'. 
    *)
    
    targetName() : TEXT;
    (* 
       Returns the name of the main target of this makefile. 
    *)

    targetType() : TargetType;
    (* 
       Returns the type of the main target of this makefile. 
    *)

    unknownElements() : BOOLEAN;
    (*
      <=> The m3makefile contains non-declarative elements, e.g.
          procedure definitions or assignments.
    *)
  END;

  T <: Public;

(*--------------------------------------------------------------------------*)
PROCEDURE New(fn : Pathname.T) : T RAISES {OSError.E, ParseError};
  (* Parse the m3makefile `fn' and return an object of
     M3makefile.T. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Copy(self, from : M3DeclarationList) : M3DeclarationList;

END M3makefile.
