(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CodeForType.i3                                        *)
(* Last Modified On Fri Feb 18 17:30:06 PST 1994 by kalsow     *)
(*      Modified On Mon May 17 13:26:35 PDT 1993 by mjordan    *)
(*      Modified On Thu Apr 22 11:43:51 PDT 1993 by owicki     *)

INTERFACE CodeForType;

IMPORT Atom, AtomList, Formatter, ImportList, Type;

(* Return a textual representation of the type t.  If byName = TRUE, and 
   type T is named, then return the name.  Otherwise return text 
   for the delcaration of T.  Note that even with byName = FALSE,
   typeNames will be used within the text for t's declaration *)

PROCEDURE ToText(t: Type.T; byName: BOOLEAN := TRUE; 
                 exports: AtomList.T := NIL): TEXT;

(* Output on "f" a procedure header with name "procName" and
   signature derived from "sig".  The signature differs from "sig" in
   that a first parameter "self" of type "objType" is inserted. *)

PROCEDURE QidToText(qid: Type.Qid; exports: AtomList.T := NIL): TEXT;

PROCEDURE QidToIdentf(qid: Type.Qid): TEXT;

(* Output on "f" a header for a procedure with namd "procName" and a
   signature derived from "sig".  The signature differs from "sig" in
   that it's first argument is a self argument of type "objType", and
   its formal parameters' names are suffixed by "suffix".
   "pragmas" is a list of text for pragmas to precede arguments;
   pragma[i] preceeds the ith parameter in sig. *)

PROCEDURE ProcHeader(f: Formatter.T; 
                       objType: Type.Object; 
                       procName: TEXT; 
                       sig: Type.Signature;
                       pragmas: REF ARRAY OF TEXT := NIL;
                       suffix := ""; 
                       exports: AtomList.T := NIL);

PROCEDURE PrintSig (f         : Formatter.T;
                    sig       : Type.Signature;
                    argPragmas: REF ARRAY OF TEXT := NIL;
                    suffix := ""; 
                    exports: AtomList.T := NIL);

PROCEDURE PrintArgs (f         : Formatter.T;
                     sig       : Type.Signature);

(* Add to importTbl the names of the interfaces (encoded as atoms) that
   must be imported to represent type t.  If byName = TRUE, and
   t.name # NIL then the only interface added to importTbl is
   t.name.intf.  Otherwise, the interface(s) for each component of the
   definition of t will be included, by calling ImportList recursively
   with byName = TRUE.  

   At the top level, the methods in methods[0..lastNewMethod] are
   scanned for imports. *)

TYPE MethodData = RECORD name, intf: Atom.T; sig: Type.Signature END;
     MethodList = REF ARRAY OF MethodData;

(* Add to the table importTbl the names of interfaces that must be
   imported into a module or interface for T.  This is simply the
   interface for the type "t". *)
PROCEDURE ImportLst(t: Type.Object; 
                    importTbl: ImportList.T;
                    methods: ImportList.MethodList;
                    umethods: AtomList.T);

(* Add to the table importTbl the names of interfaces that must be
   imported into a module the defines a CB object.  These are the
   interfaces for types used as arguments and results of update method
   calls. *)
PROCEDURE ImportCBLst(t: Type.Object; 
                      importTbl: ImportList.T;
                      methods: ImportList.MethodList;
                      umethods: AtomList.T);
(* Add to the table importTbl the names of interfaces that must be
   imported into the module the defines the shared object wrapper
   routines.  These are
    1. interfaces named in the RAISES clause and needed for the
       arguments to the exceptions.
    2. interfaces for marshalled reference types and the objects they
       reference. 
    3. interfaces for the index type of marshalled arrays.
    4. the interface which reveals the main type, if different.
   Here a type is said to be marshalled if it is a method argument
   or result, or is a component of a non-reference marshalled type.
*)
PROCEDURE ImportSOLst(t: Type.Object; 
                      importTbl: ImportList.T;
                      methods: ImportList.MethodList;
                      umethods: AtomList.T);

PROCEDURE AugmentImportList(importList: ImportList.T; 
     READONLY newImports: ARRAY OF Atom.T);
(* Add the interfaces in "newImports" to "importList" if they are
   not there already. *)

PROCEDURE AugmentExceptionList(VAR (*in/out*) exceptionList: AtomList.T; 
                               raises  : REF ARRAY OF Type.Exception;
                               exports: AtomList.T := NIL);
(* Add the elements in the raises list to the exception list, if they
   are not already there *)

PROCEDURE ProduceImports(fmtWr: Formatter.T; imports: ImportList.T);

END CodeForType.


