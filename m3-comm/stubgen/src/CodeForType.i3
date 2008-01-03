(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CodeForType.i3                                        *)
(* Last Modified On Fri Feb 18 17:30:06 PST 1994 by kalsow     *)
(*      Modified On Mon May 17 13:26:35 PDT 1993 by mjordan    *)
(*      Modified On Thu Apr 22 11:43:51 PDT 1993 by owicki     *)

INTERFACE CodeForType;

IMPORT Atom, Formatter, AtomRefTbl, StubCode, Type;

(* Return a textual representation of the type t.  If byName = TRUE, and 
   type T is named, then return the name.  Otherwise return text 
   for the delcaration of T.  Note that even with byName = FALSE,
   typeNames will be used within the text for t's declaration *)

PROCEDURE ToText(t: Type.T; byName: BOOLEAN := TRUE): TEXT;

(* Output on "f" a procedure header with name "procName" and
   signature derived from "sig".  The signature differs from "sig" in
   that a first parameter "self" of type "objType" is inserted. *)

PROCEDURE QidToText(qid: Type.Qid): TEXT;

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
                       suffix := "");

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
   imported for both interface and module stubs for type T *)
PROCEDURE ImportList(t: Type.Object; 
                     importTbl: AtomRefTbl.T;
                     methods: MethodList;
                     lastNewMethod: INTEGER;
                     byName := TRUE);

(* Add to importTbl the names of interfaces that must be imported
   for the stub module but not the stub interface.  These consist of,
   for each method in t.methods[0..lastNewMethod],
    1. interfaces named in the RAISES clause
    2. interfaces for marshalled reference types
    3. interfaces for the index type of marshalled arrays
   Here a type is said to be marshalled if it is a method argument
   or result, or is a component of a non-reference marshalled type.
*)
PROCEDURE AddModuleImports(importTbl: AtomRefTbl.T;
                           methods: MethodList; 
                           lastNewMethod: INTEGER);

PROCEDURE AugmentImportList(importList: AtomRefTbl.T; 
     READONLY newImports: ARRAY OF Atom.T);
(* Add the interfaces in "newImports" to "importList" if they are
   not there already. *)

(* PROCEDURE ImportRevelations(t: Type.Reference; importTbl: RefTable.T); *)
(* Add to the table importTbl the names of interfaces that provide
   revelations of t or one of its supertypes. *)

PROCEDURE ProduceImports(fmtWr: Formatter.T; objName: Type.Qid; 
                         imports: AtomRefTbl.T);

(* Output on fmtWr an IMPORT statement for the supertype, if any, whose
   methods will be used in the surrogate and dispatcher.  "lastNewMethod"
   is the index in "methods" of the last method that is not shared with
   a superType.  If "lastNewMethod" = LAST(methods^), there is
   no sharing and no import statement is produced.  *)

PROCEDURE ImportSuperStubs(fmtWr: Formatter.T; 
                           methods: StubCode.MethodList;
                           lastNewMethod: INTEGER;
                           typeName: Atom.T);


END CodeForType.
