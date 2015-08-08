(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Module.def                                            *)
(* Last modified on Mon Aug 29 09:53:53 PDT 1994 by kalsow     *)
(*      modified on Sat Mar 16 01:55:39 1991 by muller         *)

INTERFACE Module;

IMPORT M3ID, Type, Value, Scope, CG;

TYPE T <: Value.T;

PROCEDURE Parse (interfaceOnly: BOOLEAN := FALSE): T;

PROCEDURE NewDefn (name: TEXT;  safe: BOOLEAN;  syms: Scope.T): T;

PROCEDURE LookUp (name: M3ID.T;  internal: BOOLEAN): T;

PROCEDURE ImportRevelations (t: T;  source: Value.T);

PROCEDURE TypeCheck (t: T;  main: BOOLEAN;  VAR cs: Value.CheckState);

PROCEDURE Compile (t: T);

PROCEDURE IsSafe (): BOOLEAN;
PROCEDURE IsInterface (): BOOLEAN;
PROCEDURE IsExternal (): BOOLEAN;
PROCEDURE LazyAlignmentOn (): BOOLEAN;
PROCEDURE SetLazyAlignment (on: BOOLEAN);

PROCEDURE ExportScope (t: T): Scope.T;

PROCEDURE Current (): T;

PROCEDURE Name (t: T): M3ID.T;
(* t = NIL => use Current *)

PROCEDURE GetNextCounter (VAR c: ARRAY [0..4] OF CHAR);
(* Return the next counter value for the current module and
   increment the counter. *)

PROCEDURE Allocate (size, align: INTEGER;  is_const: BOOLEAN;
                    tag: TEXT := NIL;  id: M3ID.T := M3ID.NoID): INTEGER;
(* allocate 'size' bits of space with the specified alignment
   in the current module's global data or constant segment.  Return the
   bit offset of allocated data. *)

PROCEDURE GlobalData (is_const: BOOLEAN): CG.Var;
(* returns the current module's global data segment.  *)

PROCEDURE LoadGlobalAddr (t: T;  offset: INTEGER;  is_const: BOOLEAN);
(* generate code to load the address of 't's global data + 'offset'. *)

PROCEDURE ImportInterface (t: T);
(* generate the structures that force "t" to be imported and initialized at
   link time. *)

PROCEDURE GetTypeInfo (t: T): Type.ModuleInfo;
(* return the global type info for module 't' *)

PROCEDURE VisitImports (v: Visitor);
(* Call 'v(m)' for each interface 'm' imported or exported,
   directly or indirectly, by the current module.  Restrictions:
   'v' must be a top-level procedure and may not call 'VisitImports'
   directly or indirectly.  *)

TYPE Visitor = PROCEDURE (t: T);

PROCEDURE Reset ();
PROCEDURE MakeCurrent (t: T);
(* refresh 't' and its imports for the current compilation *)

(* Exception handling support, using setjmp/longjmp, without front/middle-end
   knowing jmpbuf size -- use alloca at runtime getting size from
   constant initialized in C; these functions are in Module
   to accomodate hypothetical multi-threaded m3front,
   i.e. instead of having globals initialized on-demand in Jmpbufs.
   i.e. Module compilation still assumed single threadied, but
   possibly multiple Modules compiled on separate threads. *)
PROCEDURE GetAlloca (t: T) : CG.Proc;
PROCEDURE GetJmpbufSize (t: T): CG.Var;
PROCEDURE GetSetjmp (t: T) : CG.Proc;

END Module.
