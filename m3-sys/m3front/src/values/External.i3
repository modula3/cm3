(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: External.i3                                           *)
(* Last Modified On Fri Jul  1 08:11:44 PDT 1994 By kalsow     *)

INTERFACE External;

IMPORT M3ID, Module, Value;

TYPE Set <: REFANY;

PROCEDURE NewSet (): Set;

PROCEDURE NoteExport (s: Set;  name: M3ID.T);
PROCEDURE NoteImport (s: Set;  im: Module.T;  name: M3ID.T);

PROCEDURE ParseImports (s: Set;  self: Module.T);
PROCEDURE LoadImports  (s: Set;  self: Module.T);

PROCEDURE GenLinkInfo  (s: Set);
PROCEDURE GenImports   (s: Set);

PROCEDURE InitGlobals  (s: Set);

PROCEDURE IsExportable (v: Value.T): BOOLEAN;
PROCEDURE Redirect (intf, impl: Value.T);

PROCEDURE Visit (s: Set;  v: Module.Visitor);

END External.
