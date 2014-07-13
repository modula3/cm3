(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3Compiler.i3                                         *)
(* Last modified on Tue Dec  6 09:13:12 PST 1994 by kalsow     *)
(*      modified on Sat May 12 07:05:52 1990 by muller         *)

INTERFACE M3Compiler;

(* This module defines the environment object passed to
   the Modula-3 compiler.
*)

IMPORT File, Fingerprint;
IMPORT M3ID, M3CG;

TYPE
  TypeID     = INTEGER;  (* A compiler generated type id. *)
  SourceFile = RECORD  name: TEXT;  contents: File.T  END;
  ImplList   = REF RECORD  impl: M3ID.T;  next: ImplList;  END;

TYPE
  Environment = OBJECT METHODS

    report_error (file: TEXT;  line: INTEGER;  msg: TEXT);

    find_source (unit: M3ID.T;  interface, generic: BOOLEAN): SourceFile;

    note_unit (name: M3ID.T;  interface: BOOLEAN);

    note_comment (msg: TEXT);

    note_interface_use (name: M3ID.T;  imported: BOOLEAN);

    note_generic_use (name: M3ID.T);

    note_version_stamp (unit, symbol: M3ID.T;
                        READONLY vs: Fingerprint.T;
                        imported, implemented: BOOLEAN);

    note_opaque (type, super_type: TypeID; Name: M3ID.T);

    note_revelation (unit: M3ID.T;  interface: BOOLEAN;
                    lhs, rhs: TypeID;  full, imported: BOOLEAN);

    note_opaque_magic (type, super_type: TypeID;
                       data_size, data_align, method_size : INTEGER);

    find_opaque_magic (type: TypeID;  VAR(*OUT*) super_type: TypeID;
            VAR(*OUT*) data_size, data_align, method_size: INTEGER): BOOLEAN;

    note_ast (unit: M3ID.T;  ast: REFANY);
    find_ast (unit: M3ID.T): REFANY;

    note_type (type: TypeID;  imported: BOOLEAN);

    init_code_generator (): M3CG.T;

    note_webinfo (t: TEXT);

    get_implementations (interface: M3ID.T): ImplList;
  END;

END M3Compiler.

(* The compiler makes all environmental queries and reports through
   its environment parameter:

     "report_error" is called to report error and warning messages.

     "find_source" is called to locate the source files needed to
      satisfy "IMPORT"s.

     "note_unit" is called to announce the current unit.  The remainder
      of the "note" calls attach information needed by the linker
      and smart recompilation system to the announced unit.

     "note_comment" attaches a comment to the current unit.

     "note_interface_use" records the named interface as is either imported
      or exported by the current unit.

     "note_generic_use" records the use of the specified generic unit.

     "note_version_stamp" records the import(export) of "symbol" with
      version stamp "vs" from(to) the interface "unit".

     "note_opaque" records the opaque declaration "type <: super_type"
      in the current unit.

     "note_revelation" records the import(export) of a revelation.
      With "note_opaque" it is used to verify the all opaque types
      are defined and the all compilation units saw a consistent
      set of revelations.

     "note_opaque_magic" announces the size of a previously declared
      opaque type.

     "find_opaque_magic" attempts to locate the size of an opaque type.
      Returns TRUE iff it succeeded in finding the information.

     "note_ast" records an (interface name, ast) pair in the ast
     cache.

     "find_ast" returns the ast cached under the given name.  Returns
     NIL if no such ast exists.

     "init_code_generator" returns the code generator to be used.
     To avoid empty object files, "init_code_generator" isn't called
     until after type checking.
*)

 

