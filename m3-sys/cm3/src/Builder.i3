(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE Builder;

IMPORT Arg, M3Unit, Quake, QMachine;

PROCEDURE BuildPgm (prog: TEXT;  READONLY units: M3Unit.Set;
                    sys_libs: Arg.List;  shared: BOOLEAN;  m: Quake.Machine);
(* Build a new program named "prog" from the sources in "units" *)

PROCEDURE BuildLib (lib: TEXT;  READONLY units: M3Unit.Set;
                    sys_libs: Arg.List;  shared: BOOLEAN;  m: Quake.Machine);
(* Build a new library named "lib" from the sources in "units" *)

PROCEDURE BuildCPgm (prog: TEXT;  READONLY units: M3Unit.Set;
                     sys_libs: Arg.List;  shared: BOOLEAN;  m: Quake.Machine);
(* Build a new C program named "prog" from the C sources in "units" *)

PROCEDURE JustCompile (READONLY units: M3Unit.Set;
                       sys_libs: Arg.List;  m: Quake.Machine);
(* Compile the sources in "units". *)

PROCEDURE CleanUp ();
(* Last chance to dump state and clean up *)

PROCEDURE EmitPkgImports (READONLY units: M3Unit.Set);
(* Output all imported packages *)

PROCEDURE SetupNamingConventions (mach : QMachine.T);

END Builder.
