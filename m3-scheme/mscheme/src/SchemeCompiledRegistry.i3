(* Copyright (c) 2026 Mika Nystrom.  All rights reserved. *)

INTERFACE SchemeCompiledRegistry;
IMPORT Scheme;

TYPE Installer = PROCEDURE(interp : Scheme.T) RAISES { Scheme.E };

PROCEDURE Register(name : TEXT; installer : Installer);
PROCEDURE Unregister(name : TEXT);
PROCEDURE Lookup(name : TEXT) : Installer;  (* NIL if not found *)

PROCEDURE Disable();  (* Force interpreter mode: Lookup returns NIL *)
PROCEDURE Enable();

END SchemeCompiledRegistry.
