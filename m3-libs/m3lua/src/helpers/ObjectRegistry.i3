(* --------------------------------------------------------------------
 * FILE:     ObjectRegistry.i3
 * AUTHOR:   Peter Eiserloh
 * LANUGAGE: Modula-3
 * PURPOSE:  Register traced objects, so can be referenced by an
 *           untraced source.
 * VERSION:  0.0.1 (21-Nov-2010) PPE
 * ----------------------------------------------------------------- *)

INTERFACE ObjectRegistry;

IMPORT Ctypes;


PROCEDURE Register(obj: ROOT): Ctypes.void_star;
PROCEDURE UnRegister(obj: ROOT);

PROCEDURE LookupObj(obj: ROOT): Ctypes.void_star;
PROCEDURE LookupId(id: Ctypes.void_star) : ROOT;



END ObjectRegistry.
