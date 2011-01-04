(* --------------------------------------------------------------------
 * FILE:     SimpleMemory.i3
 * AUTHOR:   Peter Eiserloh
 * LANUGAGE: Modula-3
 * PURPOSE:  Simple memory allocation
 * VERSION:  0.0.1 (12-Dec-2010) PPE
 * ----------------------------------------------------------------- *)

INTERFACE SimpleMemory;

IMPORT Ctypes, Cstddef;


PROCEDURE Alloc(size: Cstddef.size_t): Ctypes.void_star;
PROCEDURE Free(ptr: Ctypes.void_star);



END SimpleMemory.
