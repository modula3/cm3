(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

UNSAFE MODULE RTOSbrk EXPORTS RTOS;

IMPORT Unix;

(*------------------------------------------------------------- allocator ---*)

PROCEDURE GetMemory (size: INTEGER): ADDRESS =
  (* Return the address of "size" bytes of unused storage *)
  BEGIN
    RETURN Unix.sbrk(size);
  END GetMemory;

(*---------------------------------------------------------------------------*)

BEGIN
END RTOSbrk.
