(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

UNSAFE MODULE RTOSmmap EXPORTS RTOS;

IMPORT Unix, Umman, Word, Utypes;

(*------------------------------------------------------------- allocator ---*)

PROCEDURE GetMemory (size: INTEGER): ADDRESS =
  (* Return the address of "size" bytes of unused storage *)
  BEGIN
    WITH addr = LOOPHOLE(0, ADDRESS),
         prot = Word.Or(Umman.PROT_READ, Umman.PROT_WRITE),
         flags = Word.Or(Umman.MAP_ANON, Umman.MAP_PRIVATE) DO
      RETURN LOOPHOLE(Umman.mmap(addr, size, prot, flags, -1,
                                 VAL(0, Utypes.off_t)), ADDRESS);
    END;
  END GetMemory;

(*---------------------------------------------------------------------------*)

BEGIN
END RTOSmmap.
