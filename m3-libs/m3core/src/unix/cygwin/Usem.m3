(* $Id: Usem.m3,v 1.1 2008-02-10 02:45:46 jkrell Exp $ *)

UNSAFE MODULE Usem;
FROM Ctypes IMPORT int, unsigned_int;

(*
Cygwin wants the parameter to sem_init to contain
a dereferencable pointer that does NOT point to the correct magic value.
It dereferences the value and either access-violates on the typical NULL, or
errors more nicely if it happens to contain the magic value.
The magic number is 0xDF0DF04C. We just have to avoid that.
This seems so wrong.
*)

PROCEDURE init (VAR sem: sem_t; pshared: int; value: unsigned_int): int =
    VAR NotMagic := 0;
BEGIN
    sem.data := ADR(NotMagic);
    RETURN sem_init(sem, pshared, value);
END init;

BEGIN
END Usem.
