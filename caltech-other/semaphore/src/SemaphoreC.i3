(* $Id$ *)

INTERFACE SemaphoreC;
FROM Ctypes IMPORT int;

<*EXTERNAL "SemaphoreC__sem_alloc"*>
PROCEDURE sem_alloc() : ADDRESS;

<*EXTERNAL "SemaphoreC__sem_init"*>
PROCEDURE sem_init(sem : ADDRESS) : int;

<*EXTERNAL "SemaphoreC__sem_destroy"*>
PROCEDURE sem_destroy(sem : ADDRESS) : int;

<*EXTERNAL "SemaphoreC__sem_post"*>
PROCEDURE sem_post(sem : ADDRESS) : int;

<*EXTERNAL "SemaphoreC__sem_wait"*>
PROCEDURE sem_wait(sem : ADDRESS) : int;

<*EXTERNAL "SemaphoreC__sem_getvalue"*>
PROCEDURE sem_getvalue(sem : ADDRESS; VAR value : int) : int;

END SemaphoreC.
