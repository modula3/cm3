UNSAFE INTERFACE UnixC;

FROM Ctypes IMPORT int, int_star;

<*EXTERNAL "UnixC__pipe"*>PROCEDURE pipe (fildes: int_star): int;

END UnixC.
