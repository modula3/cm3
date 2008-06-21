
INTERFACE Ulib;
FROM Ctypes IMPORT char_star, int;

<*EXTERNAL*> PROCEDURE getcwd(result: char_star; size: int): char_star;

END Ulib.
