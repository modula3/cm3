INTERFACE Cprintf;
FROM Ctypes IMPORT char_star;
<* EXTERNAL printf_prints *>
PROCEDURE prints(s: char_star);
END Cprintf.
