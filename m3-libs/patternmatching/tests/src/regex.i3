INTERFACE regex;

IMPORT Ctypes;

<*EXTERNAL*>
PROCEDURE re_comp(s : Ctypes.char_star) : Ctypes.char_star;

<*EXTERNAL*>
PROCEDURE re_exec(s : Ctypes.char_star) : Ctypes.int;

END regex.
