INTERFACE MxConfigC;
IMPORT Ctypes;

<*EXTERNAL "MxConfigC__ifdef_win32"*> PROCEDURE ifdef_win32(): BOOLEAN;
<*EXTERNAL "MxConfigC__HOST"*> PROCEDURE HOST(): Ctypes.const_char_star;

END MxConfigC.
