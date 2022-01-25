INTERFACE MxConfigC;
IMPORT Ctypes;

<*EXTERNAL "MxConfigC__ifdef_win32"*> PROCEDURE ifdef_win32(): BOOLEAN;
<*EXTERNAL "MxConfigC__HOST"*> PROCEDURE HOST(): Ctypes.const_char_star;

<*EXTERNAL "MxConfigC__CaseInsensitive"*> PROCEDURE CaseInsensitive(): BOOLEAN;
<*EXTERNAL "MxConfigC__DeviceSeparator"*> PROCEDURE DeviceSeparator(): CHAR;
<*EXTERNAL "MxConfigC__DirectorySeparator"*> PROCEDURE DirectorySeparator(): CHAR;

END MxConfigC.
