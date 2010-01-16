(*--------------------------------------------------------------------------*)
UNSAFE INTERFACE FSUtilsUnsafe;

IMPORT Ctypes;

(*---------------------------------------------------------------------------*)
<*EXTERNAL FSUtilsUnsafe__GetFileSize32*> 
PROCEDURE GetFileSize32(path:Ctypes.const_char_star):INTEGER;

<*EXTERNAL FSUtilsUnsafe__GetFileSize*> 
PROCEDURE GetFileSize(path:Ctypes.const_char_star):INTEGER;

END FSUtilsUnsafe.
