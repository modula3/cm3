(*--------------------------------------------------------------------------*)
UNSAFE INTERFACE FSUtilsUnsafe;

IMPORT Ctypes;

(*---------------------------------------------------------------------------*)
<*EXTERNAL FSUtilsUnsafe__GetFileSize32*> 
PROCEDURE GetFileSize32(VAR path:Ctypes.const_char_star):INTEGER;

END FSUtilsUnsafe.
