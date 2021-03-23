INTERFACE MxConfigC;

IMPORT Ctypes;

<*EXTERNAL "MxConfig__os_type"*> VAR os_type: Ctypes.int; (*MxConfig.OS_TYPE*)
<*EXTERNAL "MxConfig__word_size"*> VAR word_size: Ctypes.int;

END MxConfigC.
