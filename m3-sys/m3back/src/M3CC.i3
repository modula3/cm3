INTERFACE M3CC;
IMPORT Ctypes, Long; (* favor Ctypes over Cstdint to work with older m3core *)

TYPE INT32 = Ctypes.int;
TYPE  INT64 = LONGINT;
TYPE UINT32 = Ctypes.unsigned;
TYPE UINT64 = Long.T;
TYPE Base_t = [2..36];

<*EXTERNAL M3CC__UInt64ToText*> PROCEDURE UInt64ToText(a: UINT64; base: Base_t): TEXT;

END M3CC.
