INTERFACE M3CC;
IMPORT Cstdint, Long;

TYPE  INT32 = Cstdint.int32_t;
TYPE  INT64 = LONGINT;
TYPE UINT32 = Cstdint.uint32_t;
TYPE UINT64 = Long.T;
TYPE Base_t = [2..36];

<*EXTERNAL M3CC__UInt64ToText*> PROCEDURE UInt64ToText(a: UINT64; base: Base_t): TEXT;

END M3CC.
