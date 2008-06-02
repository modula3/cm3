(* This is the portably-definable subset of stdint.h.
"Fast" and "at least" types are deliberately omited,
in order to not encourage a false sense of compatibility with C.
Exact types suffice for at least and fast, though are not necessarily fastest.
Note that the signed types below are defined as using two's complement.
*)

INTERFACE Cstdint;

IMPORT Ctypes;

TYPE
    int8_t = BITS 8 FOR Ctypes.signed_char;
    uint8_t = BITS 8 FOR Ctypes.unsigned_char;
    int16_t = BITS 16 FOR Ctypes.short;
    uint16_t = BITS 16 FOR Ctypes.unsigned_short;
    int32_t = BITS 32 FOR Ctypes.int;
    uint32_t = BITS 32 FOR Ctypes.unsigned;
    int64_t = BITS 64 FOR Ctypes.long_long;
    uint64_t = BITS 64 FOR Ctypes.unsigned_long_long;
    intptr_t = BITS BITSIZE(ADDRESS) FOR INTEGER;
    uintptr_t = BITS BITSIZE(ADDRESS) FOR CARDINAL;

CONST
    INT8_MIN = FIRST(int8_t);
    INT8_MAX = LAST(int8_t);
    UINT8_MAX = LAST(uint8_t);
    INT16_MIN = FIRST(int16_t);
    INT16_MAX = LAST(int16_t);
    UINT16_MAX = LAST(uint16_t);
    INT32_MIN = FIRST(int32_t);
    INT32_MAX = LAST(int32_t);
    UINT32_MAX = LAST(uint32_t);
    INT64_MIN = FIRST(int32_t);
    INT64_MAX = LAST(int64_t);
    UINT64_MAX = LAST(uint64_t);

END Cstdint.
