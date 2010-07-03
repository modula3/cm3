(* This is the portably-definable subset of stdint.h.
"Fast" and "at least" types are deliberately omited,
in order to not encourage a false sense of compatibility with C.
Exact types suffice for at least and fast, though are not necessarily fastest.
Note that the signed types below are defined as using two's complement.
*)

INTERFACE Cstdint;

IMPORT Ctypes, Word;

TYPE
       int8_t = Ctypes.signed_char;
      uint8_t = Ctypes.unsigned_char;
      int16_t = Ctypes.short;
     uint16_t = Ctypes.unsigned_short;
      int32_t = Ctypes.int;
     uint32_t = Ctypes.unsigned;
      int64_t = Ctypes.long_long;
     uint64_t = Ctypes.unsigned_long_long;
     intptr_t = INTEGER;
    uintptr_t = Word.T;

CONST
      INT8_MIN = FIRST( int8_t);
      INT8_MAX =  LAST( int8_t);
     INT16_MIN = FIRST(int16_t);
     INT16_MAX =  LAST(int16_t);
     INT32_MIN = FIRST(int32_t);
     INT32_MAX =  LAST(int32_t);
     INT64_MIN = FIRST(int32_t);
     INT64_MAX =  LAST(int64_t);

     UINT8_MAX = LAST(uint8_t);
    UINT16_MAX = LAST(uint16_t);
    UINT32_MAX = 16_FFFFFFFFL; (* Perhaps not the desired type, alas. *)
    (* UINT64_MAX cannot be defined. *)

END Cstdint.
