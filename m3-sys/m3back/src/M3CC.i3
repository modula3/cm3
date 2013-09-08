INTERFACE M3CC;
IMPORT (*Cstdint, *)Long;       (* Cstdint requires newer m3core *)

(*TYPE  INT32 = Cstdint.int32_t; requires newer m3core *)
TYPE INT32 = Ctypes.int;        (* not true for nonexistant 16bit targets but ok *)
TYPE  INT64 = LONGINT;
(*TYPE UINT32 = Cstdint.uint32_t; requires newer m3core *)
TYPE UINT32 = Ctypes.unsigned;  (* not true for nonexistant 16bit targets but ok *)
TYPE UINT64 = Long.T;
TYPE Base_t = [2..36];

<*EXTERNAL M3CC__UInt64ToText*> PROCEDURE UInt64ToText(a: UINT64; base: Base_t): TEXT;

END M3CC.
