MODULE RightShift;
<*NOWARN*>IMPORT Cstdint, Word, Long;

(* define some types *)

<*NOWARN*>TYPE FLOAT32 = REAL;
<*NOWARN*>TYPE FLOAT64 = LONGREAL;
<*NOWARN*>TYPE INT8 = Cstdint.int8_t;
<*NOWARN*>TYPE UINT64 = Cstdint.uint64_t;
<*NOWARN*>TYPE INT32 = Cstdint.int32_t;
<*NOWARN*>TYPE UINT16 = Cstdint.uint16_t;
<*NOWARN*>TYPE INT64 = Cstdint.int64_t;
<*NOWARN*>TYPE INT16 = Cstdint.int16_t;
<*NOWARN*>TYPE UINT32 = Cstdint.uint32_t;
<*NOWARN*>TYPE UINT8 = Cstdint.uint8_t;

(* constants *)

<*NOWARN*> CONST ki8:INT8=641;
<*NOWARN*> CONST ku64:UINT64=642L;
<*NOWARN*> CONST kf64:FLOAT64=643.644d0;
<*NOWARN*> CONST ki32:INT32=645;
<*NOWARN*> CONST kLC:LONGCARD=646L;
<*NOWARN*> CONST ku16:UINT16=647;
<*NOWARN*> CONST kI:INTEGER=648;
<*NOWARN*> CONST ki64:INT64=649L;
<*NOWARN*> CONST kf32:FLOAT32=650.651e0;
<*NOWARN*> CONST ki16:INT16=652;
<*NOWARN*> CONST kC:CARDINAL=653;
<*NOWARN*> CONST ku32:UINT32=654;
<*NOWARN*> CONST ku8:UINT8=655;
<*NOWARN*> CONST kL:LONGINT=656L;

(* variables *)

<*NOWARN*> VAR vi8:INT8:=657;
<*NOWARN*> VAR vu64:UINT64:=658L;
<*NOWARN*> VAR vf64:FLOAT64:=659.660d0;
<*NOWARN*> VAR vi32:INT32:=661;
<*NOWARN*> VAR vLC:LONGCARD:=662L;
<*NOWARN*> VAR vu16:UINT16:=663;
<*NOWARN*> VAR vI:INTEGER:=664;
<*NOWARN*> VAR vi64:INT64:=665L;
<*NOWARN*> VAR vf32:FLOAT32:=666.667e0;
<*NOWARN*> VAR vi16:INT16:=668;
<*NOWARN*> VAR vC:CARDINAL:=669;
<*NOWARN*> VAR vu32:UINT32:=670;
<*NOWARN*> VAR vu8:UINT8:=671;
<*NOWARN*> VAR vL:LONGINT:=672L;
<*NOWARN*>VAR offset, count:CARDINAL;

(* RightShift *)

<*NOWARN*>PROCEDURE uRightShift_var_i8_I():Word.T=BEGIN RETURN Word.RightShift(vi8,vI);END uRightShift_var_i8_I;
<*NOWARN*>PROCEDURE uRightShift_param_i8_I(a:INT8;b:INTEGER):Word.T=BEGIN RETURN Word.RightShift(a,b);END uRightShift_param_i8_I;
<*NOWARN*>PROCEDURE uRightShift_var_i8_C():Word.T=BEGIN RETURN Word.RightShift(vi8,vC);END uRightShift_var_i8_C;
<*NOWARN*>PROCEDURE uRightShift_param_i8_C(a:INT8;b:CARDINAL):Word.T=BEGIN RETURN Word.RightShift(a,b);END uRightShift_param_i8_C;
<*NOWARN*>PROCEDURE uRightShift_var_u64_I():Long.T=BEGIN RETURN Long.RightShift(vu64,vI);END uRightShift_var_u64_I;
<*NOWARN*>PROCEDURE uRightShift_param_u64_I(a:UINT64;b:INTEGER):Long.T=BEGIN RETURN Long.RightShift(a,b);END uRightShift_param_u64_I;
<*NOWARN*>PROCEDURE uRightShift_var_u64_C():Long.T=BEGIN RETURN Long.RightShift(vu64,vC);END uRightShift_var_u64_C;
<*NOWARN*>PROCEDURE uRightShift_param_u64_C(a:UINT64;b:CARDINAL):Long.T=BEGIN RETURN Long.RightShift(a,b);END uRightShift_param_u64_C;
<*NOWARN*>PROCEDURE uRightShift_var_i32_I():Word.T=BEGIN RETURN Word.RightShift(vi32,vI);END uRightShift_var_i32_I;
<*NOWARN*>PROCEDURE uRightShift_param_i32_I(a:INT32;b:INTEGER):Word.T=BEGIN RETURN Word.RightShift(a,b);END uRightShift_param_i32_I;
<*NOWARN*>PROCEDURE uRightShift_var_i32_C():Word.T=BEGIN RETURN Word.RightShift(vi32,vC);END uRightShift_var_i32_C;
<*NOWARN*>PROCEDURE uRightShift_param_i32_C(a:INT32;b:CARDINAL):Word.T=BEGIN RETURN Word.RightShift(a,b);END uRightShift_param_i32_C;
<*NOWARN*>PROCEDURE uRightShift_var_LC_I():Long.T=BEGIN RETURN Long.RightShift(vLC,vI);END uRightShift_var_LC_I;
<*NOWARN*>PROCEDURE uRightShift_param_LC_I(a:LONGCARD;b:INTEGER):Long.T=BEGIN RETURN Long.RightShift(a,b);END uRightShift_param_LC_I;
<*NOWARN*>PROCEDURE uRightShift_var_LC_C():Long.T=BEGIN RETURN Long.RightShift(vLC,vC);END uRightShift_var_LC_C;
<*NOWARN*>PROCEDURE uRightShift_param_LC_C(a:LONGCARD;b:CARDINAL):Long.T=BEGIN RETURN Long.RightShift(a,b);END uRightShift_param_LC_C;
<*NOWARN*>PROCEDURE uRightShift_var_u16_I():Word.T=BEGIN RETURN Word.RightShift(vu16,vI);END uRightShift_var_u16_I;
<*NOWARN*>PROCEDURE uRightShift_param_u16_I(a:UINT16;b:INTEGER):Word.T=BEGIN RETURN Word.RightShift(a,b);END uRightShift_param_u16_I;
<*NOWARN*>PROCEDURE uRightShift_var_u16_C():Word.T=BEGIN RETURN Word.RightShift(vu16,vC);END uRightShift_var_u16_C;
<*NOWARN*>PROCEDURE uRightShift_param_u16_C(a:UINT16;b:CARDINAL):Word.T=BEGIN RETURN Word.RightShift(a,b);END uRightShift_param_u16_C;
<*NOWARN*>PROCEDURE uRightShift_var_I_I():Word.T=BEGIN RETURN Word.RightShift(vI,vI);END uRightShift_var_I_I;
<*NOWARN*>PROCEDURE uRightShift_param_I_I(a:INTEGER;b:INTEGER):Word.T=BEGIN RETURN Word.RightShift(a,b);END uRightShift_param_I_I;
<*NOWARN*>PROCEDURE uRightShift_var_I_C():Word.T=BEGIN RETURN Word.RightShift(vI,vC);END uRightShift_var_I_C;
<*NOWARN*>PROCEDURE uRightShift_param_I_C(a:INTEGER;b:CARDINAL):Word.T=BEGIN RETURN Word.RightShift(a,b);END uRightShift_param_I_C;
<*NOWARN*>PROCEDURE uRightShift_var_i64_I():Long.T=BEGIN RETURN Long.RightShift(vi64,vI);END uRightShift_var_i64_I;
<*NOWARN*>PROCEDURE uRightShift_param_i64_I(a:INT64;b:INTEGER):Long.T=BEGIN RETURN Long.RightShift(a,b);END uRightShift_param_i64_I;
<*NOWARN*>PROCEDURE uRightShift_var_i64_C():Long.T=BEGIN RETURN Long.RightShift(vi64,vC);END uRightShift_var_i64_C;
<*NOWARN*>PROCEDURE uRightShift_param_i64_C(a:INT64;b:CARDINAL):Long.T=BEGIN RETURN Long.RightShift(a,b);END uRightShift_param_i64_C;
<*NOWARN*>PROCEDURE uRightShift_var_i16_I():Word.T=BEGIN RETURN Word.RightShift(vi16,vI);END uRightShift_var_i16_I;
<*NOWARN*>PROCEDURE uRightShift_param_i16_I(a:INT16;b:INTEGER):Word.T=BEGIN RETURN Word.RightShift(a,b);END uRightShift_param_i16_I;
<*NOWARN*>PROCEDURE uRightShift_var_i16_C():Word.T=BEGIN RETURN Word.RightShift(vi16,vC);END uRightShift_var_i16_C;
<*NOWARN*>PROCEDURE uRightShift_param_i16_C(a:INT16;b:CARDINAL):Word.T=BEGIN RETURN Word.RightShift(a,b);END uRightShift_param_i16_C;
<*NOWARN*>PROCEDURE uRightShift_var_C_I():Word.T=BEGIN RETURN Word.RightShift(vC,vI);END uRightShift_var_C_I;
<*NOWARN*>PROCEDURE uRightShift_param_C_I(a:CARDINAL;b:INTEGER):Word.T=BEGIN RETURN Word.RightShift(a,b);END uRightShift_param_C_I;
<*NOWARN*>PROCEDURE uRightShift_var_C_C():Word.T=BEGIN RETURN Word.RightShift(vC,vC);END uRightShift_var_C_C;
<*NOWARN*>PROCEDURE uRightShift_param_C_C(a:CARDINAL;b:CARDINAL):Word.T=BEGIN RETURN Word.RightShift(a,b);END uRightShift_param_C_C;
<*NOWARN*>PROCEDURE uRightShift_var_u32_I():Word.T=BEGIN RETURN Word.RightShift(vu32,vI);END uRightShift_var_u32_I;
<*NOWARN*>PROCEDURE uRightShift_param_u32_I(a:UINT32;b:INTEGER):Word.T=BEGIN RETURN Word.RightShift(a,b);END uRightShift_param_u32_I;
<*NOWARN*>PROCEDURE uRightShift_var_u32_C():Word.T=BEGIN RETURN Word.RightShift(vu32,vC);END uRightShift_var_u32_C;
<*NOWARN*>PROCEDURE uRightShift_param_u32_C(a:UINT32;b:CARDINAL):Word.T=BEGIN RETURN Word.RightShift(a,b);END uRightShift_param_u32_C;
<*NOWARN*>PROCEDURE uRightShift_var_u8_I():Word.T=BEGIN RETURN Word.RightShift(vu8,vI);END uRightShift_var_u8_I;
<*NOWARN*>PROCEDURE uRightShift_param_u8_I(a:UINT8;b:INTEGER):Word.T=BEGIN RETURN Word.RightShift(a,b);END uRightShift_param_u8_I;
<*NOWARN*>PROCEDURE uRightShift_var_u8_C():Word.T=BEGIN RETURN Word.RightShift(vu8,vC);END uRightShift_var_u8_C;
<*NOWARN*>PROCEDURE uRightShift_param_u8_C(a:UINT8;b:CARDINAL):Word.T=BEGIN RETURN Word.RightShift(a,b);END uRightShift_param_u8_C;
<*NOWARN*>PROCEDURE uRightShift_var_L_I():Long.T=BEGIN RETURN Long.RightShift(vL,vI);END uRightShift_var_L_I;
<*NOWARN*>PROCEDURE uRightShift_param_L_I(a:LONGINT;b:INTEGER):Long.T=BEGIN RETURN Long.RightShift(a,b);END uRightShift_param_L_I;
<*NOWARN*>PROCEDURE uRightShift_var_L_C():Long.T=BEGIN RETURN Long.RightShift(vL,vC);END uRightShift_var_L_C;
<*NOWARN*>PROCEDURE uRightShift_param_L_C(a:LONGINT;b:CARDINAL):Long.T=BEGIN RETURN Long.RightShift(a,b);END uRightShift_param_L_C;
BEGIN
END RightShift.
