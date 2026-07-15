MODULE RightRotate;
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

<*NOWARN*> CONST ki8:INT8=737;
<*NOWARN*> CONST ku64:UINT64=738L;
<*NOWARN*> CONST kf64:FLOAT64=739.740d0;
<*NOWARN*> CONST ki32:INT32=741;
<*NOWARN*> CONST kLC:LONGCARD=742L;
<*NOWARN*> CONST ku16:UINT16=743;
<*NOWARN*> CONST kI:INTEGER=744;
<*NOWARN*> CONST ki64:INT64=745L;
<*NOWARN*> CONST kf32:FLOAT32=746.747e0;
<*NOWARN*> CONST ki16:INT16=748;
<*NOWARN*> CONST kC:CARDINAL=749;
<*NOWARN*> CONST ku32:UINT32=750;
<*NOWARN*> CONST ku8:UINT8=751;
<*NOWARN*> CONST kL:LONGINT=752L;

(* variables *)

<*NOWARN*> VAR vi8:INT8:=753;
<*NOWARN*> VAR vu64:UINT64:=754L;
<*NOWARN*> VAR vf64:FLOAT64:=755.756d0;
<*NOWARN*> VAR vi32:INT32:=757;
<*NOWARN*> VAR vLC:LONGCARD:=758L;
<*NOWARN*> VAR vu16:UINT16:=759;
<*NOWARN*> VAR vI:INTEGER:=760;
<*NOWARN*> VAR vi64:INT64:=761L;
<*NOWARN*> VAR vf32:FLOAT32:=762.763e0;
<*NOWARN*> VAR vi16:INT16:=764;
<*NOWARN*> VAR vC:CARDINAL:=765;
<*NOWARN*> VAR vu32:UINT32:=766;
<*NOWARN*> VAR vu8:UINT8:=767;
<*NOWARN*> VAR vL:LONGINT:=768L;
<*NOWARN*>VAR offset, count:CARDINAL;

(* RightRotate *)

<*NOWARN*>PROCEDURE uRightRotate_var_i8_I():Word.T=BEGIN RETURN Word.RightRotate(vi8,vI);END uRightRotate_var_i8_I;
<*NOWARN*>PROCEDURE uRightRotate_param_i8_I(a:INT8;b:INTEGER):Word.T=BEGIN RETURN Word.RightRotate(a,b);END uRightRotate_param_i8_I;
<*NOWARN*>PROCEDURE uRightRotate_var_i8_C():Word.T=BEGIN RETURN Word.RightRotate(vi8,vC);END uRightRotate_var_i8_C;
<*NOWARN*>PROCEDURE uRightRotate_param_i8_C(a:INT8;b:CARDINAL):Word.T=BEGIN RETURN Word.RightRotate(a,b);END uRightRotate_param_i8_C;
<*NOWARN*>PROCEDURE uRightRotate_var_u64_I():Long.T=BEGIN RETURN Long.RightRotate(vu64,vI);END uRightRotate_var_u64_I;
<*NOWARN*>PROCEDURE uRightRotate_param_u64_I(a:UINT64;b:INTEGER):Long.T=BEGIN RETURN Long.RightRotate(a,b);END uRightRotate_param_u64_I;
<*NOWARN*>PROCEDURE uRightRotate_var_u64_C():Long.T=BEGIN RETURN Long.RightRotate(vu64,vC);END uRightRotate_var_u64_C;
<*NOWARN*>PROCEDURE uRightRotate_param_u64_C(a:UINT64;b:CARDINAL):Long.T=BEGIN RETURN Long.RightRotate(a,b);END uRightRotate_param_u64_C;
<*NOWARN*>PROCEDURE uRightRotate_var_i32_I():Word.T=BEGIN RETURN Word.RightRotate(vi32,vI);END uRightRotate_var_i32_I;
<*NOWARN*>PROCEDURE uRightRotate_param_i32_I(a:INT32;b:INTEGER):Word.T=BEGIN RETURN Word.RightRotate(a,b);END uRightRotate_param_i32_I;
<*NOWARN*>PROCEDURE uRightRotate_var_i32_C():Word.T=BEGIN RETURN Word.RightRotate(vi32,vC);END uRightRotate_var_i32_C;
<*NOWARN*>PROCEDURE uRightRotate_param_i32_C(a:INT32;b:CARDINAL):Word.T=BEGIN RETURN Word.RightRotate(a,b);END uRightRotate_param_i32_C;
<*NOWARN*>PROCEDURE uRightRotate_var_LC_I():Long.T=BEGIN RETURN Long.RightRotate(vLC,vI);END uRightRotate_var_LC_I;
<*NOWARN*>PROCEDURE uRightRotate_param_LC_I(a:LONGCARD;b:INTEGER):Long.T=BEGIN RETURN Long.RightRotate(a,b);END uRightRotate_param_LC_I;
<*NOWARN*>PROCEDURE uRightRotate_var_LC_C():Long.T=BEGIN RETURN Long.RightRotate(vLC,vC);END uRightRotate_var_LC_C;
<*NOWARN*>PROCEDURE uRightRotate_param_LC_C(a:LONGCARD;b:CARDINAL):Long.T=BEGIN RETURN Long.RightRotate(a,b);END uRightRotate_param_LC_C;
<*NOWARN*>PROCEDURE uRightRotate_var_u16_I():Word.T=BEGIN RETURN Word.RightRotate(vu16,vI);END uRightRotate_var_u16_I;
<*NOWARN*>PROCEDURE uRightRotate_param_u16_I(a:UINT16;b:INTEGER):Word.T=BEGIN RETURN Word.RightRotate(a,b);END uRightRotate_param_u16_I;
<*NOWARN*>PROCEDURE uRightRotate_var_u16_C():Word.T=BEGIN RETURN Word.RightRotate(vu16,vC);END uRightRotate_var_u16_C;
<*NOWARN*>PROCEDURE uRightRotate_param_u16_C(a:UINT16;b:CARDINAL):Word.T=BEGIN RETURN Word.RightRotate(a,b);END uRightRotate_param_u16_C;
<*NOWARN*>PROCEDURE uRightRotate_var_I_I():Word.T=BEGIN RETURN Word.RightRotate(vI,vI);END uRightRotate_var_I_I;
<*NOWARN*>PROCEDURE uRightRotate_param_I_I(a:INTEGER;b:INTEGER):Word.T=BEGIN RETURN Word.RightRotate(a,b);END uRightRotate_param_I_I;
<*NOWARN*>PROCEDURE uRightRotate_var_I_C():Word.T=BEGIN RETURN Word.RightRotate(vI,vC);END uRightRotate_var_I_C;
<*NOWARN*>PROCEDURE uRightRotate_param_I_C(a:INTEGER;b:CARDINAL):Word.T=BEGIN RETURN Word.RightRotate(a,b);END uRightRotate_param_I_C;
<*NOWARN*>PROCEDURE uRightRotate_var_i64_I():Long.T=BEGIN RETURN Long.RightRotate(vi64,vI);END uRightRotate_var_i64_I;
<*NOWARN*>PROCEDURE uRightRotate_param_i64_I(a:INT64;b:INTEGER):Long.T=BEGIN RETURN Long.RightRotate(a,b);END uRightRotate_param_i64_I;
<*NOWARN*>PROCEDURE uRightRotate_var_i64_C():Long.T=BEGIN RETURN Long.RightRotate(vi64,vC);END uRightRotate_var_i64_C;
<*NOWARN*>PROCEDURE uRightRotate_param_i64_C(a:INT64;b:CARDINAL):Long.T=BEGIN RETURN Long.RightRotate(a,b);END uRightRotate_param_i64_C;
<*NOWARN*>PROCEDURE uRightRotate_var_i16_I():Word.T=BEGIN RETURN Word.RightRotate(vi16,vI);END uRightRotate_var_i16_I;
<*NOWARN*>PROCEDURE uRightRotate_param_i16_I(a:INT16;b:INTEGER):Word.T=BEGIN RETURN Word.RightRotate(a,b);END uRightRotate_param_i16_I;
<*NOWARN*>PROCEDURE uRightRotate_var_i16_C():Word.T=BEGIN RETURN Word.RightRotate(vi16,vC);END uRightRotate_var_i16_C;
<*NOWARN*>PROCEDURE uRightRotate_param_i16_C(a:INT16;b:CARDINAL):Word.T=BEGIN RETURN Word.RightRotate(a,b);END uRightRotate_param_i16_C;
<*NOWARN*>PROCEDURE uRightRotate_var_C_I():Word.T=BEGIN RETURN Word.RightRotate(vC,vI);END uRightRotate_var_C_I;
<*NOWARN*>PROCEDURE uRightRotate_param_C_I(a:CARDINAL;b:INTEGER):Word.T=BEGIN RETURN Word.RightRotate(a,b);END uRightRotate_param_C_I;
<*NOWARN*>PROCEDURE uRightRotate_var_C_C():Word.T=BEGIN RETURN Word.RightRotate(vC,vC);END uRightRotate_var_C_C;
<*NOWARN*>PROCEDURE uRightRotate_param_C_C(a:CARDINAL;b:CARDINAL):Word.T=BEGIN RETURN Word.RightRotate(a,b);END uRightRotate_param_C_C;
<*NOWARN*>PROCEDURE uRightRotate_var_u32_I():Word.T=BEGIN RETURN Word.RightRotate(vu32,vI);END uRightRotate_var_u32_I;
<*NOWARN*>PROCEDURE uRightRotate_param_u32_I(a:UINT32;b:INTEGER):Word.T=BEGIN RETURN Word.RightRotate(a,b);END uRightRotate_param_u32_I;
<*NOWARN*>PROCEDURE uRightRotate_var_u32_C():Word.T=BEGIN RETURN Word.RightRotate(vu32,vC);END uRightRotate_var_u32_C;
<*NOWARN*>PROCEDURE uRightRotate_param_u32_C(a:UINT32;b:CARDINAL):Word.T=BEGIN RETURN Word.RightRotate(a,b);END uRightRotate_param_u32_C;
<*NOWARN*>PROCEDURE uRightRotate_var_u8_I():Word.T=BEGIN RETURN Word.RightRotate(vu8,vI);END uRightRotate_var_u8_I;
<*NOWARN*>PROCEDURE uRightRotate_param_u8_I(a:UINT8;b:INTEGER):Word.T=BEGIN RETURN Word.RightRotate(a,b);END uRightRotate_param_u8_I;
<*NOWARN*>PROCEDURE uRightRotate_var_u8_C():Word.T=BEGIN RETURN Word.RightRotate(vu8,vC);END uRightRotate_var_u8_C;
<*NOWARN*>PROCEDURE uRightRotate_param_u8_C(a:UINT8;b:CARDINAL):Word.T=BEGIN RETURN Word.RightRotate(a,b);END uRightRotate_param_u8_C;
<*NOWARN*>PROCEDURE uRightRotate_var_L_I():Long.T=BEGIN RETURN Long.RightRotate(vL,vI);END uRightRotate_var_L_I;
<*NOWARN*>PROCEDURE uRightRotate_param_L_I(a:LONGINT;b:INTEGER):Long.T=BEGIN RETURN Long.RightRotate(a,b);END uRightRotate_param_L_I;
<*NOWARN*>PROCEDURE uRightRotate_var_L_C():Long.T=BEGIN RETURN Long.RightRotate(vL,vC);END uRightRotate_var_L_C;
<*NOWARN*>PROCEDURE uRightRotate_param_L_C(a:LONGINT;b:CARDINAL):Long.T=BEGIN RETURN Long.RightRotate(a,b);END uRightRotate_param_L_C;
BEGIN
END RightRotate.
