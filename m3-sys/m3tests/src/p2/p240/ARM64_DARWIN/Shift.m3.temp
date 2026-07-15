MODULE Shift;
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

<*NOWARN*> CONST ki8:INT8=673;
<*NOWARN*> CONST ku64:UINT64=674L;
<*NOWARN*> CONST kf64:FLOAT64=675.676d0;
<*NOWARN*> CONST ki32:INT32=677;
<*NOWARN*> CONST kLC:LONGCARD=678L;
<*NOWARN*> CONST ku16:UINT16=679;
<*NOWARN*> CONST kI:INTEGER=680;
<*NOWARN*> CONST ki64:INT64=681L;
<*NOWARN*> CONST kf32:FLOAT32=682.683e0;
<*NOWARN*> CONST ki16:INT16=684;
<*NOWARN*> CONST kC:CARDINAL=685;
<*NOWARN*> CONST ku32:UINT32=686;
<*NOWARN*> CONST ku8:UINT8=687;
<*NOWARN*> CONST kL:LONGINT=688L;

(* variables *)

<*NOWARN*> VAR vi8:INT8:=689;
<*NOWARN*> VAR vu64:UINT64:=690L;
<*NOWARN*> VAR vf64:FLOAT64:=691.692d0;
<*NOWARN*> VAR vi32:INT32:=693;
<*NOWARN*> VAR vLC:LONGCARD:=694L;
<*NOWARN*> VAR vu16:UINT16:=695;
<*NOWARN*> VAR vI:INTEGER:=696;
<*NOWARN*> VAR vi64:INT64:=697L;
<*NOWARN*> VAR vf32:FLOAT32:=698.699e0;
<*NOWARN*> VAR vi16:INT16:=700;
<*NOWARN*> VAR vC:CARDINAL:=701;
<*NOWARN*> VAR vu32:UINT32:=702;
<*NOWARN*> VAR vu8:UINT8:=703;
<*NOWARN*> VAR vL:LONGINT:=704L;
<*NOWARN*>VAR offset, count:CARDINAL;

(* Shift *)

<*NOWARN*>PROCEDURE uShift_var_i8_I():Word.T=BEGIN RETURN Word.Shift(vi8,vI);END uShift_var_i8_I;
<*NOWARN*>PROCEDURE uShift_param_i8_I(a:INT8;b:INTEGER):Word.T=BEGIN RETURN Word.Shift(a,b);END uShift_param_i8_I;
<*NOWARN*>PROCEDURE uShift_var_i8_C():Word.T=BEGIN RETURN Word.Shift(vi8,vC);END uShift_var_i8_C;
<*NOWARN*>PROCEDURE uShift_param_i8_C(a:INT8;b:CARDINAL):Word.T=BEGIN RETURN Word.Shift(a,b);END uShift_param_i8_C;
<*NOWARN*>PROCEDURE uShift_var_u64_I():Long.T=BEGIN RETURN Long.Shift(vu64,vI);END uShift_var_u64_I;
<*NOWARN*>PROCEDURE uShift_param_u64_I(a:UINT64;b:INTEGER):Long.T=BEGIN RETURN Long.Shift(a,b);END uShift_param_u64_I;
<*NOWARN*>PROCEDURE uShift_var_u64_C():Long.T=BEGIN RETURN Long.Shift(vu64,vC);END uShift_var_u64_C;
<*NOWARN*>PROCEDURE uShift_param_u64_C(a:UINT64;b:CARDINAL):Long.T=BEGIN RETURN Long.Shift(a,b);END uShift_param_u64_C;
<*NOWARN*>PROCEDURE uShift_var_i32_I():Word.T=BEGIN RETURN Word.Shift(vi32,vI);END uShift_var_i32_I;
<*NOWARN*>PROCEDURE uShift_param_i32_I(a:INT32;b:INTEGER):Word.T=BEGIN RETURN Word.Shift(a,b);END uShift_param_i32_I;
<*NOWARN*>PROCEDURE uShift_var_i32_C():Word.T=BEGIN RETURN Word.Shift(vi32,vC);END uShift_var_i32_C;
<*NOWARN*>PROCEDURE uShift_param_i32_C(a:INT32;b:CARDINAL):Word.T=BEGIN RETURN Word.Shift(a,b);END uShift_param_i32_C;
<*NOWARN*>PROCEDURE uShift_var_LC_I():Long.T=BEGIN RETURN Long.Shift(vLC,vI);END uShift_var_LC_I;
<*NOWARN*>PROCEDURE uShift_param_LC_I(a:LONGCARD;b:INTEGER):Long.T=BEGIN RETURN Long.Shift(a,b);END uShift_param_LC_I;
<*NOWARN*>PROCEDURE uShift_var_LC_C():Long.T=BEGIN RETURN Long.Shift(vLC,vC);END uShift_var_LC_C;
<*NOWARN*>PROCEDURE uShift_param_LC_C(a:LONGCARD;b:CARDINAL):Long.T=BEGIN RETURN Long.Shift(a,b);END uShift_param_LC_C;
<*NOWARN*>PROCEDURE uShift_var_u16_I():Word.T=BEGIN RETURN Word.Shift(vu16,vI);END uShift_var_u16_I;
<*NOWARN*>PROCEDURE uShift_param_u16_I(a:UINT16;b:INTEGER):Word.T=BEGIN RETURN Word.Shift(a,b);END uShift_param_u16_I;
<*NOWARN*>PROCEDURE uShift_var_u16_C():Word.T=BEGIN RETURN Word.Shift(vu16,vC);END uShift_var_u16_C;
<*NOWARN*>PROCEDURE uShift_param_u16_C(a:UINT16;b:CARDINAL):Word.T=BEGIN RETURN Word.Shift(a,b);END uShift_param_u16_C;
<*NOWARN*>PROCEDURE uShift_var_I_I():Word.T=BEGIN RETURN Word.Shift(vI,vI);END uShift_var_I_I;
<*NOWARN*>PROCEDURE uShift_param_I_I(a:INTEGER;b:INTEGER):Word.T=BEGIN RETURN Word.Shift(a,b);END uShift_param_I_I;
<*NOWARN*>PROCEDURE uShift_var_I_C():Word.T=BEGIN RETURN Word.Shift(vI,vC);END uShift_var_I_C;
<*NOWARN*>PROCEDURE uShift_param_I_C(a:INTEGER;b:CARDINAL):Word.T=BEGIN RETURN Word.Shift(a,b);END uShift_param_I_C;
<*NOWARN*>PROCEDURE uShift_var_i64_I():Long.T=BEGIN RETURN Long.Shift(vi64,vI);END uShift_var_i64_I;
<*NOWARN*>PROCEDURE uShift_param_i64_I(a:INT64;b:INTEGER):Long.T=BEGIN RETURN Long.Shift(a,b);END uShift_param_i64_I;
<*NOWARN*>PROCEDURE uShift_var_i64_C():Long.T=BEGIN RETURN Long.Shift(vi64,vC);END uShift_var_i64_C;
<*NOWARN*>PROCEDURE uShift_param_i64_C(a:INT64;b:CARDINAL):Long.T=BEGIN RETURN Long.Shift(a,b);END uShift_param_i64_C;
<*NOWARN*>PROCEDURE uShift_var_i16_I():Word.T=BEGIN RETURN Word.Shift(vi16,vI);END uShift_var_i16_I;
<*NOWARN*>PROCEDURE uShift_param_i16_I(a:INT16;b:INTEGER):Word.T=BEGIN RETURN Word.Shift(a,b);END uShift_param_i16_I;
<*NOWARN*>PROCEDURE uShift_var_i16_C():Word.T=BEGIN RETURN Word.Shift(vi16,vC);END uShift_var_i16_C;
<*NOWARN*>PROCEDURE uShift_param_i16_C(a:INT16;b:CARDINAL):Word.T=BEGIN RETURN Word.Shift(a,b);END uShift_param_i16_C;
<*NOWARN*>PROCEDURE uShift_var_C_I():Word.T=BEGIN RETURN Word.Shift(vC,vI);END uShift_var_C_I;
<*NOWARN*>PROCEDURE uShift_param_C_I(a:CARDINAL;b:INTEGER):Word.T=BEGIN RETURN Word.Shift(a,b);END uShift_param_C_I;
<*NOWARN*>PROCEDURE uShift_var_C_C():Word.T=BEGIN RETURN Word.Shift(vC,vC);END uShift_var_C_C;
<*NOWARN*>PROCEDURE uShift_param_C_C(a:CARDINAL;b:CARDINAL):Word.T=BEGIN RETURN Word.Shift(a,b);END uShift_param_C_C;
<*NOWARN*>PROCEDURE uShift_var_u32_I():Word.T=BEGIN RETURN Word.Shift(vu32,vI);END uShift_var_u32_I;
<*NOWARN*>PROCEDURE uShift_param_u32_I(a:UINT32;b:INTEGER):Word.T=BEGIN RETURN Word.Shift(a,b);END uShift_param_u32_I;
<*NOWARN*>PROCEDURE uShift_var_u32_C():Word.T=BEGIN RETURN Word.Shift(vu32,vC);END uShift_var_u32_C;
<*NOWARN*>PROCEDURE uShift_param_u32_C(a:UINT32;b:CARDINAL):Word.T=BEGIN RETURN Word.Shift(a,b);END uShift_param_u32_C;
<*NOWARN*>PROCEDURE uShift_var_u8_I():Word.T=BEGIN RETURN Word.Shift(vu8,vI);END uShift_var_u8_I;
<*NOWARN*>PROCEDURE uShift_param_u8_I(a:UINT8;b:INTEGER):Word.T=BEGIN RETURN Word.Shift(a,b);END uShift_param_u8_I;
<*NOWARN*>PROCEDURE uShift_var_u8_C():Word.T=BEGIN RETURN Word.Shift(vu8,vC);END uShift_var_u8_C;
<*NOWARN*>PROCEDURE uShift_param_u8_C(a:UINT8;b:CARDINAL):Word.T=BEGIN RETURN Word.Shift(a,b);END uShift_param_u8_C;
<*NOWARN*>PROCEDURE uShift_var_L_I():Long.T=BEGIN RETURN Long.Shift(vL,vI);END uShift_var_L_I;
<*NOWARN*>PROCEDURE uShift_param_L_I(a:LONGINT;b:INTEGER):Long.T=BEGIN RETURN Long.Shift(a,b);END uShift_param_L_I;
<*NOWARN*>PROCEDURE uShift_var_L_C():Long.T=BEGIN RETURN Long.Shift(vL,vC);END uShift_var_L_C;
<*NOWARN*>PROCEDURE uShift_param_L_C(a:LONGINT;b:CARDINAL):Long.T=BEGIN RETURN Long.Shift(a,b);END uShift_param_L_C;
BEGIN
END Shift.
