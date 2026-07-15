MODULE LeftShift;
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

<*NOWARN*> CONST ki8:INT8=609;
<*NOWARN*> CONST ku64:UINT64=610L;
<*NOWARN*> CONST kf64:FLOAT64=611.612d0;
<*NOWARN*> CONST ki32:INT32=613;
<*NOWARN*> CONST kLC:LONGCARD=614L;
<*NOWARN*> CONST ku16:UINT16=615;
<*NOWARN*> CONST kI:INTEGER=616;
<*NOWARN*> CONST ki64:INT64=617L;
<*NOWARN*> CONST kf32:FLOAT32=618.619e0;
<*NOWARN*> CONST ki16:INT16=620;
<*NOWARN*> CONST kC:CARDINAL=621;
<*NOWARN*> CONST ku32:UINT32=622;
<*NOWARN*> CONST ku8:UINT8=623;
<*NOWARN*> CONST kL:LONGINT=624L;

(* variables *)

<*NOWARN*> VAR vi8:INT8:=625;
<*NOWARN*> VAR vu64:UINT64:=626L;
<*NOWARN*> VAR vf64:FLOAT64:=627.628d0;
<*NOWARN*> VAR vi32:INT32:=629;
<*NOWARN*> VAR vLC:LONGCARD:=630L;
<*NOWARN*> VAR vu16:UINT16:=631;
<*NOWARN*> VAR vI:INTEGER:=632;
<*NOWARN*> VAR vi64:INT64:=633L;
<*NOWARN*> VAR vf32:FLOAT32:=634.635e0;
<*NOWARN*> VAR vi16:INT16:=636;
<*NOWARN*> VAR vC:CARDINAL:=637;
<*NOWARN*> VAR vu32:UINT32:=638;
<*NOWARN*> VAR vu8:UINT8:=639;
<*NOWARN*> VAR vL:LONGINT:=640L;
<*NOWARN*>VAR offset, count:CARDINAL;

(* LeftShift *)

<*NOWARN*>PROCEDURE uLeftShift_var_i8_I():Word.T=BEGIN RETURN Word.LeftShift(vi8,vI);END uLeftShift_var_i8_I;
<*NOWARN*>PROCEDURE uLeftShift_param_i8_I(a:INT8;b:INTEGER):Word.T=BEGIN RETURN Word.LeftShift(a,b);END uLeftShift_param_i8_I;
<*NOWARN*>PROCEDURE uLeftShift_var_i8_C():Word.T=BEGIN RETURN Word.LeftShift(vi8,vC);END uLeftShift_var_i8_C;
<*NOWARN*>PROCEDURE uLeftShift_param_i8_C(a:INT8;b:CARDINAL):Word.T=BEGIN RETURN Word.LeftShift(a,b);END uLeftShift_param_i8_C;
<*NOWARN*>PROCEDURE uLeftShift_var_u64_I():Long.T=BEGIN RETURN Long.LeftShift(vu64,vI);END uLeftShift_var_u64_I;
<*NOWARN*>PROCEDURE uLeftShift_param_u64_I(a:UINT64;b:INTEGER):Long.T=BEGIN RETURN Long.LeftShift(a,b);END uLeftShift_param_u64_I;
<*NOWARN*>PROCEDURE uLeftShift_var_u64_C():Long.T=BEGIN RETURN Long.LeftShift(vu64,vC);END uLeftShift_var_u64_C;
<*NOWARN*>PROCEDURE uLeftShift_param_u64_C(a:UINT64;b:CARDINAL):Long.T=BEGIN RETURN Long.LeftShift(a,b);END uLeftShift_param_u64_C;
<*NOWARN*>PROCEDURE uLeftShift_var_i32_I():Word.T=BEGIN RETURN Word.LeftShift(vi32,vI);END uLeftShift_var_i32_I;
<*NOWARN*>PROCEDURE uLeftShift_param_i32_I(a:INT32;b:INTEGER):Word.T=BEGIN RETURN Word.LeftShift(a,b);END uLeftShift_param_i32_I;
<*NOWARN*>PROCEDURE uLeftShift_var_i32_C():Word.T=BEGIN RETURN Word.LeftShift(vi32,vC);END uLeftShift_var_i32_C;
<*NOWARN*>PROCEDURE uLeftShift_param_i32_C(a:INT32;b:CARDINAL):Word.T=BEGIN RETURN Word.LeftShift(a,b);END uLeftShift_param_i32_C;
<*NOWARN*>PROCEDURE uLeftShift_var_LC_I():Long.T=BEGIN RETURN Long.LeftShift(vLC,vI);END uLeftShift_var_LC_I;
<*NOWARN*>PROCEDURE uLeftShift_param_LC_I(a:LONGCARD;b:INTEGER):Long.T=BEGIN RETURN Long.LeftShift(a,b);END uLeftShift_param_LC_I;
<*NOWARN*>PROCEDURE uLeftShift_var_LC_C():Long.T=BEGIN RETURN Long.LeftShift(vLC,vC);END uLeftShift_var_LC_C;
<*NOWARN*>PROCEDURE uLeftShift_param_LC_C(a:LONGCARD;b:CARDINAL):Long.T=BEGIN RETURN Long.LeftShift(a,b);END uLeftShift_param_LC_C;
<*NOWARN*>PROCEDURE uLeftShift_var_u16_I():Word.T=BEGIN RETURN Word.LeftShift(vu16,vI);END uLeftShift_var_u16_I;
<*NOWARN*>PROCEDURE uLeftShift_param_u16_I(a:UINT16;b:INTEGER):Word.T=BEGIN RETURN Word.LeftShift(a,b);END uLeftShift_param_u16_I;
<*NOWARN*>PROCEDURE uLeftShift_var_u16_C():Word.T=BEGIN RETURN Word.LeftShift(vu16,vC);END uLeftShift_var_u16_C;
<*NOWARN*>PROCEDURE uLeftShift_param_u16_C(a:UINT16;b:CARDINAL):Word.T=BEGIN RETURN Word.LeftShift(a,b);END uLeftShift_param_u16_C;
<*NOWARN*>PROCEDURE uLeftShift_var_I_I():Word.T=BEGIN RETURN Word.LeftShift(vI,vI);END uLeftShift_var_I_I;
<*NOWARN*>PROCEDURE uLeftShift_param_I_I(a:INTEGER;b:INTEGER):Word.T=BEGIN RETURN Word.LeftShift(a,b);END uLeftShift_param_I_I;
<*NOWARN*>PROCEDURE uLeftShift_var_I_C():Word.T=BEGIN RETURN Word.LeftShift(vI,vC);END uLeftShift_var_I_C;
<*NOWARN*>PROCEDURE uLeftShift_param_I_C(a:INTEGER;b:CARDINAL):Word.T=BEGIN RETURN Word.LeftShift(a,b);END uLeftShift_param_I_C;
<*NOWARN*>PROCEDURE uLeftShift_var_i64_I():Long.T=BEGIN RETURN Long.LeftShift(vi64,vI);END uLeftShift_var_i64_I;
<*NOWARN*>PROCEDURE uLeftShift_param_i64_I(a:INT64;b:INTEGER):Long.T=BEGIN RETURN Long.LeftShift(a,b);END uLeftShift_param_i64_I;
<*NOWARN*>PROCEDURE uLeftShift_var_i64_C():Long.T=BEGIN RETURN Long.LeftShift(vi64,vC);END uLeftShift_var_i64_C;
<*NOWARN*>PROCEDURE uLeftShift_param_i64_C(a:INT64;b:CARDINAL):Long.T=BEGIN RETURN Long.LeftShift(a,b);END uLeftShift_param_i64_C;
<*NOWARN*>PROCEDURE uLeftShift_var_i16_I():Word.T=BEGIN RETURN Word.LeftShift(vi16,vI);END uLeftShift_var_i16_I;
<*NOWARN*>PROCEDURE uLeftShift_param_i16_I(a:INT16;b:INTEGER):Word.T=BEGIN RETURN Word.LeftShift(a,b);END uLeftShift_param_i16_I;
<*NOWARN*>PROCEDURE uLeftShift_var_i16_C():Word.T=BEGIN RETURN Word.LeftShift(vi16,vC);END uLeftShift_var_i16_C;
<*NOWARN*>PROCEDURE uLeftShift_param_i16_C(a:INT16;b:CARDINAL):Word.T=BEGIN RETURN Word.LeftShift(a,b);END uLeftShift_param_i16_C;
<*NOWARN*>PROCEDURE uLeftShift_var_C_I():Word.T=BEGIN RETURN Word.LeftShift(vC,vI);END uLeftShift_var_C_I;
<*NOWARN*>PROCEDURE uLeftShift_param_C_I(a:CARDINAL;b:INTEGER):Word.T=BEGIN RETURN Word.LeftShift(a,b);END uLeftShift_param_C_I;
<*NOWARN*>PROCEDURE uLeftShift_var_C_C():Word.T=BEGIN RETURN Word.LeftShift(vC,vC);END uLeftShift_var_C_C;
<*NOWARN*>PROCEDURE uLeftShift_param_C_C(a:CARDINAL;b:CARDINAL):Word.T=BEGIN RETURN Word.LeftShift(a,b);END uLeftShift_param_C_C;
<*NOWARN*>PROCEDURE uLeftShift_var_u32_I():Word.T=BEGIN RETURN Word.LeftShift(vu32,vI);END uLeftShift_var_u32_I;
<*NOWARN*>PROCEDURE uLeftShift_param_u32_I(a:UINT32;b:INTEGER):Word.T=BEGIN RETURN Word.LeftShift(a,b);END uLeftShift_param_u32_I;
<*NOWARN*>PROCEDURE uLeftShift_var_u32_C():Word.T=BEGIN RETURN Word.LeftShift(vu32,vC);END uLeftShift_var_u32_C;
<*NOWARN*>PROCEDURE uLeftShift_param_u32_C(a:UINT32;b:CARDINAL):Word.T=BEGIN RETURN Word.LeftShift(a,b);END uLeftShift_param_u32_C;
<*NOWARN*>PROCEDURE uLeftShift_var_u8_I():Word.T=BEGIN RETURN Word.LeftShift(vu8,vI);END uLeftShift_var_u8_I;
<*NOWARN*>PROCEDURE uLeftShift_param_u8_I(a:UINT8;b:INTEGER):Word.T=BEGIN RETURN Word.LeftShift(a,b);END uLeftShift_param_u8_I;
<*NOWARN*>PROCEDURE uLeftShift_var_u8_C():Word.T=BEGIN RETURN Word.LeftShift(vu8,vC);END uLeftShift_var_u8_C;
<*NOWARN*>PROCEDURE uLeftShift_param_u8_C(a:UINT8;b:CARDINAL):Word.T=BEGIN RETURN Word.LeftShift(a,b);END uLeftShift_param_u8_C;
<*NOWARN*>PROCEDURE uLeftShift_var_L_I():Long.T=BEGIN RETURN Long.LeftShift(vL,vI);END uLeftShift_var_L_I;
<*NOWARN*>PROCEDURE uLeftShift_param_L_I(a:LONGINT;b:INTEGER):Long.T=BEGIN RETURN Long.LeftShift(a,b);END uLeftShift_param_L_I;
<*NOWARN*>PROCEDURE uLeftShift_var_L_C():Long.T=BEGIN RETURN Long.LeftShift(vL,vC);END uLeftShift_var_L_C;
<*NOWARN*>PROCEDURE uLeftShift_param_L_C(a:LONGINT;b:CARDINAL):Long.T=BEGIN RETURN Long.LeftShift(a,b);END uLeftShift_param_L_C;
BEGIN
END LeftShift.
