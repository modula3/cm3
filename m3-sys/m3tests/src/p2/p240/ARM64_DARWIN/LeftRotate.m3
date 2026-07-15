MODULE LeftRotate;
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

<*NOWARN*> CONST ki8:INT8=705;
<*NOWARN*> CONST ku64:UINT64=706L;
<*NOWARN*> CONST kf64:FLOAT64=707.708d0;
<*NOWARN*> CONST ki32:INT32=709;
<*NOWARN*> CONST kLC:LONGCARD=710L;
<*NOWARN*> CONST ku16:UINT16=711;
<*NOWARN*> CONST kI:INTEGER=712;
<*NOWARN*> CONST ki64:INT64=713L;
<*NOWARN*> CONST kf32:FLOAT32=714.715e0;
<*NOWARN*> CONST ki16:INT16=716;
<*NOWARN*> CONST kC:CARDINAL=717;
<*NOWARN*> CONST ku32:UINT32=718;
<*NOWARN*> CONST ku8:UINT8=719;
<*NOWARN*> CONST kL:LONGINT=720L;

(* variables *)

<*NOWARN*> VAR vi8:INT8:=721;
<*NOWARN*> VAR vu64:UINT64:=722L;
<*NOWARN*> VAR vf64:FLOAT64:=723.724d0;
<*NOWARN*> VAR vi32:INT32:=725;
<*NOWARN*> VAR vLC:LONGCARD:=726L;
<*NOWARN*> VAR vu16:UINT16:=727;
<*NOWARN*> VAR vI:INTEGER:=728;
<*NOWARN*> VAR vi64:INT64:=729L;
<*NOWARN*> VAR vf32:FLOAT32:=730.731e0;
<*NOWARN*> VAR vi16:INT16:=732;
<*NOWARN*> VAR vC:CARDINAL:=733;
<*NOWARN*> VAR vu32:UINT32:=734;
<*NOWARN*> VAR vu8:UINT8:=735;
<*NOWARN*> VAR vL:LONGINT:=736L;
<*NOWARN*>VAR offset, count:CARDINAL;

(* LeftRotate *)

<*NOWARN*>PROCEDURE uLeftRotate_var_i8_I():Word.T=BEGIN RETURN Word.LeftRotate(vi8,vI);END uLeftRotate_var_i8_I;
<*NOWARN*>PROCEDURE uLeftRotate_param_i8_I(a:INT8;b:INTEGER):Word.T=BEGIN RETURN Word.LeftRotate(a,b);END uLeftRotate_param_i8_I;
<*NOWARN*>PROCEDURE uLeftRotate_var_i8_C():Word.T=BEGIN RETURN Word.LeftRotate(vi8,vC);END uLeftRotate_var_i8_C;
<*NOWARN*>PROCEDURE uLeftRotate_param_i8_C(a:INT8;b:CARDINAL):Word.T=BEGIN RETURN Word.LeftRotate(a,b);END uLeftRotate_param_i8_C;
<*NOWARN*>PROCEDURE uLeftRotate_var_u64_I():Long.T=BEGIN RETURN Long.LeftRotate(vu64,vI);END uLeftRotate_var_u64_I;
<*NOWARN*>PROCEDURE uLeftRotate_param_u64_I(a:UINT64;b:INTEGER):Long.T=BEGIN RETURN Long.LeftRotate(a,b);END uLeftRotate_param_u64_I;
<*NOWARN*>PROCEDURE uLeftRotate_var_u64_C():Long.T=BEGIN RETURN Long.LeftRotate(vu64,vC);END uLeftRotate_var_u64_C;
<*NOWARN*>PROCEDURE uLeftRotate_param_u64_C(a:UINT64;b:CARDINAL):Long.T=BEGIN RETURN Long.LeftRotate(a,b);END uLeftRotate_param_u64_C;
<*NOWARN*>PROCEDURE uLeftRotate_var_i32_I():Word.T=BEGIN RETURN Word.LeftRotate(vi32,vI);END uLeftRotate_var_i32_I;
<*NOWARN*>PROCEDURE uLeftRotate_param_i32_I(a:INT32;b:INTEGER):Word.T=BEGIN RETURN Word.LeftRotate(a,b);END uLeftRotate_param_i32_I;
<*NOWARN*>PROCEDURE uLeftRotate_var_i32_C():Word.T=BEGIN RETURN Word.LeftRotate(vi32,vC);END uLeftRotate_var_i32_C;
<*NOWARN*>PROCEDURE uLeftRotate_param_i32_C(a:INT32;b:CARDINAL):Word.T=BEGIN RETURN Word.LeftRotate(a,b);END uLeftRotate_param_i32_C;
<*NOWARN*>PROCEDURE uLeftRotate_var_LC_I():Long.T=BEGIN RETURN Long.LeftRotate(vLC,vI);END uLeftRotate_var_LC_I;
<*NOWARN*>PROCEDURE uLeftRotate_param_LC_I(a:LONGCARD;b:INTEGER):Long.T=BEGIN RETURN Long.LeftRotate(a,b);END uLeftRotate_param_LC_I;
<*NOWARN*>PROCEDURE uLeftRotate_var_LC_C():Long.T=BEGIN RETURN Long.LeftRotate(vLC,vC);END uLeftRotate_var_LC_C;
<*NOWARN*>PROCEDURE uLeftRotate_param_LC_C(a:LONGCARD;b:CARDINAL):Long.T=BEGIN RETURN Long.LeftRotate(a,b);END uLeftRotate_param_LC_C;
<*NOWARN*>PROCEDURE uLeftRotate_var_u16_I():Word.T=BEGIN RETURN Word.LeftRotate(vu16,vI);END uLeftRotate_var_u16_I;
<*NOWARN*>PROCEDURE uLeftRotate_param_u16_I(a:UINT16;b:INTEGER):Word.T=BEGIN RETURN Word.LeftRotate(a,b);END uLeftRotate_param_u16_I;
<*NOWARN*>PROCEDURE uLeftRotate_var_u16_C():Word.T=BEGIN RETURN Word.LeftRotate(vu16,vC);END uLeftRotate_var_u16_C;
<*NOWARN*>PROCEDURE uLeftRotate_param_u16_C(a:UINT16;b:CARDINAL):Word.T=BEGIN RETURN Word.LeftRotate(a,b);END uLeftRotate_param_u16_C;
<*NOWARN*>PROCEDURE uLeftRotate_var_I_I():Word.T=BEGIN RETURN Word.LeftRotate(vI,vI);END uLeftRotate_var_I_I;
<*NOWARN*>PROCEDURE uLeftRotate_param_I_I(a:INTEGER;b:INTEGER):Word.T=BEGIN RETURN Word.LeftRotate(a,b);END uLeftRotate_param_I_I;
<*NOWARN*>PROCEDURE uLeftRotate_var_I_C():Word.T=BEGIN RETURN Word.LeftRotate(vI,vC);END uLeftRotate_var_I_C;
<*NOWARN*>PROCEDURE uLeftRotate_param_I_C(a:INTEGER;b:CARDINAL):Word.T=BEGIN RETURN Word.LeftRotate(a,b);END uLeftRotate_param_I_C;
<*NOWARN*>PROCEDURE uLeftRotate_var_i64_I():Long.T=BEGIN RETURN Long.LeftRotate(vi64,vI);END uLeftRotate_var_i64_I;
<*NOWARN*>PROCEDURE uLeftRotate_param_i64_I(a:INT64;b:INTEGER):Long.T=BEGIN RETURN Long.LeftRotate(a,b);END uLeftRotate_param_i64_I;
<*NOWARN*>PROCEDURE uLeftRotate_var_i64_C():Long.T=BEGIN RETURN Long.LeftRotate(vi64,vC);END uLeftRotate_var_i64_C;
<*NOWARN*>PROCEDURE uLeftRotate_param_i64_C(a:INT64;b:CARDINAL):Long.T=BEGIN RETURN Long.LeftRotate(a,b);END uLeftRotate_param_i64_C;
<*NOWARN*>PROCEDURE uLeftRotate_var_i16_I():Word.T=BEGIN RETURN Word.LeftRotate(vi16,vI);END uLeftRotate_var_i16_I;
<*NOWARN*>PROCEDURE uLeftRotate_param_i16_I(a:INT16;b:INTEGER):Word.T=BEGIN RETURN Word.LeftRotate(a,b);END uLeftRotate_param_i16_I;
<*NOWARN*>PROCEDURE uLeftRotate_var_i16_C():Word.T=BEGIN RETURN Word.LeftRotate(vi16,vC);END uLeftRotate_var_i16_C;
<*NOWARN*>PROCEDURE uLeftRotate_param_i16_C(a:INT16;b:CARDINAL):Word.T=BEGIN RETURN Word.LeftRotate(a,b);END uLeftRotate_param_i16_C;
<*NOWARN*>PROCEDURE uLeftRotate_var_C_I():Word.T=BEGIN RETURN Word.LeftRotate(vC,vI);END uLeftRotate_var_C_I;
<*NOWARN*>PROCEDURE uLeftRotate_param_C_I(a:CARDINAL;b:INTEGER):Word.T=BEGIN RETURN Word.LeftRotate(a,b);END uLeftRotate_param_C_I;
<*NOWARN*>PROCEDURE uLeftRotate_var_C_C():Word.T=BEGIN RETURN Word.LeftRotate(vC,vC);END uLeftRotate_var_C_C;
<*NOWARN*>PROCEDURE uLeftRotate_param_C_C(a:CARDINAL;b:CARDINAL):Word.T=BEGIN RETURN Word.LeftRotate(a,b);END uLeftRotate_param_C_C;
<*NOWARN*>PROCEDURE uLeftRotate_var_u32_I():Word.T=BEGIN RETURN Word.LeftRotate(vu32,vI);END uLeftRotate_var_u32_I;
<*NOWARN*>PROCEDURE uLeftRotate_param_u32_I(a:UINT32;b:INTEGER):Word.T=BEGIN RETURN Word.LeftRotate(a,b);END uLeftRotate_param_u32_I;
<*NOWARN*>PROCEDURE uLeftRotate_var_u32_C():Word.T=BEGIN RETURN Word.LeftRotate(vu32,vC);END uLeftRotate_var_u32_C;
<*NOWARN*>PROCEDURE uLeftRotate_param_u32_C(a:UINT32;b:CARDINAL):Word.T=BEGIN RETURN Word.LeftRotate(a,b);END uLeftRotate_param_u32_C;
<*NOWARN*>PROCEDURE uLeftRotate_var_u8_I():Word.T=BEGIN RETURN Word.LeftRotate(vu8,vI);END uLeftRotate_var_u8_I;
<*NOWARN*>PROCEDURE uLeftRotate_param_u8_I(a:UINT8;b:INTEGER):Word.T=BEGIN RETURN Word.LeftRotate(a,b);END uLeftRotate_param_u8_I;
<*NOWARN*>PROCEDURE uLeftRotate_var_u8_C():Word.T=BEGIN RETURN Word.LeftRotate(vu8,vC);END uLeftRotate_var_u8_C;
<*NOWARN*>PROCEDURE uLeftRotate_param_u8_C(a:UINT8;b:CARDINAL):Word.T=BEGIN RETURN Word.LeftRotate(a,b);END uLeftRotate_param_u8_C;
<*NOWARN*>PROCEDURE uLeftRotate_var_L_I():Long.T=BEGIN RETURN Long.LeftRotate(vL,vI);END uLeftRotate_var_L_I;
<*NOWARN*>PROCEDURE uLeftRotate_param_L_I(a:LONGINT;b:INTEGER):Long.T=BEGIN RETURN Long.LeftRotate(a,b);END uLeftRotate_param_L_I;
<*NOWARN*>PROCEDURE uLeftRotate_var_L_C():Long.T=BEGIN RETURN Long.LeftRotate(vL,vC);END uLeftRotate_var_L_C;
<*NOWARN*>PROCEDURE uLeftRotate_param_L_C(a:LONGINT;b:CARDINAL):Long.T=BEGIN RETURN Long.LeftRotate(a,b);END uLeftRotate_param_L_C;
BEGIN
END LeftRotate.
