MODULE Rotate;
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

<*NOWARN*> CONST ki8:INT8=769;
<*NOWARN*> CONST ku64:UINT64=770L;
<*NOWARN*> CONST kf64:FLOAT64=771.772d0;
<*NOWARN*> CONST ki32:INT32=773;
<*NOWARN*> CONST kLC:LONGCARD=774L;
<*NOWARN*> CONST ku16:UINT16=775;
<*NOWARN*> CONST kI:INTEGER=776;
<*NOWARN*> CONST ki64:INT64=777L;
<*NOWARN*> CONST kf32:FLOAT32=778.779e0;
<*NOWARN*> CONST ki16:INT16=780;
<*NOWARN*> CONST kC:CARDINAL=781;
<*NOWARN*> CONST ku32:UINT32=782;
<*NOWARN*> CONST ku8:UINT8=783;
<*NOWARN*> CONST kL:LONGINT=784L;

(* variables *)

<*NOWARN*> VAR vi8:INT8:=785;
<*NOWARN*> VAR vu64:UINT64:=786L;
<*NOWARN*> VAR vf64:FLOAT64:=787.788d0;
<*NOWARN*> VAR vi32:INT32:=789;
<*NOWARN*> VAR vLC:LONGCARD:=790L;
<*NOWARN*> VAR vu16:UINT16:=791;
<*NOWARN*> VAR vI:INTEGER:=792;
<*NOWARN*> VAR vi64:INT64:=793L;
<*NOWARN*> VAR vf32:FLOAT32:=794.795e0;
<*NOWARN*> VAR vi16:INT16:=796;
<*NOWARN*> VAR vC:CARDINAL:=797;
<*NOWARN*> VAR vu32:UINT32:=798;
<*NOWARN*> VAR vu8:UINT8:=799;
<*NOWARN*> VAR vL:LONGINT:=800L;
<*NOWARN*>VAR offset, count:CARDINAL;

(* Rotate *)

<*NOWARN*>PROCEDURE uRotate_var_i8_I():Word.T=BEGIN RETURN Word.Rotate(vi8,vI);END uRotate_var_i8_I;
<*NOWARN*>PROCEDURE uRotate_param_i8_I(a:INT8;b:INTEGER):Word.T=BEGIN RETURN Word.Rotate(a,b);END uRotate_param_i8_I;
<*NOWARN*>PROCEDURE uRotate_var_i8_C():Word.T=BEGIN RETURN Word.Rotate(vi8,vC);END uRotate_var_i8_C;
<*NOWARN*>PROCEDURE uRotate_param_i8_C(a:INT8;b:CARDINAL):Word.T=BEGIN RETURN Word.Rotate(a,b);END uRotate_param_i8_C;
<*NOWARN*>PROCEDURE uRotate_var_u64_I():Long.T=BEGIN RETURN Long.Rotate(vu64,vI);END uRotate_var_u64_I;
<*NOWARN*>PROCEDURE uRotate_param_u64_I(a:UINT64;b:INTEGER):Long.T=BEGIN RETURN Long.Rotate(a,b);END uRotate_param_u64_I;
<*NOWARN*>PROCEDURE uRotate_var_u64_C():Long.T=BEGIN RETURN Long.Rotate(vu64,vC);END uRotate_var_u64_C;
<*NOWARN*>PROCEDURE uRotate_param_u64_C(a:UINT64;b:CARDINAL):Long.T=BEGIN RETURN Long.Rotate(a,b);END uRotate_param_u64_C;
<*NOWARN*>PROCEDURE uRotate_var_i32_I():Word.T=BEGIN RETURN Word.Rotate(vi32,vI);END uRotate_var_i32_I;
<*NOWARN*>PROCEDURE uRotate_param_i32_I(a:INT32;b:INTEGER):Word.T=BEGIN RETURN Word.Rotate(a,b);END uRotate_param_i32_I;
<*NOWARN*>PROCEDURE uRotate_var_i32_C():Word.T=BEGIN RETURN Word.Rotate(vi32,vC);END uRotate_var_i32_C;
<*NOWARN*>PROCEDURE uRotate_param_i32_C(a:INT32;b:CARDINAL):Word.T=BEGIN RETURN Word.Rotate(a,b);END uRotate_param_i32_C;
<*NOWARN*>PROCEDURE uRotate_var_LC_I():Long.T=BEGIN RETURN Long.Rotate(vLC,vI);END uRotate_var_LC_I;
<*NOWARN*>PROCEDURE uRotate_param_LC_I(a:LONGCARD;b:INTEGER):Long.T=BEGIN RETURN Long.Rotate(a,b);END uRotate_param_LC_I;
<*NOWARN*>PROCEDURE uRotate_var_LC_C():Long.T=BEGIN RETURN Long.Rotate(vLC,vC);END uRotate_var_LC_C;
<*NOWARN*>PROCEDURE uRotate_param_LC_C(a:LONGCARD;b:CARDINAL):Long.T=BEGIN RETURN Long.Rotate(a,b);END uRotate_param_LC_C;
<*NOWARN*>PROCEDURE uRotate_var_u16_I():Word.T=BEGIN RETURN Word.Rotate(vu16,vI);END uRotate_var_u16_I;
<*NOWARN*>PROCEDURE uRotate_param_u16_I(a:UINT16;b:INTEGER):Word.T=BEGIN RETURN Word.Rotate(a,b);END uRotate_param_u16_I;
<*NOWARN*>PROCEDURE uRotate_var_u16_C():Word.T=BEGIN RETURN Word.Rotate(vu16,vC);END uRotate_var_u16_C;
<*NOWARN*>PROCEDURE uRotate_param_u16_C(a:UINT16;b:CARDINAL):Word.T=BEGIN RETURN Word.Rotate(a,b);END uRotate_param_u16_C;
<*NOWARN*>PROCEDURE uRotate_var_I_I():Word.T=BEGIN RETURN Word.Rotate(vI,vI);END uRotate_var_I_I;
<*NOWARN*>PROCEDURE uRotate_param_I_I(a:INTEGER;b:INTEGER):Word.T=BEGIN RETURN Word.Rotate(a,b);END uRotate_param_I_I;
<*NOWARN*>PROCEDURE uRotate_var_I_C():Word.T=BEGIN RETURN Word.Rotate(vI,vC);END uRotate_var_I_C;
<*NOWARN*>PROCEDURE uRotate_param_I_C(a:INTEGER;b:CARDINAL):Word.T=BEGIN RETURN Word.Rotate(a,b);END uRotate_param_I_C;
<*NOWARN*>PROCEDURE uRotate_var_i64_I():Long.T=BEGIN RETURN Long.Rotate(vi64,vI);END uRotate_var_i64_I;
<*NOWARN*>PROCEDURE uRotate_param_i64_I(a:INT64;b:INTEGER):Long.T=BEGIN RETURN Long.Rotate(a,b);END uRotate_param_i64_I;
<*NOWARN*>PROCEDURE uRotate_var_i64_C():Long.T=BEGIN RETURN Long.Rotate(vi64,vC);END uRotate_var_i64_C;
<*NOWARN*>PROCEDURE uRotate_param_i64_C(a:INT64;b:CARDINAL):Long.T=BEGIN RETURN Long.Rotate(a,b);END uRotate_param_i64_C;
<*NOWARN*>PROCEDURE uRotate_var_i16_I():Word.T=BEGIN RETURN Word.Rotate(vi16,vI);END uRotate_var_i16_I;
<*NOWARN*>PROCEDURE uRotate_param_i16_I(a:INT16;b:INTEGER):Word.T=BEGIN RETURN Word.Rotate(a,b);END uRotate_param_i16_I;
<*NOWARN*>PROCEDURE uRotate_var_i16_C():Word.T=BEGIN RETURN Word.Rotate(vi16,vC);END uRotate_var_i16_C;
<*NOWARN*>PROCEDURE uRotate_param_i16_C(a:INT16;b:CARDINAL):Word.T=BEGIN RETURN Word.Rotate(a,b);END uRotate_param_i16_C;
<*NOWARN*>PROCEDURE uRotate_var_C_I():Word.T=BEGIN RETURN Word.Rotate(vC,vI);END uRotate_var_C_I;
<*NOWARN*>PROCEDURE uRotate_param_C_I(a:CARDINAL;b:INTEGER):Word.T=BEGIN RETURN Word.Rotate(a,b);END uRotate_param_C_I;
<*NOWARN*>PROCEDURE uRotate_var_C_C():Word.T=BEGIN RETURN Word.Rotate(vC,vC);END uRotate_var_C_C;
<*NOWARN*>PROCEDURE uRotate_param_C_C(a:CARDINAL;b:CARDINAL):Word.T=BEGIN RETURN Word.Rotate(a,b);END uRotate_param_C_C;
<*NOWARN*>PROCEDURE uRotate_var_u32_I():Word.T=BEGIN RETURN Word.Rotate(vu32,vI);END uRotate_var_u32_I;
<*NOWARN*>PROCEDURE uRotate_param_u32_I(a:UINT32;b:INTEGER):Word.T=BEGIN RETURN Word.Rotate(a,b);END uRotate_param_u32_I;
<*NOWARN*>PROCEDURE uRotate_var_u32_C():Word.T=BEGIN RETURN Word.Rotate(vu32,vC);END uRotate_var_u32_C;
<*NOWARN*>PROCEDURE uRotate_param_u32_C(a:UINT32;b:CARDINAL):Word.T=BEGIN RETURN Word.Rotate(a,b);END uRotate_param_u32_C;
<*NOWARN*>PROCEDURE uRotate_var_u8_I():Word.T=BEGIN RETURN Word.Rotate(vu8,vI);END uRotate_var_u8_I;
<*NOWARN*>PROCEDURE uRotate_param_u8_I(a:UINT8;b:INTEGER):Word.T=BEGIN RETURN Word.Rotate(a,b);END uRotate_param_u8_I;
<*NOWARN*>PROCEDURE uRotate_var_u8_C():Word.T=BEGIN RETURN Word.Rotate(vu8,vC);END uRotate_var_u8_C;
<*NOWARN*>PROCEDURE uRotate_param_u8_C(a:UINT8;b:CARDINAL):Word.T=BEGIN RETURN Word.Rotate(a,b);END uRotate_param_u8_C;
<*NOWARN*>PROCEDURE uRotate_var_L_I():Long.T=BEGIN RETURN Long.Rotate(vL,vI);END uRotate_var_L_I;
<*NOWARN*>PROCEDURE uRotate_param_L_I(a:LONGINT;b:INTEGER):Long.T=BEGIN RETURN Long.Rotate(a,b);END uRotate_param_L_I;
<*NOWARN*>PROCEDURE uRotate_var_L_C():Long.T=BEGIN RETURN Long.Rotate(vL,vC);END uRotate_var_L_C;
<*NOWARN*>PROCEDURE uRotate_param_L_C(a:LONGINT;b:CARDINAL):Long.T=BEGIN RETURN Long.Rotate(a,b);END uRotate_param_L_C;
BEGIN
END Rotate.
