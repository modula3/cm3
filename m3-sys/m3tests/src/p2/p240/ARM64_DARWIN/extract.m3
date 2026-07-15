MODULE extract;
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

<*NOWARN*> CONST ki8:INT8=833;
<*NOWARN*> CONST ku64:UINT64=834L;
<*NOWARN*> CONST kf64:FLOAT64=835.836d0;
<*NOWARN*> CONST ki32:INT32=837;
<*NOWARN*> CONST kLC:LONGCARD=838L;
<*NOWARN*> CONST ku16:UINT16=839;
<*NOWARN*> CONST kI:INTEGER=840;
<*NOWARN*> CONST ki64:INT64=841L;
<*NOWARN*> CONST kf32:FLOAT32=842.843e0;
<*NOWARN*> CONST ki16:INT16=844;
<*NOWARN*> CONST kC:CARDINAL=845;
<*NOWARN*> CONST ku32:UINT32=846;
<*NOWARN*> CONST ku8:UINT8=847;
<*NOWARN*> CONST kL:LONGINT=848L;

(* variables *)

<*NOWARN*> VAR vi8:INT8:=849;
<*NOWARN*> VAR vu64:UINT64:=850L;
<*NOWARN*> VAR vf64:FLOAT64:=851.852d0;
<*NOWARN*> VAR vi32:INT32:=853;
<*NOWARN*> VAR vLC:LONGCARD:=854L;
<*NOWARN*> VAR vu16:UINT16:=855;
<*NOWARN*> VAR vI:INTEGER:=856;
<*NOWARN*> VAR vi64:INT64:=857L;
<*NOWARN*> VAR vf32:FLOAT32:=858.859e0;
<*NOWARN*> VAR vi16:INT16:=860;
<*NOWARN*> VAR vC:CARDINAL:=861;
<*NOWARN*> VAR vu32:UINT32:=862;
<*NOWARN*> VAR vu8:UINT8:=863;
<*NOWARN*> VAR vL:LONGINT:=864L;
<*NOWARN*>VAR offset, count:CARDINAL;

(* Extract *)

<*NOWARN*>PROCEDURE uExtract_var_i8_i8():Word.T=BEGIN RETURN Word.Extract(vi8,offset,count);END uExtract_var_i8_i8;
<*NOWARN*>PROCEDURE uExtract_param_i8_i8(a:INT8;b:INT8;offset,count:CARDINAL):Word.T=BEGIN RETURN Word.Extract(a,offset,count);END uExtract_param_i8_i8;
<*NOWARN*>PROCEDURE uExtract_var_u64_u64():Long.T=BEGIN RETURN Long.Extract(vu64,offset,count);END uExtract_var_u64_u64;
<*NOWARN*>PROCEDURE uExtract_param_u64_u64(a:UINT64;b:UINT64;offset,count:CARDINAL):Long.T=BEGIN RETURN Long.Extract(a,offset,count);END uExtract_param_u64_u64;
<*NOWARN*>PROCEDURE uExtract_var_i32_i32():Word.T=BEGIN RETURN Word.Extract(vi32,offset,count);END uExtract_var_i32_i32;
<*NOWARN*>PROCEDURE uExtract_param_i32_i32(a:INT32;b:INT32;offset,count:CARDINAL):Word.T=BEGIN RETURN Word.Extract(a,offset,count);END uExtract_param_i32_i32;
<*NOWARN*>PROCEDURE uExtract_var_LC_LC():Long.T=BEGIN RETURN Long.Extract(vLC,offset,count);END uExtract_var_LC_LC;
<*NOWARN*>PROCEDURE uExtract_param_LC_LC(a:LONGCARD;b:LONGCARD;offset,count:CARDINAL):Long.T=BEGIN RETURN Long.Extract(a,offset,count);END uExtract_param_LC_LC;
<*NOWARN*>PROCEDURE uExtract_var_u16_u16():Word.T=BEGIN RETURN Word.Extract(vu16,offset,count);END uExtract_var_u16_u16;
<*NOWARN*>PROCEDURE uExtract_param_u16_u16(a:UINT16;b:UINT16;offset,count:CARDINAL):Word.T=BEGIN RETURN Word.Extract(a,offset,count);END uExtract_param_u16_u16;
<*NOWARN*>PROCEDURE uExtract_var_I_I():Word.T=BEGIN RETURN Word.Extract(vI,offset,count);END uExtract_var_I_I;
<*NOWARN*>PROCEDURE uExtract_param_I_I(a:INTEGER;b:INTEGER;offset,count:CARDINAL):Word.T=BEGIN RETURN Word.Extract(a,offset,count);END uExtract_param_I_I;
<*NOWARN*>PROCEDURE uExtract_var_i64_i64():Long.T=BEGIN RETURN Long.Extract(vi64,offset,count);END uExtract_var_i64_i64;
<*NOWARN*>PROCEDURE uExtract_param_i64_i64(a:INT64;b:INT64;offset,count:CARDINAL):Long.T=BEGIN RETURN Long.Extract(a,offset,count);END uExtract_param_i64_i64;
<*NOWARN*>PROCEDURE uExtract_var_i16_i16():Word.T=BEGIN RETURN Word.Extract(vi16,offset,count);END uExtract_var_i16_i16;
<*NOWARN*>PROCEDURE uExtract_param_i16_i16(a:INT16;b:INT16;offset,count:CARDINAL):Word.T=BEGIN RETURN Word.Extract(a,offset,count);END uExtract_param_i16_i16;
<*NOWARN*>PROCEDURE uExtract_var_C_C():Word.T=BEGIN RETURN Word.Extract(vC,offset,count);END uExtract_var_C_C;
<*NOWARN*>PROCEDURE uExtract_param_C_C(a:CARDINAL;b:CARDINAL;offset,count:CARDINAL):Word.T=BEGIN RETURN Word.Extract(a,offset,count);END uExtract_param_C_C;
<*NOWARN*>PROCEDURE uExtract_var_u32_u32():Word.T=BEGIN RETURN Word.Extract(vu32,offset,count);END uExtract_var_u32_u32;
<*NOWARN*>PROCEDURE uExtract_param_u32_u32(a:UINT32;b:UINT32;offset,count:CARDINAL):Word.T=BEGIN RETURN Word.Extract(a,offset,count);END uExtract_param_u32_u32;
<*NOWARN*>PROCEDURE uExtract_var_u8_u8():Word.T=BEGIN RETURN Word.Extract(vu8,offset,count);END uExtract_var_u8_u8;
<*NOWARN*>PROCEDURE uExtract_param_u8_u8(a:UINT8;b:UINT8;offset,count:CARDINAL):Word.T=BEGIN RETURN Word.Extract(a,offset,count);END uExtract_param_u8_u8;
<*NOWARN*>PROCEDURE uExtract_var_L_L():Long.T=BEGIN RETURN Long.Extract(vL,offset,count);END uExtract_var_L_L;
<*NOWARN*>PROCEDURE uExtract_param_L_L(a:LONGINT;b:LONGINT;offset,count:CARDINAL):Long.T=BEGIN RETURN Long.Extract(a,offset,count);END uExtract_param_L_L;
BEGIN
END extract.
