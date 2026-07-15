MODULE insert;
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

<*NOWARN*> CONST ki8:INT8=801;
<*NOWARN*> CONST ku64:UINT64=802L;
<*NOWARN*> CONST kf64:FLOAT64=803.804d0;
<*NOWARN*> CONST ki32:INT32=805;
<*NOWARN*> CONST kLC:LONGCARD=806L;
<*NOWARN*> CONST ku16:UINT16=807;
<*NOWARN*> CONST kI:INTEGER=808;
<*NOWARN*> CONST ki64:INT64=809L;
<*NOWARN*> CONST kf32:FLOAT32=810.811e0;
<*NOWARN*> CONST ki16:INT16=812;
<*NOWARN*> CONST kC:CARDINAL=813;
<*NOWARN*> CONST ku32:UINT32=814;
<*NOWARN*> CONST ku8:UINT8=815;
<*NOWARN*> CONST kL:LONGINT=816L;

(* variables *)

<*NOWARN*> VAR vi8:INT8:=817;
<*NOWARN*> VAR vu64:UINT64:=818L;
<*NOWARN*> VAR vf64:FLOAT64:=819.820d0;
<*NOWARN*> VAR vi32:INT32:=821;
<*NOWARN*> VAR vLC:LONGCARD:=822L;
<*NOWARN*> VAR vu16:UINT16:=823;
<*NOWARN*> VAR vI:INTEGER:=824;
<*NOWARN*> VAR vi64:INT64:=825L;
<*NOWARN*> VAR vf32:FLOAT32:=826.827e0;
<*NOWARN*> VAR vi16:INT16:=828;
<*NOWARN*> VAR vC:CARDINAL:=829;
<*NOWARN*> VAR vu32:UINT32:=830;
<*NOWARN*> VAR vu8:UINT8:=831;
<*NOWARN*> VAR vL:LONGINT:=832L;
<*NOWARN*>VAR offset, count:CARDINAL;

(* Insert *)

<*NOWARN*>PROCEDURE uInsert_var_i8_i8():Word.T=BEGIN RETURN Word.Insert(vi8,vi8,offset,count);END uInsert_var_i8_i8;
<*NOWARN*>PROCEDURE uInsert_param_i8_i8(a:INT8;b:INT8;offset,count:CARDINAL):Word.T=BEGIN RETURN Word.Insert(a,b,offset,count);END uInsert_param_i8_i8;
<*NOWARN*>PROCEDURE uInsert_var_u64_u64():Long.T=BEGIN RETURN Long.Insert(vu64,vu64,offset,count);END uInsert_var_u64_u64;
<*NOWARN*>PROCEDURE uInsert_param_u64_u64(a:UINT64;b:UINT64;offset,count:CARDINAL):Long.T=BEGIN RETURN Long.Insert(a,b,offset,count);END uInsert_param_u64_u64;
<*NOWARN*>PROCEDURE uInsert_var_i32_i32():Word.T=BEGIN RETURN Word.Insert(vi32,vi32,offset,count);END uInsert_var_i32_i32;
<*NOWARN*>PROCEDURE uInsert_param_i32_i32(a:INT32;b:INT32;offset,count:CARDINAL):Word.T=BEGIN RETURN Word.Insert(a,b,offset,count);END uInsert_param_i32_i32;
<*NOWARN*>PROCEDURE uInsert_var_LC_LC():Long.T=BEGIN RETURN Long.Insert(vLC,vLC,offset,count);END uInsert_var_LC_LC;
<*NOWARN*>PROCEDURE uInsert_param_LC_LC(a:LONGCARD;b:LONGCARD;offset,count:CARDINAL):Long.T=BEGIN RETURN Long.Insert(a,b,offset,count);END uInsert_param_LC_LC;
<*NOWARN*>PROCEDURE uInsert_var_u16_u16():Word.T=BEGIN RETURN Word.Insert(vu16,vu16,offset,count);END uInsert_var_u16_u16;
<*NOWARN*>PROCEDURE uInsert_param_u16_u16(a:UINT16;b:UINT16;offset,count:CARDINAL):Word.T=BEGIN RETURN Word.Insert(a,b,offset,count);END uInsert_param_u16_u16;
<*NOWARN*>PROCEDURE uInsert_var_I_I():Word.T=BEGIN RETURN Word.Insert(vI,vI,offset,count);END uInsert_var_I_I;
<*NOWARN*>PROCEDURE uInsert_param_I_I(a:INTEGER;b:INTEGER;offset,count:CARDINAL):Word.T=BEGIN RETURN Word.Insert(a,b,offset,count);END uInsert_param_I_I;
<*NOWARN*>PROCEDURE uInsert_var_i64_i64():Long.T=BEGIN RETURN Long.Insert(vi64,vi64,offset,count);END uInsert_var_i64_i64;
<*NOWARN*>PROCEDURE uInsert_param_i64_i64(a:INT64;b:INT64;offset,count:CARDINAL):Long.T=BEGIN RETURN Long.Insert(a,b,offset,count);END uInsert_param_i64_i64;
<*NOWARN*>PROCEDURE uInsert_var_i16_i16():Word.T=BEGIN RETURN Word.Insert(vi16,vi16,offset,count);END uInsert_var_i16_i16;
<*NOWARN*>PROCEDURE uInsert_param_i16_i16(a:INT16;b:INT16;offset,count:CARDINAL):Word.T=BEGIN RETURN Word.Insert(a,b,offset,count);END uInsert_param_i16_i16;
<*NOWARN*>PROCEDURE uInsert_var_C_C():Word.T=BEGIN RETURN Word.Insert(vC,vC,offset,count);END uInsert_var_C_C;
<*NOWARN*>PROCEDURE uInsert_param_C_C(a:CARDINAL;b:CARDINAL;offset,count:CARDINAL):Word.T=BEGIN RETURN Word.Insert(a,b,offset,count);END uInsert_param_C_C;
<*NOWARN*>PROCEDURE uInsert_var_u32_u32():Word.T=BEGIN RETURN Word.Insert(vu32,vu32,offset,count);END uInsert_var_u32_u32;
<*NOWARN*>PROCEDURE uInsert_param_u32_u32(a:UINT32;b:UINT32;offset,count:CARDINAL):Word.T=BEGIN RETURN Word.Insert(a,b,offset,count);END uInsert_param_u32_u32;
<*NOWARN*>PROCEDURE uInsert_var_u8_u8():Word.T=BEGIN RETURN Word.Insert(vu8,vu8,offset,count);END uInsert_var_u8_u8;
<*NOWARN*>PROCEDURE uInsert_param_u8_u8(a:UINT8;b:UINT8;offset,count:CARDINAL):Word.T=BEGIN RETURN Word.Insert(a,b,offset,count);END uInsert_param_u8_u8;
<*NOWARN*>PROCEDURE uInsert_var_L_L():Long.T=BEGIN RETURN Long.Insert(vL,vL,offset,count);END uInsert_var_L_L;
<*NOWARN*>PROCEDURE uInsert_param_L_L(a:LONGINT;b:LONGINT;offset,count:CARDINAL):Long.T=BEGIN RETURN Long.Insert(a,b,offset,count);END uInsert_param_L_L;
BEGIN
END insert.
