MODULE extract_constant_offset;
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

<*NOWARN*> CONST ki8:INT8=1057;
<*NOWARN*> CONST ku64:UINT64=1058L;
<*NOWARN*> CONST kf64:FLOAT64=1059.1060d0;
<*NOWARN*> CONST ki32:INT32=1061;
<*NOWARN*> CONST kLC:LONGCARD=1062L;
<*NOWARN*> CONST ku16:UINT16=1063;
<*NOWARN*> CONST kI:INTEGER=1064;
<*NOWARN*> CONST ki64:INT64=1065L;
<*NOWARN*> CONST kf32:FLOAT32=1066.1067e0;
<*NOWARN*> CONST ki16:INT16=1068;
<*NOWARN*> CONST kC:CARDINAL=1069;
<*NOWARN*> CONST ku32:UINT32=1070;
<*NOWARN*> CONST ku8:UINT8=1071;
<*NOWARN*> CONST kL:LONGINT=1072L;

(* variables *)

<*NOWARN*> VAR vi8:INT8:=1073;
<*NOWARN*> VAR vu64:UINT64:=1074L;
<*NOWARN*> VAR vf64:FLOAT64:=1075.1076d0;
<*NOWARN*> VAR vi32:INT32:=1077;
<*NOWARN*> VAR vLC:LONGCARD:=1078L;
<*NOWARN*> VAR vu16:UINT16:=1079;
<*NOWARN*> VAR vI:INTEGER:=1080;
<*NOWARN*> VAR vi64:INT64:=1081L;
<*NOWARN*> VAR vf32:FLOAT32:=1082.1083e0;
<*NOWARN*> VAR vi16:INT16:=1084;
<*NOWARN*> VAR vC:CARDINAL:=1085;
<*NOWARN*> VAR vu32:UINT32:=1086;
<*NOWARN*> VAR vu8:UINT8:=1087;
<*NOWARN*> VAR vL:LONGINT:=1088L;
<*NOWARN*>VAR offset, count:CARDINAL;

(* extract with constant offset *)

<*NOWARN*> PROCEDURE Word_extract_constant_offset_0(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,0,count); END Word_extract_constant_offset_0;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_1(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,1,count); END Word_extract_constant_offset_1;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_2(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,2,count); END Word_extract_constant_offset_2;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_3(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,3,count); END Word_extract_constant_offset_3;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_4(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,4,count); END Word_extract_constant_offset_4;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_5(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,5,count); END Word_extract_constant_offset_5;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_6(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,6,count); END Word_extract_constant_offset_6;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_7(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,7,count); END Word_extract_constant_offset_7;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_8(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,8,count); END Word_extract_constant_offset_8;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_9(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,9,count); END Word_extract_constant_offset_9;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_10(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,10,count); END Word_extract_constant_offset_10;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_11(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,11,count); END Word_extract_constant_offset_11;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_12(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,12,count); END Word_extract_constant_offset_12;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_13(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,13,count); END Word_extract_constant_offset_13;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_14(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,14,count); END Word_extract_constant_offset_14;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_15(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,15,count); END Word_extract_constant_offset_15;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_16(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,16,count); END Word_extract_constant_offset_16;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_17(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,17,count); END Word_extract_constant_offset_17;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_18(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,18,count); END Word_extract_constant_offset_18;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_19(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,19,count); END Word_extract_constant_offset_19;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_20(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,20,count); END Word_extract_constant_offset_20;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_21(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,21,count); END Word_extract_constant_offset_21;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_22(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,22,count); END Word_extract_constant_offset_22;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_23(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,23,count); END Word_extract_constant_offset_23;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_24(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,24,count); END Word_extract_constant_offset_24;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_25(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,25,count); END Word_extract_constant_offset_25;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_26(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,26,count); END Word_extract_constant_offset_26;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_27(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,27,count); END Word_extract_constant_offset_27;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_28(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,28,count); END Word_extract_constant_offset_28;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_29(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,29,count); END Word_extract_constant_offset_29;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_30(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,30,count); END Word_extract_constant_offset_30;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_31(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,31,count); END Word_extract_constant_offset_31;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_32(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,32,count); END Word_extract_constant_offset_32;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_33(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,33,count); END Word_extract_constant_offset_33;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_34(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,34,count); END Word_extract_constant_offset_34;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_35(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,35,count); END Word_extract_constant_offset_35;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_36(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,36,count); END Word_extract_constant_offset_36;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_37(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,37,count); END Word_extract_constant_offset_37;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_38(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,38,count); END Word_extract_constant_offset_38;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_39(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,39,count); END Word_extract_constant_offset_39;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_40(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,40,count); END Word_extract_constant_offset_40;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_41(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,41,count); END Word_extract_constant_offset_41;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_42(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,42,count); END Word_extract_constant_offset_42;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_43(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,43,count); END Word_extract_constant_offset_43;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_44(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,44,count); END Word_extract_constant_offset_44;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_45(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,45,count); END Word_extract_constant_offset_45;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_46(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,46,count); END Word_extract_constant_offset_46;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_47(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,47,count); END Word_extract_constant_offset_47;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_48(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,48,count); END Word_extract_constant_offset_48;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_49(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,49,count); END Word_extract_constant_offset_49;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_50(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,50,count); END Word_extract_constant_offset_50;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_51(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,51,count); END Word_extract_constant_offset_51;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_52(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,52,count); END Word_extract_constant_offset_52;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_53(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,53,count); END Word_extract_constant_offset_53;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_54(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,54,count); END Word_extract_constant_offset_54;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_55(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,55,count); END Word_extract_constant_offset_55;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_56(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,56,count); END Word_extract_constant_offset_56;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_57(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,57,count); END Word_extract_constant_offset_57;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_58(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,58,count); END Word_extract_constant_offset_58;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_59(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,59,count); END Word_extract_constant_offset_59;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_60(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,60,count); END Word_extract_constant_offset_60;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_61(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,61,count); END Word_extract_constant_offset_61;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_62(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,62,count); END Word_extract_constant_offset_62;
<*NOWARN*> PROCEDURE Word_extract_constant_offset_63(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,63,count); END Word_extract_constant_offset_63;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_0(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,0,count); END Long_extract_constant_offset_0;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_1(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,1,count); END Long_extract_constant_offset_1;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_2(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,2,count); END Long_extract_constant_offset_2;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_3(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,3,count); END Long_extract_constant_offset_3;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_4(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,4,count); END Long_extract_constant_offset_4;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_5(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,5,count); END Long_extract_constant_offset_5;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_6(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,6,count); END Long_extract_constant_offset_6;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_7(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,7,count); END Long_extract_constant_offset_7;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_8(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,8,count); END Long_extract_constant_offset_8;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_9(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,9,count); END Long_extract_constant_offset_9;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_10(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,10,count); END Long_extract_constant_offset_10;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_11(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,11,count); END Long_extract_constant_offset_11;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_12(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,12,count); END Long_extract_constant_offset_12;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_13(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,13,count); END Long_extract_constant_offset_13;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_14(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,14,count); END Long_extract_constant_offset_14;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_15(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,15,count); END Long_extract_constant_offset_15;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_16(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,16,count); END Long_extract_constant_offset_16;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_17(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,17,count); END Long_extract_constant_offset_17;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_18(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,18,count); END Long_extract_constant_offset_18;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_19(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,19,count); END Long_extract_constant_offset_19;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_20(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,20,count); END Long_extract_constant_offset_20;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_21(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,21,count); END Long_extract_constant_offset_21;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_22(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,22,count); END Long_extract_constant_offset_22;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_23(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,23,count); END Long_extract_constant_offset_23;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_24(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,24,count); END Long_extract_constant_offset_24;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_25(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,25,count); END Long_extract_constant_offset_25;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_26(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,26,count); END Long_extract_constant_offset_26;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_27(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,27,count); END Long_extract_constant_offset_27;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_28(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,28,count); END Long_extract_constant_offset_28;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_29(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,29,count); END Long_extract_constant_offset_29;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_30(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,30,count); END Long_extract_constant_offset_30;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_31(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,31,count); END Long_extract_constant_offset_31;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_32(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,32,count); END Long_extract_constant_offset_32;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_33(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,33,count); END Long_extract_constant_offset_33;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_34(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,34,count); END Long_extract_constant_offset_34;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_35(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,35,count); END Long_extract_constant_offset_35;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_36(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,36,count); END Long_extract_constant_offset_36;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_37(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,37,count); END Long_extract_constant_offset_37;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_38(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,38,count); END Long_extract_constant_offset_38;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_39(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,39,count); END Long_extract_constant_offset_39;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_40(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,40,count); END Long_extract_constant_offset_40;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_41(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,41,count); END Long_extract_constant_offset_41;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_42(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,42,count); END Long_extract_constant_offset_42;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_43(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,43,count); END Long_extract_constant_offset_43;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_44(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,44,count); END Long_extract_constant_offset_44;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_45(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,45,count); END Long_extract_constant_offset_45;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_46(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,46,count); END Long_extract_constant_offset_46;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_47(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,47,count); END Long_extract_constant_offset_47;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_48(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,48,count); END Long_extract_constant_offset_48;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_49(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,49,count); END Long_extract_constant_offset_49;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_50(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,50,count); END Long_extract_constant_offset_50;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_51(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,51,count); END Long_extract_constant_offset_51;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_52(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,52,count); END Long_extract_constant_offset_52;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_53(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,53,count); END Long_extract_constant_offset_53;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_54(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,54,count); END Long_extract_constant_offset_54;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_55(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,55,count); END Long_extract_constant_offset_55;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_56(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,56,count); END Long_extract_constant_offset_56;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_57(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,57,count); END Long_extract_constant_offset_57;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_58(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,58,count); END Long_extract_constant_offset_58;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_59(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,59,count); END Long_extract_constant_offset_59;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_60(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,60,count); END Long_extract_constant_offset_60;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_61(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,61,count); END Long_extract_constant_offset_61;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_62(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,62,count); END Long_extract_constant_offset_62;
<*NOWARN*> PROCEDURE Long_extract_constant_offset_63(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,63,count); END Long_extract_constant_offset_63;
BEGIN
END extract_constant_offset.
