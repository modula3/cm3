MODULE extract_constant_count;
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

<*NOWARN*> CONST ki8:INT8=1089;
<*NOWARN*> CONST ku64:UINT64=1090L;
<*NOWARN*> CONST kf64:FLOAT64=1091.1092d0;
<*NOWARN*> CONST ki32:INT32=1093;
<*NOWARN*> CONST kLC:LONGCARD=1094L;
<*NOWARN*> CONST ku16:UINT16=1095;
<*NOWARN*> CONST kI:INTEGER=1096;
<*NOWARN*> CONST ki64:INT64=1097L;
<*NOWARN*> CONST kf32:FLOAT32=1098.1099e0;
<*NOWARN*> CONST ki16:INT16=1100;
<*NOWARN*> CONST kC:CARDINAL=1101;
<*NOWARN*> CONST ku32:UINT32=1102;
<*NOWARN*> CONST ku8:UINT8=1103;
<*NOWARN*> CONST kL:LONGINT=1104L;

(* variables *)

<*NOWARN*> VAR vi8:INT8:=1105;
<*NOWARN*> VAR vu64:UINT64:=1106L;
<*NOWARN*> VAR vf64:FLOAT64:=1107.1108d0;
<*NOWARN*> VAR vi32:INT32:=1109;
<*NOWARN*> VAR vLC:LONGCARD:=1110L;
<*NOWARN*> VAR vu16:UINT16:=1111;
<*NOWARN*> VAR vI:INTEGER:=1112;
<*NOWARN*> VAR vi64:INT64:=1113L;
<*NOWARN*> VAR vf32:FLOAT32:=1114.1115e0;
<*NOWARN*> VAR vi16:INT16:=1116;
<*NOWARN*> VAR vC:CARDINAL:=1117;
<*NOWARN*> VAR vu32:UINT32:=1118;
<*NOWARN*> VAR vu8:UINT8:=1119;
<*NOWARN*> VAR vL:LONGINT:=1120L;
<*NOWARN*>VAR offset, count:CARDINAL;

(* extract with constant count *)

<*NOWARN*> PROCEDURE Word_extract_constant_count_0(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,0); END Word_extract_constant_count_0;
<*NOWARN*> PROCEDURE Word_extract_constant_count_1(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,1); END Word_extract_constant_count_1;
<*NOWARN*> PROCEDURE Word_extract_constant_count_2(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,2); END Word_extract_constant_count_2;
<*NOWARN*> PROCEDURE Word_extract_constant_count_3(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,3); END Word_extract_constant_count_3;
<*NOWARN*> PROCEDURE Word_extract_constant_count_4(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,4); END Word_extract_constant_count_4;
<*NOWARN*> PROCEDURE Word_extract_constant_count_5(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,5); END Word_extract_constant_count_5;
<*NOWARN*> PROCEDURE Word_extract_constant_count_6(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,6); END Word_extract_constant_count_6;
<*NOWARN*> PROCEDURE Word_extract_constant_count_7(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,7); END Word_extract_constant_count_7;
<*NOWARN*> PROCEDURE Word_extract_constant_count_8(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,8); END Word_extract_constant_count_8;
<*NOWARN*> PROCEDURE Word_extract_constant_count_9(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,9); END Word_extract_constant_count_9;
<*NOWARN*> PROCEDURE Word_extract_constant_count_10(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,10); END Word_extract_constant_count_10;
<*NOWARN*> PROCEDURE Word_extract_constant_count_11(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,11); END Word_extract_constant_count_11;
<*NOWARN*> PROCEDURE Word_extract_constant_count_12(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,12); END Word_extract_constant_count_12;
<*NOWARN*> PROCEDURE Word_extract_constant_count_13(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,13); END Word_extract_constant_count_13;
<*NOWARN*> PROCEDURE Word_extract_constant_count_14(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,14); END Word_extract_constant_count_14;
<*NOWARN*> PROCEDURE Word_extract_constant_count_15(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,15); END Word_extract_constant_count_15;
<*NOWARN*> PROCEDURE Word_extract_constant_count_16(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,16); END Word_extract_constant_count_16;
<*NOWARN*> PROCEDURE Word_extract_constant_count_17(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,17); END Word_extract_constant_count_17;
<*NOWARN*> PROCEDURE Word_extract_constant_count_18(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,18); END Word_extract_constant_count_18;
<*NOWARN*> PROCEDURE Word_extract_constant_count_19(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,19); END Word_extract_constant_count_19;
<*NOWARN*> PROCEDURE Word_extract_constant_count_20(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,20); END Word_extract_constant_count_20;
<*NOWARN*> PROCEDURE Word_extract_constant_count_21(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,21); END Word_extract_constant_count_21;
<*NOWARN*> PROCEDURE Word_extract_constant_count_22(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,22); END Word_extract_constant_count_22;
<*NOWARN*> PROCEDURE Word_extract_constant_count_23(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,23); END Word_extract_constant_count_23;
<*NOWARN*> PROCEDURE Word_extract_constant_count_24(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,24); END Word_extract_constant_count_24;
<*NOWARN*> PROCEDURE Word_extract_constant_count_25(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,25); END Word_extract_constant_count_25;
<*NOWARN*> PROCEDURE Word_extract_constant_count_26(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,26); END Word_extract_constant_count_26;
<*NOWARN*> PROCEDURE Word_extract_constant_count_27(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,27); END Word_extract_constant_count_27;
<*NOWARN*> PROCEDURE Word_extract_constant_count_28(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,28); END Word_extract_constant_count_28;
<*NOWARN*> PROCEDURE Word_extract_constant_count_29(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,29); END Word_extract_constant_count_29;
<*NOWARN*> PROCEDURE Word_extract_constant_count_30(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,30); END Word_extract_constant_count_30;
<*NOWARN*> PROCEDURE Word_extract_constant_count_31(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,31); END Word_extract_constant_count_31;
<*NOWARN*> PROCEDURE Word_extract_constant_count_32(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,32); END Word_extract_constant_count_32;
<*NOWARN*> PROCEDURE Word_extract_constant_count_33(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,33); END Word_extract_constant_count_33;
<*NOWARN*> PROCEDURE Word_extract_constant_count_34(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,34); END Word_extract_constant_count_34;
<*NOWARN*> PROCEDURE Word_extract_constant_count_35(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,35); END Word_extract_constant_count_35;
<*NOWARN*> PROCEDURE Word_extract_constant_count_36(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,36); END Word_extract_constant_count_36;
<*NOWARN*> PROCEDURE Word_extract_constant_count_37(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,37); END Word_extract_constant_count_37;
<*NOWARN*> PROCEDURE Word_extract_constant_count_38(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,38); END Word_extract_constant_count_38;
<*NOWARN*> PROCEDURE Word_extract_constant_count_39(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,39); END Word_extract_constant_count_39;
<*NOWARN*> PROCEDURE Word_extract_constant_count_40(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,40); END Word_extract_constant_count_40;
<*NOWARN*> PROCEDURE Word_extract_constant_count_41(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,41); END Word_extract_constant_count_41;
<*NOWARN*> PROCEDURE Word_extract_constant_count_42(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,42); END Word_extract_constant_count_42;
<*NOWARN*> PROCEDURE Word_extract_constant_count_43(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,43); END Word_extract_constant_count_43;
<*NOWARN*> PROCEDURE Word_extract_constant_count_44(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,44); END Word_extract_constant_count_44;
<*NOWARN*> PROCEDURE Word_extract_constant_count_45(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,45); END Word_extract_constant_count_45;
<*NOWARN*> PROCEDURE Word_extract_constant_count_46(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,46); END Word_extract_constant_count_46;
<*NOWARN*> PROCEDURE Word_extract_constant_count_47(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,47); END Word_extract_constant_count_47;
<*NOWARN*> PROCEDURE Word_extract_constant_count_48(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,48); END Word_extract_constant_count_48;
<*NOWARN*> PROCEDURE Word_extract_constant_count_49(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,49); END Word_extract_constant_count_49;
<*NOWARN*> PROCEDURE Word_extract_constant_count_50(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,50); END Word_extract_constant_count_50;
<*NOWARN*> PROCEDURE Word_extract_constant_count_51(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,51); END Word_extract_constant_count_51;
<*NOWARN*> PROCEDURE Word_extract_constant_count_52(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,52); END Word_extract_constant_count_52;
<*NOWARN*> PROCEDURE Word_extract_constant_count_53(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,53); END Word_extract_constant_count_53;
<*NOWARN*> PROCEDURE Word_extract_constant_count_54(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,54); END Word_extract_constant_count_54;
<*NOWARN*> PROCEDURE Word_extract_constant_count_55(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,55); END Word_extract_constant_count_55;
<*NOWARN*> PROCEDURE Word_extract_constant_count_56(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,56); END Word_extract_constant_count_56;
<*NOWARN*> PROCEDURE Word_extract_constant_count_57(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,57); END Word_extract_constant_count_57;
<*NOWARN*> PROCEDURE Word_extract_constant_count_58(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,58); END Word_extract_constant_count_58;
<*NOWARN*> PROCEDURE Word_extract_constant_count_59(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,59); END Word_extract_constant_count_59;
<*NOWARN*> PROCEDURE Word_extract_constant_count_60(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,60); END Word_extract_constant_count_60;
<*NOWARN*> PROCEDURE Word_extract_constant_count_61(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,61); END Word_extract_constant_count_61;
<*NOWARN*> PROCEDURE Word_extract_constant_count_62(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,62); END Word_extract_constant_count_62;
<*NOWARN*> PROCEDURE Word_extract_constant_count_63(a:Word.T):Word.T=
           BEGIN RETURN Word.Extract(a,offset,63); END Word_extract_constant_count_63;
<*NOWARN*> PROCEDURE Long_extract_constant_count_0(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,0); END Long_extract_constant_count_0;
<*NOWARN*> PROCEDURE Long_extract_constant_count_1(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,1); END Long_extract_constant_count_1;
<*NOWARN*> PROCEDURE Long_extract_constant_count_2(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,2); END Long_extract_constant_count_2;
<*NOWARN*> PROCEDURE Long_extract_constant_count_3(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,3); END Long_extract_constant_count_3;
<*NOWARN*> PROCEDURE Long_extract_constant_count_4(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,4); END Long_extract_constant_count_4;
<*NOWARN*> PROCEDURE Long_extract_constant_count_5(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,5); END Long_extract_constant_count_5;
<*NOWARN*> PROCEDURE Long_extract_constant_count_6(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,6); END Long_extract_constant_count_6;
<*NOWARN*> PROCEDURE Long_extract_constant_count_7(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,7); END Long_extract_constant_count_7;
<*NOWARN*> PROCEDURE Long_extract_constant_count_8(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,8); END Long_extract_constant_count_8;
<*NOWARN*> PROCEDURE Long_extract_constant_count_9(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,9); END Long_extract_constant_count_9;
<*NOWARN*> PROCEDURE Long_extract_constant_count_10(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,10); END Long_extract_constant_count_10;
<*NOWARN*> PROCEDURE Long_extract_constant_count_11(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,11); END Long_extract_constant_count_11;
<*NOWARN*> PROCEDURE Long_extract_constant_count_12(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,12); END Long_extract_constant_count_12;
<*NOWARN*> PROCEDURE Long_extract_constant_count_13(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,13); END Long_extract_constant_count_13;
<*NOWARN*> PROCEDURE Long_extract_constant_count_14(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,14); END Long_extract_constant_count_14;
<*NOWARN*> PROCEDURE Long_extract_constant_count_15(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,15); END Long_extract_constant_count_15;
<*NOWARN*> PROCEDURE Long_extract_constant_count_16(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,16); END Long_extract_constant_count_16;
<*NOWARN*> PROCEDURE Long_extract_constant_count_17(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,17); END Long_extract_constant_count_17;
<*NOWARN*> PROCEDURE Long_extract_constant_count_18(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,18); END Long_extract_constant_count_18;
<*NOWARN*> PROCEDURE Long_extract_constant_count_19(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,19); END Long_extract_constant_count_19;
<*NOWARN*> PROCEDURE Long_extract_constant_count_20(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,20); END Long_extract_constant_count_20;
<*NOWARN*> PROCEDURE Long_extract_constant_count_21(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,21); END Long_extract_constant_count_21;
<*NOWARN*> PROCEDURE Long_extract_constant_count_22(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,22); END Long_extract_constant_count_22;
<*NOWARN*> PROCEDURE Long_extract_constant_count_23(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,23); END Long_extract_constant_count_23;
<*NOWARN*> PROCEDURE Long_extract_constant_count_24(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,24); END Long_extract_constant_count_24;
<*NOWARN*> PROCEDURE Long_extract_constant_count_25(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,25); END Long_extract_constant_count_25;
<*NOWARN*> PROCEDURE Long_extract_constant_count_26(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,26); END Long_extract_constant_count_26;
<*NOWARN*> PROCEDURE Long_extract_constant_count_27(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,27); END Long_extract_constant_count_27;
<*NOWARN*> PROCEDURE Long_extract_constant_count_28(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,28); END Long_extract_constant_count_28;
<*NOWARN*> PROCEDURE Long_extract_constant_count_29(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,29); END Long_extract_constant_count_29;
<*NOWARN*> PROCEDURE Long_extract_constant_count_30(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,30); END Long_extract_constant_count_30;
<*NOWARN*> PROCEDURE Long_extract_constant_count_31(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,31); END Long_extract_constant_count_31;
<*NOWARN*> PROCEDURE Long_extract_constant_count_32(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,32); END Long_extract_constant_count_32;
<*NOWARN*> PROCEDURE Long_extract_constant_count_33(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,33); END Long_extract_constant_count_33;
<*NOWARN*> PROCEDURE Long_extract_constant_count_34(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,34); END Long_extract_constant_count_34;
<*NOWARN*> PROCEDURE Long_extract_constant_count_35(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,35); END Long_extract_constant_count_35;
<*NOWARN*> PROCEDURE Long_extract_constant_count_36(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,36); END Long_extract_constant_count_36;
<*NOWARN*> PROCEDURE Long_extract_constant_count_37(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,37); END Long_extract_constant_count_37;
<*NOWARN*> PROCEDURE Long_extract_constant_count_38(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,38); END Long_extract_constant_count_38;
<*NOWARN*> PROCEDURE Long_extract_constant_count_39(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,39); END Long_extract_constant_count_39;
<*NOWARN*> PROCEDURE Long_extract_constant_count_40(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,40); END Long_extract_constant_count_40;
<*NOWARN*> PROCEDURE Long_extract_constant_count_41(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,41); END Long_extract_constant_count_41;
<*NOWARN*> PROCEDURE Long_extract_constant_count_42(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,42); END Long_extract_constant_count_42;
<*NOWARN*> PROCEDURE Long_extract_constant_count_43(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,43); END Long_extract_constant_count_43;
<*NOWARN*> PROCEDURE Long_extract_constant_count_44(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,44); END Long_extract_constant_count_44;
<*NOWARN*> PROCEDURE Long_extract_constant_count_45(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,45); END Long_extract_constant_count_45;
<*NOWARN*> PROCEDURE Long_extract_constant_count_46(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,46); END Long_extract_constant_count_46;
<*NOWARN*> PROCEDURE Long_extract_constant_count_47(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,47); END Long_extract_constant_count_47;
<*NOWARN*> PROCEDURE Long_extract_constant_count_48(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,48); END Long_extract_constant_count_48;
<*NOWARN*> PROCEDURE Long_extract_constant_count_49(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,49); END Long_extract_constant_count_49;
<*NOWARN*> PROCEDURE Long_extract_constant_count_50(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,50); END Long_extract_constant_count_50;
<*NOWARN*> PROCEDURE Long_extract_constant_count_51(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,51); END Long_extract_constant_count_51;
<*NOWARN*> PROCEDURE Long_extract_constant_count_52(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,52); END Long_extract_constant_count_52;
<*NOWARN*> PROCEDURE Long_extract_constant_count_53(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,53); END Long_extract_constant_count_53;
<*NOWARN*> PROCEDURE Long_extract_constant_count_54(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,54); END Long_extract_constant_count_54;
<*NOWARN*> PROCEDURE Long_extract_constant_count_55(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,55); END Long_extract_constant_count_55;
<*NOWARN*> PROCEDURE Long_extract_constant_count_56(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,56); END Long_extract_constant_count_56;
<*NOWARN*> PROCEDURE Long_extract_constant_count_57(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,57); END Long_extract_constant_count_57;
<*NOWARN*> PROCEDURE Long_extract_constant_count_58(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,58); END Long_extract_constant_count_58;
<*NOWARN*> PROCEDURE Long_extract_constant_count_59(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,59); END Long_extract_constant_count_59;
<*NOWARN*> PROCEDURE Long_extract_constant_count_60(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,60); END Long_extract_constant_count_60;
<*NOWARN*> PROCEDURE Long_extract_constant_count_61(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,61); END Long_extract_constant_count_61;
<*NOWARN*> PROCEDURE Long_extract_constant_count_62(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,62); END Long_extract_constant_count_62;
<*NOWARN*> PROCEDURE Long_extract_constant_count_63(a:Long.T):Long.T=
           BEGIN RETURN Long.Extract(a,offset,63); END Long_extract_constant_count_63;
BEGIN
END extract_constant_count.
