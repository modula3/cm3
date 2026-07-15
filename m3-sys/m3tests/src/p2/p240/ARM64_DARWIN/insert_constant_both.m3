MODULE insert_constant_both;
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

<*NOWARN*> CONST ki8:INT8=1025;
<*NOWARN*> CONST ku64:UINT64=1026L;
<*NOWARN*> CONST kf64:FLOAT64=1027.1028d0;
<*NOWARN*> CONST ki32:INT32=1029;
<*NOWARN*> CONST kLC:LONGCARD=1030L;
<*NOWARN*> CONST ku16:UINT16=1031;
<*NOWARN*> CONST kI:INTEGER=1032;
<*NOWARN*> CONST ki64:INT64=1033L;
<*NOWARN*> CONST kf32:FLOAT32=1034.1035e0;
<*NOWARN*> CONST ki16:INT16=1036;
<*NOWARN*> CONST kC:CARDINAL=1037;
<*NOWARN*> CONST ku32:UINT32=1038;
<*NOWARN*> CONST ku8:UINT8=1039;
<*NOWARN*> CONST kL:LONGINT=1040L;

(* variables *)

<*NOWARN*> VAR vi8:INT8:=1041;
<*NOWARN*> VAR vu64:UINT64:=1042L;
<*NOWARN*> VAR vf64:FLOAT64:=1043.1044d0;
<*NOWARN*> VAR vi32:INT32:=1045;
<*NOWARN*> VAR vLC:LONGCARD:=1046L;
<*NOWARN*> VAR vu16:UINT16:=1047;
<*NOWARN*> VAR vI:INTEGER:=1048;
<*NOWARN*> VAR vi64:INT64:=1049L;
<*NOWARN*> VAR vf32:FLOAT32:=1050.1051e0;
<*NOWARN*> VAR vi16:INT16:=1052;
<*NOWARN*> VAR vC:CARDINAL:=1053;
<*NOWARN*> VAR vu32:UINT32:=1054;
<*NOWARN*> VAR vu8:UINT8:=1055;
<*NOWARN*> VAR vL:LONGINT:=1056L;
<*NOWARN*>VAR offset, count:CARDINAL;

(* insert with constant offset and count *)

<*NOWARN*> PROCEDURE Word_insert_constants_0_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,0); END Word_insert_constants_0_0;
<*NOWARN*> PROCEDURE Word_insert_constants_0_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,1); END Word_insert_constants_0_1;
<*NOWARN*> PROCEDURE Word_insert_constants_0_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,2); END Word_insert_constants_0_2;
<*NOWARN*> PROCEDURE Word_insert_constants_0_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,3); END Word_insert_constants_0_3;
<*NOWARN*> PROCEDURE Word_insert_constants_0_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,4); END Word_insert_constants_0_4;
<*NOWARN*> PROCEDURE Word_insert_constants_0_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,5); END Word_insert_constants_0_5;
<*NOWARN*> PROCEDURE Word_insert_constants_0_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,6); END Word_insert_constants_0_6;
<*NOWARN*> PROCEDURE Word_insert_constants_0_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,7); END Word_insert_constants_0_7;
<*NOWARN*> PROCEDURE Word_insert_constants_0_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,8); END Word_insert_constants_0_8;
<*NOWARN*> PROCEDURE Word_insert_constants_0_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,9); END Word_insert_constants_0_9;
<*NOWARN*> PROCEDURE Word_insert_constants_0_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,10); END Word_insert_constants_0_10;
<*NOWARN*> PROCEDURE Word_insert_constants_0_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,11); END Word_insert_constants_0_11;
<*NOWARN*> PROCEDURE Word_insert_constants_0_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,12); END Word_insert_constants_0_12;
<*NOWARN*> PROCEDURE Word_insert_constants_0_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,13); END Word_insert_constants_0_13;
<*NOWARN*> PROCEDURE Word_insert_constants_0_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,14); END Word_insert_constants_0_14;
<*NOWARN*> PROCEDURE Word_insert_constants_0_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,15); END Word_insert_constants_0_15;
<*NOWARN*> PROCEDURE Word_insert_constants_0_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,16); END Word_insert_constants_0_16;
<*NOWARN*> PROCEDURE Word_insert_constants_0_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,17); END Word_insert_constants_0_17;
<*NOWARN*> PROCEDURE Word_insert_constants_0_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,18); END Word_insert_constants_0_18;
<*NOWARN*> PROCEDURE Word_insert_constants_0_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,19); END Word_insert_constants_0_19;
<*NOWARN*> PROCEDURE Word_insert_constants_0_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,20); END Word_insert_constants_0_20;
<*NOWARN*> PROCEDURE Word_insert_constants_0_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,21); END Word_insert_constants_0_21;
<*NOWARN*> PROCEDURE Word_insert_constants_0_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,22); END Word_insert_constants_0_22;
<*NOWARN*> PROCEDURE Word_insert_constants_0_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,23); END Word_insert_constants_0_23;
<*NOWARN*> PROCEDURE Word_insert_constants_0_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,24); END Word_insert_constants_0_24;
<*NOWARN*> PROCEDURE Word_insert_constants_0_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,25); END Word_insert_constants_0_25;
<*NOWARN*> PROCEDURE Word_insert_constants_0_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,26); END Word_insert_constants_0_26;
<*NOWARN*> PROCEDURE Word_insert_constants_0_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,27); END Word_insert_constants_0_27;
<*NOWARN*> PROCEDURE Word_insert_constants_0_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,28); END Word_insert_constants_0_28;
<*NOWARN*> PROCEDURE Word_insert_constants_0_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,29); END Word_insert_constants_0_29;
<*NOWARN*> PROCEDURE Word_insert_constants_0_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,30); END Word_insert_constants_0_30;
<*NOWARN*> PROCEDURE Word_insert_constants_0_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,31); END Word_insert_constants_0_31;
<*NOWARN*> PROCEDURE Word_insert_constants_0_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,32); END Word_insert_constants_0_32;
<*NOWARN*> PROCEDURE Word_insert_constants_0_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,33); END Word_insert_constants_0_33;
<*NOWARN*> PROCEDURE Word_insert_constants_0_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,34); END Word_insert_constants_0_34;
<*NOWARN*> PROCEDURE Word_insert_constants_0_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,35); END Word_insert_constants_0_35;
<*NOWARN*> PROCEDURE Word_insert_constants_0_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,36); END Word_insert_constants_0_36;
<*NOWARN*> PROCEDURE Word_insert_constants_0_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,37); END Word_insert_constants_0_37;
<*NOWARN*> PROCEDURE Word_insert_constants_0_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,38); END Word_insert_constants_0_38;
<*NOWARN*> PROCEDURE Word_insert_constants_0_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,39); END Word_insert_constants_0_39;
<*NOWARN*> PROCEDURE Word_insert_constants_0_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,40); END Word_insert_constants_0_40;
<*NOWARN*> PROCEDURE Word_insert_constants_0_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,41); END Word_insert_constants_0_41;
<*NOWARN*> PROCEDURE Word_insert_constants_0_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,42); END Word_insert_constants_0_42;
<*NOWARN*> PROCEDURE Word_insert_constants_0_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,43); END Word_insert_constants_0_43;
<*NOWARN*> PROCEDURE Word_insert_constants_0_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,44); END Word_insert_constants_0_44;
<*NOWARN*> PROCEDURE Word_insert_constants_0_45(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,45); END Word_insert_constants_0_45;
<*NOWARN*> PROCEDURE Word_insert_constants_0_46(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,46); END Word_insert_constants_0_46;
<*NOWARN*> PROCEDURE Word_insert_constants_0_47(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,47); END Word_insert_constants_0_47;
<*NOWARN*> PROCEDURE Word_insert_constants_0_48(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,48); END Word_insert_constants_0_48;
<*NOWARN*> PROCEDURE Word_insert_constants_0_49(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,49); END Word_insert_constants_0_49;
<*NOWARN*> PROCEDURE Word_insert_constants_0_50(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,50); END Word_insert_constants_0_50;
<*NOWARN*> PROCEDURE Word_insert_constants_0_51(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,51); END Word_insert_constants_0_51;
<*NOWARN*> PROCEDURE Word_insert_constants_0_52(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,52); END Word_insert_constants_0_52;
<*NOWARN*> PROCEDURE Word_insert_constants_0_53(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,53); END Word_insert_constants_0_53;
<*NOWARN*> PROCEDURE Word_insert_constants_0_54(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,54); END Word_insert_constants_0_54;
<*NOWARN*> PROCEDURE Word_insert_constants_0_55(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,55); END Word_insert_constants_0_55;
<*NOWARN*> PROCEDURE Word_insert_constants_0_56(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,56); END Word_insert_constants_0_56;
<*NOWARN*> PROCEDURE Word_insert_constants_0_57(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,57); END Word_insert_constants_0_57;
<*NOWARN*> PROCEDURE Word_insert_constants_0_58(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,58); END Word_insert_constants_0_58;
<*NOWARN*> PROCEDURE Word_insert_constants_0_59(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,59); END Word_insert_constants_0_59;
<*NOWARN*> PROCEDURE Word_insert_constants_0_60(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,60); END Word_insert_constants_0_60;
<*NOWARN*> PROCEDURE Word_insert_constants_0_61(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,61); END Word_insert_constants_0_61;
<*NOWARN*> PROCEDURE Word_insert_constants_0_62(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,62); END Word_insert_constants_0_62;
<*NOWARN*> PROCEDURE Word_insert_constants_0_63(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,63); END Word_insert_constants_0_63;
<*NOWARN*> PROCEDURE Word_insert_constants_0_64(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,0,64); END Word_insert_constants_0_64;
<*NOWARN*> PROCEDURE Word_insert_constants_1_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,0); END Word_insert_constants_1_0;
<*NOWARN*> PROCEDURE Word_insert_constants_1_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,1); END Word_insert_constants_1_1;
<*NOWARN*> PROCEDURE Word_insert_constants_1_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,2); END Word_insert_constants_1_2;
<*NOWARN*> PROCEDURE Word_insert_constants_1_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,3); END Word_insert_constants_1_3;
<*NOWARN*> PROCEDURE Word_insert_constants_1_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,4); END Word_insert_constants_1_4;
<*NOWARN*> PROCEDURE Word_insert_constants_1_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,5); END Word_insert_constants_1_5;
<*NOWARN*> PROCEDURE Word_insert_constants_1_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,6); END Word_insert_constants_1_6;
<*NOWARN*> PROCEDURE Word_insert_constants_1_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,7); END Word_insert_constants_1_7;
<*NOWARN*> PROCEDURE Word_insert_constants_1_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,8); END Word_insert_constants_1_8;
<*NOWARN*> PROCEDURE Word_insert_constants_1_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,9); END Word_insert_constants_1_9;
<*NOWARN*> PROCEDURE Word_insert_constants_1_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,10); END Word_insert_constants_1_10;
<*NOWARN*> PROCEDURE Word_insert_constants_1_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,11); END Word_insert_constants_1_11;
<*NOWARN*> PROCEDURE Word_insert_constants_1_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,12); END Word_insert_constants_1_12;
<*NOWARN*> PROCEDURE Word_insert_constants_1_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,13); END Word_insert_constants_1_13;
<*NOWARN*> PROCEDURE Word_insert_constants_1_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,14); END Word_insert_constants_1_14;
<*NOWARN*> PROCEDURE Word_insert_constants_1_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,15); END Word_insert_constants_1_15;
<*NOWARN*> PROCEDURE Word_insert_constants_1_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,16); END Word_insert_constants_1_16;
<*NOWARN*> PROCEDURE Word_insert_constants_1_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,17); END Word_insert_constants_1_17;
<*NOWARN*> PROCEDURE Word_insert_constants_1_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,18); END Word_insert_constants_1_18;
<*NOWARN*> PROCEDURE Word_insert_constants_1_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,19); END Word_insert_constants_1_19;
<*NOWARN*> PROCEDURE Word_insert_constants_1_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,20); END Word_insert_constants_1_20;
<*NOWARN*> PROCEDURE Word_insert_constants_1_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,21); END Word_insert_constants_1_21;
<*NOWARN*> PROCEDURE Word_insert_constants_1_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,22); END Word_insert_constants_1_22;
<*NOWARN*> PROCEDURE Word_insert_constants_1_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,23); END Word_insert_constants_1_23;
<*NOWARN*> PROCEDURE Word_insert_constants_1_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,24); END Word_insert_constants_1_24;
<*NOWARN*> PROCEDURE Word_insert_constants_1_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,25); END Word_insert_constants_1_25;
<*NOWARN*> PROCEDURE Word_insert_constants_1_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,26); END Word_insert_constants_1_26;
<*NOWARN*> PROCEDURE Word_insert_constants_1_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,27); END Word_insert_constants_1_27;
<*NOWARN*> PROCEDURE Word_insert_constants_1_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,28); END Word_insert_constants_1_28;
<*NOWARN*> PROCEDURE Word_insert_constants_1_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,29); END Word_insert_constants_1_29;
<*NOWARN*> PROCEDURE Word_insert_constants_1_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,30); END Word_insert_constants_1_30;
<*NOWARN*> PROCEDURE Word_insert_constants_1_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,31); END Word_insert_constants_1_31;
<*NOWARN*> PROCEDURE Word_insert_constants_1_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,32); END Word_insert_constants_1_32;
<*NOWARN*> PROCEDURE Word_insert_constants_1_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,33); END Word_insert_constants_1_33;
<*NOWARN*> PROCEDURE Word_insert_constants_1_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,34); END Word_insert_constants_1_34;
<*NOWARN*> PROCEDURE Word_insert_constants_1_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,35); END Word_insert_constants_1_35;
<*NOWARN*> PROCEDURE Word_insert_constants_1_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,36); END Word_insert_constants_1_36;
<*NOWARN*> PROCEDURE Word_insert_constants_1_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,37); END Word_insert_constants_1_37;
<*NOWARN*> PROCEDURE Word_insert_constants_1_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,38); END Word_insert_constants_1_38;
<*NOWARN*> PROCEDURE Word_insert_constants_1_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,39); END Word_insert_constants_1_39;
<*NOWARN*> PROCEDURE Word_insert_constants_1_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,40); END Word_insert_constants_1_40;
<*NOWARN*> PROCEDURE Word_insert_constants_1_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,41); END Word_insert_constants_1_41;
<*NOWARN*> PROCEDURE Word_insert_constants_1_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,42); END Word_insert_constants_1_42;
<*NOWARN*> PROCEDURE Word_insert_constants_1_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,43); END Word_insert_constants_1_43;
<*NOWARN*> PROCEDURE Word_insert_constants_1_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,44); END Word_insert_constants_1_44;
<*NOWARN*> PROCEDURE Word_insert_constants_1_45(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,45); END Word_insert_constants_1_45;
<*NOWARN*> PROCEDURE Word_insert_constants_1_46(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,46); END Word_insert_constants_1_46;
<*NOWARN*> PROCEDURE Word_insert_constants_1_47(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,47); END Word_insert_constants_1_47;
<*NOWARN*> PROCEDURE Word_insert_constants_1_48(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,48); END Word_insert_constants_1_48;
<*NOWARN*> PROCEDURE Word_insert_constants_1_49(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,49); END Word_insert_constants_1_49;
<*NOWARN*> PROCEDURE Word_insert_constants_1_50(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,50); END Word_insert_constants_1_50;
<*NOWARN*> PROCEDURE Word_insert_constants_1_51(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,51); END Word_insert_constants_1_51;
<*NOWARN*> PROCEDURE Word_insert_constants_1_52(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,52); END Word_insert_constants_1_52;
<*NOWARN*> PROCEDURE Word_insert_constants_1_53(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,53); END Word_insert_constants_1_53;
<*NOWARN*> PROCEDURE Word_insert_constants_1_54(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,54); END Word_insert_constants_1_54;
<*NOWARN*> PROCEDURE Word_insert_constants_1_55(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,55); END Word_insert_constants_1_55;
<*NOWARN*> PROCEDURE Word_insert_constants_1_56(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,56); END Word_insert_constants_1_56;
<*NOWARN*> PROCEDURE Word_insert_constants_1_57(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,57); END Word_insert_constants_1_57;
<*NOWARN*> PROCEDURE Word_insert_constants_1_58(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,58); END Word_insert_constants_1_58;
<*NOWARN*> PROCEDURE Word_insert_constants_1_59(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,59); END Word_insert_constants_1_59;
<*NOWARN*> PROCEDURE Word_insert_constants_1_60(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,60); END Word_insert_constants_1_60;
<*NOWARN*> PROCEDURE Word_insert_constants_1_61(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,61); END Word_insert_constants_1_61;
<*NOWARN*> PROCEDURE Word_insert_constants_1_62(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,62); END Word_insert_constants_1_62;
<*NOWARN*> PROCEDURE Word_insert_constants_1_63(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,1,63); END Word_insert_constants_1_63;
<*NOWARN*> PROCEDURE Word_insert_constants_2_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,0); END Word_insert_constants_2_0;
<*NOWARN*> PROCEDURE Word_insert_constants_2_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,1); END Word_insert_constants_2_1;
<*NOWARN*> PROCEDURE Word_insert_constants_2_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,2); END Word_insert_constants_2_2;
<*NOWARN*> PROCEDURE Word_insert_constants_2_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,3); END Word_insert_constants_2_3;
<*NOWARN*> PROCEDURE Word_insert_constants_2_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,4); END Word_insert_constants_2_4;
<*NOWARN*> PROCEDURE Word_insert_constants_2_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,5); END Word_insert_constants_2_5;
<*NOWARN*> PROCEDURE Word_insert_constants_2_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,6); END Word_insert_constants_2_6;
<*NOWARN*> PROCEDURE Word_insert_constants_2_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,7); END Word_insert_constants_2_7;
<*NOWARN*> PROCEDURE Word_insert_constants_2_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,8); END Word_insert_constants_2_8;
<*NOWARN*> PROCEDURE Word_insert_constants_2_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,9); END Word_insert_constants_2_9;
<*NOWARN*> PROCEDURE Word_insert_constants_2_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,10); END Word_insert_constants_2_10;
<*NOWARN*> PROCEDURE Word_insert_constants_2_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,11); END Word_insert_constants_2_11;
<*NOWARN*> PROCEDURE Word_insert_constants_2_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,12); END Word_insert_constants_2_12;
<*NOWARN*> PROCEDURE Word_insert_constants_2_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,13); END Word_insert_constants_2_13;
<*NOWARN*> PROCEDURE Word_insert_constants_2_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,14); END Word_insert_constants_2_14;
<*NOWARN*> PROCEDURE Word_insert_constants_2_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,15); END Word_insert_constants_2_15;
<*NOWARN*> PROCEDURE Word_insert_constants_2_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,16); END Word_insert_constants_2_16;
<*NOWARN*> PROCEDURE Word_insert_constants_2_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,17); END Word_insert_constants_2_17;
<*NOWARN*> PROCEDURE Word_insert_constants_2_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,18); END Word_insert_constants_2_18;
<*NOWARN*> PROCEDURE Word_insert_constants_2_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,19); END Word_insert_constants_2_19;
<*NOWARN*> PROCEDURE Word_insert_constants_2_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,20); END Word_insert_constants_2_20;
<*NOWARN*> PROCEDURE Word_insert_constants_2_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,21); END Word_insert_constants_2_21;
<*NOWARN*> PROCEDURE Word_insert_constants_2_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,22); END Word_insert_constants_2_22;
<*NOWARN*> PROCEDURE Word_insert_constants_2_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,23); END Word_insert_constants_2_23;
<*NOWARN*> PROCEDURE Word_insert_constants_2_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,24); END Word_insert_constants_2_24;
<*NOWARN*> PROCEDURE Word_insert_constants_2_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,25); END Word_insert_constants_2_25;
<*NOWARN*> PROCEDURE Word_insert_constants_2_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,26); END Word_insert_constants_2_26;
<*NOWARN*> PROCEDURE Word_insert_constants_2_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,27); END Word_insert_constants_2_27;
<*NOWARN*> PROCEDURE Word_insert_constants_2_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,28); END Word_insert_constants_2_28;
<*NOWARN*> PROCEDURE Word_insert_constants_2_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,29); END Word_insert_constants_2_29;
<*NOWARN*> PROCEDURE Word_insert_constants_2_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,30); END Word_insert_constants_2_30;
<*NOWARN*> PROCEDURE Word_insert_constants_2_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,31); END Word_insert_constants_2_31;
<*NOWARN*> PROCEDURE Word_insert_constants_2_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,32); END Word_insert_constants_2_32;
<*NOWARN*> PROCEDURE Word_insert_constants_2_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,33); END Word_insert_constants_2_33;
<*NOWARN*> PROCEDURE Word_insert_constants_2_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,34); END Word_insert_constants_2_34;
<*NOWARN*> PROCEDURE Word_insert_constants_2_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,35); END Word_insert_constants_2_35;
<*NOWARN*> PROCEDURE Word_insert_constants_2_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,36); END Word_insert_constants_2_36;
<*NOWARN*> PROCEDURE Word_insert_constants_2_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,37); END Word_insert_constants_2_37;
<*NOWARN*> PROCEDURE Word_insert_constants_2_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,38); END Word_insert_constants_2_38;
<*NOWARN*> PROCEDURE Word_insert_constants_2_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,39); END Word_insert_constants_2_39;
<*NOWARN*> PROCEDURE Word_insert_constants_2_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,40); END Word_insert_constants_2_40;
<*NOWARN*> PROCEDURE Word_insert_constants_2_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,41); END Word_insert_constants_2_41;
<*NOWARN*> PROCEDURE Word_insert_constants_2_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,42); END Word_insert_constants_2_42;
<*NOWARN*> PROCEDURE Word_insert_constants_2_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,43); END Word_insert_constants_2_43;
<*NOWARN*> PROCEDURE Word_insert_constants_2_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,44); END Word_insert_constants_2_44;
<*NOWARN*> PROCEDURE Word_insert_constants_2_45(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,45); END Word_insert_constants_2_45;
<*NOWARN*> PROCEDURE Word_insert_constants_2_46(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,46); END Word_insert_constants_2_46;
<*NOWARN*> PROCEDURE Word_insert_constants_2_47(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,47); END Word_insert_constants_2_47;
<*NOWARN*> PROCEDURE Word_insert_constants_2_48(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,48); END Word_insert_constants_2_48;
<*NOWARN*> PROCEDURE Word_insert_constants_2_49(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,49); END Word_insert_constants_2_49;
<*NOWARN*> PROCEDURE Word_insert_constants_2_50(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,50); END Word_insert_constants_2_50;
<*NOWARN*> PROCEDURE Word_insert_constants_2_51(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,51); END Word_insert_constants_2_51;
<*NOWARN*> PROCEDURE Word_insert_constants_2_52(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,52); END Word_insert_constants_2_52;
<*NOWARN*> PROCEDURE Word_insert_constants_2_53(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,53); END Word_insert_constants_2_53;
<*NOWARN*> PROCEDURE Word_insert_constants_2_54(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,54); END Word_insert_constants_2_54;
<*NOWARN*> PROCEDURE Word_insert_constants_2_55(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,55); END Word_insert_constants_2_55;
<*NOWARN*> PROCEDURE Word_insert_constants_2_56(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,56); END Word_insert_constants_2_56;
<*NOWARN*> PROCEDURE Word_insert_constants_2_57(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,57); END Word_insert_constants_2_57;
<*NOWARN*> PROCEDURE Word_insert_constants_2_58(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,58); END Word_insert_constants_2_58;
<*NOWARN*> PROCEDURE Word_insert_constants_2_59(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,59); END Word_insert_constants_2_59;
<*NOWARN*> PROCEDURE Word_insert_constants_2_60(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,60); END Word_insert_constants_2_60;
<*NOWARN*> PROCEDURE Word_insert_constants_2_61(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,61); END Word_insert_constants_2_61;
<*NOWARN*> PROCEDURE Word_insert_constants_2_62(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,2,62); END Word_insert_constants_2_62;
<*NOWARN*> PROCEDURE Word_insert_constants_3_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,0); END Word_insert_constants_3_0;
<*NOWARN*> PROCEDURE Word_insert_constants_3_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,1); END Word_insert_constants_3_1;
<*NOWARN*> PROCEDURE Word_insert_constants_3_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,2); END Word_insert_constants_3_2;
<*NOWARN*> PROCEDURE Word_insert_constants_3_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,3); END Word_insert_constants_3_3;
<*NOWARN*> PROCEDURE Word_insert_constants_3_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,4); END Word_insert_constants_3_4;
<*NOWARN*> PROCEDURE Word_insert_constants_3_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,5); END Word_insert_constants_3_5;
<*NOWARN*> PROCEDURE Word_insert_constants_3_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,6); END Word_insert_constants_3_6;
<*NOWARN*> PROCEDURE Word_insert_constants_3_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,7); END Word_insert_constants_3_7;
<*NOWARN*> PROCEDURE Word_insert_constants_3_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,8); END Word_insert_constants_3_8;
<*NOWARN*> PROCEDURE Word_insert_constants_3_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,9); END Word_insert_constants_3_9;
<*NOWARN*> PROCEDURE Word_insert_constants_3_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,10); END Word_insert_constants_3_10;
<*NOWARN*> PROCEDURE Word_insert_constants_3_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,11); END Word_insert_constants_3_11;
<*NOWARN*> PROCEDURE Word_insert_constants_3_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,12); END Word_insert_constants_3_12;
<*NOWARN*> PROCEDURE Word_insert_constants_3_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,13); END Word_insert_constants_3_13;
<*NOWARN*> PROCEDURE Word_insert_constants_3_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,14); END Word_insert_constants_3_14;
<*NOWARN*> PROCEDURE Word_insert_constants_3_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,15); END Word_insert_constants_3_15;
<*NOWARN*> PROCEDURE Word_insert_constants_3_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,16); END Word_insert_constants_3_16;
<*NOWARN*> PROCEDURE Word_insert_constants_3_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,17); END Word_insert_constants_3_17;
<*NOWARN*> PROCEDURE Word_insert_constants_3_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,18); END Word_insert_constants_3_18;
<*NOWARN*> PROCEDURE Word_insert_constants_3_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,19); END Word_insert_constants_3_19;
<*NOWARN*> PROCEDURE Word_insert_constants_3_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,20); END Word_insert_constants_3_20;
<*NOWARN*> PROCEDURE Word_insert_constants_3_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,21); END Word_insert_constants_3_21;
<*NOWARN*> PROCEDURE Word_insert_constants_3_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,22); END Word_insert_constants_3_22;
<*NOWARN*> PROCEDURE Word_insert_constants_3_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,23); END Word_insert_constants_3_23;
<*NOWARN*> PROCEDURE Word_insert_constants_3_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,24); END Word_insert_constants_3_24;
<*NOWARN*> PROCEDURE Word_insert_constants_3_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,25); END Word_insert_constants_3_25;
<*NOWARN*> PROCEDURE Word_insert_constants_3_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,26); END Word_insert_constants_3_26;
<*NOWARN*> PROCEDURE Word_insert_constants_3_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,27); END Word_insert_constants_3_27;
<*NOWARN*> PROCEDURE Word_insert_constants_3_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,28); END Word_insert_constants_3_28;
<*NOWARN*> PROCEDURE Word_insert_constants_3_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,29); END Word_insert_constants_3_29;
<*NOWARN*> PROCEDURE Word_insert_constants_3_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,30); END Word_insert_constants_3_30;
<*NOWARN*> PROCEDURE Word_insert_constants_3_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,31); END Word_insert_constants_3_31;
<*NOWARN*> PROCEDURE Word_insert_constants_3_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,32); END Word_insert_constants_3_32;
<*NOWARN*> PROCEDURE Word_insert_constants_3_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,33); END Word_insert_constants_3_33;
<*NOWARN*> PROCEDURE Word_insert_constants_3_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,34); END Word_insert_constants_3_34;
<*NOWARN*> PROCEDURE Word_insert_constants_3_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,35); END Word_insert_constants_3_35;
<*NOWARN*> PROCEDURE Word_insert_constants_3_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,36); END Word_insert_constants_3_36;
<*NOWARN*> PROCEDURE Word_insert_constants_3_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,37); END Word_insert_constants_3_37;
<*NOWARN*> PROCEDURE Word_insert_constants_3_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,38); END Word_insert_constants_3_38;
<*NOWARN*> PROCEDURE Word_insert_constants_3_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,39); END Word_insert_constants_3_39;
<*NOWARN*> PROCEDURE Word_insert_constants_3_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,40); END Word_insert_constants_3_40;
<*NOWARN*> PROCEDURE Word_insert_constants_3_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,41); END Word_insert_constants_3_41;
<*NOWARN*> PROCEDURE Word_insert_constants_3_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,42); END Word_insert_constants_3_42;
<*NOWARN*> PROCEDURE Word_insert_constants_3_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,43); END Word_insert_constants_3_43;
<*NOWARN*> PROCEDURE Word_insert_constants_3_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,44); END Word_insert_constants_3_44;
<*NOWARN*> PROCEDURE Word_insert_constants_3_45(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,45); END Word_insert_constants_3_45;
<*NOWARN*> PROCEDURE Word_insert_constants_3_46(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,46); END Word_insert_constants_3_46;
<*NOWARN*> PROCEDURE Word_insert_constants_3_47(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,47); END Word_insert_constants_3_47;
<*NOWARN*> PROCEDURE Word_insert_constants_3_48(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,48); END Word_insert_constants_3_48;
<*NOWARN*> PROCEDURE Word_insert_constants_3_49(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,49); END Word_insert_constants_3_49;
<*NOWARN*> PROCEDURE Word_insert_constants_3_50(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,50); END Word_insert_constants_3_50;
<*NOWARN*> PROCEDURE Word_insert_constants_3_51(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,51); END Word_insert_constants_3_51;
<*NOWARN*> PROCEDURE Word_insert_constants_3_52(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,52); END Word_insert_constants_3_52;
<*NOWARN*> PROCEDURE Word_insert_constants_3_53(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,53); END Word_insert_constants_3_53;
<*NOWARN*> PROCEDURE Word_insert_constants_3_54(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,54); END Word_insert_constants_3_54;
<*NOWARN*> PROCEDURE Word_insert_constants_3_55(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,55); END Word_insert_constants_3_55;
<*NOWARN*> PROCEDURE Word_insert_constants_3_56(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,56); END Word_insert_constants_3_56;
<*NOWARN*> PROCEDURE Word_insert_constants_3_57(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,57); END Word_insert_constants_3_57;
<*NOWARN*> PROCEDURE Word_insert_constants_3_58(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,58); END Word_insert_constants_3_58;
<*NOWARN*> PROCEDURE Word_insert_constants_3_59(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,59); END Word_insert_constants_3_59;
<*NOWARN*> PROCEDURE Word_insert_constants_3_60(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,60); END Word_insert_constants_3_60;
<*NOWARN*> PROCEDURE Word_insert_constants_3_61(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,3,61); END Word_insert_constants_3_61;
<*NOWARN*> PROCEDURE Word_insert_constants_4_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,0); END Word_insert_constants_4_0;
<*NOWARN*> PROCEDURE Word_insert_constants_4_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,1); END Word_insert_constants_4_1;
<*NOWARN*> PROCEDURE Word_insert_constants_4_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,2); END Word_insert_constants_4_2;
<*NOWARN*> PROCEDURE Word_insert_constants_4_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,3); END Word_insert_constants_4_3;
<*NOWARN*> PROCEDURE Word_insert_constants_4_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,4); END Word_insert_constants_4_4;
<*NOWARN*> PROCEDURE Word_insert_constants_4_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,5); END Word_insert_constants_4_5;
<*NOWARN*> PROCEDURE Word_insert_constants_4_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,6); END Word_insert_constants_4_6;
<*NOWARN*> PROCEDURE Word_insert_constants_4_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,7); END Word_insert_constants_4_7;
<*NOWARN*> PROCEDURE Word_insert_constants_4_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,8); END Word_insert_constants_4_8;
<*NOWARN*> PROCEDURE Word_insert_constants_4_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,9); END Word_insert_constants_4_9;
<*NOWARN*> PROCEDURE Word_insert_constants_4_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,10); END Word_insert_constants_4_10;
<*NOWARN*> PROCEDURE Word_insert_constants_4_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,11); END Word_insert_constants_4_11;
<*NOWARN*> PROCEDURE Word_insert_constants_4_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,12); END Word_insert_constants_4_12;
<*NOWARN*> PROCEDURE Word_insert_constants_4_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,13); END Word_insert_constants_4_13;
<*NOWARN*> PROCEDURE Word_insert_constants_4_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,14); END Word_insert_constants_4_14;
<*NOWARN*> PROCEDURE Word_insert_constants_4_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,15); END Word_insert_constants_4_15;
<*NOWARN*> PROCEDURE Word_insert_constants_4_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,16); END Word_insert_constants_4_16;
<*NOWARN*> PROCEDURE Word_insert_constants_4_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,17); END Word_insert_constants_4_17;
<*NOWARN*> PROCEDURE Word_insert_constants_4_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,18); END Word_insert_constants_4_18;
<*NOWARN*> PROCEDURE Word_insert_constants_4_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,19); END Word_insert_constants_4_19;
<*NOWARN*> PROCEDURE Word_insert_constants_4_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,20); END Word_insert_constants_4_20;
<*NOWARN*> PROCEDURE Word_insert_constants_4_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,21); END Word_insert_constants_4_21;
<*NOWARN*> PROCEDURE Word_insert_constants_4_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,22); END Word_insert_constants_4_22;
<*NOWARN*> PROCEDURE Word_insert_constants_4_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,23); END Word_insert_constants_4_23;
<*NOWARN*> PROCEDURE Word_insert_constants_4_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,24); END Word_insert_constants_4_24;
<*NOWARN*> PROCEDURE Word_insert_constants_4_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,25); END Word_insert_constants_4_25;
<*NOWARN*> PROCEDURE Word_insert_constants_4_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,26); END Word_insert_constants_4_26;
<*NOWARN*> PROCEDURE Word_insert_constants_4_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,27); END Word_insert_constants_4_27;
<*NOWARN*> PROCEDURE Word_insert_constants_4_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,28); END Word_insert_constants_4_28;
<*NOWARN*> PROCEDURE Word_insert_constants_4_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,29); END Word_insert_constants_4_29;
<*NOWARN*> PROCEDURE Word_insert_constants_4_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,30); END Word_insert_constants_4_30;
<*NOWARN*> PROCEDURE Word_insert_constants_4_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,31); END Word_insert_constants_4_31;
<*NOWARN*> PROCEDURE Word_insert_constants_4_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,32); END Word_insert_constants_4_32;
<*NOWARN*> PROCEDURE Word_insert_constants_4_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,33); END Word_insert_constants_4_33;
<*NOWARN*> PROCEDURE Word_insert_constants_4_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,34); END Word_insert_constants_4_34;
<*NOWARN*> PROCEDURE Word_insert_constants_4_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,35); END Word_insert_constants_4_35;
<*NOWARN*> PROCEDURE Word_insert_constants_4_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,36); END Word_insert_constants_4_36;
<*NOWARN*> PROCEDURE Word_insert_constants_4_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,37); END Word_insert_constants_4_37;
<*NOWARN*> PROCEDURE Word_insert_constants_4_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,38); END Word_insert_constants_4_38;
<*NOWARN*> PROCEDURE Word_insert_constants_4_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,39); END Word_insert_constants_4_39;
<*NOWARN*> PROCEDURE Word_insert_constants_4_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,40); END Word_insert_constants_4_40;
<*NOWARN*> PROCEDURE Word_insert_constants_4_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,41); END Word_insert_constants_4_41;
<*NOWARN*> PROCEDURE Word_insert_constants_4_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,42); END Word_insert_constants_4_42;
<*NOWARN*> PROCEDURE Word_insert_constants_4_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,43); END Word_insert_constants_4_43;
<*NOWARN*> PROCEDURE Word_insert_constants_4_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,44); END Word_insert_constants_4_44;
<*NOWARN*> PROCEDURE Word_insert_constants_4_45(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,45); END Word_insert_constants_4_45;
<*NOWARN*> PROCEDURE Word_insert_constants_4_46(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,46); END Word_insert_constants_4_46;
<*NOWARN*> PROCEDURE Word_insert_constants_4_47(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,47); END Word_insert_constants_4_47;
<*NOWARN*> PROCEDURE Word_insert_constants_4_48(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,48); END Word_insert_constants_4_48;
<*NOWARN*> PROCEDURE Word_insert_constants_4_49(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,49); END Word_insert_constants_4_49;
<*NOWARN*> PROCEDURE Word_insert_constants_4_50(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,50); END Word_insert_constants_4_50;
<*NOWARN*> PROCEDURE Word_insert_constants_4_51(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,51); END Word_insert_constants_4_51;
<*NOWARN*> PROCEDURE Word_insert_constants_4_52(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,52); END Word_insert_constants_4_52;
<*NOWARN*> PROCEDURE Word_insert_constants_4_53(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,53); END Word_insert_constants_4_53;
<*NOWARN*> PROCEDURE Word_insert_constants_4_54(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,54); END Word_insert_constants_4_54;
<*NOWARN*> PROCEDURE Word_insert_constants_4_55(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,55); END Word_insert_constants_4_55;
<*NOWARN*> PROCEDURE Word_insert_constants_4_56(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,56); END Word_insert_constants_4_56;
<*NOWARN*> PROCEDURE Word_insert_constants_4_57(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,57); END Word_insert_constants_4_57;
<*NOWARN*> PROCEDURE Word_insert_constants_4_58(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,58); END Word_insert_constants_4_58;
<*NOWARN*> PROCEDURE Word_insert_constants_4_59(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,59); END Word_insert_constants_4_59;
<*NOWARN*> PROCEDURE Word_insert_constants_4_60(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,4,60); END Word_insert_constants_4_60;
<*NOWARN*> PROCEDURE Word_insert_constants_5_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,0); END Word_insert_constants_5_0;
<*NOWARN*> PROCEDURE Word_insert_constants_5_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,1); END Word_insert_constants_5_1;
<*NOWARN*> PROCEDURE Word_insert_constants_5_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,2); END Word_insert_constants_5_2;
<*NOWARN*> PROCEDURE Word_insert_constants_5_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,3); END Word_insert_constants_5_3;
<*NOWARN*> PROCEDURE Word_insert_constants_5_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,4); END Word_insert_constants_5_4;
<*NOWARN*> PROCEDURE Word_insert_constants_5_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,5); END Word_insert_constants_5_5;
<*NOWARN*> PROCEDURE Word_insert_constants_5_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,6); END Word_insert_constants_5_6;
<*NOWARN*> PROCEDURE Word_insert_constants_5_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,7); END Word_insert_constants_5_7;
<*NOWARN*> PROCEDURE Word_insert_constants_5_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,8); END Word_insert_constants_5_8;
<*NOWARN*> PROCEDURE Word_insert_constants_5_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,9); END Word_insert_constants_5_9;
<*NOWARN*> PROCEDURE Word_insert_constants_5_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,10); END Word_insert_constants_5_10;
<*NOWARN*> PROCEDURE Word_insert_constants_5_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,11); END Word_insert_constants_5_11;
<*NOWARN*> PROCEDURE Word_insert_constants_5_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,12); END Word_insert_constants_5_12;
<*NOWARN*> PROCEDURE Word_insert_constants_5_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,13); END Word_insert_constants_5_13;
<*NOWARN*> PROCEDURE Word_insert_constants_5_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,14); END Word_insert_constants_5_14;
<*NOWARN*> PROCEDURE Word_insert_constants_5_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,15); END Word_insert_constants_5_15;
<*NOWARN*> PROCEDURE Word_insert_constants_5_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,16); END Word_insert_constants_5_16;
<*NOWARN*> PROCEDURE Word_insert_constants_5_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,17); END Word_insert_constants_5_17;
<*NOWARN*> PROCEDURE Word_insert_constants_5_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,18); END Word_insert_constants_5_18;
<*NOWARN*> PROCEDURE Word_insert_constants_5_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,19); END Word_insert_constants_5_19;
<*NOWARN*> PROCEDURE Word_insert_constants_5_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,20); END Word_insert_constants_5_20;
<*NOWARN*> PROCEDURE Word_insert_constants_5_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,21); END Word_insert_constants_5_21;
<*NOWARN*> PROCEDURE Word_insert_constants_5_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,22); END Word_insert_constants_5_22;
<*NOWARN*> PROCEDURE Word_insert_constants_5_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,23); END Word_insert_constants_5_23;
<*NOWARN*> PROCEDURE Word_insert_constants_5_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,24); END Word_insert_constants_5_24;
<*NOWARN*> PROCEDURE Word_insert_constants_5_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,25); END Word_insert_constants_5_25;
<*NOWARN*> PROCEDURE Word_insert_constants_5_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,26); END Word_insert_constants_5_26;
<*NOWARN*> PROCEDURE Word_insert_constants_5_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,27); END Word_insert_constants_5_27;
<*NOWARN*> PROCEDURE Word_insert_constants_5_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,28); END Word_insert_constants_5_28;
<*NOWARN*> PROCEDURE Word_insert_constants_5_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,29); END Word_insert_constants_5_29;
<*NOWARN*> PROCEDURE Word_insert_constants_5_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,30); END Word_insert_constants_5_30;
<*NOWARN*> PROCEDURE Word_insert_constants_5_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,31); END Word_insert_constants_5_31;
<*NOWARN*> PROCEDURE Word_insert_constants_5_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,32); END Word_insert_constants_5_32;
<*NOWARN*> PROCEDURE Word_insert_constants_5_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,33); END Word_insert_constants_5_33;
<*NOWARN*> PROCEDURE Word_insert_constants_5_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,34); END Word_insert_constants_5_34;
<*NOWARN*> PROCEDURE Word_insert_constants_5_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,35); END Word_insert_constants_5_35;
<*NOWARN*> PROCEDURE Word_insert_constants_5_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,36); END Word_insert_constants_5_36;
<*NOWARN*> PROCEDURE Word_insert_constants_5_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,37); END Word_insert_constants_5_37;
<*NOWARN*> PROCEDURE Word_insert_constants_5_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,38); END Word_insert_constants_5_38;
<*NOWARN*> PROCEDURE Word_insert_constants_5_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,39); END Word_insert_constants_5_39;
<*NOWARN*> PROCEDURE Word_insert_constants_5_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,40); END Word_insert_constants_5_40;
<*NOWARN*> PROCEDURE Word_insert_constants_5_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,41); END Word_insert_constants_5_41;
<*NOWARN*> PROCEDURE Word_insert_constants_5_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,42); END Word_insert_constants_5_42;
<*NOWARN*> PROCEDURE Word_insert_constants_5_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,43); END Word_insert_constants_5_43;
<*NOWARN*> PROCEDURE Word_insert_constants_5_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,44); END Word_insert_constants_5_44;
<*NOWARN*> PROCEDURE Word_insert_constants_5_45(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,45); END Word_insert_constants_5_45;
<*NOWARN*> PROCEDURE Word_insert_constants_5_46(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,46); END Word_insert_constants_5_46;
<*NOWARN*> PROCEDURE Word_insert_constants_5_47(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,47); END Word_insert_constants_5_47;
<*NOWARN*> PROCEDURE Word_insert_constants_5_48(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,48); END Word_insert_constants_5_48;
<*NOWARN*> PROCEDURE Word_insert_constants_5_49(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,49); END Word_insert_constants_5_49;
<*NOWARN*> PROCEDURE Word_insert_constants_5_50(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,50); END Word_insert_constants_5_50;
<*NOWARN*> PROCEDURE Word_insert_constants_5_51(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,51); END Word_insert_constants_5_51;
<*NOWARN*> PROCEDURE Word_insert_constants_5_52(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,52); END Word_insert_constants_5_52;
<*NOWARN*> PROCEDURE Word_insert_constants_5_53(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,53); END Word_insert_constants_5_53;
<*NOWARN*> PROCEDURE Word_insert_constants_5_54(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,54); END Word_insert_constants_5_54;
<*NOWARN*> PROCEDURE Word_insert_constants_5_55(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,55); END Word_insert_constants_5_55;
<*NOWARN*> PROCEDURE Word_insert_constants_5_56(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,56); END Word_insert_constants_5_56;
<*NOWARN*> PROCEDURE Word_insert_constants_5_57(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,57); END Word_insert_constants_5_57;
<*NOWARN*> PROCEDURE Word_insert_constants_5_58(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,58); END Word_insert_constants_5_58;
<*NOWARN*> PROCEDURE Word_insert_constants_5_59(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,5,59); END Word_insert_constants_5_59;
<*NOWARN*> PROCEDURE Word_insert_constants_6_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,0); END Word_insert_constants_6_0;
<*NOWARN*> PROCEDURE Word_insert_constants_6_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,1); END Word_insert_constants_6_1;
<*NOWARN*> PROCEDURE Word_insert_constants_6_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,2); END Word_insert_constants_6_2;
<*NOWARN*> PROCEDURE Word_insert_constants_6_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,3); END Word_insert_constants_6_3;
<*NOWARN*> PROCEDURE Word_insert_constants_6_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,4); END Word_insert_constants_6_4;
<*NOWARN*> PROCEDURE Word_insert_constants_6_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,5); END Word_insert_constants_6_5;
<*NOWARN*> PROCEDURE Word_insert_constants_6_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,6); END Word_insert_constants_6_6;
<*NOWARN*> PROCEDURE Word_insert_constants_6_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,7); END Word_insert_constants_6_7;
<*NOWARN*> PROCEDURE Word_insert_constants_6_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,8); END Word_insert_constants_6_8;
<*NOWARN*> PROCEDURE Word_insert_constants_6_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,9); END Word_insert_constants_6_9;
<*NOWARN*> PROCEDURE Word_insert_constants_6_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,10); END Word_insert_constants_6_10;
<*NOWARN*> PROCEDURE Word_insert_constants_6_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,11); END Word_insert_constants_6_11;
<*NOWARN*> PROCEDURE Word_insert_constants_6_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,12); END Word_insert_constants_6_12;
<*NOWARN*> PROCEDURE Word_insert_constants_6_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,13); END Word_insert_constants_6_13;
<*NOWARN*> PROCEDURE Word_insert_constants_6_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,14); END Word_insert_constants_6_14;
<*NOWARN*> PROCEDURE Word_insert_constants_6_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,15); END Word_insert_constants_6_15;
<*NOWARN*> PROCEDURE Word_insert_constants_6_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,16); END Word_insert_constants_6_16;
<*NOWARN*> PROCEDURE Word_insert_constants_6_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,17); END Word_insert_constants_6_17;
<*NOWARN*> PROCEDURE Word_insert_constants_6_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,18); END Word_insert_constants_6_18;
<*NOWARN*> PROCEDURE Word_insert_constants_6_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,19); END Word_insert_constants_6_19;
<*NOWARN*> PROCEDURE Word_insert_constants_6_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,20); END Word_insert_constants_6_20;
<*NOWARN*> PROCEDURE Word_insert_constants_6_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,21); END Word_insert_constants_6_21;
<*NOWARN*> PROCEDURE Word_insert_constants_6_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,22); END Word_insert_constants_6_22;
<*NOWARN*> PROCEDURE Word_insert_constants_6_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,23); END Word_insert_constants_6_23;
<*NOWARN*> PROCEDURE Word_insert_constants_6_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,24); END Word_insert_constants_6_24;
<*NOWARN*> PROCEDURE Word_insert_constants_6_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,25); END Word_insert_constants_6_25;
<*NOWARN*> PROCEDURE Word_insert_constants_6_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,26); END Word_insert_constants_6_26;
<*NOWARN*> PROCEDURE Word_insert_constants_6_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,27); END Word_insert_constants_6_27;
<*NOWARN*> PROCEDURE Word_insert_constants_6_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,28); END Word_insert_constants_6_28;
<*NOWARN*> PROCEDURE Word_insert_constants_6_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,29); END Word_insert_constants_6_29;
<*NOWARN*> PROCEDURE Word_insert_constants_6_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,30); END Word_insert_constants_6_30;
<*NOWARN*> PROCEDURE Word_insert_constants_6_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,31); END Word_insert_constants_6_31;
<*NOWARN*> PROCEDURE Word_insert_constants_6_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,32); END Word_insert_constants_6_32;
<*NOWARN*> PROCEDURE Word_insert_constants_6_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,33); END Word_insert_constants_6_33;
<*NOWARN*> PROCEDURE Word_insert_constants_6_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,34); END Word_insert_constants_6_34;
<*NOWARN*> PROCEDURE Word_insert_constants_6_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,35); END Word_insert_constants_6_35;
<*NOWARN*> PROCEDURE Word_insert_constants_6_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,36); END Word_insert_constants_6_36;
<*NOWARN*> PROCEDURE Word_insert_constants_6_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,37); END Word_insert_constants_6_37;
<*NOWARN*> PROCEDURE Word_insert_constants_6_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,38); END Word_insert_constants_6_38;
<*NOWARN*> PROCEDURE Word_insert_constants_6_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,39); END Word_insert_constants_6_39;
<*NOWARN*> PROCEDURE Word_insert_constants_6_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,40); END Word_insert_constants_6_40;
<*NOWARN*> PROCEDURE Word_insert_constants_6_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,41); END Word_insert_constants_6_41;
<*NOWARN*> PROCEDURE Word_insert_constants_6_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,42); END Word_insert_constants_6_42;
<*NOWARN*> PROCEDURE Word_insert_constants_6_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,43); END Word_insert_constants_6_43;
<*NOWARN*> PROCEDURE Word_insert_constants_6_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,44); END Word_insert_constants_6_44;
<*NOWARN*> PROCEDURE Word_insert_constants_6_45(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,45); END Word_insert_constants_6_45;
<*NOWARN*> PROCEDURE Word_insert_constants_6_46(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,46); END Word_insert_constants_6_46;
<*NOWARN*> PROCEDURE Word_insert_constants_6_47(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,47); END Word_insert_constants_6_47;
<*NOWARN*> PROCEDURE Word_insert_constants_6_48(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,48); END Word_insert_constants_6_48;
<*NOWARN*> PROCEDURE Word_insert_constants_6_49(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,49); END Word_insert_constants_6_49;
<*NOWARN*> PROCEDURE Word_insert_constants_6_50(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,50); END Word_insert_constants_6_50;
<*NOWARN*> PROCEDURE Word_insert_constants_6_51(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,51); END Word_insert_constants_6_51;
<*NOWARN*> PROCEDURE Word_insert_constants_6_52(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,52); END Word_insert_constants_6_52;
<*NOWARN*> PROCEDURE Word_insert_constants_6_53(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,53); END Word_insert_constants_6_53;
<*NOWARN*> PROCEDURE Word_insert_constants_6_54(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,54); END Word_insert_constants_6_54;
<*NOWARN*> PROCEDURE Word_insert_constants_6_55(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,55); END Word_insert_constants_6_55;
<*NOWARN*> PROCEDURE Word_insert_constants_6_56(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,56); END Word_insert_constants_6_56;
<*NOWARN*> PROCEDURE Word_insert_constants_6_57(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,57); END Word_insert_constants_6_57;
<*NOWARN*> PROCEDURE Word_insert_constants_6_58(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,6,58); END Word_insert_constants_6_58;
<*NOWARN*> PROCEDURE Word_insert_constants_7_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,0); END Word_insert_constants_7_0;
<*NOWARN*> PROCEDURE Word_insert_constants_7_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,1); END Word_insert_constants_7_1;
<*NOWARN*> PROCEDURE Word_insert_constants_7_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,2); END Word_insert_constants_7_2;
<*NOWARN*> PROCEDURE Word_insert_constants_7_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,3); END Word_insert_constants_7_3;
<*NOWARN*> PROCEDURE Word_insert_constants_7_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,4); END Word_insert_constants_7_4;
<*NOWARN*> PROCEDURE Word_insert_constants_7_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,5); END Word_insert_constants_7_5;
<*NOWARN*> PROCEDURE Word_insert_constants_7_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,6); END Word_insert_constants_7_6;
<*NOWARN*> PROCEDURE Word_insert_constants_7_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,7); END Word_insert_constants_7_7;
<*NOWARN*> PROCEDURE Word_insert_constants_7_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,8); END Word_insert_constants_7_8;
<*NOWARN*> PROCEDURE Word_insert_constants_7_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,9); END Word_insert_constants_7_9;
<*NOWARN*> PROCEDURE Word_insert_constants_7_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,10); END Word_insert_constants_7_10;
<*NOWARN*> PROCEDURE Word_insert_constants_7_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,11); END Word_insert_constants_7_11;
<*NOWARN*> PROCEDURE Word_insert_constants_7_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,12); END Word_insert_constants_7_12;
<*NOWARN*> PROCEDURE Word_insert_constants_7_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,13); END Word_insert_constants_7_13;
<*NOWARN*> PROCEDURE Word_insert_constants_7_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,14); END Word_insert_constants_7_14;
<*NOWARN*> PROCEDURE Word_insert_constants_7_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,15); END Word_insert_constants_7_15;
<*NOWARN*> PROCEDURE Word_insert_constants_7_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,16); END Word_insert_constants_7_16;
<*NOWARN*> PROCEDURE Word_insert_constants_7_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,17); END Word_insert_constants_7_17;
<*NOWARN*> PROCEDURE Word_insert_constants_7_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,18); END Word_insert_constants_7_18;
<*NOWARN*> PROCEDURE Word_insert_constants_7_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,19); END Word_insert_constants_7_19;
<*NOWARN*> PROCEDURE Word_insert_constants_7_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,20); END Word_insert_constants_7_20;
<*NOWARN*> PROCEDURE Word_insert_constants_7_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,21); END Word_insert_constants_7_21;
<*NOWARN*> PROCEDURE Word_insert_constants_7_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,22); END Word_insert_constants_7_22;
<*NOWARN*> PROCEDURE Word_insert_constants_7_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,23); END Word_insert_constants_7_23;
<*NOWARN*> PROCEDURE Word_insert_constants_7_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,24); END Word_insert_constants_7_24;
<*NOWARN*> PROCEDURE Word_insert_constants_7_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,25); END Word_insert_constants_7_25;
<*NOWARN*> PROCEDURE Word_insert_constants_7_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,26); END Word_insert_constants_7_26;
<*NOWARN*> PROCEDURE Word_insert_constants_7_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,27); END Word_insert_constants_7_27;
<*NOWARN*> PROCEDURE Word_insert_constants_7_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,28); END Word_insert_constants_7_28;
<*NOWARN*> PROCEDURE Word_insert_constants_7_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,29); END Word_insert_constants_7_29;
<*NOWARN*> PROCEDURE Word_insert_constants_7_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,30); END Word_insert_constants_7_30;
<*NOWARN*> PROCEDURE Word_insert_constants_7_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,31); END Word_insert_constants_7_31;
<*NOWARN*> PROCEDURE Word_insert_constants_7_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,32); END Word_insert_constants_7_32;
<*NOWARN*> PROCEDURE Word_insert_constants_7_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,33); END Word_insert_constants_7_33;
<*NOWARN*> PROCEDURE Word_insert_constants_7_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,34); END Word_insert_constants_7_34;
<*NOWARN*> PROCEDURE Word_insert_constants_7_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,35); END Word_insert_constants_7_35;
<*NOWARN*> PROCEDURE Word_insert_constants_7_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,36); END Word_insert_constants_7_36;
<*NOWARN*> PROCEDURE Word_insert_constants_7_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,37); END Word_insert_constants_7_37;
<*NOWARN*> PROCEDURE Word_insert_constants_7_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,38); END Word_insert_constants_7_38;
<*NOWARN*> PROCEDURE Word_insert_constants_7_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,39); END Word_insert_constants_7_39;
<*NOWARN*> PROCEDURE Word_insert_constants_7_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,40); END Word_insert_constants_7_40;
<*NOWARN*> PROCEDURE Word_insert_constants_7_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,41); END Word_insert_constants_7_41;
<*NOWARN*> PROCEDURE Word_insert_constants_7_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,42); END Word_insert_constants_7_42;
<*NOWARN*> PROCEDURE Word_insert_constants_7_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,43); END Word_insert_constants_7_43;
<*NOWARN*> PROCEDURE Word_insert_constants_7_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,44); END Word_insert_constants_7_44;
<*NOWARN*> PROCEDURE Word_insert_constants_7_45(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,45); END Word_insert_constants_7_45;
<*NOWARN*> PROCEDURE Word_insert_constants_7_46(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,46); END Word_insert_constants_7_46;
<*NOWARN*> PROCEDURE Word_insert_constants_7_47(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,47); END Word_insert_constants_7_47;
<*NOWARN*> PROCEDURE Word_insert_constants_7_48(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,48); END Word_insert_constants_7_48;
<*NOWARN*> PROCEDURE Word_insert_constants_7_49(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,49); END Word_insert_constants_7_49;
<*NOWARN*> PROCEDURE Word_insert_constants_7_50(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,50); END Word_insert_constants_7_50;
<*NOWARN*> PROCEDURE Word_insert_constants_7_51(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,51); END Word_insert_constants_7_51;
<*NOWARN*> PROCEDURE Word_insert_constants_7_52(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,52); END Word_insert_constants_7_52;
<*NOWARN*> PROCEDURE Word_insert_constants_7_53(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,53); END Word_insert_constants_7_53;
<*NOWARN*> PROCEDURE Word_insert_constants_7_54(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,54); END Word_insert_constants_7_54;
<*NOWARN*> PROCEDURE Word_insert_constants_7_55(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,55); END Word_insert_constants_7_55;
<*NOWARN*> PROCEDURE Word_insert_constants_7_56(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,56); END Word_insert_constants_7_56;
<*NOWARN*> PROCEDURE Word_insert_constants_7_57(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,7,57); END Word_insert_constants_7_57;
<*NOWARN*> PROCEDURE Word_insert_constants_8_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,0); END Word_insert_constants_8_0;
<*NOWARN*> PROCEDURE Word_insert_constants_8_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,1); END Word_insert_constants_8_1;
<*NOWARN*> PROCEDURE Word_insert_constants_8_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,2); END Word_insert_constants_8_2;
<*NOWARN*> PROCEDURE Word_insert_constants_8_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,3); END Word_insert_constants_8_3;
<*NOWARN*> PROCEDURE Word_insert_constants_8_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,4); END Word_insert_constants_8_4;
<*NOWARN*> PROCEDURE Word_insert_constants_8_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,5); END Word_insert_constants_8_5;
<*NOWARN*> PROCEDURE Word_insert_constants_8_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,6); END Word_insert_constants_8_6;
<*NOWARN*> PROCEDURE Word_insert_constants_8_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,7); END Word_insert_constants_8_7;
<*NOWARN*> PROCEDURE Word_insert_constants_8_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,8); END Word_insert_constants_8_8;
<*NOWARN*> PROCEDURE Word_insert_constants_8_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,9); END Word_insert_constants_8_9;
<*NOWARN*> PROCEDURE Word_insert_constants_8_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,10); END Word_insert_constants_8_10;
<*NOWARN*> PROCEDURE Word_insert_constants_8_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,11); END Word_insert_constants_8_11;
<*NOWARN*> PROCEDURE Word_insert_constants_8_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,12); END Word_insert_constants_8_12;
<*NOWARN*> PROCEDURE Word_insert_constants_8_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,13); END Word_insert_constants_8_13;
<*NOWARN*> PROCEDURE Word_insert_constants_8_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,14); END Word_insert_constants_8_14;
<*NOWARN*> PROCEDURE Word_insert_constants_8_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,15); END Word_insert_constants_8_15;
<*NOWARN*> PROCEDURE Word_insert_constants_8_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,16); END Word_insert_constants_8_16;
<*NOWARN*> PROCEDURE Word_insert_constants_8_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,17); END Word_insert_constants_8_17;
<*NOWARN*> PROCEDURE Word_insert_constants_8_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,18); END Word_insert_constants_8_18;
<*NOWARN*> PROCEDURE Word_insert_constants_8_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,19); END Word_insert_constants_8_19;
<*NOWARN*> PROCEDURE Word_insert_constants_8_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,20); END Word_insert_constants_8_20;
<*NOWARN*> PROCEDURE Word_insert_constants_8_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,21); END Word_insert_constants_8_21;
<*NOWARN*> PROCEDURE Word_insert_constants_8_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,22); END Word_insert_constants_8_22;
<*NOWARN*> PROCEDURE Word_insert_constants_8_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,23); END Word_insert_constants_8_23;
<*NOWARN*> PROCEDURE Word_insert_constants_8_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,24); END Word_insert_constants_8_24;
<*NOWARN*> PROCEDURE Word_insert_constants_8_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,25); END Word_insert_constants_8_25;
<*NOWARN*> PROCEDURE Word_insert_constants_8_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,26); END Word_insert_constants_8_26;
<*NOWARN*> PROCEDURE Word_insert_constants_8_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,27); END Word_insert_constants_8_27;
<*NOWARN*> PROCEDURE Word_insert_constants_8_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,28); END Word_insert_constants_8_28;
<*NOWARN*> PROCEDURE Word_insert_constants_8_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,29); END Word_insert_constants_8_29;
<*NOWARN*> PROCEDURE Word_insert_constants_8_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,30); END Word_insert_constants_8_30;
<*NOWARN*> PROCEDURE Word_insert_constants_8_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,31); END Word_insert_constants_8_31;
<*NOWARN*> PROCEDURE Word_insert_constants_8_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,32); END Word_insert_constants_8_32;
<*NOWARN*> PROCEDURE Word_insert_constants_8_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,33); END Word_insert_constants_8_33;
<*NOWARN*> PROCEDURE Word_insert_constants_8_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,34); END Word_insert_constants_8_34;
<*NOWARN*> PROCEDURE Word_insert_constants_8_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,35); END Word_insert_constants_8_35;
<*NOWARN*> PROCEDURE Word_insert_constants_8_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,36); END Word_insert_constants_8_36;
<*NOWARN*> PROCEDURE Word_insert_constants_8_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,37); END Word_insert_constants_8_37;
<*NOWARN*> PROCEDURE Word_insert_constants_8_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,38); END Word_insert_constants_8_38;
<*NOWARN*> PROCEDURE Word_insert_constants_8_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,39); END Word_insert_constants_8_39;
<*NOWARN*> PROCEDURE Word_insert_constants_8_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,40); END Word_insert_constants_8_40;
<*NOWARN*> PROCEDURE Word_insert_constants_8_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,41); END Word_insert_constants_8_41;
<*NOWARN*> PROCEDURE Word_insert_constants_8_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,42); END Word_insert_constants_8_42;
<*NOWARN*> PROCEDURE Word_insert_constants_8_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,43); END Word_insert_constants_8_43;
<*NOWARN*> PROCEDURE Word_insert_constants_8_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,44); END Word_insert_constants_8_44;
<*NOWARN*> PROCEDURE Word_insert_constants_8_45(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,45); END Word_insert_constants_8_45;
<*NOWARN*> PROCEDURE Word_insert_constants_8_46(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,46); END Word_insert_constants_8_46;
<*NOWARN*> PROCEDURE Word_insert_constants_8_47(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,47); END Word_insert_constants_8_47;
<*NOWARN*> PROCEDURE Word_insert_constants_8_48(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,48); END Word_insert_constants_8_48;
<*NOWARN*> PROCEDURE Word_insert_constants_8_49(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,49); END Word_insert_constants_8_49;
<*NOWARN*> PROCEDURE Word_insert_constants_8_50(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,50); END Word_insert_constants_8_50;
<*NOWARN*> PROCEDURE Word_insert_constants_8_51(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,51); END Word_insert_constants_8_51;
<*NOWARN*> PROCEDURE Word_insert_constants_8_52(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,52); END Word_insert_constants_8_52;
<*NOWARN*> PROCEDURE Word_insert_constants_8_53(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,53); END Word_insert_constants_8_53;
<*NOWARN*> PROCEDURE Word_insert_constants_8_54(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,54); END Word_insert_constants_8_54;
<*NOWARN*> PROCEDURE Word_insert_constants_8_55(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,55); END Word_insert_constants_8_55;
<*NOWARN*> PROCEDURE Word_insert_constants_8_56(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,8,56); END Word_insert_constants_8_56;
<*NOWARN*> PROCEDURE Word_insert_constants_9_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,0); END Word_insert_constants_9_0;
<*NOWARN*> PROCEDURE Word_insert_constants_9_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,1); END Word_insert_constants_9_1;
<*NOWARN*> PROCEDURE Word_insert_constants_9_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,2); END Word_insert_constants_9_2;
<*NOWARN*> PROCEDURE Word_insert_constants_9_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,3); END Word_insert_constants_9_3;
<*NOWARN*> PROCEDURE Word_insert_constants_9_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,4); END Word_insert_constants_9_4;
<*NOWARN*> PROCEDURE Word_insert_constants_9_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,5); END Word_insert_constants_9_5;
<*NOWARN*> PROCEDURE Word_insert_constants_9_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,6); END Word_insert_constants_9_6;
<*NOWARN*> PROCEDURE Word_insert_constants_9_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,7); END Word_insert_constants_9_7;
<*NOWARN*> PROCEDURE Word_insert_constants_9_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,8); END Word_insert_constants_9_8;
<*NOWARN*> PROCEDURE Word_insert_constants_9_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,9); END Word_insert_constants_9_9;
<*NOWARN*> PROCEDURE Word_insert_constants_9_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,10); END Word_insert_constants_9_10;
<*NOWARN*> PROCEDURE Word_insert_constants_9_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,11); END Word_insert_constants_9_11;
<*NOWARN*> PROCEDURE Word_insert_constants_9_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,12); END Word_insert_constants_9_12;
<*NOWARN*> PROCEDURE Word_insert_constants_9_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,13); END Word_insert_constants_9_13;
<*NOWARN*> PROCEDURE Word_insert_constants_9_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,14); END Word_insert_constants_9_14;
<*NOWARN*> PROCEDURE Word_insert_constants_9_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,15); END Word_insert_constants_9_15;
<*NOWARN*> PROCEDURE Word_insert_constants_9_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,16); END Word_insert_constants_9_16;
<*NOWARN*> PROCEDURE Word_insert_constants_9_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,17); END Word_insert_constants_9_17;
<*NOWARN*> PROCEDURE Word_insert_constants_9_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,18); END Word_insert_constants_9_18;
<*NOWARN*> PROCEDURE Word_insert_constants_9_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,19); END Word_insert_constants_9_19;
<*NOWARN*> PROCEDURE Word_insert_constants_9_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,20); END Word_insert_constants_9_20;
<*NOWARN*> PROCEDURE Word_insert_constants_9_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,21); END Word_insert_constants_9_21;
<*NOWARN*> PROCEDURE Word_insert_constants_9_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,22); END Word_insert_constants_9_22;
<*NOWARN*> PROCEDURE Word_insert_constants_9_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,23); END Word_insert_constants_9_23;
<*NOWARN*> PROCEDURE Word_insert_constants_9_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,24); END Word_insert_constants_9_24;
<*NOWARN*> PROCEDURE Word_insert_constants_9_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,25); END Word_insert_constants_9_25;
<*NOWARN*> PROCEDURE Word_insert_constants_9_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,26); END Word_insert_constants_9_26;
<*NOWARN*> PROCEDURE Word_insert_constants_9_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,27); END Word_insert_constants_9_27;
<*NOWARN*> PROCEDURE Word_insert_constants_9_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,28); END Word_insert_constants_9_28;
<*NOWARN*> PROCEDURE Word_insert_constants_9_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,29); END Word_insert_constants_9_29;
<*NOWARN*> PROCEDURE Word_insert_constants_9_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,30); END Word_insert_constants_9_30;
<*NOWARN*> PROCEDURE Word_insert_constants_9_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,31); END Word_insert_constants_9_31;
<*NOWARN*> PROCEDURE Word_insert_constants_9_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,32); END Word_insert_constants_9_32;
<*NOWARN*> PROCEDURE Word_insert_constants_9_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,33); END Word_insert_constants_9_33;
<*NOWARN*> PROCEDURE Word_insert_constants_9_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,34); END Word_insert_constants_9_34;
<*NOWARN*> PROCEDURE Word_insert_constants_9_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,35); END Word_insert_constants_9_35;
<*NOWARN*> PROCEDURE Word_insert_constants_9_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,36); END Word_insert_constants_9_36;
<*NOWARN*> PROCEDURE Word_insert_constants_9_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,37); END Word_insert_constants_9_37;
<*NOWARN*> PROCEDURE Word_insert_constants_9_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,38); END Word_insert_constants_9_38;
<*NOWARN*> PROCEDURE Word_insert_constants_9_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,39); END Word_insert_constants_9_39;
<*NOWARN*> PROCEDURE Word_insert_constants_9_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,40); END Word_insert_constants_9_40;
<*NOWARN*> PROCEDURE Word_insert_constants_9_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,41); END Word_insert_constants_9_41;
<*NOWARN*> PROCEDURE Word_insert_constants_9_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,42); END Word_insert_constants_9_42;
<*NOWARN*> PROCEDURE Word_insert_constants_9_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,43); END Word_insert_constants_9_43;
<*NOWARN*> PROCEDURE Word_insert_constants_9_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,44); END Word_insert_constants_9_44;
<*NOWARN*> PROCEDURE Word_insert_constants_9_45(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,45); END Word_insert_constants_9_45;
<*NOWARN*> PROCEDURE Word_insert_constants_9_46(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,46); END Word_insert_constants_9_46;
<*NOWARN*> PROCEDURE Word_insert_constants_9_47(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,47); END Word_insert_constants_9_47;
<*NOWARN*> PROCEDURE Word_insert_constants_9_48(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,48); END Word_insert_constants_9_48;
<*NOWARN*> PROCEDURE Word_insert_constants_9_49(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,49); END Word_insert_constants_9_49;
<*NOWARN*> PROCEDURE Word_insert_constants_9_50(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,50); END Word_insert_constants_9_50;
<*NOWARN*> PROCEDURE Word_insert_constants_9_51(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,51); END Word_insert_constants_9_51;
<*NOWARN*> PROCEDURE Word_insert_constants_9_52(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,52); END Word_insert_constants_9_52;
<*NOWARN*> PROCEDURE Word_insert_constants_9_53(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,53); END Word_insert_constants_9_53;
<*NOWARN*> PROCEDURE Word_insert_constants_9_54(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,54); END Word_insert_constants_9_54;
<*NOWARN*> PROCEDURE Word_insert_constants_9_55(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,9,55); END Word_insert_constants_9_55;
<*NOWARN*> PROCEDURE Word_insert_constants_10_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,0); END Word_insert_constants_10_0;
<*NOWARN*> PROCEDURE Word_insert_constants_10_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,1); END Word_insert_constants_10_1;
<*NOWARN*> PROCEDURE Word_insert_constants_10_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,2); END Word_insert_constants_10_2;
<*NOWARN*> PROCEDURE Word_insert_constants_10_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,3); END Word_insert_constants_10_3;
<*NOWARN*> PROCEDURE Word_insert_constants_10_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,4); END Word_insert_constants_10_4;
<*NOWARN*> PROCEDURE Word_insert_constants_10_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,5); END Word_insert_constants_10_5;
<*NOWARN*> PROCEDURE Word_insert_constants_10_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,6); END Word_insert_constants_10_6;
<*NOWARN*> PROCEDURE Word_insert_constants_10_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,7); END Word_insert_constants_10_7;
<*NOWARN*> PROCEDURE Word_insert_constants_10_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,8); END Word_insert_constants_10_8;
<*NOWARN*> PROCEDURE Word_insert_constants_10_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,9); END Word_insert_constants_10_9;
<*NOWARN*> PROCEDURE Word_insert_constants_10_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,10); END Word_insert_constants_10_10;
<*NOWARN*> PROCEDURE Word_insert_constants_10_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,11); END Word_insert_constants_10_11;
<*NOWARN*> PROCEDURE Word_insert_constants_10_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,12); END Word_insert_constants_10_12;
<*NOWARN*> PROCEDURE Word_insert_constants_10_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,13); END Word_insert_constants_10_13;
<*NOWARN*> PROCEDURE Word_insert_constants_10_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,14); END Word_insert_constants_10_14;
<*NOWARN*> PROCEDURE Word_insert_constants_10_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,15); END Word_insert_constants_10_15;
<*NOWARN*> PROCEDURE Word_insert_constants_10_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,16); END Word_insert_constants_10_16;
<*NOWARN*> PROCEDURE Word_insert_constants_10_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,17); END Word_insert_constants_10_17;
<*NOWARN*> PROCEDURE Word_insert_constants_10_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,18); END Word_insert_constants_10_18;
<*NOWARN*> PROCEDURE Word_insert_constants_10_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,19); END Word_insert_constants_10_19;
<*NOWARN*> PROCEDURE Word_insert_constants_10_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,20); END Word_insert_constants_10_20;
<*NOWARN*> PROCEDURE Word_insert_constants_10_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,21); END Word_insert_constants_10_21;
<*NOWARN*> PROCEDURE Word_insert_constants_10_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,22); END Word_insert_constants_10_22;
<*NOWARN*> PROCEDURE Word_insert_constants_10_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,23); END Word_insert_constants_10_23;
<*NOWARN*> PROCEDURE Word_insert_constants_10_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,24); END Word_insert_constants_10_24;
<*NOWARN*> PROCEDURE Word_insert_constants_10_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,25); END Word_insert_constants_10_25;
<*NOWARN*> PROCEDURE Word_insert_constants_10_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,26); END Word_insert_constants_10_26;
<*NOWARN*> PROCEDURE Word_insert_constants_10_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,27); END Word_insert_constants_10_27;
<*NOWARN*> PROCEDURE Word_insert_constants_10_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,28); END Word_insert_constants_10_28;
<*NOWARN*> PROCEDURE Word_insert_constants_10_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,29); END Word_insert_constants_10_29;
<*NOWARN*> PROCEDURE Word_insert_constants_10_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,30); END Word_insert_constants_10_30;
<*NOWARN*> PROCEDURE Word_insert_constants_10_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,31); END Word_insert_constants_10_31;
<*NOWARN*> PROCEDURE Word_insert_constants_10_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,32); END Word_insert_constants_10_32;
<*NOWARN*> PROCEDURE Word_insert_constants_10_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,33); END Word_insert_constants_10_33;
<*NOWARN*> PROCEDURE Word_insert_constants_10_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,34); END Word_insert_constants_10_34;
<*NOWARN*> PROCEDURE Word_insert_constants_10_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,35); END Word_insert_constants_10_35;
<*NOWARN*> PROCEDURE Word_insert_constants_10_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,36); END Word_insert_constants_10_36;
<*NOWARN*> PROCEDURE Word_insert_constants_10_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,37); END Word_insert_constants_10_37;
<*NOWARN*> PROCEDURE Word_insert_constants_10_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,38); END Word_insert_constants_10_38;
<*NOWARN*> PROCEDURE Word_insert_constants_10_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,39); END Word_insert_constants_10_39;
<*NOWARN*> PROCEDURE Word_insert_constants_10_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,40); END Word_insert_constants_10_40;
<*NOWARN*> PROCEDURE Word_insert_constants_10_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,41); END Word_insert_constants_10_41;
<*NOWARN*> PROCEDURE Word_insert_constants_10_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,42); END Word_insert_constants_10_42;
<*NOWARN*> PROCEDURE Word_insert_constants_10_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,43); END Word_insert_constants_10_43;
<*NOWARN*> PROCEDURE Word_insert_constants_10_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,44); END Word_insert_constants_10_44;
<*NOWARN*> PROCEDURE Word_insert_constants_10_45(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,45); END Word_insert_constants_10_45;
<*NOWARN*> PROCEDURE Word_insert_constants_10_46(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,46); END Word_insert_constants_10_46;
<*NOWARN*> PROCEDURE Word_insert_constants_10_47(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,47); END Word_insert_constants_10_47;
<*NOWARN*> PROCEDURE Word_insert_constants_10_48(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,48); END Word_insert_constants_10_48;
<*NOWARN*> PROCEDURE Word_insert_constants_10_49(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,49); END Word_insert_constants_10_49;
<*NOWARN*> PROCEDURE Word_insert_constants_10_50(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,50); END Word_insert_constants_10_50;
<*NOWARN*> PROCEDURE Word_insert_constants_10_51(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,51); END Word_insert_constants_10_51;
<*NOWARN*> PROCEDURE Word_insert_constants_10_52(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,52); END Word_insert_constants_10_52;
<*NOWARN*> PROCEDURE Word_insert_constants_10_53(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,53); END Word_insert_constants_10_53;
<*NOWARN*> PROCEDURE Word_insert_constants_10_54(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,10,54); END Word_insert_constants_10_54;
<*NOWARN*> PROCEDURE Word_insert_constants_11_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,0); END Word_insert_constants_11_0;
<*NOWARN*> PROCEDURE Word_insert_constants_11_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,1); END Word_insert_constants_11_1;
<*NOWARN*> PROCEDURE Word_insert_constants_11_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,2); END Word_insert_constants_11_2;
<*NOWARN*> PROCEDURE Word_insert_constants_11_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,3); END Word_insert_constants_11_3;
<*NOWARN*> PROCEDURE Word_insert_constants_11_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,4); END Word_insert_constants_11_4;
<*NOWARN*> PROCEDURE Word_insert_constants_11_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,5); END Word_insert_constants_11_5;
<*NOWARN*> PROCEDURE Word_insert_constants_11_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,6); END Word_insert_constants_11_6;
<*NOWARN*> PROCEDURE Word_insert_constants_11_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,7); END Word_insert_constants_11_7;
<*NOWARN*> PROCEDURE Word_insert_constants_11_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,8); END Word_insert_constants_11_8;
<*NOWARN*> PROCEDURE Word_insert_constants_11_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,9); END Word_insert_constants_11_9;
<*NOWARN*> PROCEDURE Word_insert_constants_11_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,10); END Word_insert_constants_11_10;
<*NOWARN*> PROCEDURE Word_insert_constants_11_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,11); END Word_insert_constants_11_11;
<*NOWARN*> PROCEDURE Word_insert_constants_11_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,12); END Word_insert_constants_11_12;
<*NOWARN*> PROCEDURE Word_insert_constants_11_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,13); END Word_insert_constants_11_13;
<*NOWARN*> PROCEDURE Word_insert_constants_11_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,14); END Word_insert_constants_11_14;
<*NOWARN*> PROCEDURE Word_insert_constants_11_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,15); END Word_insert_constants_11_15;
<*NOWARN*> PROCEDURE Word_insert_constants_11_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,16); END Word_insert_constants_11_16;
<*NOWARN*> PROCEDURE Word_insert_constants_11_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,17); END Word_insert_constants_11_17;
<*NOWARN*> PROCEDURE Word_insert_constants_11_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,18); END Word_insert_constants_11_18;
<*NOWARN*> PROCEDURE Word_insert_constants_11_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,19); END Word_insert_constants_11_19;
<*NOWARN*> PROCEDURE Word_insert_constants_11_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,20); END Word_insert_constants_11_20;
<*NOWARN*> PROCEDURE Word_insert_constants_11_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,21); END Word_insert_constants_11_21;
<*NOWARN*> PROCEDURE Word_insert_constants_11_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,22); END Word_insert_constants_11_22;
<*NOWARN*> PROCEDURE Word_insert_constants_11_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,23); END Word_insert_constants_11_23;
<*NOWARN*> PROCEDURE Word_insert_constants_11_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,24); END Word_insert_constants_11_24;
<*NOWARN*> PROCEDURE Word_insert_constants_11_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,25); END Word_insert_constants_11_25;
<*NOWARN*> PROCEDURE Word_insert_constants_11_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,26); END Word_insert_constants_11_26;
<*NOWARN*> PROCEDURE Word_insert_constants_11_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,27); END Word_insert_constants_11_27;
<*NOWARN*> PROCEDURE Word_insert_constants_11_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,28); END Word_insert_constants_11_28;
<*NOWARN*> PROCEDURE Word_insert_constants_11_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,29); END Word_insert_constants_11_29;
<*NOWARN*> PROCEDURE Word_insert_constants_11_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,30); END Word_insert_constants_11_30;
<*NOWARN*> PROCEDURE Word_insert_constants_11_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,31); END Word_insert_constants_11_31;
<*NOWARN*> PROCEDURE Word_insert_constants_11_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,32); END Word_insert_constants_11_32;
<*NOWARN*> PROCEDURE Word_insert_constants_11_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,33); END Word_insert_constants_11_33;
<*NOWARN*> PROCEDURE Word_insert_constants_11_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,34); END Word_insert_constants_11_34;
<*NOWARN*> PROCEDURE Word_insert_constants_11_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,35); END Word_insert_constants_11_35;
<*NOWARN*> PROCEDURE Word_insert_constants_11_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,36); END Word_insert_constants_11_36;
<*NOWARN*> PROCEDURE Word_insert_constants_11_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,37); END Word_insert_constants_11_37;
<*NOWARN*> PROCEDURE Word_insert_constants_11_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,38); END Word_insert_constants_11_38;
<*NOWARN*> PROCEDURE Word_insert_constants_11_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,39); END Word_insert_constants_11_39;
<*NOWARN*> PROCEDURE Word_insert_constants_11_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,40); END Word_insert_constants_11_40;
<*NOWARN*> PROCEDURE Word_insert_constants_11_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,41); END Word_insert_constants_11_41;
<*NOWARN*> PROCEDURE Word_insert_constants_11_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,42); END Word_insert_constants_11_42;
<*NOWARN*> PROCEDURE Word_insert_constants_11_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,43); END Word_insert_constants_11_43;
<*NOWARN*> PROCEDURE Word_insert_constants_11_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,44); END Word_insert_constants_11_44;
<*NOWARN*> PROCEDURE Word_insert_constants_11_45(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,45); END Word_insert_constants_11_45;
<*NOWARN*> PROCEDURE Word_insert_constants_11_46(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,46); END Word_insert_constants_11_46;
<*NOWARN*> PROCEDURE Word_insert_constants_11_47(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,47); END Word_insert_constants_11_47;
<*NOWARN*> PROCEDURE Word_insert_constants_11_48(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,48); END Word_insert_constants_11_48;
<*NOWARN*> PROCEDURE Word_insert_constants_11_49(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,49); END Word_insert_constants_11_49;
<*NOWARN*> PROCEDURE Word_insert_constants_11_50(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,50); END Word_insert_constants_11_50;
<*NOWARN*> PROCEDURE Word_insert_constants_11_51(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,51); END Word_insert_constants_11_51;
<*NOWARN*> PROCEDURE Word_insert_constants_11_52(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,52); END Word_insert_constants_11_52;
<*NOWARN*> PROCEDURE Word_insert_constants_11_53(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,11,53); END Word_insert_constants_11_53;
<*NOWARN*> PROCEDURE Word_insert_constants_12_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,0); END Word_insert_constants_12_0;
<*NOWARN*> PROCEDURE Word_insert_constants_12_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,1); END Word_insert_constants_12_1;
<*NOWARN*> PROCEDURE Word_insert_constants_12_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,2); END Word_insert_constants_12_2;
<*NOWARN*> PROCEDURE Word_insert_constants_12_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,3); END Word_insert_constants_12_3;
<*NOWARN*> PROCEDURE Word_insert_constants_12_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,4); END Word_insert_constants_12_4;
<*NOWARN*> PROCEDURE Word_insert_constants_12_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,5); END Word_insert_constants_12_5;
<*NOWARN*> PROCEDURE Word_insert_constants_12_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,6); END Word_insert_constants_12_6;
<*NOWARN*> PROCEDURE Word_insert_constants_12_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,7); END Word_insert_constants_12_7;
<*NOWARN*> PROCEDURE Word_insert_constants_12_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,8); END Word_insert_constants_12_8;
<*NOWARN*> PROCEDURE Word_insert_constants_12_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,9); END Word_insert_constants_12_9;
<*NOWARN*> PROCEDURE Word_insert_constants_12_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,10); END Word_insert_constants_12_10;
<*NOWARN*> PROCEDURE Word_insert_constants_12_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,11); END Word_insert_constants_12_11;
<*NOWARN*> PROCEDURE Word_insert_constants_12_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,12); END Word_insert_constants_12_12;
<*NOWARN*> PROCEDURE Word_insert_constants_12_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,13); END Word_insert_constants_12_13;
<*NOWARN*> PROCEDURE Word_insert_constants_12_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,14); END Word_insert_constants_12_14;
<*NOWARN*> PROCEDURE Word_insert_constants_12_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,15); END Word_insert_constants_12_15;
<*NOWARN*> PROCEDURE Word_insert_constants_12_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,16); END Word_insert_constants_12_16;
<*NOWARN*> PROCEDURE Word_insert_constants_12_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,17); END Word_insert_constants_12_17;
<*NOWARN*> PROCEDURE Word_insert_constants_12_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,18); END Word_insert_constants_12_18;
<*NOWARN*> PROCEDURE Word_insert_constants_12_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,19); END Word_insert_constants_12_19;
<*NOWARN*> PROCEDURE Word_insert_constants_12_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,20); END Word_insert_constants_12_20;
<*NOWARN*> PROCEDURE Word_insert_constants_12_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,21); END Word_insert_constants_12_21;
<*NOWARN*> PROCEDURE Word_insert_constants_12_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,22); END Word_insert_constants_12_22;
<*NOWARN*> PROCEDURE Word_insert_constants_12_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,23); END Word_insert_constants_12_23;
<*NOWARN*> PROCEDURE Word_insert_constants_12_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,24); END Word_insert_constants_12_24;
<*NOWARN*> PROCEDURE Word_insert_constants_12_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,25); END Word_insert_constants_12_25;
<*NOWARN*> PROCEDURE Word_insert_constants_12_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,26); END Word_insert_constants_12_26;
<*NOWARN*> PROCEDURE Word_insert_constants_12_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,27); END Word_insert_constants_12_27;
<*NOWARN*> PROCEDURE Word_insert_constants_12_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,28); END Word_insert_constants_12_28;
<*NOWARN*> PROCEDURE Word_insert_constants_12_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,29); END Word_insert_constants_12_29;
<*NOWARN*> PROCEDURE Word_insert_constants_12_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,30); END Word_insert_constants_12_30;
<*NOWARN*> PROCEDURE Word_insert_constants_12_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,31); END Word_insert_constants_12_31;
<*NOWARN*> PROCEDURE Word_insert_constants_12_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,32); END Word_insert_constants_12_32;
<*NOWARN*> PROCEDURE Word_insert_constants_12_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,33); END Word_insert_constants_12_33;
<*NOWARN*> PROCEDURE Word_insert_constants_12_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,34); END Word_insert_constants_12_34;
<*NOWARN*> PROCEDURE Word_insert_constants_12_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,35); END Word_insert_constants_12_35;
<*NOWARN*> PROCEDURE Word_insert_constants_12_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,36); END Word_insert_constants_12_36;
<*NOWARN*> PROCEDURE Word_insert_constants_12_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,37); END Word_insert_constants_12_37;
<*NOWARN*> PROCEDURE Word_insert_constants_12_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,38); END Word_insert_constants_12_38;
<*NOWARN*> PROCEDURE Word_insert_constants_12_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,39); END Word_insert_constants_12_39;
<*NOWARN*> PROCEDURE Word_insert_constants_12_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,40); END Word_insert_constants_12_40;
<*NOWARN*> PROCEDURE Word_insert_constants_12_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,41); END Word_insert_constants_12_41;
<*NOWARN*> PROCEDURE Word_insert_constants_12_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,42); END Word_insert_constants_12_42;
<*NOWARN*> PROCEDURE Word_insert_constants_12_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,43); END Word_insert_constants_12_43;
<*NOWARN*> PROCEDURE Word_insert_constants_12_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,44); END Word_insert_constants_12_44;
<*NOWARN*> PROCEDURE Word_insert_constants_12_45(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,45); END Word_insert_constants_12_45;
<*NOWARN*> PROCEDURE Word_insert_constants_12_46(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,46); END Word_insert_constants_12_46;
<*NOWARN*> PROCEDURE Word_insert_constants_12_47(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,47); END Word_insert_constants_12_47;
<*NOWARN*> PROCEDURE Word_insert_constants_12_48(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,48); END Word_insert_constants_12_48;
<*NOWARN*> PROCEDURE Word_insert_constants_12_49(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,49); END Word_insert_constants_12_49;
<*NOWARN*> PROCEDURE Word_insert_constants_12_50(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,50); END Word_insert_constants_12_50;
<*NOWARN*> PROCEDURE Word_insert_constants_12_51(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,51); END Word_insert_constants_12_51;
<*NOWARN*> PROCEDURE Word_insert_constants_12_52(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,12,52); END Word_insert_constants_12_52;
<*NOWARN*> PROCEDURE Word_insert_constants_13_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,0); END Word_insert_constants_13_0;
<*NOWARN*> PROCEDURE Word_insert_constants_13_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,1); END Word_insert_constants_13_1;
<*NOWARN*> PROCEDURE Word_insert_constants_13_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,2); END Word_insert_constants_13_2;
<*NOWARN*> PROCEDURE Word_insert_constants_13_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,3); END Word_insert_constants_13_3;
<*NOWARN*> PROCEDURE Word_insert_constants_13_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,4); END Word_insert_constants_13_4;
<*NOWARN*> PROCEDURE Word_insert_constants_13_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,5); END Word_insert_constants_13_5;
<*NOWARN*> PROCEDURE Word_insert_constants_13_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,6); END Word_insert_constants_13_6;
<*NOWARN*> PROCEDURE Word_insert_constants_13_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,7); END Word_insert_constants_13_7;
<*NOWARN*> PROCEDURE Word_insert_constants_13_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,8); END Word_insert_constants_13_8;
<*NOWARN*> PROCEDURE Word_insert_constants_13_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,9); END Word_insert_constants_13_9;
<*NOWARN*> PROCEDURE Word_insert_constants_13_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,10); END Word_insert_constants_13_10;
<*NOWARN*> PROCEDURE Word_insert_constants_13_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,11); END Word_insert_constants_13_11;
<*NOWARN*> PROCEDURE Word_insert_constants_13_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,12); END Word_insert_constants_13_12;
<*NOWARN*> PROCEDURE Word_insert_constants_13_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,13); END Word_insert_constants_13_13;
<*NOWARN*> PROCEDURE Word_insert_constants_13_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,14); END Word_insert_constants_13_14;
<*NOWARN*> PROCEDURE Word_insert_constants_13_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,15); END Word_insert_constants_13_15;
<*NOWARN*> PROCEDURE Word_insert_constants_13_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,16); END Word_insert_constants_13_16;
<*NOWARN*> PROCEDURE Word_insert_constants_13_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,17); END Word_insert_constants_13_17;
<*NOWARN*> PROCEDURE Word_insert_constants_13_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,18); END Word_insert_constants_13_18;
<*NOWARN*> PROCEDURE Word_insert_constants_13_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,19); END Word_insert_constants_13_19;
<*NOWARN*> PROCEDURE Word_insert_constants_13_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,20); END Word_insert_constants_13_20;
<*NOWARN*> PROCEDURE Word_insert_constants_13_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,21); END Word_insert_constants_13_21;
<*NOWARN*> PROCEDURE Word_insert_constants_13_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,22); END Word_insert_constants_13_22;
<*NOWARN*> PROCEDURE Word_insert_constants_13_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,23); END Word_insert_constants_13_23;
<*NOWARN*> PROCEDURE Word_insert_constants_13_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,24); END Word_insert_constants_13_24;
<*NOWARN*> PROCEDURE Word_insert_constants_13_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,25); END Word_insert_constants_13_25;
<*NOWARN*> PROCEDURE Word_insert_constants_13_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,26); END Word_insert_constants_13_26;
<*NOWARN*> PROCEDURE Word_insert_constants_13_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,27); END Word_insert_constants_13_27;
<*NOWARN*> PROCEDURE Word_insert_constants_13_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,28); END Word_insert_constants_13_28;
<*NOWARN*> PROCEDURE Word_insert_constants_13_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,29); END Word_insert_constants_13_29;
<*NOWARN*> PROCEDURE Word_insert_constants_13_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,30); END Word_insert_constants_13_30;
<*NOWARN*> PROCEDURE Word_insert_constants_13_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,31); END Word_insert_constants_13_31;
<*NOWARN*> PROCEDURE Word_insert_constants_13_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,32); END Word_insert_constants_13_32;
<*NOWARN*> PROCEDURE Word_insert_constants_13_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,33); END Word_insert_constants_13_33;
<*NOWARN*> PROCEDURE Word_insert_constants_13_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,34); END Word_insert_constants_13_34;
<*NOWARN*> PROCEDURE Word_insert_constants_13_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,35); END Word_insert_constants_13_35;
<*NOWARN*> PROCEDURE Word_insert_constants_13_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,36); END Word_insert_constants_13_36;
<*NOWARN*> PROCEDURE Word_insert_constants_13_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,37); END Word_insert_constants_13_37;
<*NOWARN*> PROCEDURE Word_insert_constants_13_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,38); END Word_insert_constants_13_38;
<*NOWARN*> PROCEDURE Word_insert_constants_13_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,39); END Word_insert_constants_13_39;
<*NOWARN*> PROCEDURE Word_insert_constants_13_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,40); END Word_insert_constants_13_40;
<*NOWARN*> PROCEDURE Word_insert_constants_13_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,41); END Word_insert_constants_13_41;
<*NOWARN*> PROCEDURE Word_insert_constants_13_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,42); END Word_insert_constants_13_42;
<*NOWARN*> PROCEDURE Word_insert_constants_13_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,43); END Word_insert_constants_13_43;
<*NOWARN*> PROCEDURE Word_insert_constants_13_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,44); END Word_insert_constants_13_44;
<*NOWARN*> PROCEDURE Word_insert_constants_13_45(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,45); END Word_insert_constants_13_45;
<*NOWARN*> PROCEDURE Word_insert_constants_13_46(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,46); END Word_insert_constants_13_46;
<*NOWARN*> PROCEDURE Word_insert_constants_13_47(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,47); END Word_insert_constants_13_47;
<*NOWARN*> PROCEDURE Word_insert_constants_13_48(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,48); END Word_insert_constants_13_48;
<*NOWARN*> PROCEDURE Word_insert_constants_13_49(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,49); END Word_insert_constants_13_49;
<*NOWARN*> PROCEDURE Word_insert_constants_13_50(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,50); END Word_insert_constants_13_50;
<*NOWARN*> PROCEDURE Word_insert_constants_13_51(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,13,51); END Word_insert_constants_13_51;
<*NOWARN*> PROCEDURE Word_insert_constants_14_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,0); END Word_insert_constants_14_0;
<*NOWARN*> PROCEDURE Word_insert_constants_14_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,1); END Word_insert_constants_14_1;
<*NOWARN*> PROCEDURE Word_insert_constants_14_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,2); END Word_insert_constants_14_2;
<*NOWARN*> PROCEDURE Word_insert_constants_14_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,3); END Word_insert_constants_14_3;
<*NOWARN*> PROCEDURE Word_insert_constants_14_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,4); END Word_insert_constants_14_4;
<*NOWARN*> PROCEDURE Word_insert_constants_14_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,5); END Word_insert_constants_14_5;
<*NOWARN*> PROCEDURE Word_insert_constants_14_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,6); END Word_insert_constants_14_6;
<*NOWARN*> PROCEDURE Word_insert_constants_14_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,7); END Word_insert_constants_14_7;
<*NOWARN*> PROCEDURE Word_insert_constants_14_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,8); END Word_insert_constants_14_8;
<*NOWARN*> PROCEDURE Word_insert_constants_14_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,9); END Word_insert_constants_14_9;
<*NOWARN*> PROCEDURE Word_insert_constants_14_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,10); END Word_insert_constants_14_10;
<*NOWARN*> PROCEDURE Word_insert_constants_14_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,11); END Word_insert_constants_14_11;
<*NOWARN*> PROCEDURE Word_insert_constants_14_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,12); END Word_insert_constants_14_12;
<*NOWARN*> PROCEDURE Word_insert_constants_14_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,13); END Word_insert_constants_14_13;
<*NOWARN*> PROCEDURE Word_insert_constants_14_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,14); END Word_insert_constants_14_14;
<*NOWARN*> PROCEDURE Word_insert_constants_14_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,15); END Word_insert_constants_14_15;
<*NOWARN*> PROCEDURE Word_insert_constants_14_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,16); END Word_insert_constants_14_16;
<*NOWARN*> PROCEDURE Word_insert_constants_14_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,17); END Word_insert_constants_14_17;
<*NOWARN*> PROCEDURE Word_insert_constants_14_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,18); END Word_insert_constants_14_18;
<*NOWARN*> PROCEDURE Word_insert_constants_14_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,19); END Word_insert_constants_14_19;
<*NOWARN*> PROCEDURE Word_insert_constants_14_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,20); END Word_insert_constants_14_20;
<*NOWARN*> PROCEDURE Word_insert_constants_14_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,21); END Word_insert_constants_14_21;
<*NOWARN*> PROCEDURE Word_insert_constants_14_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,22); END Word_insert_constants_14_22;
<*NOWARN*> PROCEDURE Word_insert_constants_14_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,23); END Word_insert_constants_14_23;
<*NOWARN*> PROCEDURE Word_insert_constants_14_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,24); END Word_insert_constants_14_24;
<*NOWARN*> PROCEDURE Word_insert_constants_14_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,25); END Word_insert_constants_14_25;
<*NOWARN*> PROCEDURE Word_insert_constants_14_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,26); END Word_insert_constants_14_26;
<*NOWARN*> PROCEDURE Word_insert_constants_14_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,27); END Word_insert_constants_14_27;
<*NOWARN*> PROCEDURE Word_insert_constants_14_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,28); END Word_insert_constants_14_28;
<*NOWARN*> PROCEDURE Word_insert_constants_14_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,29); END Word_insert_constants_14_29;
<*NOWARN*> PROCEDURE Word_insert_constants_14_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,30); END Word_insert_constants_14_30;
<*NOWARN*> PROCEDURE Word_insert_constants_14_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,31); END Word_insert_constants_14_31;
<*NOWARN*> PROCEDURE Word_insert_constants_14_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,32); END Word_insert_constants_14_32;
<*NOWARN*> PROCEDURE Word_insert_constants_14_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,33); END Word_insert_constants_14_33;
<*NOWARN*> PROCEDURE Word_insert_constants_14_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,34); END Word_insert_constants_14_34;
<*NOWARN*> PROCEDURE Word_insert_constants_14_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,35); END Word_insert_constants_14_35;
<*NOWARN*> PROCEDURE Word_insert_constants_14_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,36); END Word_insert_constants_14_36;
<*NOWARN*> PROCEDURE Word_insert_constants_14_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,37); END Word_insert_constants_14_37;
<*NOWARN*> PROCEDURE Word_insert_constants_14_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,38); END Word_insert_constants_14_38;
<*NOWARN*> PROCEDURE Word_insert_constants_14_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,39); END Word_insert_constants_14_39;
<*NOWARN*> PROCEDURE Word_insert_constants_14_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,40); END Word_insert_constants_14_40;
<*NOWARN*> PROCEDURE Word_insert_constants_14_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,41); END Word_insert_constants_14_41;
<*NOWARN*> PROCEDURE Word_insert_constants_14_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,42); END Word_insert_constants_14_42;
<*NOWARN*> PROCEDURE Word_insert_constants_14_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,43); END Word_insert_constants_14_43;
<*NOWARN*> PROCEDURE Word_insert_constants_14_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,44); END Word_insert_constants_14_44;
<*NOWARN*> PROCEDURE Word_insert_constants_14_45(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,45); END Word_insert_constants_14_45;
<*NOWARN*> PROCEDURE Word_insert_constants_14_46(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,46); END Word_insert_constants_14_46;
<*NOWARN*> PROCEDURE Word_insert_constants_14_47(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,47); END Word_insert_constants_14_47;
<*NOWARN*> PROCEDURE Word_insert_constants_14_48(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,48); END Word_insert_constants_14_48;
<*NOWARN*> PROCEDURE Word_insert_constants_14_49(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,49); END Word_insert_constants_14_49;
<*NOWARN*> PROCEDURE Word_insert_constants_14_50(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,14,50); END Word_insert_constants_14_50;
<*NOWARN*> PROCEDURE Word_insert_constants_15_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,0); END Word_insert_constants_15_0;
<*NOWARN*> PROCEDURE Word_insert_constants_15_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,1); END Word_insert_constants_15_1;
<*NOWARN*> PROCEDURE Word_insert_constants_15_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,2); END Word_insert_constants_15_2;
<*NOWARN*> PROCEDURE Word_insert_constants_15_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,3); END Word_insert_constants_15_3;
<*NOWARN*> PROCEDURE Word_insert_constants_15_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,4); END Word_insert_constants_15_4;
<*NOWARN*> PROCEDURE Word_insert_constants_15_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,5); END Word_insert_constants_15_5;
<*NOWARN*> PROCEDURE Word_insert_constants_15_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,6); END Word_insert_constants_15_6;
<*NOWARN*> PROCEDURE Word_insert_constants_15_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,7); END Word_insert_constants_15_7;
<*NOWARN*> PROCEDURE Word_insert_constants_15_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,8); END Word_insert_constants_15_8;
<*NOWARN*> PROCEDURE Word_insert_constants_15_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,9); END Word_insert_constants_15_9;
<*NOWARN*> PROCEDURE Word_insert_constants_15_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,10); END Word_insert_constants_15_10;
<*NOWARN*> PROCEDURE Word_insert_constants_15_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,11); END Word_insert_constants_15_11;
<*NOWARN*> PROCEDURE Word_insert_constants_15_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,12); END Word_insert_constants_15_12;
<*NOWARN*> PROCEDURE Word_insert_constants_15_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,13); END Word_insert_constants_15_13;
<*NOWARN*> PROCEDURE Word_insert_constants_15_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,14); END Word_insert_constants_15_14;
<*NOWARN*> PROCEDURE Word_insert_constants_15_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,15); END Word_insert_constants_15_15;
<*NOWARN*> PROCEDURE Word_insert_constants_15_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,16); END Word_insert_constants_15_16;
<*NOWARN*> PROCEDURE Word_insert_constants_15_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,17); END Word_insert_constants_15_17;
<*NOWARN*> PROCEDURE Word_insert_constants_15_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,18); END Word_insert_constants_15_18;
<*NOWARN*> PROCEDURE Word_insert_constants_15_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,19); END Word_insert_constants_15_19;
<*NOWARN*> PROCEDURE Word_insert_constants_15_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,20); END Word_insert_constants_15_20;
<*NOWARN*> PROCEDURE Word_insert_constants_15_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,21); END Word_insert_constants_15_21;
<*NOWARN*> PROCEDURE Word_insert_constants_15_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,22); END Word_insert_constants_15_22;
<*NOWARN*> PROCEDURE Word_insert_constants_15_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,23); END Word_insert_constants_15_23;
<*NOWARN*> PROCEDURE Word_insert_constants_15_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,24); END Word_insert_constants_15_24;
<*NOWARN*> PROCEDURE Word_insert_constants_15_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,25); END Word_insert_constants_15_25;
<*NOWARN*> PROCEDURE Word_insert_constants_15_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,26); END Word_insert_constants_15_26;
<*NOWARN*> PROCEDURE Word_insert_constants_15_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,27); END Word_insert_constants_15_27;
<*NOWARN*> PROCEDURE Word_insert_constants_15_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,28); END Word_insert_constants_15_28;
<*NOWARN*> PROCEDURE Word_insert_constants_15_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,29); END Word_insert_constants_15_29;
<*NOWARN*> PROCEDURE Word_insert_constants_15_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,30); END Word_insert_constants_15_30;
<*NOWARN*> PROCEDURE Word_insert_constants_15_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,31); END Word_insert_constants_15_31;
<*NOWARN*> PROCEDURE Word_insert_constants_15_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,32); END Word_insert_constants_15_32;
<*NOWARN*> PROCEDURE Word_insert_constants_15_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,33); END Word_insert_constants_15_33;
<*NOWARN*> PROCEDURE Word_insert_constants_15_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,34); END Word_insert_constants_15_34;
<*NOWARN*> PROCEDURE Word_insert_constants_15_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,35); END Word_insert_constants_15_35;
<*NOWARN*> PROCEDURE Word_insert_constants_15_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,36); END Word_insert_constants_15_36;
<*NOWARN*> PROCEDURE Word_insert_constants_15_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,37); END Word_insert_constants_15_37;
<*NOWARN*> PROCEDURE Word_insert_constants_15_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,38); END Word_insert_constants_15_38;
<*NOWARN*> PROCEDURE Word_insert_constants_15_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,39); END Word_insert_constants_15_39;
<*NOWARN*> PROCEDURE Word_insert_constants_15_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,40); END Word_insert_constants_15_40;
<*NOWARN*> PROCEDURE Word_insert_constants_15_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,41); END Word_insert_constants_15_41;
<*NOWARN*> PROCEDURE Word_insert_constants_15_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,42); END Word_insert_constants_15_42;
<*NOWARN*> PROCEDURE Word_insert_constants_15_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,43); END Word_insert_constants_15_43;
<*NOWARN*> PROCEDURE Word_insert_constants_15_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,44); END Word_insert_constants_15_44;
<*NOWARN*> PROCEDURE Word_insert_constants_15_45(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,45); END Word_insert_constants_15_45;
<*NOWARN*> PROCEDURE Word_insert_constants_15_46(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,46); END Word_insert_constants_15_46;
<*NOWARN*> PROCEDURE Word_insert_constants_15_47(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,47); END Word_insert_constants_15_47;
<*NOWARN*> PROCEDURE Word_insert_constants_15_48(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,48); END Word_insert_constants_15_48;
<*NOWARN*> PROCEDURE Word_insert_constants_15_49(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,15,49); END Word_insert_constants_15_49;
<*NOWARN*> PROCEDURE Word_insert_constants_16_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,0); END Word_insert_constants_16_0;
<*NOWARN*> PROCEDURE Word_insert_constants_16_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,1); END Word_insert_constants_16_1;
<*NOWARN*> PROCEDURE Word_insert_constants_16_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,2); END Word_insert_constants_16_2;
<*NOWARN*> PROCEDURE Word_insert_constants_16_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,3); END Word_insert_constants_16_3;
<*NOWARN*> PROCEDURE Word_insert_constants_16_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,4); END Word_insert_constants_16_4;
<*NOWARN*> PROCEDURE Word_insert_constants_16_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,5); END Word_insert_constants_16_5;
<*NOWARN*> PROCEDURE Word_insert_constants_16_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,6); END Word_insert_constants_16_6;
<*NOWARN*> PROCEDURE Word_insert_constants_16_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,7); END Word_insert_constants_16_7;
<*NOWARN*> PROCEDURE Word_insert_constants_16_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,8); END Word_insert_constants_16_8;
<*NOWARN*> PROCEDURE Word_insert_constants_16_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,9); END Word_insert_constants_16_9;
<*NOWARN*> PROCEDURE Word_insert_constants_16_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,10); END Word_insert_constants_16_10;
<*NOWARN*> PROCEDURE Word_insert_constants_16_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,11); END Word_insert_constants_16_11;
<*NOWARN*> PROCEDURE Word_insert_constants_16_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,12); END Word_insert_constants_16_12;
<*NOWARN*> PROCEDURE Word_insert_constants_16_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,13); END Word_insert_constants_16_13;
<*NOWARN*> PROCEDURE Word_insert_constants_16_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,14); END Word_insert_constants_16_14;
<*NOWARN*> PROCEDURE Word_insert_constants_16_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,15); END Word_insert_constants_16_15;
<*NOWARN*> PROCEDURE Word_insert_constants_16_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,16); END Word_insert_constants_16_16;
<*NOWARN*> PROCEDURE Word_insert_constants_16_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,17); END Word_insert_constants_16_17;
<*NOWARN*> PROCEDURE Word_insert_constants_16_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,18); END Word_insert_constants_16_18;
<*NOWARN*> PROCEDURE Word_insert_constants_16_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,19); END Word_insert_constants_16_19;
<*NOWARN*> PROCEDURE Word_insert_constants_16_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,20); END Word_insert_constants_16_20;
<*NOWARN*> PROCEDURE Word_insert_constants_16_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,21); END Word_insert_constants_16_21;
<*NOWARN*> PROCEDURE Word_insert_constants_16_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,22); END Word_insert_constants_16_22;
<*NOWARN*> PROCEDURE Word_insert_constants_16_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,23); END Word_insert_constants_16_23;
<*NOWARN*> PROCEDURE Word_insert_constants_16_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,24); END Word_insert_constants_16_24;
<*NOWARN*> PROCEDURE Word_insert_constants_16_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,25); END Word_insert_constants_16_25;
<*NOWARN*> PROCEDURE Word_insert_constants_16_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,26); END Word_insert_constants_16_26;
<*NOWARN*> PROCEDURE Word_insert_constants_16_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,27); END Word_insert_constants_16_27;
<*NOWARN*> PROCEDURE Word_insert_constants_16_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,28); END Word_insert_constants_16_28;
<*NOWARN*> PROCEDURE Word_insert_constants_16_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,29); END Word_insert_constants_16_29;
<*NOWARN*> PROCEDURE Word_insert_constants_16_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,30); END Word_insert_constants_16_30;
<*NOWARN*> PROCEDURE Word_insert_constants_16_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,31); END Word_insert_constants_16_31;
<*NOWARN*> PROCEDURE Word_insert_constants_16_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,32); END Word_insert_constants_16_32;
<*NOWARN*> PROCEDURE Word_insert_constants_16_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,33); END Word_insert_constants_16_33;
<*NOWARN*> PROCEDURE Word_insert_constants_16_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,34); END Word_insert_constants_16_34;
<*NOWARN*> PROCEDURE Word_insert_constants_16_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,35); END Word_insert_constants_16_35;
<*NOWARN*> PROCEDURE Word_insert_constants_16_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,36); END Word_insert_constants_16_36;
<*NOWARN*> PROCEDURE Word_insert_constants_16_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,37); END Word_insert_constants_16_37;
<*NOWARN*> PROCEDURE Word_insert_constants_16_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,38); END Word_insert_constants_16_38;
<*NOWARN*> PROCEDURE Word_insert_constants_16_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,39); END Word_insert_constants_16_39;
<*NOWARN*> PROCEDURE Word_insert_constants_16_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,40); END Word_insert_constants_16_40;
<*NOWARN*> PROCEDURE Word_insert_constants_16_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,41); END Word_insert_constants_16_41;
<*NOWARN*> PROCEDURE Word_insert_constants_16_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,42); END Word_insert_constants_16_42;
<*NOWARN*> PROCEDURE Word_insert_constants_16_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,43); END Word_insert_constants_16_43;
<*NOWARN*> PROCEDURE Word_insert_constants_16_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,44); END Word_insert_constants_16_44;
<*NOWARN*> PROCEDURE Word_insert_constants_16_45(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,45); END Word_insert_constants_16_45;
<*NOWARN*> PROCEDURE Word_insert_constants_16_46(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,46); END Word_insert_constants_16_46;
<*NOWARN*> PROCEDURE Word_insert_constants_16_47(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,47); END Word_insert_constants_16_47;
<*NOWARN*> PROCEDURE Word_insert_constants_16_48(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,16,48); END Word_insert_constants_16_48;
<*NOWARN*> PROCEDURE Word_insert_constants_17_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,0); END Word_insert_constants_17_0;
<*NOWARN*> PROCEDURE Word_insert_constants_17_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,1); END Word_insert_constants_17_1;
<*NOWARN*> PROCEDURE Word_insert_constants_17_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,2); END Word_insert_constants_17_2;
<*NOWARN*> PROCEDURE Word_insert_constants_17_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,3); END Word_insert_constants_17_3;
<*NOWARN*> PROCEDURE Word_insert_constants_17_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,4); END Word_insert_constants_17_4;
<*NOWARN*> PROCEDURE Word_insert_constants_17_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,5); END Word_insert_constants_17_5;
<*NOWARN*> PROCEDURE Word_insert_constants_17_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,6); END Word_insert_constants_17_6;
<*NOWARN*> PROCEDURE Word_insert_constants_17_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,7); END Word_insert_constants_17_7;
<*NOWARN*> PROCEDURE Word_insert_constants_17_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,8); END Word_insert_constants_17_8;
<*NOWARN*> PROCEDURE Word_insert_constants_17_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,9); END Word_insert_constants_17_9;
<*NOWARN*> PROCEDURE Word_insert_constants_17_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,10); END Word_insert_constants_17_10;
<*NOWARN*> PROCEDURE Word_insert_constants_17_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,11); END Word_insert_constants_17_11;
<*NOWARN*> PROCEDURE Word_insert_constants_17_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,12); END Word_insert_constants_17_12;
<*NOWARN*> PROCEDURE Word_insert_constants_17_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,13); END Word_insert_constants_17_13;
<*NOWARN*> PROCEDURE Word_insert_constants_17_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,14); END Word_insert_constants_17_14;
<*NOWARN*> PROCEDURE Word_insert_constants_17_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,15); END Word_insert_constants_17_15;
<*NOWARN*> PROCEDURE Word_insert_constants_17_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,16); END Word_insert_constants_17_16;
<*NOWARN*> PROCEDURE Word_insert_constants_17_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,17); END Word_insert_constants_17_17;
<*NOWARN*> PROCEDURE Word_insert_constants_17_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,18); END Word_insert_constants_17_18;
<*NOWARN*> PROCEDURE Word_insert_constants_17_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,19); END Word_insert_constants_17_19;
<*NOWARN*> PROCEDURE Word_insert_constants_17_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,20); END Word_insert_constants_17_20;
<*NOWARN*> PROCEDURE Word_insert_constants_17_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,21); END Word_insert_constants_17_21;
<*NOWARN*> PROCEDURE Word_insert_constants_17_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,22); END Word_insert_constants_17_22;
<*NOWARN*> PROCEDURE Word_insert_constants_17_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,23); END Word_insert_constants_17_23;
<*NOWARN*> PROCEDURE Word_insert_constants_17_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,24); END Word_insert_constants_17_24;
<*NOWARN*> PROCEDURE Word_insert_constants_17_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,25); END Word_insert_constants_17_25;
<*NOWARN*> PROCEDURE Word_insert_constants_17_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,26); END Word_insert_constants_17_26;
<*NOWARN*> PROCEDURE Word_insert_constants_17_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,27); END Word_insert_constants_17_27;
<*NOWARN*> PROCEDURE Word_insert_constants_17_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,28); END Word_insert_constants_17_28;
<*NOWARN*> PROCEDURE Word_insert_constants_17_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,29); END Word_insert_constants_17_29;
<*NOWARN*> PROCEDURE Word_insert_constants_17_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,30); END Word_insert_constants_17_30;
<*NOWARN*> PROCEDURE Word_insert_constants_17_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,31); END Word_insert_constants_17_31;
<*NOWARN*> PROCEDURE Word_insert_constants_17_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,32); END Word_insert_constants_17_32;
<*NOWARN*> PROCEDURE Word_insert_constants_17_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,33); END Word_insert_constants_17_33;
<*NOWARN*> PROCEDURE Word_insert_constants_17_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,34); END Word_insert_constants_17_34;
<*NOWARN*> PROCEDURE Word_insert_constants_17_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,35); END Word_insert_constants_17_35;
<*NOWARN*> PROCEDURE Word_insert_constants_17_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,36); END Word_insert_constants_17_36;
<*NOWARN*> PROCEDURE Word_insert_constants_17_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,37); END Word_insert_constants_17_37;
<*NOWARN*> PROCEDURE Word_insert_constants_17_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,38); END Word_insert_constants_17_38;
<*NOWARN*> PROCEDURE Word_insert_constants_17_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,39); END Word_insert_constants_17_39;
<*NOWARN*> PROCEDURE Word_insert_constants_17_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,40); END Word_insert_constants_17_40;
<*NOWARN*> PROCEDURE Word_insert_constants_17_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,41); END Word_insert_constants_17_41;
<*NOWARN*> PROCEDURE Word_insert_constants_17_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,42); END Word_insert_constants_17_42;
<*NOWARN*> PROCEDURE Word_insert_constants_17_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,43); END Word_insert_constants_17_43;
<*NOWARN*> PROCEDURE Word_insert_constants_17_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,44); END Word_insert_constants_17_44;
<*NOWARN*> PROCEDURE Word_insert_constants_17_45(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,45); END Word_insert_constants_17_45;
<*NOWARN*> PROCEDURE Word_insert_constants_17_46(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,46); END Word_insert_constants_17_46;
<*NOWARN*> PROCEDURE Word_insert_constants_17_47(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,17,47); END Word_insert_constants_17_47;
<*NOWARN*> PROCEDURE Word_insert_constants_18_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,0); END Word_insert_constants_18_0;
<*NOWARN*> PROCEDURE Word_insert_constants_18_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,1); END Word_insert_constants_18_1;
<*NOWARN*> PROCEDURE Word_insert_constants_18_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,2); END Word_insert_constants_18_2;
<*NOWARN*> PROCEDURE Word_insert_constants_18_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,3); END Word_insert_constants_18_3;
<*NOWARN*> PROCEDURE Word_insert_constants_18_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,4); END Word_insert_constants_18_4;
<*NOWARN*> PROCEDURE Word_insert_constants_18_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,5); END Word_insert_constants_18_5;
<*NOWARN*> PROCEDURE Word_insert_constants_18_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,6); END Word_insert_constants_18_6;
<*NOWARN*> PROCEDURE Word_insert_constants_18_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,7); END Word_insert_constants_18_7;
<*NOWARN*> PROCEDURE Word_insert_constants_18_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,8); END Word_insert_constants_18_8;
<*NOWARN*> PROCEDURE Word_insert_constants_18_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,9); END Word_insert_constants_18_9;
<*NOWARN*> PROCEDURE Word_insert_constants_18_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,10); END Word_insert_constants_18_10;
<*NOWARN*> PROCEDURE Word_insert_constants_18_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,11); END Word_insert_constants_18_11;
<*NOWARN*> PROCEDURE Word_insert_constants_18_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,12); END Word_insert_constants_18_12;
<*NOWARN*> PROCEDURE Word_insert_constants_18_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,13); END Word_insert_constants_18_13;
<*NOWARN*> PROCEDURE Word_insert_constants_18_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,14); END Word_insert_constants_18_14;
<*NOWARN*> PROCEDURE Word_insert_constants_18_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,15); END Word_insert_constants_18_15;
<*NOWARN*> PROCEDURE Word_insert_constants_18_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,16); END Word_insert_constants_18_16;
<*NOWARN*> PROCEDURE Word_insert_constants_18_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,17); END Word_insert_constants_18_17;
<*NOWARN*> PROCEDURE Word_insert_constants_18_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,18); END Word_insert_constants_18_18;
<*NOWARN*> PROCEDURE Word_insert_constants_18_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,19); END Word_insert_constants_18_19;
<*NOWARN*> PROCEDURE Word_insert_constants_18_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,20); END Word_insert_constants_18_20;
<*NOWARN*> PROCEDURE Word_insert_constants_18_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,21); END Word_insert_constants_18_21;
<*NOWARN*> PROCEDURE Word_insert_constants_18_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,22); END Word_insert_constants_18_22;
<*NOWARN*> PROCEDURE Word_insert_constants_18_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,23); END Word_insert_constants_18_23;
<*NOWARN*> PROCEDURE Word_insert_constants_18_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,24); END Word_insert_constants_18_24;
<*NOWARN*> PROCEDURE Word_insert_constants_18_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,25); END Word_insert_constants_18_25;
<*NOWARN*> PROCEDURE Word_insert_constants_18_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,26); END Word_insert_constants_18_26;
<*NOWARN*> PROCEDURE Word_insert_constants_18_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,27); END Word_insert_constants_18_27;
<*NOWARN*> PROCEDURE Word_insert_constants_18_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,28); END Word_insert_constants_18_28;
<*NOWARN*> PROCEDURE Word_insert_constants_18_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,29); END Word_insert_constants_18_29;
<*NOWARN*> PROCEDURE Word_insert_constants_18_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,30); END Word_insert_constants_18_30;
<*NOWARN*> PROCEDURE Word_insert_constants_18_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,31); END Word_insert_constants_18_31;
<*NOWARN*> PROCEDURE Word_insert_constants_18_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,32); END Word_insert_constants_18_32;
<*NOWARN*> PROCEDURE Word_insert_constants_18_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,33); END Word_insert_constants_18_33;
<*NOWARN*> PROCEDURE Word_insert_constants_18_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,34); END Word_insert_constants_18_34;
<*NOWARN*> PROCEDURE Word_insert_constants_18_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,35); END Word_insert_constants_18_35;
<*NOWARN*> PROCEDURE Word_insert_constants_18_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,36); END Word_insert_constants_18_36;
<*NOWARN*> PROCEDURE Word_insert_constants_18_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,37); END Word_insert_constants_18_37;
<*NOWARN*> PROCEDURE Word_insert_constants_18_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,38); END Word_insert_constants_18_38;
<*NOWARN*> PROCEDURE Word_insert_constants_18_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,39); END Word_insert_constants_18_39;
<*NOWARN*> PROCEDURE Word_insert_constants_18_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,40); END Word_insert_constants_18_40;
<*NOWARN*> PROCEDURE Word_insert_constants_18_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,41); END Word_insert_constants_18_41;
<*NOWARN*> PROCEDURE Word_insert_constants_18_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,42); END Word_insert_constants_18_42;
<*NOWARN*> PROCEDURE Word_insert_constants_18_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,43); END Word_insert_constants_18_43;
<*NOWARN*> PROCEDURE Word_insert_constants_18_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,44); END Word_insert_constants_18_44;
<*NOWARN*> PROCEDURE Word_insert_constants_18_45(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,45); END Word_insert_constants_18_45;
<*NOWARN*> PROCEDURE Word_insert_constants_18_46(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,18,46); END Word_insert_constants_18_46;
<*NOWARN*> PROCEDURE Word_insert_constants_19_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,0); END Word_insert_constants_19_0;
<*NOWARN*> PROCEDURE Word_insert_constants_19_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,1); END Word_insert_constants_19_1;
<*NOWARN*> PROCEDURE Word_insert_constants_19_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,2); END Word_insert_constants_19_2;
<*NOWARN*> PROCEDURE Word_insert_constants_19_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,3); END Word_insert_constants_19_3;
<*NOWARN*> PROCEDURE Word_insert_constants_19_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,4); END Word_insert_constants_19_4;
<*NOWARN*> PROCEDURE Word_insert_constants_19_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,5); END Word_insert_constants_19_5;
<*NOWARN*> PROCEDURE Word_insert_constants_19_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,6); END Word_insert_constants_19_6;
<*NOWARN*> PROCEDURE Word_insert_constants_19_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,7); END Word_insert_constants_19_7;
<*NOWARN*> PROCEDURE Word_insert_constants_19_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,8); END Word_insert_constants_19_8;
<*NOWARN*> PROCEDURE Word_insert_constants_19_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,9); END Word_insert_constants_19_9;
<*NOWARN*> PROCEDURE Word_insert_constants_19_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,10); END Word_insert_constants_19_10;
<*NOWARN*> PROCEDURE Word_insert_constants_19_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,11); END Word_insert_constants_19_11;
<*NOWARN*> PROCEDURE Word_insert_constants_19_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,12); END Word_insert_constants_19_12;
<*NOWARN*> PROCEDURE Word_insert_constants_19_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,13); END Word_insert_constants_19_13;
<*NOWARN*> PROCEDURE Word_insert_constants_19_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,14); END Word_insert_constants_19_14;
<*NOWARN*> PROCEDURE Word_insert_constants_19_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,15); END Word_insert_constants_19_15;
<*NOWARN*> PROCEDURE Word_insert_constants_19_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,16); END Word_insert_constants_19_16;
<*NOWARN*> PROCEDURE Word_insert_constants_19_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,17); END Word_insert_constants_19_17;
<*NOWARN*> PROCEDURE Word_insert_constants_19_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,18); END Word_insert_constants_19_18;
<*NOWARN*> PROCEDURE Word_insert_constants_19_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,19); END Word_insert_constants_19_19;
<*NOWARN*> PROCEDURE Word_insert_constants_19_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,20); END Word_insert_constants_19_20;
<*NOWARN*> PROCEDURE Word_insert_constants_19_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,21); END Word_insert_constants_19_21;
<*NOWARN*> PROCEDURE Word_insert_constants_19_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,22); END Word_insert_constants_19_22;
<*NOWARN*> PROCEDURE Word_insert_constants_19_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,23); END Word_insert_constants_19_23;
<*NOWARN*> PROCEDURE Word_insert_constants_19_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,24); END Word_insert_constants_19_24;
<*NOWARN*> PROCEDURE Word_insert_constants_19_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,25); END Word_insert_constants_19_25;
<*NOWARN*> PROCEDURE Word_insert_constants_19_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,26); END Word_insert_constants_19_26;
<*NOWARN*> PROCEDURE Word_insert_constants_19_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,27); END Word_insert_constants_19_27;
<*NOWARN*> PROCEDURE Word_insert_constants_19_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,28); END Word_insert_constants_19_28;
<*NOWARN*> PROCEDURE Word_insert_constants_19_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,29); END Word_insert_constants_19_29;
<*NOWARN*> PROCEDURE Word_insert_constants_19_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,30); END Word_insert_constants_19_30;
<*NOWARN*> PROCEDURE Word_insert_constants_19_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,31); END Word_insert_constants_19_31;
<*NOWARN*> PROCEDURE Word_insert_constants_19_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,32); END Word_insert_constants_19_32;
<*NOWARN*> PROCEDURE Word_insert_constants_19_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,33); END Word_insert_constants_19_33;
<*NOWARN*> PROCEDURE Word_insert_constants_19_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,34); END Word_insert_constants_19_34;
<*NOWARN*> PROCEDURE Word_insert_constants_19_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,35); END Word_insert_constants_19_35;
<*NOWARN*> PROCEDURE Word_insert_constants_19_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,36); END Word_insert_constants_19_36;
<*NOWARN*> PROCEDURE Word_insert_constants_19_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,37); END Word_insert_constants_19_37;
<*NOWARN*> PROCEDURE Word_insert_constants_19_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,38); END Word_insert_constants_19_38;
<*NOWARN*> PROCEDURE Word_insert_constants_19_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,39); END Word_insert_constants_19_39;
<*NOWARN*> PROCEDURE Word_insert_constants_19_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,40); END Word_insert_constants_19_40;
<*NOWARN*> PROCEDURE Word_insert_constants_19_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,41); END Word_insert_constants_19_41;
<*NOWARN*> PROCEDURE Word_insert_constants_19_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,42); END Word_insert_constants_19_42;
<*NOWARN*> PROCEDURE Word_insert_constants_19_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,43); END Word_insert_constants_19_43;
<*NOWARN*> PROCEDURE Word_insert_constants_19_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,44); END Word_insert_constants_19_44;
<*NOWARN*> PROCEDURE Word_insert_constants_19_45(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,19,45); END Word_insert_constants_19_45;
<*NOWARN*> PROCEDURE Word_insert_constants_20_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,0); END Word_insert_constants_20_0;
<*NOWARN*> PROCEDURE Word_insert_constants_20_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,1); END Word_insert_constants_20_1;
<*NOWARN*> PROCEDURE Word_insert_constants_20_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,2); END Word_insert_constants_20_2;
<*NOWARN*> PROCEDURE Word_insert_constants_20_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,3); END Word_insert_constants_20_3;
<*NOWARN*> PROCEDURE Word_insert_constants_20_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,4); END Word_insert_constants_20_4;
<*NOWARN*> PROCEDURE Word_insert_constants_20_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,5); END Word_insert_constants_20_5;
<*NOWARN*> PROCEDURE Word_insert_constants_20_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,6); END Word_insert_constants_20_6;
<*NOWARN*> PROCEDURE Word_insert_constants_20_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,7); END Word_insert_constants_20_7;
<*NOWARN*> PROCEDURE Word_insert_constants_20_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,8); END Word_insert_constants_20_8;
<*NOWARN*> PROCEDURE Word_insert_constants_20_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,9); END Word_insert_constants_20_9;
<*NOWARN*> PROCEDURE Word_insert_constants_20_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,10); END Word_insert_constants_20_10;
<*NOWARN*> PROCEDURE Word_insert_constants_20_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,11); END Word_insert_constants_20_11;
<*NOWARN*> PROCEDURE Word_insert_constants_20_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,12); END Word_insert_constants_20_12;
<*NOWARN*> PROCEDURE Word_insert_constants_20_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,13); END Word_insert_constants_20_13;
<*NOWARN*> PROCEDURE Word_insert_constants_20_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,14); END Word_insert_constants_20_14;
<*NOWARN*> PROCEDURE Word_insert_constants_20_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,15); END Word_insert_constants_20_15;
<*NOWARN*> PROCEDURE Word_insert_constants_20_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,16); END Word_insert_constants_20_16;
<*NOWARN*> PROCEDURE Word_insert_constants_20_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,17); END Word_insert_constants_20_17;
<*NOWARN*> PROCEDURE Word_insert_constants_20_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,18); END Word_insert_constants_20_18;
<*NOWARN*> PROCEDURE Word_insert_constants_20_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,19); END Word_insert_constants_20_19;
<*NOWARN*> PROCEDURE Word_insert_constants_20_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,20); END Word_insert_constants_20_20;
<*NOWARN*> PROCEDURE Word_insert_constants_20_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,21); END Word_insert_constants_20_21;
<*NOWARN*> PROCEDURE Word_insert_constants_20_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,22); END Word_insert_constants_20_22;
<*NOWARN*> PROCEDURE Word_insert_constants_20_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,23); END Word_insert_constants_20_23;
<*NOWARN*> PROCEDURE Word_insert_constants_20_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,24); END Word_insert_constants_20_24;
<*NOWARN*> PROCEDURE Word_insert_constants_20_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,25); END Word_insert_constants_20_25;
<*NOWARN*> PROCEDURE Word_insert_constants_20_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,26); END Word_insert_constants_20_26;
<*NOWARN*> PROCEDURE Word_insert_constants_20_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,27); END Word_insert_constants_20_27;
<*NOWARN*> PROCEDURE Word_insert_constants_20_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,28); END Word_insert_constants_20_28;
<*NOWARN*> PROCEDURE Word_insert_constants_20_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,29); END Word_insert_constants_20_29;
<*NOWARN*> PROCEDURE Word_insert_constants_20_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,30); END Word_insert_constants_20_30;
<*NOWARN*> PROCEDURE Word_insert_constants_20_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,31); END Word_insert_constants_20_31;
<*NOWARN*> PROCEDURE Word_insert_constants_20_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,32); END Word_insert_constants_20_32;
<*NOWARN*> PROCEDURE Word_insert_constants_20_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,33); END Word_insert_constants_20_33;
<*NOWARN*> PROCEDURE Word_insert_constants_20_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,34); END Word_insert_constants_20_34;
<*NOWARN*> PROCEDURE Word_insert_constants_20_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,35); END Word_insert_constants_20_35;
<*NOWARN*> PROCEDURE Word_insert_constants_20_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,36); END Word_insert_constants_20_36;
<*NOWARN*> PROCEDURE Word_insert_constants_20_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,37); END Word_insert_constants_20_37;
<*NOWARN*> PROCEDURE Word_insert_constants_20_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,38); END Word_insert_constants_20_38;
<*NOWARN*> PROCEDURE Word_insert_constants_20_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,39); END Word_insert_constants_20_39;
<*NOWARN*> PROCEDURE Word_insert_constants_20_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,40); END Word_insert_constants_20_40;
<*NOWARN*> PROCEDURE Word_insert_constants_20_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,41); END Word_insert_constants_20_41;
<*NOWARN*> PROCEDURE Word_insert_constants_20_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,42); END Word_insert_constants_20_42;
<*NOWARN*> PROCEDURE Word_insert_constants_20_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,43); END Word_insert_constants_20_43;
<*NOWARN*> PROCEDURE Word_insert_constants_20_44(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,20,44); END Word_insert_constants_20_44;
<*NOWARN*> PROCEDURE Word_insert_constants_21_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,0); END Word_insert_constants_21_0;
<*NOWARN*> PROCEDURE Word_insert_constants_21_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,1); END Word_insert_constants_21_1;
<*NOWARN*> PROCEDURE Word_insert_constants_21_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,2); END Word_insert_constants_21_2;
<*NOWARN*> PROCEDURE Word_insert_constants_21_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,3); END Word_insert_constants_21_3;
<*NOWARN*> PROCEDURE Word_insert_constants_21_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,4); END Word_insert_constants_21_4;
<*NOWARN*> PROCEDURE Word_insert_constants_21_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,5); END Word_insert_constants_21_5;
<*NOWARN*> PROCEDURE Word_insert_constants_21_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,6); END Word_insert_constants_21_6;
<*NOWARN*> PROCEDURE Word_insert_constants_21_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,7); END Word_insert_constants_21_7;
<*NOWARN*> PROCEDURE Word_insert_constants_21_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,8); END Word_insert_constants_21_8;
<*NOWARN*> PROCEDURE Word_insert_constants_21_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,9); END Word_insert_constants_21_9;
<*NOWARN*> PROCEDURE Word_insert_constants_21_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,10); END Word_insert_constants_21_10;
<*NOWARN*> PROCEDURE Word_insert_constants_21_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,11); END Word_insert_constants_21_11;
<*NOWARN*> PROCEDURE Word_insert_constants_21_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,12); END Word_insert_constants_21_12;
<*NOWARN*> PROCEDURE Word_insert_constants_21_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,13); END Word_insert_constants_21_13;
<*NOWARN*> PROCEDURE Word_insert_constants_21_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,14); END Word_insert_constants_21_14;
<*NOWARN*> PROCEDURE Word_insert_constants_21_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,15); END Word_insert_constants_21_15;
<*NOWARN*> PROCEDURE Word_insert_constants_21_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,16); END Word_insert_constants_21_16;
<*NOWARN*> PROCEDURE Word_insert_constants_21_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,17); END Word_insert_constants_21_17;
<*NOWARN*> PROCEDURE Word_insert_constants_21_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,18); END Word_insert_constants_21_18;
<*NOWARN*> PROCEDURE Word_insert_constants_21_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,19); END Word_insert_constants_21_19;
<*NOWARN*> PROCEDURE Word_insert_constants_21_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,20); END Word_insert_constants_21_20;
<*NOWARN*> PROCEDURE Word_insert_constants_21_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,21); END Word_insert_constants_21_21;
<*NOWARN*> PROCEDURE Word_insert_constants_21_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,22); END Word_insert_constants_21_22;
<*NOWARN*> PROCEDURE Word_insert_constants_21_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,23); END Word_insert_constants_21_23;
<*NOWARN*> PROCEDURE Word_insert_constants_21_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,24); END Word_insert_constants_21_24;
<*NOWARN*> PROCEDURE Word_insert_constants_21_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,25); END Word_insert_constants_21_25;
<*NOWARN*> PROCEDURE Word_insert_constants_21_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,26); END Word_insert_constants_21_26;
<*NOWARN*> PROCEDURE Word_insert_constants_21_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,27); END Word_insert_constants_21_27;
<*NOWARN*> PROCEDURE Word_insert_constants_21_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,28); END Word_insert_constants_21_28;
<*NOWARN*> PROCEDURE Word_insert_constants_21_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,29); END Word_insert_constants_21_29;
<*NOWARN*> PROCEDURE Word_insert_constants_21_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,30); END Word_insert_constants_21_30;
<*NOWARN*> PROCEDURE Word_insert_constants_21_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,31); END Word_insert_constants_21_31;
<*NOWARN*> PROCEDURE Word_insert_constants_21_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,32); END Word_insert_constants_21_32;
<*NOWARN*> PROCEDURE Word_insert_constants_21_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,33); END Word_insert_constants_21_33;
<*NOWARN*> PROCEDURE Word_insert_constants_21_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,34); END Word_insert_constants_21_34;
<*NOWARN*> PROCEDURE Word_insert_constants_21_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,35); END Word_insert_constants_21_35;
<*NOWARN*> PROCEDURE Word_insert_constants_21_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,36); END Word_insert_constants_21_36;
<*NOWARN*> PROCEDURE Word_insert_constants_21_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,37); END Word_insert_constants_21_37;
<*NOWARN*> PROCEDURE Word_insert_constants_21_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,38); END Word_insert_constants_21_38;
<*NOWARN*> PROCEDURE Word_insert_constants_21_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,39); END Word_insert_constants_21_39;
<*NOWARN*> PROCEDURE Word_insert_constants_21_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,40); END Word_insert_constants_21_40;
<*NOWARN*> PROCEDURE Word_insert_constants_21_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,41); END Word_insert_constants_21_41;
<*NOWARN*> PROCEDURE Word_insert_constants_21_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,42); END Word_insert_constants_21_42;
<*NOWARN*> PROCEDURE Word_insert_constants_21_43(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,21,43); END Word_insert_constants_21_43;
<*NOWARN*> PROCEDURE Word_insert_constants_22_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,0); END Word_insert_constants_22_0;
<*NOWARN*> PROCEDURE Word_insert_constants_22_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,1); END Word_insert_constants_22_1;
<*NOWARN*> PROCEDURE Word_insert_constants_22_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,2); END Word_insert_constants_22_2;
<*NOWARN*> PROCEDURE Word_insert_constants_22_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,3); END Word_insert_constants_22_3;
<*NOWARN*> PROCEDURE Word_insert_constants_22_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,4); END Word_insert_constants_22_4;
<*NOWARN*> PROCEDURE Word_insert_constants_22_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,5); END Word_insert_constants_22_5;
<*NOWARN*> PROCEDURE Word_insert_constants_22_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,6); END Word_insert_constants_22_6;
<*NOWARN*> PROCEDURE Word_insert_constants_22_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,7); END Word_insert_constants_22_7;
<*NOWARN*> PROCEDURE Word_insert_constants_22_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,8); END Word_insert_constants_22_8;
<*NOWARN*> PROCEDURE Word_insert_constants_22_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,9); END Word_insert_constants_22_9;
<*NOWARN*> PROCEDURE Word_insert_constants_22_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,10); END Word_insert_constants_22_10;
<*NOWARN*> PROCEDURE Word_insert_constants_22_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,11); END Word_insert_constants_22_11;
<*NOWARN*> PROCEDURE Word_insert_constants_22_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,12); END Word_insert_constants_22_12;
<*NOWARN*> PROCEDURE Word_insert_constants_22_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,13); END Word_insert_constants_22_13;
<*NOWARN*> PROCEDURE Word_insert_constants_22_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,14); END Word_insert_constants_22_14;
<*NOWARN*> PROCEDURE Word_insert_constants_22_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,15); END Word_insert_constants_22_15;
<*NOWARN*> PROCEDURE Word_insert_constants_22_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,16); END Word_insert_constants_22_16;
<*NOWARN*> PROCEDURE Word_insert_constants_22_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,17); END Word_insert_constants_22_17;
<*NOWARN*> PROCEDURE Word_insert_constants_22_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,18); END Word_insert_constants_22_18;
<*NOWARN*> PROCEDURE Word_insert_constants_22_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,19); END Word_insert_constants_22_19;
<*NOWARN*> PROCEDURE Word_insert_constants_22_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,20); END Word_insert_constants_22_20;
<*NOWARN*> PROCEDURE Word_insert_constants_22_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,21); END Word_insert_constants_22_21;
<*NOWARN*> PROCEDURE Word_insert_constants_22_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,22); END Word_insert_constants_22_22;
<*NOWARN*> PROCEDURE Word_insert_constants_22_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,23); END Word_insert_constants_22_23;
<*NOWARN*> PROCEDURE Word_insert_constants_22_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,24); END Word_insert_constants_22_24;
<*NOWARN*> PROCEDURE Word_insert_constants_22_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,25); END Word_insert_constants_22_25;
<*NOWARN*> PROCEDURE Word_insert_constants_22_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,26); END Word_insert_constants_22_26;
<*NOWARN*> PROCEDURE Word_insert_constants_22_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,27); END Word_insert_constants_22_27;
<*NOWARN*> PROCEDURE Word_insert_constants_22_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,28); END Word_insert_constants_22_28;
<*NOWARN*> PROCEDURE Word_insert_constants_22_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,29); END Word_insert_constants_22_29;
<*NOWARN*> PROCEDURE Word_insert_constants_22_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,30); END Word_insert_constants_22_30;
<*NOWARN*> PROCEDURE Word_insert_constants_22_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,31); END Word_insert_constants_22_31;
<*NOWARN*> PROCEDURE Word_insert_constants_22_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,32); END Word_insert_constants_22_32;
<*NOWARN*> PROCEDURE Word_insert_constants_22_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,33); END Word_insert_constants_22_33;
<*NOWARN*> PROCEDURE Word_insert_constants_22_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,34); END Word_insert_constants_22_34;
<*NOWARN*> PROCEDURE Word_insert_constants_22_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,35); END Word_insert_constants_22_35;
<*NOWARN*> PROCEDURE Word_insert_constants_22_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,36); END Word_insert_constants_22_36;
<*NOWARN*> PROCEDURE Word_insert_constants_22_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,37); END Word_insert_constants_22_37;
<*NOWARN*> PROCEDURE Word_insert_constants_22_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,38); END Word_insert_constants_22_38;
<*NOWARN*> PROCEDURE Word_insert_constants_22_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,39); END Word_insert_constants_22_39;
<*NOWARN*> PROCEDURE Word_insert_constants_22_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,40); END Word_insert_constants_22_40;
<*NOWARN*> PROCEDURE Word_insert_constants_22_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,41); END Word_insert_constants_22_41;
<*NOWARN*> PROCEDURE Word_insert_constants_22_42(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,22,42); END Word_insert_constants_22_42;
<*NOWARN*> PROCEDURE Word_insert_constants_23_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,0); END Word_insert_constants_23_0;
<*NOWARN*> PROCEDURE Word_insert_constants_23_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,1); END Word_insert_constants_23_1;
<*NOWARN*> PROCEDURE Word_insert_constants_23_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,2); END Word_insert_constants_23_2;
<*NOWARN*> PROCEDURE Word_insert_constants_23_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,3); END Word_insert_constants_23_3;
<*NOWARN*> PROCEDURE Word_insert_constants_23_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,4); END Word_insert_constants_23_4;
<*NOWARN*> PROCEDURE Word_insert_constants_23_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,5); END Word_insert_constants_23_5;
<*NOWARN*> PROCEDURE Word_insert_constants_23_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,6); END Word_insert_constants_23_6;
<*NOWARN*> PROCEDURE Word_insert_constants_23_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,7); END Word_insert_constants_23_7;
<*NOWARN*> PROCEDURE Word_insert_constants_23_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,8); END Word_insert_constants_23_8;
<*NOWARN*> PROCEDURE Word_insert_constants_23_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,9); END Word_insert_constants_23_9;
<*NOWARN*> PROCEDURE Word_insert_constants_23_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,10); END Word_insert_constants_23_10;
<*NOWARN*> PROCEDURE Word_insert_constants_23_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,11); END Word_insert_constants_23_11;
<*NOWARN*> PROCEDURE Word_insert_constants_23_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,12); END Word_insert_constants_23_12;
<*NOWARN*> PROCEDURE Word_insert_constants_23_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,13); END Word_insert_constants_23_13;
<*NOWARN*> PROCEDURE Word_insert_constants_23_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,14); END Word_insert_constants_23_14;
<*NOWARN*> PROCEDURE Word_insert_constants_23_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,15); END Word_insert_constants_23_15;
<*NOWARN*> PROCEDURE Word_insert_constants_23_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,16); END Word_insert_constants_23_16;
<*NOWARN*> PROCEDURE Word_insert_constants_23_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,17); END Word_insert_constants_23_17;
<*NOWARN*> PROCEDURE Word_insert_constants_23_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,18); END Word_insert_constants_23_18;
<*NOWARN*> PROCEDURE Word_insert_constants_23_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,19); END Word_insert_constants_23_19;
<*NOWARN*> PROCEDURE Word_insert_constants_23_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,20); END Word_insert_constants_23_20;
<*NOWARN*> PROCEDURE Word_insert_constants_23_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,21); END Word_insert_constants_23_21;
<*NOWARN*> PROCEDURE Word_insert_constants_23_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,22); END Word_insert_constants_23_22;
<*NOWARN*> PROCEDURE Word_insert_constants_23_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,23); END Word_insert_constants_23_23;
<*NOWARN*> PROCEDURE Word_insert_constants_23_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,24); END Word_insert_constants_23_24;
<*NOWARN*> PROCEDURE Word_insert_constants_23_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,25); END Word_insert_constants_23_25;
<*NOWARN*> PROCEDURE Word_insert_constants_23_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,26); END Word_insert_constants_23_26;
<*NOWARN*> PROCEDURE Word_insert_constants_23_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,27); END Word_insert_constants_23_27;
<*NOWARN*> PROCEDURE Word_insert_constants_23_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,28); END Word_insert_constants_23_28;
<*NOWARN*> PROCEDURE Word_insert_constants_23_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,29); END Word_insert_constants_23_29;
<*NOWARN*> PROCEDURE Word_insert_constants_23_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,30); END Word_insert_constants_23_30;
<*NOWARN*> PROCEDURE Word_insert_constants_23_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,31); END Word_insert_constants_23_31;
<*NOWARN*> PROCEDURE Word_insert_constants_23_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,32); END Word_insert_constants_23_32;
<*NOWARN*> PROCEDURE Word_insert_constants_23_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,33); END Word_insert_constants_23_33;
<*NOWARN*> PROCEDURE Word_insert_constants_23_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,34); END Word_insert_constants_23_34;
<*NOWARN*> PROCEDURE Word_insert_constants_23_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,35); END Word_insert_constants_23_35;
<*NOWARN*> PROCEDURE Word_insert_constants_23_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,36); END Word_insert_constants_23_36;
<*NOWARN*> PROCEDURE Word_insert_constants_23_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,37); END Word_insert_constants_23_37;
<*NOWARN*> PROCEDURE Word_insert_constants_23_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,38); END Word_insert_constants_23_38;
<*NOWARN*> PROCEDURE Word_insert_constants_23_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,39); END Word_insert_constants_23_39;
<*NOWARN*> PROCEDURE Word_insert_constants_23_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,40); END Word_insert_constants_23_40;
<*NOWARN*> PROCEDURE Word_insert_constants_23_41(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,23,41); END Word_insert_constants_23_41;
<*NOWARN*> PROCEDURE Word_insert_constants_24_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,0); END Word_insert_constants_24_0;
<*NOWARN*> PROCEDURE Word_insert_constants_24_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,1); END Word_insert_constants_24_1;
<*NOWARN*> PROCEDURE Word_insert_constants_24_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,2); END Word_insert_constants_24_2;
<*NOWARN*> PROCEDURE Word_insert_constants_24_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,3); END Word_insert_constants_24_3;
<*NOWARN*> PROCEDURE Word_insert_constants_24_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,4); END Word_insert_constants_24_4;
<*NOWARN*> PROCEDURE Word_insert_constants_24_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,5); END Word_insert_constants_24_5;
<*NOWARN*> PROCEDURE Word_insert_constants_24_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,6); END Word_insert_constants_24_6;
<*NOWARN*> PROCEDURE Word_insert_constants_24_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,7); END Word_insert_constants_24_7;
<*NOWARN*> PROCEDURE Word_insert_constants_24_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,8); END Word_insert_constants_24_8;
<*NOWARN*> PROCEDURE Word_insert_constants_24_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,9); END Word_insert_constants_24_9;
<*NOWARN*> PROCEDURE Word_insert_constants_24_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,10); END Word_insert_constants_24_10;
<*NOWARN*> PROCEDURE Word_insert_constants_24_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,11); END Word_insert_constants_24_11;
<*NOWARN*> PROCEDURE Word_insert_constants_24_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,12); END Word_insert_constants_24_12;
<*NOWARN*> PROCEDURE Word_insert_constants_24_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,13); END Word_insert_constants_24_13;
<*NOWARN*> PROCEDURE Word_insert_constants_24_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,14); END Word_insert_constants_24_14;
<*NOWARN*> PROCEDURE Word_insert_constants_24_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,15); END Word_insert_constants_24_15;
<*NOWARN*> PROCEDURE Word_insert_constants_24_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,16); END Word_insert_constants_24_16;
<*NOWARN*> PROCEDURE Word_insert_constants_24_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,17); END Word_insert_constants_24_17;
<*NOWARN*> PROCEDURE Word_insert_constants_24_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,18); END Word_insert_constants_24_18;
<*NOWARN*> PROCEDURE Word_insert_constants_24_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,19); END Word_insert_constants_24_19;
<*NOWARN*> PROCEDURE Word_insert_constants_24_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,20); END Word_insert_constants_24_20;
<*NOWARN*> PROCEDURE Word_insert_constants_24_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,21); END Word_insert_constants_24_21;
<*NOWARN*> PROCEDURE Word_insert_constants_24_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,22); END Word_insert_constants_24_22;
<*NOWARN*> PROCEDURE Word_insert_constants_24_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,23); END Word_insert_constants_24_23;
<*NOWARN*> PROCEDURE Word_insert_constants_24_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,24); END Word_insert_constants_24_24;
<*NOWARN*> PROCEDURE Word_insert_constants_24_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,25); END Word_insert_constants_24_25;
<*NOWARN*> PROCEDURE Word_insert_constants_24_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,26); END Word_insert_constants_24_26;
<*NOWARN*> PROCEDURE Word_insert_constants_24_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,27); END Word_insert_constants_24_27;
<*NOWARN*> PROCEDURE Word_insert_constants_24_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,28); END Word_insert_constants_24_28;
<*NOWARN*> PROCEDURE Word_insert_constants_24_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,29); END Word_insert_constants_24_29;
<*NOWARN*> PROCEDURE Word_insert_constants_24_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,30); END Word_insert_constants_24_30;
<*NOWARN*> PROCEDURE Word_insert_constants_24_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,31); END Word_insert_constants_24_31;
<*NOWARN*> PROCEDURE Word_insert_constants_24_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,32); END Word_insert_constants_24_32;
<*NOWARN*> PROCEDURE Word_insert_constants_24_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,33); END Word_insert_constants_24_33;
<*NOWARN*> PROCEDURE Word_insert_constants_24_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,34); END Word_insert_constants_24_34;
<*NOWARN*> PROCEDURE Word_insert_constants_24_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,35); END Word_insert_constants_24_35;
<*NOWARN*> PROCEDURE Word_insert_constants_24_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,36); END Word_insert_constants_24_36;
<*NOWARN*> PROCEDURE Word_insert_constants_24_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,37); END Word_insert_constants_24_37;
<*NOWARN*> PROCEDURE Word_insert_constants_24_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,38); END Word_insert_constants_24_38;
<*NOWARN*> PROCEDURE Word_insert_constants_24_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,39); END Word_insert_constants_24_39;
<*NOWARN*> PROCEDURE Word_insert_constants_24_40(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,24,40); END Word_insert_constants_24_40;
<*NOWARN*> PROCEDURE Word_insert_constants_25_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,0); END Word_insert_constants_25_0;
<*NOWARN*> PROCEDURE Word_insert_constants_25_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,1); END Word_insert_constants_25_1;
<*NOWARN*> PROCEDURE Word_insert_constants_25_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,2); END Word_insert_constants_25_2;
<*NOWARN*> PROCEDURE Word_insert_constants_25_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,3); END Word_insert_constants_25_3;
<*NOWARN*> PROCEDURE Word_insert_constants_25_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,4); END Word_insert_constants_25_4;
<*NOWARN*> PROCEDURE Word_insert_constants_25_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,5); END Word_insert_constants_25_5;
<*NOWARN*> PROCEDURE Word_insert_constants_25_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,6); END Word_insert_constants_25_6;
<*NOWARN*> PROCEDURE Word_insert_constants_25_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,7); END Word_insert_constants_25_7;
<*NOWARN*> PROCEDURE Word_insert_constants_25_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,8); END Word_insert_constants_25_8;
<*NOWARN*> PROCEDURE Word_insert_constants_25_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,9); END Word_insert_constants_25_9;
<*NOWARN*> PROCEDURE Word_insert_constants_25_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,10); END Word_insert_constants_25_10;
<*NOWARN*> PROCEDURE Word_insert_constants_25_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,11); END Word_insert_constants_25_11;
<*NOWARN*> PROCEDURE Word_insert_constants_25_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,12); END Word_insert_constants_25_12;
<*NOWARN*> PROCEDURE Word_insert_constants_25_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,13); END Word_insert_constants_25_13;
<*NOWARN*> PROCEDURE Word_insert_constants_25_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,14); END Word_insert_constants_25_14;
<*NOWARN*> PROCEDURE Word_insert_constants_25_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,15); END Word_insert_constants_25_15;
<*NOWARN*> PROCEDURE Word_insert_constants_25_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,16); END Word_insert_constants_25_16;
<*NOWARN*> PROCEDURE Word_insert_constants_25_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,17); END Word_insert_constants_25_17;
<*NOWARN*> PROCEDURE Word_insert_constants_25_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,18); END Word_insert_constants_25_18;
<*NOWARN*> PROCEDURE Word_insert_constants_25_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,19); END Word_insert_constants_25_19;
<*NOWARN*> PROCEDURE Word_insert_constants_25_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,20); END Word_insert_constants_25_20;
<*NOWARN*> PROCEDURE Word_insert_constants_25_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,21); END Word_insert_constants_25_21;
<*NOWARN*> PROCEDURE Word_insert_constants_25_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,22); END Word_insert_constants_25_22;
<*NOWARN*> PROCEDURE Word_insert_constants_25_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,23); END Word_insert_constants_25_23;
<*NOWARN*> PROCEDURE Word_insert_constants_25_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,24); END Word_insert_constants_25_24;
<*NOWARN*> PROCEDURE Word_insert_constants_25_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,25); END Word_insert_constants_25_25;
<*NOWARN*> PROCEDURE Word_insert_constants_25_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,26); END Word_insert_constants_25_26;
<*NOWARN*> PROCEDURE Word_insert_constants_25_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,27); END Word_insert_constants_25_27;
<*NOWARN*> PROCEDURE Word_insert_constants_25_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,28); END Word_insert_constants_25_28;
<*NOWARN*> PROCEDURE Word_insert_constants_25_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,29); END Word_insert_constants_25_29;
<*NOWARN*> PROCEDURE Word_insert_constants_25_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,30); END Word_insert_constants_25_30;
<*NOWARN*> PROCEDURE Word_insert_constants_25_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,31); END Word_insert_constants_25_31;
<*NOWARN*> PROCEDURE Word_insert_constants_25_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,32); END Word_insert_constants_25_32;
<*NOWARN*> PROCEDURE Word_insert_constants_25_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,33); END Word_insert_constants_25_33;
<*NOWARN*> PROCEDURE Word_insert_constants_25_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,34); END Word_insert_constants_25_34;
<*NOWARN*> PROCEDURE Word_insert_constants_25_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,35); END Word_insert_constants_25_35;
<*NOWARN*> PROCEDURE Word_insert_constants_25_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,36); END Word_insert_constants_25_36;
<*NOWARN*> PROCEDURE Word_insert_constants_25_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,37); END Word_insert_constants_25_37;
<*NOWARN*> PROCEDURE Word_insert_constants_25_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,38); END Word_insert_constants_25_38;
<*NOWARN*> PROCEDURE Word_insert_constants_25_39(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,25,39); END Word_insert_constants_25_39;
<*NOWARN*> PROCEDURE Word_insert_constants_26_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,0); END Word_insert_constants_26_0;
<*NOWARN*> PROCEDURE Word_insert_constants_26_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,1); END Word_insert_constants_26_1;
<*NOWARN*> PROCEDURE Word_insert_constants_26_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,2); END Word_insert_constants_26_2;
<*NOWARN*> PROCEDURE Word_insert_constants_26_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,3); END Word_insert_constants_26_3;
<*NOWARN*> PROCEDURE Word_insert_constants_26_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,4); END Word_insert_constants_26_4;
<*NOWARN*> PROCEDURE Word_insert_constants_26_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,5); END Word_insert_constants_26_5;
<*NOWARN*> PROCEDURE Word_insert_constants_26_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,6); END Word_insert_constants_26_6;
<*NOWARN*> PROCEDURE Word_insert_constants_26_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,7); END Word_insert_constants_26_7;
<*NOWARN*> PROCEDURE Word_insert_constants_26_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,8); END Word_insert_constants_26_8;
<*NOWARN*> PROCEDURE Word_insert_constants_26_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,9); END Word_insert_constants_26_9;
<*NOWARN*> PROCEDURE Word_insert_constants_26_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,10); END Word_insert_constants_26_10;
<*NOWARN*> PROCEDURE Word_insert_constants_26_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,11); END Word_insert_constants_26_11;
<*NOWARN*> PROCEDURE Word_insert_constants_26_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,12); END Word_insert_constants_26_12;
<*NOWARN*> PROCEDURE Word_insert_constants_26_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,13); END Word_insert_constants_26_13;
<*NOWARN*> PROCEDURE Word_insert_constants_26_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,14); END Word_insert_constants_26_14;
<*NOWARN*> PROCEDURE Word_insert_constants_26_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,15); END Word_insert_constants_26_15;
<*NOWARN*> PROCEDURE Word_insert_constants_26_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,16); END Word_insert_constants_26_16;
<*NOWARN*> PROCEDURE Word_insert_constants_26_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,17); END Word_insert_constants_26_17;
<*NOWARN*> PROCEDURE Word_insert_constants_26_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,18); END Word_insert_constants_26_18;
<*NOWARN*> PROCEDURE Word_insert_constants_26_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,19); END Word_insert_constants_26_19;
<*NOWARN*> PROCEDURE Word_insert_constants_26_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,20); END Word_insert_constants_26_20;
<*NOWARN*> PROCEDURE Word_insert_constants_26_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,21); END Word_insert_constants_26_21;
<*NOWARN*> PROCEDURE Word_insert_constants_26_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,22); END Word_insert_constants_26_22;
<*NOWARN*> PROCEDURE Word_insert_constants_26_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,23); END Word_insert_constants_26_23;
<*NOWARN*> PROCEDURE Word_insert_constants_26_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,24); END Word_insert_constants_26_24;
<*NOWARN*> PROCEDURE Word_insert_constants_26_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,25); END Word_insert_constants_26_25;
<*NOWARN*> PROCEDURE Word_insert_constants_26_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,26); END Word_insert_constants_26_26;
<*NOWARN*> PROCEDURE Word_insert_constants_26_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,27); END Word_insert_constants_26_27;
<*NOWARN*> PROCEDURE Word_insert_constants_26_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,28); END Word_insert_constants_26_28;
<*NOWARN*> PROCEDURE Word_insert_constants_26_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,29); END Word_insert_constants_26_29;
<*NOWARN*> PROCEDURE Word_insert_constants_26_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,30); END Word_insert_constants_26_30;
<*NOWARN*> PROCEDURE Word_insert_constants_26_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,31); END Word_insert_constants_26_31;
<*NOWARN*> PROCEDURE Word_insert_constants_26_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,32); END Word_insert_constants_26_32;
<*NOWARN*> PROCEDURE Word_insert_constants_26_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,33); END Word_insert_constants_26_33;
<*NOWARN*> PROCEDURE Word_insert_constants_26_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,34); END Word_insert_constants_26_34;
<*NOWARN*> PROCEDURE Word_insert_constants_26_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,35); END Word_insert_constants_26_35;
<*NOWARN*> PROCEDURE Word_insert_constants_26_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,36); END Word_insert_constants_26_36;
<*NOWARN*> PROCEDURE Word_insert_constants_26_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,37); END Word_insert_constants_26_37;
<*NOWARN*> PROCEDURE Word_insert_constants_26_38(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,26,38); END Word_insert_constants_26_38;
<*NOWARN*> PROCEDURE Word_insert_constants_27_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,0); END Word_insert_constants_27_0;
<*NOWARN*> PROCEDURE Word_insert_constants_27_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,1); END Word_insert_constants_27_1;
<*NOWARN*> PROCEDURE Word_insert_constants_27_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,2); END Word_insert_constants_27_2;
<*NOWARN*> PROCEDURE Word_insert_constants_27_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,3); END Word_insert_constants_27_3;
<*NOWARN*> PROCEDURE Word_insert_constants_27_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,4); END Word_insert_constants_27_4;
<*NOWARN*> PROCEDURE Word_insert_constants_27_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,5); END Word_insert_constants_27_5;
<*NOWARN*> PROCEDURE Word_insert_constants_27_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,6); END Word_insert_constants_27_6;
<*NOWARN*> PROCEDURE Word_insert_constants_27_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,7); END Word_insert_constants_27_7;
<*NOWARN*> PROCEDURE Word_insert_constants_27_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,8); END Word_insert_constants_27_8;
<*NOWARN*> PROCEDURE Word_insert_constants_27_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,9); END Word_insert_constants_27_9;
<*NOWARN*> PROCEDURE Word_insert_constants_27_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,10); END Word_insert_constants_27_10;
<*NOWARN*> PROCEDURE Word_insert_constants_27_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,11); END Word_insert_constants_27_11;
<*NOWARN*> PROCEDURE Word_insert_constants_27_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,12); END Word_insert_constants_27_12;
<*NOWARN*> PROCEDURE Word_insert_constants_27_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,13); END Word_insert_constants_27_13;
<*NOWARN*> PROCEDURE Word_insert_constants_27_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,14); END Word_insert_constants_27_14;
<*NOWARN*> PROCEDURE Word_insert_constants_27_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,15); END Word_insert_constants_27_15;
<*NOWARN*> PROCEDURE Word_insert_constants_27_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,16); END Word_insert_constants_27_16;
<*NOWARN*> PROCEDURE Word_insert_constants_27_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,17); END Word_insert_constants_27_17;
<*NOWARN*> PROCEDURE Word_insert_constants_27_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,18); END Word_insert_constants_27_18;
<*NOWARN*> PROCEDURE Word_insert_constants_27_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,19); END Word_insert_constants_27_19;
<*NOWARN*> PROCEDURE Word_insert_constants_27_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,20); END Word_insert_constants_27_20;
<*NOWARN*> PROCEDURE Word_insert_constants_27_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,21); END Word_insert_constants_27_21;
<*NOWARN*> PROCEDURE Word_insert_constants_27_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,22); END Word_insert_constants_27_22;
<*NOWARN*> PROCEDURE Word_insert_constants_27_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,23); END Word_insert_constants_27_23;
<*NOWARN*> PROCEDURE Word_insert_constants_27_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,24); END Word_insert_constants_27_24;
<*NOWARN*> PROCEDURE Word_insert_constants_27_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,25); END Word_insert_constants_27_25;
<*NOWARN*> PROCEDURE Word_insert_constants_27_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,26); END Word_insert_constants_27_26;
<*NOWARN*> PROCEDURE Word_insert_constants_27_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,27); END Word_insert_constants_27_27;
<*NOWARN*> PROCEDURE Word_insert_constants_27_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,28); END Word_insert_constants_27_28;
<*NOWARN*> PROCEDURE Word_insert_constants_27_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,29); END Word_insert_constants_27_29;
<*NOWARN*> PROCEDURE Word_insert_constants_27_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,30); END Word_insert_constants_27_30;
<*NOWARN*> PROCEDURE Word_insert_constants_27_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,31); END Word_insert_constants_27_31;
<*NOWARN*> PROCEDURE Word_insert_constants_27_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,32); END Word_insert_constants_27_32;
<*NOWARN*> PROCEDURE Word_insert_constants_27_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,33); END Word_insert_constants_27_33;
<*NOWARN*> PROCEDURE Word_insert_constants_27_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,34); END Word_insert_constants_27_34;
<*NOWARN*> PROCEDURE Word_insert_constants_27_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,35); END Word_insert_constants_27_35;
<*NOWARN*> PROCEDURE Word_insert_constants_27_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,36); END Word_insert_constants_27_36;
<*NOWARN*> PROCEDURE Word_insert_constants_27_37(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,27,37); END Word_insert_constants_27_37;
<*NOWARN*> PROCEDURE Word_insert_constants_28_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,0); END Word_insert_constants_28_0;
<*NOWARN*> PROCEDURE Word_insert_constants_28_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,1); END Word_insert_constants_28_1;
<*NOWARN*> PROCEDURE Word_insert_constants_28_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,2); END Word_insert_constants_28_2;
<*NOWARN*> PROCEDURE Word_insert_constants_28_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,3); END Word_insert_constants_28_3;
<*NOWARN*> PROCEDURE Word_insert_constants_28_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,4); END Word_insert_constants_28_4;
<*NOWARN*> PROCEDURE Word_insert_constants_28_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,5); END Word_insert_constants_28_5;
<*NOWARN*> PROCEDURE Word_insert_constants_28_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,6); END Word_insert_constants_28_6;
<*NOWARN*> PROCEDURE Word_insert_constants_28_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,7); END Word_insert_constants_28_7;
<*NOWARN*> PROCEDURE Word_insert_constants_28_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,8); END Word_insert_constants_28_8;
<*NOWARN*> PROCEDURE Word_insert_constants_28_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,9); END Word_insert_constants_28_9;
<*NOWARN*> PROCEDURE Word_insert_constants_28_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,10); END Word_insert_constants_28_10;
<*NOWARN*> PROCEDURE Word_insert_constants_28_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,11); END Word_insert_constants_28_11;
<*NOWARN*> PROCEDURE Word_insert_constants_28_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,12); END Word_insert_constants_28_12;
<*NOWARN*> PROCEDURE Word_insert_constants_28_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,13); END Word_insert_constants_28_13;
<*NOWARN*> PROCEDURE Word_insert_constants_28_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,14); END Word_insert_constants_28_14;
<*NOWARN*> PROCEDURE Word_insert_constants_28_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,15); END Word_insert_constants_28_15;
<*NOWARN*> PROCEDURE Word_insert_constants_28_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,16); END Word_insert_constants_28_16;
<*NOWARN*> PROCEDURE Word_insert_constants_28_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,17); END Word_insert_constants_28_17;
<*NOWARN*> PROCEDURE Word_insert_constants_28_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,18); END Word_insert_constants_28_18;
<*NOWARN*> PROCEDURE Word_insert_constants_28_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,19); END Word_insert_constants_28_19;
<*NOWARN*> PROCEDURE Word_insert_constants_28_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,20); END Word_insert_constants_28_20;
<*NOWARN*> PROCEDURE Word_insert_constants_28_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,21); END Word_insert_constants_28_21;
<*NOWARN*> PROCEDURE Word_insert_constants_28_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,22); END Word_insert_constants_28_22;
<*NOWARN*> PROCEDURE Word_insert_constants_28_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,23); END Word_insert_constants_28_23;
<*NOWARN*> PROCEDURE Word_insert_constants_28_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,24); END Word_insert_constants_28_24;
<*NOWARN*> PROCEDURE Word_insert_constants_28_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,25); END Word_insert_constants_28_25;
<*NOWARN*> PROCEDURE Word_insert_constants_28_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,26); END Word_insert_constants_28_26;
<*NOWARN*> PROCEDURE Word_insert_constants_28_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,27); END Word_insert_constants_28_27;
<*NOWARN*> PROCEDURE Word_insert_constants_28_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,28); END Word_insert_constants_28_28;
<*NOWARN*> PROCEDURE Word_insert_constants_28_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,29); END Word_insert_constants_28_29;
<*NOWARN*> PROCEDURE Word_insert_constants_28_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,30); END Word_insert_constants_28_30;
<*NOWARN*> PROCEDURE Word_insert_constants_28_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,31); END Word_insert_constants_28_31;
<*NOWARN*> PROCEDURE Word_insert_constants_28_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,32); END Word_insert_constants_28_32;
<*NOWARN*> PROCEDURE Word_insert_constants_28_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,33); END Word_insert_constants_28_33;
<*NOWARN*> PROCEDURE Word_insert_constants_28_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,34); END Word_insert_constants_28_34;
<*NOWARN*> PROCEDURE Word_insert_constants_28_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,35); END Word_insert_constants_28_35;
<*NOWARN*> PROCEDURE Word_insert_constants_28_36(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,28,36); END Word_insert_constants_28_36;
<*NOWARN*> PROCEDURE Word_insert_constants_29_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,0); END Word_insert_constants_29_0;
<*NOWARN*> PROCEDURE Word_insert_constants_29_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,1); END Word_insert_constants_29_1;
<*NOWARN*> PROCEDURE Word_insert_constants_29_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,2); END Word_insert_constants_29_2;
<*NOWARN*> PROCEDURE Word_insert_constants_29_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,3); END Word_insert_constants_29_3;
<*NOWARN*> PROCEDURE Word_insert_constants_29_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,4); END Word_insert_constants_29_4;
<*NOWARN*> PROCEDURE Word_insert_constants_29_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,5); END Word_insert_constants_29_5;
<*NOWARN*> PROCEDURE Word_insert_constants_29_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,6); END Word_insert_constants_29_6;
<*NOWARN*> PROCEDURE Word_insert_constants_29_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,7); END Word_insert_constants_29_7;
<*NOWARN*> PROCEDURE Word_insert_constants_29_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,8); END Word_insert_constants_29_8;
<*NOWARN*> PROCEDURE Word_insert_constants_29_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,9); END Word_insert_constants_29_9;
<*NOWARN*> PROCEDURE Word_insert_constants_29_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,10); END Word_insert_constants_29_10;
<*NOWARN*> PROCEDURE Word_insert_constants_29_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,11); END Word_insert_constants_29_11;
<*NOWARN*> PROCEDURE Word_insert_constants_29_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,12); END Word_insert_constants_29_12;
<*NOWARN*> PROCEDURE Word_insert_constants_29_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,13); END Word_insert_constants_29_13;
<*NOWARN*> PROCEDURE Word_insert_constants_29_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,14); END Word_insert_constants_29_14;
<*NOWARN*> PROCEDURE Word_insert_constants_29_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,15); END Word_insert_constants_29_15;
<*NOWARN*> PROCEDURE Word_insert_constants_29_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,16); END Word_insert_constants_29_16;
<*NOWARN*> PROCEDURE Word_insert_constants_29_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,17); END Word_insert_constants_29_17;
<*NOWARN*> PROCEDURE Word_insert_constants_29_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,18); END Word_insert_constants_29_18;
<*NOWARN*> PROCEDURE Word_insert_constants_29_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,19); END Word_insert_constants_29_19;
<*NOWARN*> PROCEDURE Word_insert_constants_29_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,20); END Word_insert_constants_29_20;
<*NOWARN*> PROCEDURE Word_insert_constants_29_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,21); END Word_insert_constants_29_21;
<*NOWARN*> PROCEDURE Word_insert_constants_29_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,22); END Word_insert_constants_29_22;
<*NOWARN*> PROCEDURE Word_insert_constants_29_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,23); END Word_insert_constants_29_23;
<*NOWARN*> PROCEDURE Word_insert_constants_29_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,24); END Word_insert_constants_29_24;
<*NOWARN*> PROCEDURE Word_insert_constants_29_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,25); END Word_insert_constants_29_25;
<*NOWARN*> PROCEDURE Word_insert_constants_29_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,26); END Word_insert_constants_29_26;
<*NOWARN*> PROCEDURE Word_insert_constants_29_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,27); END Word_insert_constants_29_27;
<*NOWARN*> PROCEDURE Word_insert_constants_29_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,28); END Word_insert_constants_29_28;
<*NOWARN*> PROCEDURE Word_insert_constants_29_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,29); END Word_insert_constants_29_29;
<*NOWARN*> PROCEDURE Word_insert_constants_29_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,30); END Word_insert_constants_29_30;
<*NOWARN*> PROCEDURE Word_insert_constants_29_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,31); END Word_insert_constants_29_31;
<*NOWARN*> PROCEDURE Word_insert_constants_29_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,32); END Word_insert_constants_29_32;
<*NOWARN*> PROCEDURE Word_insert_constants_29_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,33); END Word_insert_constants_29_33;
<*NOWARN*> PROCEDURE Word_insert_constants_29_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,34); END Word_insert_constants_29_34;
<*NOWARN*> PROCEDURE Word_insert_constants_29_35(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,29,35); END Word_insert_constants_29_35;
<*NOWARN*> PROCEDURE Word_insert_constants_30_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,0); END Word_insert_constants_30_0;
<*NOWARN*> PROCEDURE Word_insert_constants_30_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,1); END Word_insert_constants_30_1;
<*NOWARN*> PROCEDURE Word_insert_constants_30_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,2); END Word_insert_constants_30_2;
<*NOWARN*> PROCEDURE Word_insert_constants_30_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,3); END Word_insert_constants_30_3;
<*NOWARN*> PROCEDURE Word_insert_constants_30_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,4); END Word_insert_constants_30_4;
<*NOWARN*> PROCEDURE Word_insert_constants_30_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,5); END Word_insert_constants_30_5;
<*NOWARN*> PROCEDURE Word_insert_constants_30_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,6); END Word_insert_constants_30_6;
<*NOWARN*> PROCEDURE Word_insert_constants_30_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,7); END Word_insert_constants_30_7;
<*NOWARN*> PROCEDURE Word_insert_constants_30_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,8); END Word_insert_constants_30_8;
<*NOWARN*> PROCEDURE Word_insert_constants_30_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,9); END Word_insert_constants_30_9;
<*NOWARN*> PROCEDURE Word_insert_constants_30_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,10); END Word_insert_constants_30_10;
<*NOWARN*> PROCEDURE Word_insert_constants_30_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,11); END Word_insert_constants_30_11;
<*NOWARN*> PROCEDURE Word_insert_constants_30_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,12); END Word_insert_constants_30_12;
<*NOWARN*> PROCEDURE Word_insert_constants_30_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,13); END Word_insert_constants_30_13;
<*NOWARN*> PROCEDURE Word_insert_constants_30_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,14); END Word_insert_constants_30_14;
<*NOWARN*> PROCEDURE Word_insert_constants_30_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,15); END Word_insert_constants_30_15;
<*NOWARN*> PROCEDURE Word_insert_constants_30_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,16); END Word_insert_constants_30_16;
<*NOWARN*> PROCEDURE Word_insert_constants_30_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,17); END Word_insert_constants_30_17;
<*NOWARN*> PROCEDURE Word_insert_constants_30_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,18); END Word_insert_constants_30_18;
<*NOWARN*> PROCEDURE Word_insert_constants_30_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,19); END Word_insert_constants_30_19;
<*NOWARN*> PROCEDURE Word_insert_constants_30_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,20); END Word_insert_constants_30_20;
<*NOWARN*> PROCEDURE Word_insert_constants_30_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,21); END Word_insert_constants_30_21;
<*NOWARN*> PROCEDURE Word_insert_constants_30_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,22); END Word_insert_constants_30_22;
<*NOWARN*> PROCEDURE Word_insert_constants_30_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,23); END Word_insert_constants_30_23;
<*NOWARN*> PROCEDURE Word_insert_constants_30_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,24); END Word_insert_constants_30_24;
<*NOWARN*> PROCEDURE Word_insert_constants_30_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,25); END Word_insert_constants_30_25;
<*NOWARN*> PROCEDURE Word_insert_constants_30_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,26); END Word_insert_constants_30_26;
<*NOWARN*> PROCEDURE Word_insert_constants_30_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,27); END Word_insert_constants_30_27;
<*NOWARN*> PROCEDURE Word_insert_constants_30_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,28); END Word_insert_constants_30_28;
<*NOWARN*> PROCEDURE Word_insert_constants_30_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,29); END Word_insert_constants_30_29;
<*NOWARN*> PROCEDURE Word_insert_constants_30_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,30); END Word_insert_constants_30_30;
<*NOWARN*> PROCEDURE Word_insert_constants_30_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,31); END Word_insert_constants_30_31;
<*NOWARN*> PROCEDURE Word_insert_constants_30_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,32); END Word_insert_constants_30_32;
<*NOWARN*> PROCEDURE Word_insert_constants_30_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,33); END Word_insert_constants_30_33;
<*NOWARN*> PROCEDURE Word_insert_constants_30_34(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,30,34); END Word_insert_constants_30_34;
<*NOWARN*> PROCEDURE Word_insert_constants_31_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,0); END Word_insert_constants_31_0;
<*NOWARN*> PROCEDURE Word_insert_constants_31_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,1); END Word_insert_constants_31_1;
<*NOWARN*> PROCEDURE Word_insert_constants_31_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,2); END Word_insert_constants_31_2;
<*NOWARN*> PROCEDURE Word_insert_constants_31_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,3); END Word_insert_constants_31_3;
<*NOWARN*> PROCEDURE Word_insert_constants_31_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,4); END Word_insert_constants_31_4;
<*NOWARN*> PROCEDURE Word_insert_constants_31_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,5); END Word_insert_constants_31_5;
<*NOWARN*> PROCEDURE Word_insert_constants_31_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,6); END Word_insert_constants_31_6;
<*NOWARN*> PROCEDURE Word_insert_constants_31_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,7); END Word_insert_constants_31_7;
<*NOWARN*> PROCEDURE Word_insert_constants_31_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,8); END Word_insert_constants_31_8;
<*NOWARN*> PROCEDURE Word_insert_constants_31_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,9); END Word_insert_constants_31_9;
<*NOWARN*> PROCEDURE Word_insert_constants_31_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,10); END Word_insert_constants_31_10;
<*NOWARN*> PROCEDURE Word_insert_constants_31_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,11); END Word_insert_constants_31_11;
<*NOWARN*> PROCEDURE Word_insert_constants_31_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,12); END Word_insert_constants_31_12;
<*NOWARN*> PROCEDURE Word_insert_constants_31_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,13); END Word_insert_constants_31_13;
<*NOWARN*> PROCEDURE Word_insert_constants_31_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,14); END Word_insert_constants_31_14;
<*NOWARN*> PROCEDURE Word_insert_constants_31_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,15); END Word_insert_constants_31_15;
<*NOWARN*> PROCEDURE Word_insert_constants_31_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,16); END Word_insert_constants_31_16;
<*NOWARN*> PROCEDURE Word_insert_constants_31_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,17); END Word_insert_constants_31_17;
<*NOWARN*> PROCEDURE Word_insert_constants_31_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,18); END Word_insert_constants_31_18;
<*NOWARN*> PROCEDURE Word_insert_constants_31_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,19); END Word_insert_constants_31_19;
<*NOWARN*> PROCEDURE Word_insert_constants_31_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,20); END Word_insert_constants_31_20;
<*NOWARN*> PROCEDURE Word_insert_constants_31_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,21); END Word_insert_constants_31_21;
<*NOWARN*> PROCEDURE Word_insert_constants_31_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,22); END Word_insert_constants_31_22;
<*NOWARN*> PROCEDURE Word_insert_constants_31_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,23); END Word_insert_constants_31_23;
<*NOWARN*> PROCEDURE Word_insert_constants_31_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,24); END Word_insert_constants_31_24;
<*NOWARN*> PROCEDURE Word_insert_constants_31_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,25); END Word_insert_constants_31_25;
<*NOWARN*> PROCEDURE Word_insert_constants_31_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,26); END Word_insert_constants_31_26;
<*NOWARN*> PROCEDURE Word_insert_constants_31_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,27); END Word_insert_constants_31_27;
<*NOWARN*> PROCEDURE Word_insert_constants_31_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,28); END Word_insert_constants_31_28;
<*NOWARN*> PROCEDURE Word_insert_constants_31_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,29); END Word_insert_constants_31_29;
<*NOWARN*> PROCEDURE Word_insert_constants_31_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,30); END Word_insert_constants_31_30;
<*NOWARN*> PROCEDURE Word_insert_constants_31_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,31); END Word_insert_constants_31_31;
<*NOWARN*> PROCEDURE Word_insert_constants_31_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,32); END Word_insert_constants_31_32;
<*NOWARN*> PROCEDURE Word_insert_constants_31_33(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,31,33); END Word_insert_constants_31_33;
<*NOWARN*> PROCEDURE Word_insert_constants_32_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,0); END Word_insert_constants_32_0;
<*NOWARN*> PROCEDURE Word_insert_constants_32_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,1); END Word_insert_constants_32_1;
<*NOWARN*> PROCEDURE Word_insert_constants_32_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,2); END Word_insert_constants_32_2;
<*NOWARN*> PROCEDURE Word_insert_constants_32_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,3); END Word_insert_constants_32_3;
<*NOWARN*> PROCEDURE Word_insert_constants_32_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,4); END Word_insert_constants_32_4;
<*NOWARN*> PROCEDURE Word_insert_constants_32_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,5); END Word_insert_constants_32_5;
<*NOWARN*> PROCEDURE Word_insert_constants_32_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,6); END Word_insert_constants_32_6;
<*NOWARN*> PROCEDURE Word_insert_constants_32_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,7); END Word_insert_constants_32_7;
<*NOWARN*> PROCEDURE Word_insert_constants_32_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,8); END Word_insert_constants_32_8;
<*NOWARN*> PROCEDURE Word_insert_constants_32_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,9); END Word_insert_constants_32_9;
<*NOWARN*> PROCEDURE Word_insert_constants_32_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,10); END Word_insert_constants_32_10;
<*NOWARN*> PROCEDURE Word_insert_constants_32_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,11); END Word_insert_constants_32_11;
<*NOWARN*> PROCEDURE Word_insert_constants_32_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,12); END Word_insert_constants_32_12;
<*NOWARN*> PROCEDURE Word_insert_constants_32_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,13); END Word_insert_constants_32_13;
<*NOWARN*> PROCEDURE Word_insert_constants_32_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,14); END Word_insert_constants_32_14;
<*NOWARN*> PROCEDURE Word_insert_constants_32_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,15); END Word_insert_constants_32_15;
<*NOWARN*> PROCEDURE Word_insert_constants_32_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,16); END Word_insert_constants_32_16;
<*NOWARN*> PROCEDURE Word_insert_constants_32_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,17); END Word_insert_constants_32_17;
<*NOWARN*> PROCEDURE Word_insert_constants_32_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,18); END Word_insert_constants_32_18;
<*NOWARN*> PROCEDURE Word_insert_constants_32_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,19); END Word_insert_constants_32_19;
<*NOWARN*> PROCEDURE Word_insert_constants_32_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,20); END Word_insert_constants_32_20;
<*NOWARN*> PROCEDURE Word_insert_constants_32_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,21); END Word_insert_constants_32_21;
<*NOWARN*> PROCEDURE Word_insert_constants_32_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,22); END Word_insert_constants_32_22;
<*NOWARN*> PROCEDURE Word_insert_constants_32_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,23); END Word_insert_constants_32_23;
<*NOWARN*> PROCEDURE Word_insert_constants_32_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,24); END Word_insert_constants_32_24;
<*NOWARN*> PROCEDURE Word_insert_constants_32_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,25); END Word_insert_constants_32_25;
<*NOWARN*> PROCEDURE Word_insert_constants_32_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,26); END Word_insert_constants_32_26;
<*NOWARN*> PROCEDURE Word_insert_constants_32_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,27); END Word_insert_constants_32_27;
<*NOWARN*> PROCEDURE Word_insert_constants_32_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,28); END Word_insert_constants_32_28;
<*NOWARN*> PROCEDURE Word_insert_constants_32_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,29); END Word_insert_constants_32_29;
<*NOWARN*> PROCEDURE Word_insert_constants_32_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,30); END Word_insert_constants_32_30;
<*NOWARN*> PROCEDURE Word_insert_constants_32_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,31); END Word_insert_constants_32_31;
<*NOWARN*> PROCEDURE Word_insert_constants_32_32(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,32,32); END Word_insert_constants_32_32;
<*NOWARN*> PROCEDURE Word_insert_constants_33_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,0); END Word_insert_constants_33_0;
<*NOWARN*> PROCEDURE Word_insert_constants_33_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,1); END Word_insert_constants_33_1;
<*NOWARN*> PROCEDURE Word_insert_constants_33_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,2); END Word_insert_constants_33_2;
<*NOWARN*> PROCEDURE Word_insert_constants_33_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,3); END Word_insert_constants_33_3;
<*NOWARN*> PROCEDURE Word_insert_constants_33_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,4); END Word_insert_constants_33_4;
<*NOWARN*> PROCEDURE Word_insert_constants_33_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,5); END Word_insert_constants_33_5;
<*NOWARN*> PROCEDURE Word_insert_constants_33_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,6); END Word_insert_constants_33_6;
<*NOWARN*> PROCEDURE Word_insert_constants_33_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,7); END Word_insert_constants_33_7;
<*NOWARN*> PROCEDURE Word_insert_constants_33_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,8); END Word_insert_constants_33_8;
<*NOWARN*> PROCEDURE Word_insert_constants_33_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,9); END Word_insert_constants_33_9;
<*NOWARN*> PROCEDURE Word_insert_constants_33_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,10); END Word_insert_constants_33_10;
<*NOWARN*> PROCEDURE Word_insert_constants_33_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,11); END Word_insert_constants_33_11;
<*NOWARN*> PROCEDURE Word_insert_constants_33_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,12); END Word_insert_constants_33_12;
<*NOWARN*> PROCEDURE Word_insert_constants_33_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,13); END Word_insert_constants_33_13;
<*NOWARN*> PROCEDURE Word_insert_constants_33_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,14); END Word_insert_constants_33_14;
<*NOWARN*> PROCEDURE Word_insert_constants_33_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,15); END Word_insert_constants_33_15;
<*NOWARN*> PROCEDURE Word_insert_constants_33_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,16); END Word_insert_constants_33_16;
<*NOWARN*> PROCEDURE Word_insert_constants_33_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,17); END Word_insert_constants_33_17;
<*NOWARN*> PROCEDURE Word_insert_constants_33_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,18); END Word_insert_constants_33_18;
<*NOWARN*> PROCEDURE Word_insert_constants_33_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,19); END Word_insert_constants_33_19;
<*NOWARN*> PROCEDURE Word_insert_constants_33_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,20); END Word_insert_constants_33_20;
<*NOWARN*> PROCEDURE Word_insert_constants_33_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,21); END Word_insert_constants_33_21;
<*NOWARN*> PROCEDURE Word_insert_constants_33_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,22); END Word_insert_constants_33_22;
<*NOWARN*> PROCEDURE Word_insert_constants_33_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,23); END Word_insert_constants_33_23;
<*NOWARN*> PROCEDURE Word_insert_constants_33_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,24); END Word_insert_constants_33_24;
<*NOWARN*> PROCEDURE Word_insert_constants_33_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,25); END Word_insert_constants_33_25;
<*NOWARN*> PROCEDURE Word_insert_constants_33_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,26); END Word_insert_constants_33_26;
<*NOWARN*> PROCEDURE Word_insert_constants_33_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,27); END Word_insert_constants_33_27;
<*NOWARN*> PROCEDURE Word_insert_constants_33_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,28); END Word_insert_constants_33_28;
<*NOWARN*> PROCEDURE Word_insert_constants_33_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,29); END Word_insert_constants_33_29;
<*NOWARN*> PROCEDURE Word_insert_constants_33_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,30); END Word_insert_constants_33_30;
<*NOWARN*> PROCEDURE Word_insert_constants_33_31(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,33,31); END Word_insert_constants_33_31;
<*NOWARN*> PROCEDURE Word_insert_constants_34_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,0); END Word_insert_constants_34_0;
<*NOWARN*> PROCEDURE Word_insert_constants_34_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,1); END Word_insert_constants_34_1;
<*NOWARN*> PROCEDURE Word_insert_constants_34_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,2); END Word_insert_constants_34_2;
<*NOWARN*> PROCEDURE Word_insert_constants_34_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,3); END Word_insert_constants_34_3;
<*NOWARN*> PROCEDURE Word_insert_constants_34_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,4); END Word_insert_constants_34_4;
<*NOWARN*> PROCEDURE Word_insert_constants_34_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,5); END Word_insert_constants_34_5;
<*NOWARN*> PROCEDURE Word_insert_constants_34_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,6); END Word_insert_constants_34_6;
<*NOWARN*> PROCEDURE Word_insert_constants_34_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,7); END Word_insert_constants_34_7;
<*NOWARN*> PROCEDURE Word_insert_constants_34_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,8); END Word_insert_constants_34_8;
<*NOWARN*> PROCEDURE Word_insert_constants_34_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,9); END Word_insert_constants_34_9;
<*NOWARN*> PROCEDURE Word_insert_constants_34_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,10); END Word_insert_constants_34_10;
<*NOWARN*> PROCEDURE Word_insert_constants_34_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,11); END Word_insert_constants_34_11;
<*NOWARN*> PROCEDURE Word_insert_constants_34_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,12); END Word_insert_constants_34_12;
<*NOWARN*> PROCEDURE Word_insert_constants_34_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,13); END Word_insert_constants_34_13;
<*NOWARN*> PROCEDURE Word_insert_constants_34_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,14); END Word_insert_constants_34_14;
<*NOWARN*> PROCEDURE Word_insert_constants_34_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,15); END Word_insert_constants_34_15;
<*NOWARN*> PROCEDURE Word_insert_constants_34_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,16); END Word_insert_constants_34_16;
<*NOWARN*> PROCEDURE Word_insert_constants_34_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,17); END Word_insert_constants_34_17;
<*NOWARN*> PROCEDURE Word_insert_constants_34_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,18); END Word_insert_constants_34_18;
<*NOWARN*> PROCEDURE Word_insert_constants_34_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,19); END Word_insert_constants_34_19;
<*NOWARN*> PROCEDURE Word_insert_constants_34_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,20); END Word_insert_constants_34_20;
<*NOWARN*> PROCEDURE Word_insert_constants_34_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,21); END Word_insert_constants_34_21;
<*NOWARN*> PROCEDURE Word_insert_constants_34_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,22); END Word_insert_constants_34_22;
<*NOWARN*> PROCEDURE Word_insert_constants_34_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,23); END Word_insert_constants_34_23;
<*NOWARN*> PROCEDURE Word_insert_constants_34_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,24); END Word_insert_constants_34_24;
<*NOWARN*> PROCEDURE Word_insert_constants_34_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,25); END Word_insert_constants_34_25;
<*NOWARN*> PROCEDURE Word_insert_constants_34_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,26); END Word_insert_constants_34_26;
<*NOWARN*> PROCEDURE Word_insert_constants_34_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,27); END Word_insert_constants_34_27;
<*NOWARN*> PROCEDURE Word_insert_constants_34_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,28); END Word_insert_constants_34_28;
<*NOWARN*> PROCEDURE Word_insert_constants_34_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,29); END Word_insert_constants_34_29;
<*NOWARN*> PROCEDURE Word_insert_constants_34_30(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,34,30); END Word_insert_constants_34_30;
<*NOWARN*> PROCEDURE Word_insert_constants_35_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,0); END Word_insert_constants_35_0;
<*NOWARN*> PROCEDURE Word_insert_constants_35_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,1); END Word_insert_constants_35_1;
<*NOWARN*> PROCEDURE Word_insert_constants_35_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,2); END Word_insert_constants_35_2;
<*NOWARN*> PROCEDURE Word_insert_constants_35_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,3); END Word_insert_constants_35_3;
<*NOWARN*> PROCEDURE Word_insert_constants_35_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,4); END Word_insert_constants_35_4;
<*NOWARN*> PROCEDURE Word_insert_constants_35_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,5); END Word_insert_constants_35_5;
<*NOWARN*> PROCEDURE Word_insert_constants_35_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,6); END Word_insert_constants_35_6;
<*NOWARN*> PROCEDURE Word_insert_constants_35_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,7); END Word_insert_constants_35_7;
<*NOWARN*> PROCEDURE Word_insert_constants_35_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,8); END Word_insert_constants_35_8;
<*NOWARN*> PROCEDURE Word_insert_constants_35_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,9); END Word_insert_constants_35_9;
<*NOWARN*> PROCEDURE Word_insert_constants_35_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,10); END Word_insert_constants_35_10;
<*NOWARN*> PROCEDURE Word_insert_constants_35_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,11); END Word_insert_constants_35_11;
<*NOWARN*> PROCEDURE Word_insert_constants_35_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,12); END Word_insert_constants_35_12;
<*NOWARN*> PROCEDURE Word_insert_constants_35_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,13); END Word_insert_constants_35_13;
<*NOWARN*> PROCEDURE Word_insert_constants_35_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,14); END Word_insert_constants_35_14;
<*NOWARN*> PROCEDURE Word_insert_constants_35_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,15); END Word_insert_constants_35_15;
<*NOWARN*> PROCEDURE Word_insert_constants_35_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,16); END Word_insert_constants_35_16;
<*NOWARN*> PROCEDURE Word_insert_constants_35_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,17); END Word_insert_constants_35_17;
<*NOWARN*> PROCEDURE Word_insert_constants_35_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,18); END Word_insert_constants_35_18;
<*NOWARN*> PROCEDURE Word_insert_constants_35_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,19); END Word_insert_constants_35_19;
<*NOWARN*> PROCEDURE Word_insert_constants_35_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,20); END Word_insert_constants_35_20;
<*NOWARN*> PROCEDURE Word_insert_constants_35_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,21); END Word_insert_constants_35_21;
<*NOWARN*> PROCEDURE Word_insert_constants_35_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,22); END Word_insert_constants_35_22;
<*NOWARN*> PROCEDURE Word_insert_constants_35_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,23); END Word_insert_constants_35_23;
<*NOWARN*> PROCEDURE Word_insert_constants_35_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,24); END Word_insert_constants_35_24;
<*NOWARN*> PROCEDURE Word_insert_constants_35_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,25); END Word_insert_constants_35_25;
<*NOWARN*> PROCEDURE Word_insert_constants_35_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,26); END Word_insert_constants_35_26;
<*NOWARN*> PROCEDURE Word_insert_constants_35_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,27); END Word_insert_constants_35_27;
<*NOWARN*> PROCEDURE Word_insert_constants_35_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,28); END Word_insert_constants_35_28;
<*NOWARN*> PROCEDURE Word_insert_constants_35_29(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,35,29); END Word_insert_constants_35_29;
<*NOWARN*> PROCEDURE Word_insert_constants_36_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,0); END Word_insert_constants_36_0;
<*NOWARN*> PROCEDURE Word_insert_constants_36_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,1); END Word_insert_constants_36_1;
<*NOWARN*> PROCEDURE Word_insert_constants_36_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,2); END Word_insert_constants_36_2;
<*NOWARN*> PROCEDURE Word_insert_constants_36_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,3); END Word_insert_constants_36_3;
<*NOWARN*> PROCEDURE Word_insert_constants_36_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,4); END Word_insert_constants_36_4;
<*NOWARN*> PROCEDURE Word_insert_constants_36_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,5); END Word_insert_constants_36_5;
<*NOWARN*> PROCEDURE Word_insert_constants_36_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,6); END Word_insert_constants_36_6;
<*NOWARN*> PROCEDURE Word_insert_constants_36_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,7); END Word_insert_constants_36_7;
<*NOWARN*> PROCEDURE Word_insert_constants_36_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,8); END Word_insert_constants_36_8;
<*NOWARN*> PROCEDURE Word_insert_constants_36_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,9); END Word_insert_constants_36_9;
<*NOWARN*> PROCEDURE Word_insert_constants_36_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,10); END Word_insert_constants_36_10;
<*NOWARN*> PROCEDURE Word_insert_constants_36_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,11); END Word_insert_constants_36_11;
<*NOWARN*> PROCEDURE Word_insert_constants_36_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,12); END Word_insert_constants_36_12;
<*NOWARN*> PROCEDURE Word_insert_constants_36_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,13); END Word_insert_constants_36_13;
<*NOWARN*> PROCEDURE Word_insert_constants_36_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,14); END Word_insert_constants_36_14;
<*NOWARN*> PROCEDURE Word_insert_constants_36_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,15); END Word_insert_constants_36_15;
<*NOWARN*> PROCEDURE Word_insert_constants_36_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,16); END Word_insert_constants_36_16;
<*NOWARN*> PROCEDURE Word_insert_constants_36_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,17); END Word_insert_constants_36_17;
<*NOWARN*> PROCEDURE Word_insert_constants_36_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,18); END Word_insert_constants_36_18;
<*NOWARN*> PROCEDURE Word_insert_constants_36_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,19); END Word_insert_constants_36_19;
<*NOWARN*> PROCEDURE Word_insert_constants_36_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,20); END Word_insert_constants_36_20;
<*NOWARN*> PROCEDURE Word_insert_constants_36_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,21); END Word_insert_constants_36_21;
<*NOWARN*> PROCEDURE Word_insert_constants_36_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,22); END Word_insert_constants_36_22;
<*NOWARN*> PROCEDURE Word_insert_constants_36_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,23); END Word_insert_constants_36_23;
<*NOWARN*> PROCEDURE Word_insert_constants_36_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,24); END Word_insert_constants_36_24;
<*NOWARN*> PROCEDURE Word_insert_constants_36_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,25); END Word_insert_constants_36_25;
<*NOWARN*> PROCEDURE Word_insert_constants_36_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,26); END Word_insert_constants_36_26;
<*NOWARN*> PROCEDURE Word_insert_constants_36_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,27); END Word_insert_constants_36_27;
<*NOWARN*> PROCEDURE Word_insert_constants_36_28(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,36,28); END Word_insert_constants_36_28;
<*NOWARN*> PROCEDURE Word_insert_constants_37_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,0); END Word_insert_constants_37_0;
<*NOWARN*> PROCEDURE Word_insert_constants_37_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,1); END Word_insert_constants_37_1;
<*NOWARN*> PROCEDURE Word_insert_constants_37_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,2); END Word_insert_constants_37_2;
<*NOWARN*> PROCEDURE Word_insert_constants_37_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,3); END Word_insert_constants_37_3;
<*NOWARN*> PROCEDURE Word_insert_constants_37_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,4); END Word_insert_constants_37_4;
<*NOWARN*> PROCEDURE Word_insert_constants_37_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,5); END Word_insert_constants_37_5;
<*NOWARN*> PROCEDURE Word_insert_constants_37_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,6); END Word_insert_constants_37_6;
<*NOWARN*> PROCEDURE Word_insert_constants_37_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,7); END Word_insert_constants_37_7;
<*NOWARN*> PROCEDURE Word_insert_constants_37_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,8); END Word_insert_constants_37_8;
<*NOWARN*> PROCEDURE Word_insert_constants_37_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,9); END Word_insert_constants_37_9;
<*NOWARN*> PROCEDURE Word_insert_constants_37_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,10); END Word_insert_constants_37_10;
<*NOWARN*> PROCEDURE Word_insert_constants_37_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,11); END Word_insert_constants_37_11;
<*NOWARN*> PROCEDURE Word_insert_constants_37_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,12); END Word_insert_constants_37_12;
<*NOWARN*> PROCEDURE Word_insert_constants_37_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,13); END Word_insert_constants_37_13;
<*NOWARN*> PROCEDURE Word_insert_constants_37_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,14); END Word_insert_constants_37_14;
<*NOWARN*> PROCEDURE Word_insert_constants_37_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,15); END Word_insert_constants_37_15;
<*NOWARN*> PROCEDURE Word_insert_constants_37_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,16); END Word_insert_constants_37_16;
<*NOWARN*> PROCEDURE Word_insert_constants_37_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,17); END Word_insert_constants_37_17;
<*NOWARN*> PROCEDURE Word_insert_constants_37_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,18); END Word_insert_constants_37_18;
<*NOWARN*> PROCEDURE Word_insert_constants_37_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,19); END Word_insert_constants_37_19;
<*NOWARN*> PROCEDURE Word_insert_constants_37_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,20); END Word_insert_constants_37_20;
<*NOWARN*> PROCEDURE Word_insert_constants_37_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,21); END Word_insert_constants_37_21;
<*NOWARN*> PROCEDURE Word_insert_constants_37_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,22); END Word_insert_constants_37_22;
<*NOWARN*> PROCEDURE Word_insert_constants_37_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,23); END Word_insert_constants_37_23;
<*NOWARN*> PROCEDURE Word_insert_constants_37_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,24); END Word_insert_constants_37_24;
<*NOWARN*> PROCEDURE Word_insert_constants_37_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,25); END Word_insert_constants_37_25;
<*NOWARN*> PROCEDURE Word_insert_constants_37_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,26); END Word_insert_constants_37_26;
<*NOWARN*> PROCEDURE Word_insert_constants_37_27(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,37,27); END Word_insert_constants_37_27;
<*NOWARN*> PROCEDURE Word_insert_constants_38_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,0); END Word_insert_constants_38_0;
<*NOWARN*> PROCEDURE Word_insert_constants_38_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,1); END Word_insert_constants_38_1;
<*NOWARN*> PROCEDURE Word_insert_constants_38_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,2); END Word_insert_constants_38_2;
<*NOWARN*> PROCEDURE Word_insert_constants_38_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,3); END Word_insert_constants_38_3;
<*NOWARN*> PROCEDURE Word_insert_constants_38_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,4); END Word_insert_constants_38_4;
<*NOWARN*> PROCEDURE Word_insert_constants_38_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,5); END Word_insert_constants_38_5;
<*NOWARN*> PROCEDURE Word_insert_constants_38_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,6); END Word_insert_constants_38_6;
<*NOWARN*> PROCEDURE Word_insert_constants_38_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,7); END Word_insert_constants_38_7;
<*NOWARN*> PROCEDURE Word_insert_constants_38_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,8); END Word_insert_constants_38_8;
<*NOWARN*> PROCEDURE Word_insert_constants_38_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,9); END Word_insert_constants_38_9;
<*NOWARN*> PROCEDURE Word_insert_constants_38_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,10); END Word_insert_constants_38_10;
<*NOWARN*> PROCEDURE Word_insert_constants_38_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,11); END Word_insert_constants_38_11;
<*NOWARN*> PROCEDURE Word_insert_constants_38_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,12); END Word_insert_constants_38_12;
<*NOWARN*> PROCEDURE Word_insert_constants_38_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,13); END Word_insert_constants_38_13;
<*NOWARN*> PROCEDURE Word_insert_constants_38_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,14); END Word_insert_constants_38_14;
<*NOWARN*> PROCEDURE Word_insert_constants_38_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,15); END Word_insert_constants_38_15;
<*NOWARN*> PROCEDURE Word_insert_constants_38_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,16); END Word_insert_constants_38_16;
<*NOWARN*> PROCEDURE Word_insert_constants_38_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,17); END Word_insert_constants_38_17;
<*NOWARN*> PROCEDURE Word_insert_constants_38_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,18); END Word_insert_constants_38_18;
<*NOWARN*> PROCEDURE Word_insert_constants_38_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,19); END Word_insert_constants_38_19;
<*NOWARN*> PROCEDURE Word_insert_constants_38_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,20); END Word_insert_constants_38_20;
<*NOWARN*> PROCEDURE Word_insert_constants_38_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,21); END Word_insert_constants_38_21;
<*NOWARN*> PROCEDURE Word_insert_constants_38_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,22); END Word_insert_constants_38_22;
<*NOWARN*> PROCEDURE Word_insert_constants_38_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,23); END Word_insert_constants_38_23;
<*NOWARN*> PROCEDURE Word_insert_constants_38_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,24); END Word_insert_constants_38_24;
<*NOWARN*> PROCEDURE Word_insert_constants_38_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,25); END Word_insert_constants_38_25;
<*NOWARN*> PROCEDURE Word_insert_constants_38_26(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,38,26); END Word_insert_constants_38_26;
<*NOWARN*> PROCEDURE Word_insert_constants_39_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,0); END Word_insert_constants_39_0;
<*NOWARN*> PROCEDURE Word_insert_constants_39_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,1); END Word_insert_constants_39_1;
<*NOWARN*> PROCEDURE Word_insert_constants_39_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,2); END Word_insert_constants_39_2;
<*NOWARN*> PROCEDURE Word_insert_constants_39_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,3); END Word_insert_constants_39_3;
<*NOWARN*> PROCEDURE Word_insert_constants_39_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,4); END Word_insert_constants_39_4;
<*NOWARN*> PROCEDURE Word_insert_constants_39_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,5); END Word_insert_constants_39_5;
<*NOWARN*> PROCEDURE Word_insert_constants_39_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,6); END Word_insert_constants_39_6;
<*NOWARN*> PROCEDURE Word_insert_constants_39_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,7); END Word_insert_constants_39_7;
<*NOWARN*> PROCEDURE Word_insert_constants_39_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,8); END Word_insert_constants_39_8;
<*NOWARN*> PROCEDURE Word_insert_constants_39_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,9); END Word_insert_constants_39_9;
<*NOWARN*> PROCEDURE Word_insert_constants_39_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,10); END Word_insert_constants_39_10;
<*NOWARN*> PROCEDURE Word_insert_constants_39_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,11); END Word_insert_constants_39_11;
<*NOWARN*> PROCEDURE Word_insert_constants_39_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,12); END Word_insert_constants_39_12;
<*NOWARN*> PROCEDURE Word_insert_constants_39_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,13); END Word_insert_constants_39_13;
<*NOWARN*> PROCEDURE Word_insert_constants_39_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,14); END Word_insert_constants_39_14;
<*NOWARN*> PROCEDURE Word_insert_constants_39_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,15); END Word_insert_constants_39_15;
<*NOWARN*> PROCEDURE Word_insert_constants_39_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,16); END Word_insert_constants_39_16;
<*NOWARN*> PROCEDURE Word_insert_constants_39_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,17); END Word_insert_constants_39_17;
<*NOWARN*> PROCEDURE Word_insert_constants_39_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,18); END Word_insert_constants_39_18;
<*NOWARN*> PROCEDURE Word_insert_constants_39_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,19); END Word_insert_constants_39_19;
<*NOWARN*> PROCEDURE Word_insert_constants_39_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,20); END Word_insert_constants_39_20;
<*NOWARN*> PROCEDURE Word_insert_constants_39_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,21); END Word_insert_constants_39_21;
<*NOWARN*> PROCEDURE Word_insert_constants_39_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,22); END Word_insert_constants_39_22;
<*NOWARN*> PROCEDURE Word_insert_constants_39_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,23); END Word_insert_constants_39_23;
<*NOWARN*> PROCEDURE Word_insert_constants_39_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,24); END Word_insert_constants_39_24;
<*NOWARN*> PROCEDURE Word_insert_constants_39_25(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,39,25); END Word_insert_constants_39_25;
<*NOWARN*> PROCEDURE Word_insert_constants_40_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,0); END Word_insert_constants_40_0;
<*NOWARN*> PROCEDURE Word_insert_constants_40_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,1); END Word_insert_constants_40_1;
<*NOWARN*> PROCEDURE Word_insert_constants_40_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,2); END Word_insert_constants_40_2;
<*NOWARN*> PROCEDURE Word_insert_constants_40_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,3); END Word_insert_constants_40_3;
<*NOWARN*> PROCEDURE Word_insert_constants_40_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,4); END Word_insert_constants_40_4;
<*NOWARN*> PROCEDURE Word_insert_constants_40_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,5); END Word_insert_constants_40_5;
<*NOWARN*> PROCEDURE Word_insert_constants_40_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,6); END Word_insert_constants_40_6;
<*NOWARN*> PROCEDURE Word_insert_constants_40_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,7); END Word_insert_constants_40_7;
<*NOWARN*> PROCEDURE Word_insert_constants_40_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,8); END Word_insert_constants_40_8;
<*NOWARN*> PROCEDURE Word_insert_constants_40_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,9); END Word_insert_constants_40_9;
<*NOWARN*> PROCEDURE Word_insert_constants_40_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,10); END Word_insert_constants_40_10;
<*NOWARN*> PROCEDURE Word_insert_constants_40_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,11); END Word_insert_constants_40_11;
<*NOWARN*> PROCEDURE Word_insert_constants_40_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,12); END Word_insert_constants_40_12;
<*NOWARN*> PROCEDURE Word_insert_constants_40_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,13); END Word_insert_constants_40_13;
<*NOWARN*> PROCEDURE Word_insert_constants_40_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,14); END Word_insert_constants_40_14;
<*NOWARN*> PROCEDURE Word_insert_constants_40_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,15); END Word_insert_constants_40_15;
<*NOWARN*> PROCEDURE Word_insert_constants_40_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,16); END Word_insert_constants_40_16;
<*NOWARN*> PROCEDURE Word_insert_constants_40_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,17); END Word_insert_constants_40_17;
<*NOWARN*> PROCEDURE Word_insert_constants_40_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,18); END Word_insert_constants_40_18;
<*NOWARN*> PROCEDURE Word_insert_constants_40_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,19); END Word_insert_constants_40_19;
<*NOWARN*> PROCEDURE Word_insert_constants_40_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,20); END Word_insert_constants_40_20;
<*NOWARN*> PROCEDURE Word_insert_constants_40_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,21); END Word_insert_constants_40_21;
<*NOWARN*> PROCEDURE Word_insert_constants_40_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,22); END Word_insert_constants_40_22;
<*NOWARN*> PROCEDURE Word_insert_constants_40_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,23); END Word_insert_constants_40_23;
<*NOWARN*> PROCEDURE Word_insert_constants_40_24(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,40,24); END Word_insert_constants_40_24;
<*NOWARN*> PROCEDURE Word_insert_constants_41_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,0); END Word_insert_constants_41_0;
<*NOWARN*> PROCEDURE Word_insert_constants_41_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,1); END Word_insert_constants_41_1;
<*NOWARN*> PROCEDURE Word_insert_constants_41_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,2); END Word_insert_constants_41_2;
<*NOWARN*> PROCEDURE Word_insert_constants_41_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,3); END Word_insert_constants_41_3;
<*NOWARN*> PROCEDURE Word_insert_constants_41_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,4); END Word_insert_constants_41_4;
<*NOWARN*> PROCEDURE Word_insert_constants_41_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,5); END Word_insert_constants_41_5;
<*NOWARN*> PROCEDURE Word_insert_constants_41_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,6); END Word_insert_constants_41_6;
<*NOWARN*> PROCEDURE Word_insert_constants_41_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,7); END Word_insert_constants_41_7;
<*NOWARN*> PROCEDURE Word_insert_constants_41_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,8); END Word_insert_constants_41_8;
<*NOWARN*> PROCEDURE Word_insert_constants_41_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,9); END Word_insert_constants_41_9;
<*NOWARN*> PROCEDURE Word_insert_constants_41_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,10); END Word_insert_constants_41_10;
<*NOWARN*> PROCEDURE Word_insert_constants_41_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,11); END Word_insert_constants_41_11;
<*NOWARN*> PROCEDURE Word_insert_constants_41_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,12); END Word_insert_constants_41_12;
<*NOWARN*> PROCEDURE Word_insert_constants_41_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,13); END Word_insert_constants_41_13;
<*NOWARN*> PROCEDURE Word_insert_constants_41_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,14); END Word_insert_constants_41_14;
<*NOWARN*> PROCEDURE Word_insert_constants_41_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,15); END Word_insert_constants_41_15;
<*NOWARN*> PROCEDURE Word_insert_constants_41_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,16); END Word_insert_constants_41_16;
<*NOWARN*> PROCEDURE Word_insert_constants_41_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,17); END Word_insert_constants_41_17;
<*NOWARN*> PROCEDURE Word_insert_constants_41_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,18); END Word_insert_constants_41_18;
<*NOWARN*> PROCEDURE Word_insert_constants_41_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,19); END Word_insert_constants_41_19;
<*NOWARN*> PROCEDURE Word_insert_constants_41_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,20); END Word_insert_constants_41_20;
<*NOWARN*> PROCEDURE Word_insert_constants_41_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,21); END Word_insert_constants_41_21;
<*NOWARN*> PROCEDURE Word_insert_constants_41_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,22); END Word_insert_constants_41_22;
<*NOWARN*> PROCEDURE Word_insert_constants_41_23(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,41,23); END Word_insert_constants_41_23;
<*NOWARN*> PROCEDURE Word_insert_constants_42_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,0); END Word_insert_constants_42_0;
<*NOWARN*> PROCEDURE Word_insert_constants_42_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,1); END Word_insert_constants_42_1;
<*NOWARN*> PROCEDURE Word_insert_constants_42_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,2); END Word_insert_constants_42_2;
<*NOWARN*> PROCEDURE Word_insert_constants_42_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,3); END Word_insert_constants_42_3;
<*NOWARN*> PROCEDURE Word_insert_constants_42_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,4); END Word_insert_constants_42_4;
<*NOWARN*> PROCEDURE Word_insert_constants_42_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,5); END Word_insert_constants_42_5;
<*NOWARN*> PROCEDURE Word_insert_constants_42_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,6); END Word_insert_constants_42_6;
<*NOWARN*> PROCEDURE Word_insert_constants_42_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,7); END Word_insert_constants_42_7;
<*NOWARN*> PROCEDURE Word_insert_constants_42_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,8); END Word_insert_constants_42_8;
<*NOWARN*> PROCEDURE Word_insert_constants_42_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,9); END Word_insert_constants_42_9;
<*NOWARN*> PROCEDURE Word_insert_constants_42_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,10); END Word_insert_constants_42_10;
<*NOWARN*> PROCEDURE Word_insert_constants_42_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,11); END Word_insert_constants_42_11;
<*NOWARN*> PROCEDURE Word_insert_constants_42_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,12); END Word_insert_constants_42_12;
<*NOWARN*> PROCEDURE Word_insert_constants_42_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,13); END Word_insert_constants_42_13;
<*NOWARN*> PROCEDURE Word_insert_constants_42_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,14); END Word_insert_constants_42_14;
<*NOWARN*> PROCEDURE Word_insert_constants_42_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,15); END Word_insert_constants_42_15;
<*NOWARN*> PROCEDURE Word_insert_constants_42_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,16); END Word_insert_constants_42_16;
<*NOWARN*> PROCEDURE Word_insert_constants_42_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,17); END Word_insert_constants_42_17;
<*NOWARN*> PROCEDURE Word_insert_constants_42_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,18); END Word_insert_constants_42_18;
<*NOWARN*> PROCEDURE Word_insert_constants_42_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,19); END Word_insert_constants_42_19;
<*NOWARN*> PROCEDURE Word_insert_constants_42_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,20); END Word_insert_constants_42_20;
<*NOWARN*> PROCEDURE Word_insert_constants_42_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,21); END Word_insert_constants_42_21;
<*NOWARN*> PROCEDURE Word_insert_constants_42_22(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,42,22); END Word_insert_constants_42_22;
<*NOWARN*> PROCEDURE Word_insert_constants_43_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,0); END Word_insert_constants_43_0;
<*NOWARN*> PROCEDURE Word_insert_constants_43_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,1); END Word_insert_constants_43_1;
<*NOWARN*> PROCEDURE Word_insert_constants_43_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,2); END Word_insert_constants_43_2;
<*NOWARN*> PROCEDURE Word_insert_constants_43_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,3); END Word_insert_constants_43_3;
<*NOWARN*> PROCEDURE Word_insert_constants_43_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,4); END Word_insert_constants_43_4;
<*NOWARN*> PROCEDURE Word_insert_constants_43_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,5); END Word_insert_constants_43_5;
<*NOWARN*> PROCEDURE Word_insert_constants_43_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,6); END Word_insert_constants_43_6;
<*NOWARN*> PROCEDURE Word_insert_constants_43_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,7); END Word_insert_constants_43_7;
<*NOWARN*> PROCEDURE Word_insert_constants_43_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,8); END Word_insert_constants_43_8;
<*NOWARN*> PROCEDURE Word_insert_constants_43_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,9); END Word_insert_constants_43_9;
<*NOWARN*> PROCEDURE Word_insert_constants_43_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,10); END Word_insert_constants_43_10;
<*NOWARN*> PROCEDURE Word_insert_constants_43_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,11); END Word_insert_constants_43_11;
<*NOWARN*> PROCEDURE Word_insert_constants_43_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,12); END Word_insert_constants_43_12;
<*NOWARN*> PROCEDURE Word_insert_constants_43_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,13); END Word_insert_constants_43_13;
<*NOWARN*> PROCEDURE Word_insert_constants_43_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,14); END Word_insert_constants_43_14;
<*NOWARN*> PROCEDURE Word_insert_constants_43_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,15); END Word_insert_constants_43_15;
<*NOWARN*> PROCEDURE Word_insert_constants_43_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,16); END Word_insert_constants_43_16;
<*NOWARN*> PROCEDURE Word_insert_constants_43_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,17); END Word_insert_constants_43_17;
<*NOWARN*> PROCEDURE Word_insert_constants_43_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,18); END Word_insert_constants_43_18;
<*NOWARN*> PROCEDURE Word_insert_constants_43_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,19); END Word_insert_constants_43_19;
<*NOWARN*> PROCEDURE Word_insert_constants_43_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,20); END Word_insert_constants_43_20;
<*NOWARN*> PROCEDURE Word_insert_constants_43_21(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,43,21); END Word_insert_constants_43_21;
<*NOWARN*> PROCEDURE Word_insert_constants_44_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,0); END Word_insert_constants_44_0;
<*NOWARN*> PROCEDURE Word_insert_constants_44_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,1); END Word_insert_constants_44_1;
<*NOWARN*> PROCEDURE Word_insert_constants_44_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,2); END Word_insert_constants_44_2;
<*NOWARN*> PROCEDURE Word_insert_constants_44_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,3); END Word_insert_constants_44_3;
<*NOWARN*> PROCEDURE Word_insert_constants_44_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,4); END Word_insert_constants_44_4;
<*NOWARN*> PROCEDURE Word_insert_constants_44_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,5); END Word_insert_constants_44_5;
<*NOWARN*> PROCEDURE Word_insert_constants_44_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,6); END Word_insert_constants_44_6;
<*NOWARN*> PROCEDURE Word_insert_constants_44_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,7); END Word_insert_constants_44_7;
<*NOWARN*> PROCEDURE Word_insert_constants_44_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,8); END Word_insert_constants_44_8;
<*NOWARN*> PROCEDURE Word_insert_constants_44_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,9); END Word_insert_constants_44_9;
<*NOWARN*> PROCEDURE Word_insert_constants_44_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,10); END Word_insert_constants_44_10;
<*NOWARN*> PROCEDURE Word_insert_constants_44_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,11); END Word_insert_constants_44_11;
<*NOWARN*> PROCEDURE Word_insert_constants_44_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,12); END Word_insert_constants_44_12;
<*NOWARN*> PROCEDURE Word_insert_constants_44_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,13); END Word_insert_constants_44_13;
<*NOWARN*> PROCEDURE Word_insert_constants_44_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,14); END Word_insert_constants_44_14;
<*NOWARN*> PROCEDURE Word_insert_constants_44_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,15); END Word_insert_constants_44_15;
<*NOWARN*> PROCEDURE Word_insert_constants_44_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,16); END Word_insert_constants_44_16;
<*NOWARN*> PROCEDURE Word_insert_constants_44_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,17); END Word_insert_constants_44_17;
<*NOWARN*> PROCEDURE Word_insert_constants_44_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,18); END Word_insert_constants_44_18;
<*NOWARN*> PROCEDURE Word_insert_constants_44_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,19); END Word_insert_constants_44_19;
<*NOWARN*> PROCEDURE Word_insert_constants_44_20(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,44,20); END Word_insert_constants_44_20;
<*NOWARN*> PROCEDURE Word_insert_constants_45_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,45,0); END Word_insert_constants_45_0;
<*NOWARN*> PROCEDURE Word_insert_constants_45_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,45,1); END Word_insert_constants_45_1;
<*NOWARN*> PROCEDURE Word_insert_constants_45_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,45,2); END Word_insert_constants_45_2;
<*NOWARN*> PROCEDURE Word_insert_constants_45_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,45,3); END Word_insert_constants_45_3;
<*NOWARN*> PROCEDURE Word_insert_constants_45_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,45,4); END Word_insert_constants_45_4;
<*NOWARN*> PROCEDURE Word_insert_constants_45_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,45,5); END Word_insert_constants_45_5;
<*NOWARN*> PROCEDURE Word_insert_constants_45_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,45,6); END Word_insert_constants_45_6;
<*NOWARN*> PROCEDURE Word_insert_constants_45_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,45,7); END Word_insert_constants_45_7;
<*NOWARN*> PROCEDURE Word_insert_constants_45_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,45,8); END Word_insert_constants_45_8;
<*NOWARN*> PROCEDURE Word_insert_constants_45_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,45,9); END Word_insert_constants_45_9;
<*NOWARN*> PROCEDURE Word_insert_constants_45_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,45,10); END Word_insert_constants_45_10;
<*NOWARN*> PROCEDURE Word_insert_constants_45_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,45,11); END Word_insert_constants_45_11;
<*NOWARN*> PROCEDURE Word_insert_constants_45_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,45,12); END Word_insert_constants_45_12;
<*NOWARN*> PROCEDURE Word_insert_constants_45_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,45,13); END Word_insert_constants_45_13;
<*NOWARN*> PROCEDURE Word_insert_constants_45_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,45,14); END Word_insert_constants_45_14;
<*NOWARN*> PROCEDURE Word_insert_constants_45_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,45,15); END Word_insert_constants_45_15;
<*NOWARN*> PROCEDURE Word_insert_constants_45_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,45,16); END Word_insert_constants_45_16;
<*NOWARN*> PROCEDURE Word_insert_constants_45_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,45,17); END Word_insert_constants_45_17;
<*NOWARN*> PROCEDURE Word_insert_constants_45_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,45,18); END Word_insert_constants_45_18;
<*NOWARN*> PROCEDURE Word_insert_constants_45_19(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,45,19); END Word_insert_constants_45_19;
<*NOWARN*> PROCEDURE Word_insert_constants_46_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,46,0); END Word_insert_constants_46_0;
<*NOWARN*> PROCEDURE Word_insert_constants_46_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,46,1); END Word_insert_constants_46_1;
<*NOWARN*> PROCEDURE Word_insert_constants_46_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,46,2); END Word_insert_constants_46_2;
<*NOWARN*> PROCEDURE Word_insert_constants_46_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,46,3); END Word_insert_constants_46_3;
<*NOWARN*> PROCEDURE Word_insert_constants_46_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,46,4); END Word_insert_constants_46_4;
<*NOWARN*> PROCEDURE Word_insert_constants_46_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,46,5); END Word_insert_constants_46_5;
<*NOWARN*> PROCEDURE Word_insert_constants_46_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,46,6); END Word_insert_constants_46_6;
<*NOWARN*> PROCEDURE Word_insert_constants_46_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,46,7); END Word_insert_constants_46_7;
<*NOWARN*> PROCEDURE Word_insert_constants_46_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,46,8); END Word_insert_constants_46_8;
<*NOWARN*> PROCEDURE Word_insert_constants_46_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,46,9); END Word_insert_constants_46_9;
<*NOWARN*> PROCEDURE Word_insert_constants_46_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,46,10); END Word_insert_constants_46_10;
<*NOWARN*> PROCEDURE Word_insert_constants_46_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,46,11); END Word_insert_constants_46_11;
<*NOWARN*> PROCEDURE Word_insert_constants_46_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,46,12); END Word_insert_constants_46_12;
<*NOWARN*> PROCEDURE Word_insert_constants_46_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,46,13); END Word_insert_constants_46_13;
<*NOWARN*> PROCEDURE Word_insert_constants_46_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,46,14); END Word_insert_constants_46_14;
<*NOWARN*> PROCEDURE Word_insert_constants_46_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,46,15); END Word_insert_constants_46_15;
<*NOWARN*> PROCEDURE Word_insert_constants_46_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,46,16); END Word_insert_constants_46_16;
<*NOWARN*> PROCEDURE Word_insert_constants_46_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,46,17); END Word_insert_constants_46_17;
<*NOWARN*> PROCEDURE Word_insert_constants_46_18(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,46,18); END Word_insert_constants_46_18;
<*NOWARN*> PROCEDURE Word_insert_constants_47_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,47,0); END Word_insert_constants_47_0;
<*NOWARN*> PROCEDURE Word_insert_constants_47_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,47,1); END Word_insert_constants_47_1;
<*NOWARN*> PROCEDURE Word_insert_constants_47_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,47,2); END Word_insert_constants_47_2;
<*NOWARN*> PROCEDURE Word_insert_constants_47_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,47,3); END Word_insert_constants_47_3;
<*NOWARN*> PROCEDURE Word_insert_constants_47_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,47,4); END Word_insert_constants_47_4;
<*NOWARN*> PROCEDURE Word_insert_constants_47_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,47,5); END Word_insert_constants_47_5;
<*NOWARN*> PROCEDURE Word_insert_constants_47_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,47,6); END Word_insert_constants_47_6;
<*NOWARN*> PROCEDURE Word_insert_constants_47_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,47,7); END Word_insert_constants_47_7;
<*NOWARN*> PROCEDURE Word_insert_constants_47_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,47,8); END Word_insert_constants_47_8;
<*NOWARN*> PROCEDURE Word_insert_constants_47_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,47,9); END Word_insert_constants_47_9;
<*NOWARN*> PROCEDURE Word_insert_constants_47_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,47,10); END Word_insert_constants_47_10;
<*NOWARN*> PROCEDURE Word_insert_constants_47_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,47,11); END Word_insert_constants_47_11;
<*NOWARN*> PROCEDURE Word_insert_constants_47_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,47,12); END Word_insert_constants_47_12;
<*NOWARN*> PROCEDURE Word_insert_constants_47_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,47,13); END Word_insert_constants_47_13;
<*NOWARN*> PROCEDURE Word_insert_constants_47_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,47,14); END Word_insert_constants_47_14;
<*NOWARN*> PROCEDURE Word_insert_constants_47_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,47,15); END Word_insert_constants_47_15;
<*NOWARN*> PROCEDURE Word_insert_constants_47_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,47,16); END Word_insert_constants_47_16;
<*NOWARN*> PROCEDURE Word_insert_constants_47_17(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,47,17); END Word_insert_constants_47_17;
<*NOWARN*> PROCEDURE Word_insert_constants_48_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,48,0); END Word_insert_constants_48_0;
<*NOWARN*> PROCEDURE Word_insert_constants_48_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,48,1); END Word_insert_constants_48_1;
<*NOWARN*> PROCEDURE Word_insert_constants_48_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,48,2); END Word_insert_constants_48_2;
<*NOWARN*> PROCEDURE Word_insert_constants_48_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,48,3); END Word_insert_constants_48_3;
<*NOWARN*> PROCEDURE Word_insert_constants_48_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,48,4); END Word_insert_constants_48_4;
<*NOWARN*> PROCEDURE Word_insert_constants_48_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,48,5); END Word_insert_constants_48_5;
<*NOWARN*> PROCEDURE Word_insert_constants_48_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,48,6); END Word_insert_constants_48_6;
<*NOWARN*> PROCEDURE Word_insert_constants_48_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,48,7); END Word_insert_constants_48_7;
<*NOWARN*> PROCEDURE Word_insert_constants_48_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,48,8); END Word_insert_constants_48_8;
<*NOWARN*> PROCEDURE Word_insert_constants_48_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,48,9); END Word_insert_constants_48_9;
<*NOWARN*> PROCEDURE Word_insert_constants_48_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,48,10); END Word_insert_constants_48_10;
<*NOWARN*> PROCEDURE Word_insert_constants_48_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,48,11); END Word_insert_constants_48_11;
<*NOWARN*> PROCEDURE Word_insert_constants_48_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,48,12); END Word_insert_constants_48_12;
<*NOWARN*> PROCEDURE Word_insert_constants_48_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,48,13); END Word_insert_constants_48_13;
<*NOWARN*> PROCEDURE Word_insert_constants_48_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,48,14); END Word_insert_constants_48_14;
<*NOWARN*> PROCEDURE Word_insert_constants_48_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,48,15); END Word_insert_constants_48_15;
<*NOWARN*> PROCEDURE Word_insert_constants_48_16(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,48,16); END Word_insert_constants_48_16;
<*NOWARN*> PROCEDURE Word_insert_constants_49_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,49,0); END Word_insert_constants_49_0;
<*NOWARN*> PROCEDURE Word_insert_constants_49_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,49,1); END Word_insert_constants_49_1;
<*NOWARN*> PROCEDURE Word_insert_constants_49_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,49,2); END Word_insert_constants_49_2;
<*NOWARN*> PROCEDURE Word_insert_constants_49_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,49,3); END Word_insert_constants_49_3;
<*NOWARN*> PROCEDURE Word_insert_constants_49_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,49,4); END Word_insert_constants_49_4;
<*NOWARN*> PROCEDURE Word_insert_constants_49_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,49,5); END Word_insert_constants_49_5;
<*NOWARN*> PROCEDURE Word_insert_constants_49_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,49,6); END Word_insert_constants_49_6;
<*NOWARN*> PROCEDURE Word_insert_constants_49_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,49,7); END Word_insert_constants_49_7;
<*NOWARN*> PROCEDURE Word_insert_constants_49_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,49,8); END Word_insert_constants_49_8;
<*NOWARN*> PROCEDURE Word_insert_constants_49_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,49,9); END Word_insert_constants_49_9;
<*NOWARN*> PROCEDURE Word_insert_constants_49_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,49,10); END Word_insert_constants_49_10;
<*NOWARN*> PROCEDURE Word_insert_constants_49_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,49,11); END Word_insert_constants_49_11;
<*NOWARN*> PROCEDURE Word_insert_constants_49_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,49,12); END Word_insert_constants_49_12;
<*NOWARN*> PROCEDURE Word_insert_constants_49_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,49,13); END Word_insert_constants_49_13;
<*NOWARN*> PROCEDURE Word_insert_constants_49_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,49,14); END Word_insert_constants_49_14;
<*NOWARN*> PROCEDURE Word_insert_constants_49_15(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,49,15); END Word_insert_constants_49_15;
<*NOWARN*> PROCEDURE Word_insert_constants_50_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,50,0); END Word_insert_constants_50_0;
<*NOWARN*> PROCEDURE Word_insert_constants_50_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,50,1); END Word_insert_constants_50_1;
<*NOWARN*> PROCEDURE Word_insert_constants_50_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,50,2); END Word_insert_constants_50_2;
<*NOWARN*> PROCEDURE Word_insert_constants_50_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,50,3); END Word_insert_constants_50_3;
<*NOWARN*> PROCEDURE Word_insert_constants_50_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,50,4); END Word_insert_constants_50_4;
<*NOWARN*> PROCEDURE Word_insert_constants_50_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,50,5); END Word_insert_constants_50_5;
<*NOWARN*> PROCEDURE Word_insert_constants_50_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,50,6); END Word_insert_constants_50_6;
<*NOWARN*> PROCEDURE Word_insert_constants_50_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,50,7); END Word_insert_constants_50_7;
<*NOWARN*> PROCEDURE Word_insert_constants_50_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,50,8); END Word_insert_constants_50_8;
<*NOWARN*> PROCEDURE Word_insert_constants_50_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,50,9); END Word_insert_constants_50_9;
<*NOWARN*> PROCEDURE Word_insert_constants_50_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,50,10); END Word_insert_constants_50_10;
<*NOWARN*> PROCEDURE Word_insert_constants_50_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,50,11); END Word_insert_constants_50_11;
<*NOWARN*> PROCEDURE Word_insert_constants_50_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,50,12); END Word_insert_constants_50_12;
<*NOWARN*> PROCEDURE Word_insert_constants_50_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,50,13); END Word_insert_constants_50_13;
<*NOWARN*> PROCEDURE Word_insert_constants_50_14(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,50,14); END Word_insert_constants_50_14;
<*NOWARN*> PROCEDURE Word_insert_constants_51_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,51,0); END Word_insert_constants_51_0;
<*NOWARN*> PROCEDURE Word_insert_constants_51_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,51,1); END Word_insert_constants_51_1;
<*NOWARN*> PROCEDURE Word_insert_constants_51_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,51,2); END Word_insert_constants_51_2;
<*NOWARN*> PROCEDURE Word_insert_constants_51_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,51,3); END Word_insert_constants_51_3;
<*NOWARN*> PROCEDURE Word_insert_constants_51_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,51,4); END Word_insert_constants_51_4;
<*NOWARN*> PROCEDURE Word_insert_constants_51_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,51,5); END Word_insert_constants_51_5;
<*NOWARN*> PROCEDURE Word_insert_constants_51_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,51,6); END Word_insert_constants_51_6;
<*NOWARN*> PROCEDURE Word_insert_constants_51_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,51,7); END Word_insert_constants_51_7;
<*NOWARN*> PROCEDURE Word_insert_constants_51_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,51,8); END Word_insert_constants_51_8;
<*NOWARN*> PROCEDURE Word_insert_constants_51_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,51,9); END Word_insert_constants_51_9;
<*NOWARN*> PROCEDURE Word_insert_constants_51_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,51,10); END Word_insert_constants_51_10;
<*NOWARN*> PROCEDURE Word_insert_constants_51_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,51,11); END Word_insert_constants_51_11;
<*NOWARN*> PROCEDURE Word_insert_constants_51_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,51,12); END Word_insert_constants_51_12;
<*NOWARN*> PROCEDURE Word_insert_constants_51_13(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,51,13); END Word_insert_constants_51_13;
<*NOWARN*> PROCEDURE Word_insert_constants_52_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,52,0); END Word_insert_constants_52_0;
<*NOWARN*> PROCEDURE Word_insert_constants_52_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,52,1); END Word_insert_constants_52_1;
<*NOWARN*> PROCEDURE Word_insert_constants_52_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,52,2); END Word_insert_constants_52_2;
<*NOWARN*> PROCEDURE Word_insert_constants_52_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,52,3); END Word_insert_constants_52_3;
<*NOWARN*> PROCEDURE Word_insert_constants_52_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,52,4); END Word_insert_constants_52_4;
<*NOWARN*> PROCEDURE Word_insert_constants_52_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,52,5); END Word_insert_constants_52_5;
<*NOWARN*> PROCEDURE Word_insert_constants_52_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,52,6); END Word_insert_constants_52_6;
<*NOWARN*> PROCEDURE Word_insert_constants_52_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,52,7); END Word_insert_constants_52_7;
<*NOWARN*> PROCEDURE Word_insert_constants_52_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,52,8); END Word_insert_constants_52_8;
<*NOWARN*> PROCEDURE Word_insert_constants_52_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,52,9); END Word_insert_constants_52_9;
<*NOWARN*> PROCEDURE Word_insert_constants_52_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,52,10); END Word_insert_constants_52_10;
<*NOWARN*> PROCEDURE Word_insert_constants_52_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,52,11); END Word_insert_constants_52_11;
<*NOWARN*> PROCEDURE Word_insert_constants_52_12(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,52,12); END Word_insert_constants_52_12;
<*NOWARN*> PROCEDURE Word_insert_constants_53_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,53,0); END Word_insert_constants_53_0;
<*NOWARN*> PROCEDURE Word_insert_constants_53_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,53,1); END Word_insert_constants_53_1;
<*NOWARN*> PROCEDURE Word_insert_constants_53_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,53,2); END Word_insert_constants_53_2;
<*NOWARN*> PROCEDURE Word_insert_constants_53_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,53,3); END Word_insert_constants_53_3;
<*NOWARN*> PROCEDURE Word_insert_constants_53_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,53,4); END Word_insert_constants_53_4;
<*NOWARN*> PROCEDURE Word_insert_constants_53_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,53,5); END Word_insert_constants_53_5;
<*NOWARN*> PROCEDURE Word_insert_constants_53_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,53,6); END Word_insert_constants_53_6;
<*NOWARN*> PROCEDURE Word_insert_constants_53_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,53,7); END Word_insert_constants_53_7;
<*NOWARN*> PROCEDURE Word_insert_constants_53_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,53,8); END Word_insert_constants_53_8;
<*NOWARN*> PROCEDURE Word_insert_constants_53_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,53,9); END Word_insert_constants_53_9;
<*NOWARN*> PROCEDURE Word_insert_constants_53_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,53,10); END Word_insert_constants_53_10;
<*NOWARN*> PROCEDURE Word_insert_constants_53_11(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,53,11); END Word_insert_constants_53_11;
<*NOWARN*> PROCEDURE Word_insert_constants_54_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,54,0); END Word_insert_constants_54_0;
<*NOWARN*> PROCEDURE Word_insert_constants_54_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,54,1); END Word_insert_constants_54_1;
<*NOWARN*> PROCEDURE Word_insert_constants_54_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,54,2); END Word_insert_constants_54_2;
<*NOWARN*> PROCEDURE Word_insert_constants_54_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,54,3); END Word_insert_constants_54_3;
<*NOWARN*> PROCEDURE Word_insert_constants_54_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,54,4); END Word_insert_constants_54_4;
<*NOWARN*> PROCEDURE Word_insert_constants_54_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,54,5); END Word_insert_constants_54_5;
<*NOWARN*> PROCEDURE Word_insert_constants_54_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,54,6); END Word_insert_constants_54_6;
<*NOWARN*> PROCEDURE Word_insert_constants_54_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,54,7); END Word_insert_constants_54_7;
<*NOWARN*> PROCEDURE Word_insert_constants_54_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,54,8); END Word_insert_constants_54_8;
<*NOWARN*> PROCEDURE Word_insert_constants_54_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,54,9); END Word_insert_constants_54_9;
<*NOWARN*> PROCEDURE Word_insert_constants_54_10(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,54,10); END Word_insert_constants_54_10;
<*NOWARN*> PROCEDURE Word_insert_constants_55_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,55,0); END Word_insert_constants_55_0;
<*NOWARN*> PROCEDURE Word_insert_constants_55_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,55,1); END Word_insert_constants_55_1;
<*NOWARN*> PROCEDURE Word_insert_constants_55_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,55,2); END Word_insert_constants_55_2;
<*NOWARN*> PROCEDURE Word_insert_constants_55_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,55,3); END Word_insert_constants_55_3;
<*NOWARN*> PROCEDURE Word_insert_constants_55_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,55,4); END Word_insert_constants_55_4;
<*NOWARN*> PROCEDURE Word_insert_constants_55_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,55,5); END Word_insert_constants_55_5;
<*NOWARN*> PROCEDURE Word_insert_constants_55_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,55,6); END Word_insert_constants_55_6;
<*NOWARN*> PROCEDURE Word_insert_constants_55_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,55,7); END Word_insert_constants_55_7;
<*NOWARN*> PROCEDURE Word_insert_constants_55_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,55,8); END Word_insert_constants_55_8;
<*NOWARN*> PROCEDURE Word_insert_constants_55_9(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,55,9); END Word_insert_constants_55_9;
<*NOWARN*> PROCEDURE Word_insert_constants_56_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,56,0); END Word_insert_constants_56_0;
<*NOWARN*> PROCEDURE Word_insert_constants_56_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,56,1); END Word_insert_constants_56_1;
<*NOWARN*> PROCEDURE Word_insert_constants_56_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,56,2); END Word_insert_constants_56_2;
<*NOWARN*> PROCEDURE Word_insert_constants_56_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,56,3); END Word_insert_constants_56_3;
<*NOWARN*> PROCEDURE Word_insert_constants_56_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,56,4); END Word_insert_constants_56_4;
<*NOWARN*> PROCEDURE Word_insert_constants_56_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,56,5); END Word_insert_constants_56_5;
<*NOWARN*> PROCEDURE Word_insert_constants_56_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,56,6); END Word_insert_constants_56_6;
<*NOWARN*> PROCEDURE Word_insert_constants_56_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,56,7); END Word_insert_constants_56_7;
<*NOWARN*> PROCEDURE Word_insert_constants_56_8(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,56,8); END Word_insert_constants_56_8;
<*NOWARN*> PROCEDURE Word_insert_constants_57_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,57,0); END Word_insert_constants_57_0;
<*NOWARN*> PROCEDURE Word_insert_constants_57_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,57,1); END Word_insert_constants_57_1;
<*NOWARN*> PROCEDURE Word_insert_constants_57_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,57,2); END Word_insert_constants_57_2;
<*NOWARN*> PROCEDURE Word_insert_constants_57_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,57,3); END Word_insert_constants_57_3;
<*NOWARN*> PROCEDURE Word_insert_constants_57_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,57,4); END Word_insert_constants_57_4;
<*NOWARN*> PROCEDURE Word_insert_constants_57_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,57,5); END Word_insert_constants_57_5;
<*NOWARN*> PROCEDURE Word_insert_constants_57_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,57,6); END Word_insert_constants_57_6;
<*NOWARN*> PROCEDURE Word_insert_constants_57_7(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,57,7); END Word_insert_constants_57_7;
<*NOWARN*> PROCEDURE Word_insert_constants_58_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,58,0); END Word_insert_constants_58_0;
<*NOWARN*> PROCEDURE Word_insert_constants_58_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,58,1); END Word_insert_constants_58_1;
<*NOWARN*> PROCEDURE Word_insert_constants_58_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,58,2); END Word_insert_constants_58_2;
<*NOWARN*> PROCEDURE Word_insert_constants_58_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,58,3); END Word_insert_constants_58_3;
<*NOWARN*> PROCEDURE Word_insert_constants_58_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,58,4); END Word_insert_constants_58_4;
<*NOWARN*> PROCEDURE Word_insert_constants_58_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,58,5); END Word_insert_constants_58_5;
<*NOWARN*> PROCEDURE Word_insert_constants_58_6(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,58,6); END Word_insert_constants_58_6;
<*NOWARN*> PROCEDURE Word_insert_constants_59_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,59,0); END Word_insert_constants_59_0;
<*NOWARN*> PROCEDURE Word_insert_constants_59_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,59,1); END Word_insert_constants_59_1;
<*NOWARN*> PROCEDURE Word_insert_constants_59_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,59,2); END Word_insert_constants_59_2;
<*NOWARN*> PROCEDURE Word_insert_constants_59_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,59,3); END Word_insert_constants_59_3;
<*NOWARN*> PROCEDURE Word_insert_constants_59_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,59,4); END Word_insert_constants_59_4;
<*NOWARN*> PROCEDURE Word_insert_constants_59_5(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,59,5); END Word_insert_constants_59_5;
<*NOWARN*> PROCEDURE Word_insert_constants_60_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,60,0); END Word_insert_constants_60_0;
<*NOWARN*> PROCEDURE Word_insert_constants_60_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,60,1); END Word_insert_constants_60_1;
<*NOWARN*> PROCEDURE Word_insert_constants_60_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,60,2); END Word_insert_constants_60_2;
<*NOWARN*> PROCEDURE Word_insert_constants_60_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,60,3); END Word_insert_constants_60_3;
<*NOWARN*> PROCEDURE Word_insert_constants_60_4(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,60,4); END Word_insert_constants_60_4;
<*NOWARN*> PROCEDURE Word_insert_constants_61_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,61,0); END Word_insert_constants_61_0;
<*NOWARN*> PROCEDURE Word_insert_constants_61_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,61,1); END Word_insert_constants_61_1;
<*NOWARN*> PROCEDURE Word_insert_constants_61_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,61,2); END Word_insert_constants_61_2;
<*NOWARN*> PROCEDURE Word_insert_constants_61_3(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,61,3); END Word_insert_constants_61_3;
<*NOWARN*> PROCEDURE Word_insert_constants_62_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,62,0); END Word_insert_constants_62_0;
<*NOWARN*> PROCEDURE Word_insert_constants_62_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,62,1); END Word_insert_constants_62_1;
<*NOWARN*> PROCEDURE Word_insert_constants_62_2(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,62,2); END Word_insert_constants_62_2;
<*NOWARN*> PROCEDURE Word_insert_constants_63_0(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,63,0); END Word_insert_constants_63_0;
<*NOWARN*> PROCEDURE Word_insert_constants_63_1(a,b:Word.T):Word.T=
           BEGIN RETURN Word.Insert(a,b,63,1); END Word_insert_constants_63_1;
<*NOWARN*> PROCEDURE Long_insert_constants_0_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,0); END Long_insert_constants_0_0;
<*NOWARN*> PROCEDURE Long_insert_constants_0_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,1); END Long_insert_constants_0_1;
<*NOWARN*> PROCEDURE Long_insert_constants_0_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,2); END Long_insert_constants_0_2;
<*NOWARN*> PROCEDURE Long_insert_constants_0_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,3); END Long_insert_constants_0_3;
<*NOWARN*> PROCEDURE Long_insert_constants_0_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,4); END Long_insert_constants_0_4;
<*NOWARN*> PROCEDURE Long_insert_constants_0_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,5); END Long_insert_constants_0_5;
<*NOWARN*> PROCEDURE Long_insert_constants_0_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,6); END Long_insert_constants_0_6;
<*NOWARN*> PROCEDURE Long_insert_constants_0_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,7); END Long_insert_constants_0_7;
<*NOWARN*> PROCEDURE Long_insert_constants_0_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,8); END Long_insert_constants_0_8;
<*NOWARN*> PROCEDURE Long_insert_constants_0_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,9); END Long_insert_constants_0_9;
<*NOWARN*> PROCEDURE Long_insert_constants_0_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,10); END Long_insert_constants_0_10;
<*NOWARN*> PROCEDURE Long_insert_constants_0_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,11); END Long_insert_constants_0_11;
<*NOWARN*> PROCEDURE Long_insert_constants_0_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,12); END Long_insert_constants_0_12;
<*NOWARN*> PROCEDURE Long_insert_constants_0_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,13); END Long_insert_constants_0_13;
<*NOWARN*> PROCEDURE Long_insert_constants_0_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,14); END Long_insert_constants_0_14;
<*NOWARN*> PROCEDURE Long_insert_constants_0_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,15); END Long_insert_constants_0_15;
<*NOWARN*> PROCEDURE Long_insert_constants_0_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,16); END Long_insert_constants_0_16;
<*NOWARN*> PROCEDURE Long_insert_constants_0_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,17); END Long_insert_constants_0_17;
<*NOWARN*> PROCEDURE Long_insert_constants_0_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,18); END Long_insert_constants_0_18;
<*NOWARN*> PROCEDURE Long_insert_constants_0_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,19); END Long_insert_constants_0_19;
<*NOWARN*> PROCEDURE Long_insert_constants_0_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,20); END Long_insert_constants_0_20;
<*NOWARN*> PROCEDURE Long_insert_constants_0_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,21); END Long_insert_constants_0_21;
<*NOWARN*> PROCEDURE Long_insert_constants_0_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,22); END Long_insert_constants_0_22;
<*NOWARN*> PROCEDURE Long_insert_constants_0_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,23); END Long_insert_constants_0_23;
<*NOWARN*> PROCEDURE Long_insert_constants_0_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,24); END Long_insert_constants_0_24;
<*NOWARN*> PROCEDURE Long_insert_constants_0_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,25); END Long_insert_constants_0_25;
<*NOWARN*> PROCEDURE Long_insert_constants_0_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,26); END Long_insert_constants_0_26;
<*NOWARN*> PROCEDURE Long_insert_constants_0_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,27); END Long_insert_constants_0_27;
<*NOWARN*> PROCEDURE Long_insert_constants_0_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,28); END Long_insert_constants_0_28;
<*NOWARN*> PROCEDURE Long_insert_constants_0_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,29); END Long_insert_constants_0_29;
<*NOWARN*> PROCEDURE Long_insert_constants_0_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,30); END Long_insert_constants_0_30;
<*NOWARN*> PROCEDURE Long_insert_constants_0_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,31); END Long_insert_constants_0_31;
<*NOWARN*> PROCEDURE Long_insert_constants_0_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,32); END Long_insert_constants_0_32;
<*NOWARN*> PROCEDURE Long_insert_constants_0_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,33); END Long_insert_constants_0_33;
<*NOWARN*> PROCEDURE Long_insert_constants_0_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,34); END Long_insert_constants_0_34;
<*NOWARN*> PROCEDURE Long_insert_constants_0_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,35); END Long_insert_constants_0_35;
<*NOWARN*> PROCEDURE Long_insert_constants_0_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,36); END Long_insert_constants_0_36;
<*NOWARN*> PROCEDURE Long_insert_constants_0_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,37); END Long_insert_constants_0_37;
<*NOWARN*> PROCEDURE Long_insert_constants_0_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,38); END Long_insert_constants_0_38;
<*NOWARN*> PROCEDURE Long_insert_constants_0_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,39); END Long_insert_constants_0_39;
<*NOWARN*> PROCEDURE Long_insert_constants_0_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,40); END Long_insert_constants_0_40;
<*NOWARN*> PROCEDURE Long_insert_constants_0_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,41); END Long_insert_constants_0_41;
<*NOWARN*> PROCEDURE Long_insert_constants_0_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,42); END Long_insert_constants_0_42;
<*NOWARN*> PROCEDURE Long_insert_constants_0_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,43); END Long_insert_constants_0_43;
<*NOWARN*> PROCEDURE Long_insert_constants_0_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,44); END Long_insert_constants_0_44;
<*NOWARN*> PROCEDURE Long_insert_constants_0_45(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,45); END Long_insert_constants_0_45;
<*NOWARN*> PROCEDURE Long_insert_constants_0_46(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,46); END Long_insert_constants_0_46;
<*NOWARN*> PROCEDURE Long_insert_constants_0_47(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,47); END Long_insert_constants_0_47;
<*NOWARN*> PROCEDURE Long_insert_constants_0_48(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,48); END Long_insert_constants_0_48;
<*NOWARN*> PROCEDURE Long_insert_constants_0_49(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,49); END Long_insert_constants_0_49;
<*NOWARN*> PROCEDURE Long_insert_constants_0_50(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,50); END Long_insert_constants_0_50;
<*NOWARN*> PROCEDURE Long_insert_constants_0_51(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,51); END Long_insert_constants_0_51;
<*NOWARN*> PROCEDURE Long_insert_constants_0_52(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,52); END Long_insert_constants_0_52;
<*NOWARN*> PROCEDURE Long_insert_constants_0_53(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,53); END Long_insert_constants_0_53;
<*NOWARN*> PROCEDURE Long_insert_constants_0_54(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,54); END Long_insert_constants_0_54;
<*NOWARN*> PROCEDURE Long_insert_constants_0_55(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,55); END Long_insert_constants_0_55;
<*NOWARN*> PROCEDURE Long_insert_constants_0_56(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,56); END Long_insert_constants_0_56;
<*NOWARN*> PROCEDURE Long_insert_constants_0_57(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,57); END Long_insert_constants_0_57;
<*NOWARN*> PROCEDURE Long_insert_constants_0_58(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,58); END Long_insert_constants_0_58;
<*NOWARN*> PROCEDURE Long_insert_constants_0_59(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,59); END Long_insert_constants_0_59;
<*NOWARN*> PROCEDURE Long_insert_constants_0_60(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,60); END Long_insert_constants_0_60;
<*NOWARN*> PROCEDURE Long_insert_constants_0_61(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,61); END Long_insert_constants_0_61;
<*NOWARN*> PROCEDURE Long_insert_constants_0_62(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,62); END Long_insert_constants_0_62;
<*NOWARN*> PROCEDURE Long_insert_constants_0_63(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,63); END Long_insert_constants_0_63;
<*NOWARN*> PROCEDURE Long_insert_constants_0_64(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,0,64); END Long_insert_constants_0_64;
<*NOWARN*> PROCEDURE Long_insert_constants_1_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,0); END Long_insert_constants_1_0;
<*NOWARN*> PROCEDURE Long_insert_constants_1_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,1); END Long_insert_constants_1_1;
<*NOWARN*> PROCEDURE Long_insert_constants_1_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,2); END Long_insert_constants_1_2;
<*NOWARN*> PROCEDURE Long_insert_constants_1_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,3); END Long_insert_constants_1_3;
<*NOWARN*> PROCEDURE Long_insert_constants_1_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,4); END Long_insert_constants_1_4;
<*NOWARN*> PROCEDURE Long_insert_constants_1_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,5); END Long_insert_constants_1_5;
<*NOWARN*> PROCEDURE Long_insert_constants_1_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,6); END Long_insert_constants_1_6;
<*NOWARN*> PROCEDURE Long_insert_constants_1_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,7); END Long_insert_constants_1_7;
<*NOWARN*> PROCEDURE Long_insert_constants_1_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,8); END Long_insert_constants_1_8;
<*NOWARN*> PROCEDURE Long_insert_constants_1_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,9); END Long_insert_constants_1_9;
<*NOWARN*> PROCEDURE Long_insert_constants_1_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,10); END Long_insert_constants_1_10;
<*NOWARN*> PROCEDURE Long_insert_constants_1_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,11); END Long_insert_constants_1_11;
<*NOWARN*> PROCEDURE Long_insert_constants_1_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,12); END Long_insert_constants_1_12;
<*NOWARN*> PROCEDURE Long_insert_constants_1_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,13); END Long_insert_constants_1_13;
<*NOWARN*> PROCEDURE Long_insert_constants_1_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,14); END Long_insert_constants_1_14;
<*NOWARN*> PROCEDURE Long_insert_constants_1_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,15); END Long_insert_constants_1_15;
<*NOWARN*> PROCEDURE Long_insert_constants_1_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,16); END Long_insert_constants_1_16;
<*NOWARN*> PROCEDURE Long_insert_constants_1_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,17); END Long_insert_constants_1_17;
<*NOWARN*> PROCEDURE Long_insert_constants_1_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,18); END Long_insert_constants_1_18;
<*NOWARN*> PROCEDURE Long_insert_constants_1_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,19); END Long_insert_constants_1_19;
<*NOWARN*> PROCEDURE Long_insert_constants_1_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,20); END Long_insert_constants_1_20;
<*NOWARN*> PROCEDURE Long_insert_constants_1_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,21); END Long_insert_constants_1_21;
<*NOWARN*> PROCEDURE Long_insert_constants_1_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,22); END Long_insert_constants_1_22;
<*NOWARN*> PROCEDURE Long_insert_constants_1_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,23); END Long_insert_constants_1_23;
<*NOWARN*> PROCEDURE Long_insert_constants_1_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,24); END Long_insert_constants_1_24;
<*NOWARN*> PROCEDURE Long_insert_constants_1_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,25); END Long_insert_constants_1_25;
<*NOWARN*> PROCEDURE Long_insert_constants_1_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,26); END Long_insert_constants_1_26;
<*NOWARN*> PROCEDURE Long_insert_constants_1_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,27); END Long_insert_constants_1_27;
<*NOWARN*> PROCEDURE Long_insert_constants_1_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,28); END Long_insert_constants_1_28;
<*NOWARN*> PROCEDURE Long_insert_constants_1_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,29); END Long_insert_constants_1_29;
<*NOWARN*> PROCEDURE Long_insert_constants_1_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,30); END Long_insert_constants_1_30;
<*NOWARN*> PROCEDURE Long_insert_constants_1_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,31); END Long_insert_constants_1_31;
<*NOWARN*> PROCEDURE Long_insert_constants_1_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,32); END Long_insert_constants_1_32;
<*NOWARN*> PROCEDURE Long_insert_constants_1_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,33); END Long_insert_constants_1_33;
<*NOWARN*> PROCEDURE Long_insert_constants_1_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,34); END Long_insert_constants_1_34;
<*NOWARN*> PROCEDURE Long_insert_constants_1_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,35); END Long_insert_constants_1_35;
<*NOWARN*> PROCEDURE Long_insert_constants_1_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,36); END Long_insert_constants_1_36;
<*NOWARN*> PROCEDURE Long_insert_constants_1_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,37); END Long_insert_constants_1_37;
<*NOWARN*> PROCEDURE Long_insert_constants_1_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,38); END Long_insert_constants_1_38;
<*NOWARN*> PROCEDURE Long_insert_constants_1_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,39); END Long_insert_constants_1_39;
<*NOWARN*> PROCEDURE Long_insert_constants_1_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,40); END Long_insert_constants_1_40;
<*NOWARN*> PROCEDURE Long_insert_constants_1_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,41); END Long_insert_constants_1_41;
<*NOWARN*> PROCEDURE Long_insert_constants_1_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,42); END Long_insert_constants_1_42;
<*NOWARN*> PROCEDURE Long_insert_constants_1_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,43); END Long_insert_constants_1_43;
<*NOWARN*> PROCEDURE Long_insert_constants_1_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,44); END Long_insert_constants_1_44;
<*NOWARN*> PROCEDURE Long_insert_constants_1_45(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,45); END Long_insert_constants_1_45;
<*NOWARN*> PROCEDURE Long_insert_constants_1_46(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,46); END Long_insert_constants_1_46;
<*NOWARN*> PROCEDURE Long_insert_constants_1_47(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,47); END Long_insert_constants_1_47;
<*NOWARN*> PROCEDURE Long_insert_constants_1_48(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,48); END Long_insert_constants_1_48;
<*NOWARN*> PROCEDURE Long_insert_constants_1_49(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,49); END Long_insert_constants_1_49;
<*NOWARN*> PROCEDURE Long_insert_constants_1_50(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,50); END Long_insert_constants_1_50;
<*NOWARN*> PROCEDURE Long_insert_constants_1_51(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,51); END Long_insert_constants_1_51;
<*NOWARN*> PROCEDURE Long_insert_constants_1_52(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,52); END Long_insert_constants_1_52;
<*NOWARN*> PROCEDURE Long_insert_constants_1_53(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,53); END Long_insert_constants_1_53;
<*NOWARN*> PROCEDURE Long_insert_constants_1_54(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,54); END Long_insert_constants_1_54;
<*NOWARN*> PROCEDURE Long_insert_constants_1_55(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,55); END Long_insert_constants_1_55;
<*NOWARN*> PROCEDURE Long_insert_constants_1_56(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,56); END Long_insert_constants_1_56;
<*NOWARN*> PROCEDURE Long_insert_constants_1_57(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,57); END Long_insert_constants_1_57;
<*NOWARN*> PROCEDURE Long_insert_constants_1_58(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,58); END Long_insert_constants_1_58;
<*NOWARN*> PROCEDURE Long_insert_constants_1_59(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,59); END Long_insert_constants_1_59;
<*NOWARN*> PROCEDURE Long_insert_constants_1_60(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,60); END Long_insert_constants_1_60;
<*NOWARN*> PROCEDURE Long_insert_constants_1_61(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,61); END Long_insert_constants_1_61;
<*NOWARN*> PROCEDURE Long_insert_constants_1_62(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,62); END Long_insert_constants_1_62;
<*NOWARN*> PROCEDURE Long_insert_constants_1_63(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,1,63); END Long_insert_constants_1_63;
<*NOWARN*> PROCEDURE Long_insert_constants_2_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,0); END Long_insert_constants_2_0;
<*NOWARN*> PROCEDURE Long_insert_constants_2_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,1); END Long_insert_constants_2_1;
<*NOWARN*> PROCEDURE Long_insert_constants_2_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,2); END Long_insert_constants_2_2;
<*NOWARN*> PROCEDURE Long_insert_constants_2_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,3); END Long_insert_constants_2_3;
<*NOWARN*> PROCEDURE Long_insert_constants_2_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,4); END Long_insert_constants_2_4;
<*NOWARN*> PROCEDURE Long_insert_constants_2_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,5); END Long_insert_constants_2_5;
<*NOWARN*> PROCEDURE Long_insert_constants_2_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,6); END Long_insert_constants_2_6;
<*NOWARN*> PROCEDURE Long_insert_constants_2_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,7); END Long_insert_constants_2_7;
<*NOWARN*> PROCEDURE Long_insert_constants_2_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,8); END Long_insert_constants_2_8;
<*NOWARN*> PROCEDURE Long_insert_constants_2_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,9); END Long_insert_constants_2_9;
<*NOWARN*> PROCEDURE Long_insert_constants_2_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,10); END Long_insert_constants_2_10;
<*NOWARN*> PROCEDURE Long_insert_constants_2_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,11); END Long_insert_constants_2_11;
<*NOWARN*> PROCEDURE Long_insert_constants_2_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,12); END Long_insert_constants_2_12;
<*NOWARN*> PROCEDURE Long_insert_constants_2_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,13); END Long_insert_constants_2_13;
<*NOWARN*> PROCEDURE Long_insert_constants_2_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,14); END Long_insert_constants_2_14;
<*NOWARN*> PROCEDURE Long_insert_constants_2_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,15); END Long_insert_constants_2_15;
<*NOWARN*> PROCEDURE Long_insert_constants_2_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,16); END Long_insert_constants_2_16;
<*NOWARN*> PROCEDURE Long_insert_constants_2_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,17); END Long_insert_constants_2_17;
<*NOWARN*> PROCEDURE Long_insert_constants_2_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,18); END Long_insert_constants_2_18;
<*NOWARN*> PROCEDURE Long_insert_constants_2_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,19); END Long_insert_constants_2_19;
<*NOWARN*> PROCEDURE Long_insert_constants_2_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,20); END Long_insert_constants_2_20;
<*NOWARN*> PROCEDURE Long_insert_constants_2_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,21); END Long_insert_constants_2_21;
<*NOWARN*> PROCEDURE Long_insert_constants_2_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,22); END Long_insert_constants_2_22;
<*NOWARN*> PROCEDURE Long_insert_constants_2_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,23); END Long_insert_constants_2_23;
<*NOWARN*> PROCEDURE Long_insert_constants_2_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,24); END Long_insert_constants_2_24;
<*NOWARN*> PROCEDURE Long_insert_constants_2_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,25); END Long_insert_constants_2_25;
<*NOWARN*> PROCEDURE Long_insert_constants_2_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,26); END Long_insert_constants_2_26;
<*NOWARN*> PROCEDURE Long_insert_constants_2_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,27); END Long_insert_constants_2_27;
<*NOWARN*> PROCEDURE Long_insert_constants_2_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,28); END Long_insert_constants_2_28;
<*NOWARN*> PROCEDURE Long_insert_constants_2_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,29); END Long_insert_constants_2_29;
<*NOWARN*> PROCEDURE Long_insert_constants_2_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,30); END Long_insert_constants_2_30;
<*NOWARN*> PROCEDURE Long_insert_constants_2_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,31); END Long_insert_constants_2_31;
<*NOWARN*> PROCEDURE Long_insert_constants_2_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,32); END Long_insert_constants_2_32;
<*NOWARN*> PROCEDURE Long_insert_constants_2_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,33); END Long_insert_constants_2_33;
<*NOWARN*> PROCEDURE Long_insert_constants_2_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,34); END Long_insert_constants_2_34;
<*NOWARN*> PROCEDURE Long_insert_constants_2_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,35); END Long_insert_constants_2_35;
<*NOWARN*> PROCEDURE Long_insert_constants_2_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,36); END Long_insert_constants_2_36;
<*NOWARN*> PROCEDURE Long_insert_constants_2_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,37); END Long_insert_constants_2_37;
<*NOWARN*> PROCEDURE Long_insert_constants_2_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,38); END Long_insert_constants_2_38;
<*NOWARN*> PROCEDURE Long_insert_constants_2_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,39); END Long_insert_constants_2_39;
<*NOWARN*> PROCEDURE Long_insert_constants_2_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,40); END Long_insert_constants_2_40;
<*NOWARN*> PROCEDURE Long_insert_constants_2_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,41); END Long_insert_constants_2_41;
<*NOWARN*> PROCEDURE Long_insert_constants_2_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,42); END Long_insert_constants_2_42;
<*NOWARN*> PROCEDURE Long_insert_constants_2_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,43); END Long_insert_constants_2_43;
<*NOWARN*> PROCEDURE Long_insert_constants_2_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,44); END Long_insert_constants_2_44;
<*NOWARN*> PROCEDURE Long_insert_constants_2_45(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,45); END Long_insert_constants_2_45;
<*NOWARN*> PROCEDURE Long_insert_constants_2_46(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,46); END Long_insert_constants_2_46;
<*NOWARN*> PROCEDURE Long_insert_constants_2_47(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,47); END Long_insert_constants_2_47;
<*NOWARN*> PROCEDURE Long_insert_constants_2_48(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,48); END Long_insert_constants_2_48;
<*NOWARN*> PROCEDURE Long_insert_constants_2_49(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,49); END Long_insert_constants_2_49;
<*NOWARN*> PROCEDURE Long_insert_constants_2_50(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,50); END Long_insert_constants_2_50;
<*NOWARN*> PROCEDURE Long_insert_constants_2_51(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,51); END Long_insert_constants_2_51;
<*NOWARN*> PROCEDURE Long_insert_constants_2_52(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,52); END Long_insert_constants_2_52;
<*NOWARN*> PROCEDURE Long_insert_constants_2_53(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,53); END Long_insert_constants_2_53;
<*NOWARN*> PROCEDURE Long_insert_constants_2_54(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,54); END Long_insert_constants_2_54;
<*NOWARN*> PROCEDURE Long_insert_constants_2_55(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,55); END Long_insert_constants_2_55;
<*NOWARN*> PROCEDURE Long_insert_constants_2_56(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,56); END Long_insert_constants_2_56;
<*NOWARN*> PROCEDURE Long_insert_constants_2_57(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,57); END Long_insert_constants_2_57;
<*NOWARN*> PROCEDURE Long_insert_constants_2_58(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,58); END Long_insert_constants_2_58;
<*NOWARN*> PROCEDURE Long_insert_constants_2_59(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,59); END Long_insert_constants_2_59;
<*NOWARN*> PROCEDURE Long_insert_constants_2_60(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,60); END Long_insert_constants_2_60;
<*NOWARN*> PROCEDURE Long_insert_constants_2_61(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,61); END Long_insert_constants_2_61;
<*NOWARN*> PROCEDURE Long_insert_constants_2_62(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,2,62); END Long_insert_constants_2_62;
<*NOWARN*> PROCEDURE Long_insert_constants_3_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,0); END Long_insert_constants_3_0;
<*NOWARN*> PROCEDURE Long_insert_constants_3_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,1); END Long_insert_constants_3_1;
<*NOWARN*> PROCEDURE Long_insert_constants_3_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,2); END Long_insert_constants_3_2;
<*NOWARN*> PROCEDURE Long_insert_constants_3_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,3); END Long_insert_constants_3_3;
<*NOWARN*> PROCEDURE Long_insert_constants_3_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,4); END Long_insert_constants_3_4;
<*NOWARN*> PROCEDURE Long_insert_constants_3_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,5); END Long_insert_constants_3_5;
<*NOWARN*> PROCEDURE Long_insert_constants_3_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,6); END Long_insert_constants_3_6;
<*NOWARN*> PROCEDURE Long_insert_constants_3_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,7); END Long_insert_constants_3_7;
<*NOWARN*> PROCEDURE Long_insert_constants_3_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,8); END Long_insert_constants_3_8;
<*NOWARN*> PROCEDURE Long_insert_constants_3_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,9); END Long_insert_constants_3_9;
<*NOWARN*> PROCEDURE Long_insert_constants_3_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,10); END Long_insert_constants_3_10;
<*NOWARN*> PROCEDURE Long_insert_constants_3_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,11); END Long_insert_constants_3_11;
<*NOWARN*> PROCEDURE Long_insert_constants_3_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,12); END Long_insert_constants_3_12;
<*NOWARN*> PROCEDURE Long_insert_constants_3_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,13); END Long_insert_constants_3_13;
<*NOWARN*> PROCEDURE Long_insert_constants_3_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,14); END Long_insert_constants_3_14;
<*NOWARN*> PROCEDURE Long_insert_constants_3_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,15); END Long_insert_constants_3_15;
<*NOWARN*> PROCEDURE Long_insert_constants_3_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,16); END Long_insert_constants_3_16;
<*NOWARN*> PROCEDURE Long_insert_constants_3_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,17); END Long_insert_constants_3_17;
<*NOWARN*> PROCEDURE Long_insert_constants_3_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,18); END Long_insert_constants_3_18;
<*NOWARN*> PROCEDURE Long_insert_constants_3_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,19); END Long_insert_constants_3_19;
<*NOWARN*> PROCEDURE Long_insert_constants_3_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,20); END Long_insert_constants_3_20;
<*NOWARN*> PROCEDURE Long_insert_constants_3_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,21); END Long_insert_constants_3_21;
<*NOWARN*> PROCEDURE Long_insert_constants_3_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,22); END Long_insert_constants_3_22;
<*NOWARN*> PROCEDURE Long_insert_constants_3_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,23); END Long_insert_constants_3_23;
<*NOWARN*> PROCEDURE Long_insert_constants_3_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,24); END Long_insert_constants_3_24;
<*NOWARN*> PROCEDURE Long_insert_constants_3_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,25); END Long_insert_constants_3_25;
<*NOWARN*> PROCEDURE Long_insert_constants_3_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,26); END Long_insert_constants_3_26;
<*NOWARN*> PROCEDURE Long_insert_constants_3_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,27); END Long_insert_constants_3_27;
<*NOWARN*> PROCEDURE Long_insert_constants_3_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,28); END Long_insert_constants_3_28;
<*NOWARN*> PROCEDURE Long_insert_constants_3_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,29); END Long_insert_constants_3_29;
<*NOWARN*> PROCEDURE Long_insert_constants_3_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,30); END Long_insert_constants_3_30;
<*NOWARN*> PROCEDURE Long_insert_constants_3_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,31); END Long_insert_constants_3_31;
<*NOWARN*> PROCEDURE Long_insert_constants_3_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,32); END Long_insert_constants_3_32;
<*NOWARN*> PROCEDURE Long_insert_constants_3_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,33); END Long_insert_constants_3_33;
<*NOWARN*> PROCEDURE Long_insert_constants_3_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,34); END Long_insert_constants_3_34;
<*NOWARN*> PROCEDURE Long_insert_constants_3_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,35); END Long_insert_constants_3_35;
<*NOWARN*> PROCEDURE Long_insert_constants_3_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,36); END Long_insert_constants_3_36;
<*NOWARN*> PROCEDURE Long_insert_constants_3_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,37); END Long_insert_constants_3_37;
<*NOWARN*> PROCEDURE Long_insert_constants_3_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,38); END Long_insert_constants_3_38;
<*NOWARN*> PROCEDURE Long_insert_constants_3_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,39); END Long_insert_constants_3_39;
<*NOWARN*> PROCEDURE Long_insert_constants_3_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,40); END Long_insert_constants_3_40;
<*NOWARN*> PROCEDURE Long_insert_constants_3_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,41); END Long_insert_constants_3_41;
<*NOWARN*> PROCEDURE Long_insert_constants_3_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,42); END Long_insert_constants_3_42;
<*NOWARN*> PROCEDURE Long_insert_constants_3_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,43); END Long_insert_constants_3_43;
<*NOWARN*> PROCEDURE Long_insert_constants_3_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,44); END Long_insert_constants_3_44;
<*NOWARN*> PROCEDURE Long_insert_constants_3_45(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,45); END Long_insert_constants_3_45;
<*NOWARN*> PROCEDURE Long_insert_constants_3_46(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,46); END Long_insert_constants_3_46;
<*NOWARN*> PROCEDURE Long_insert_constants_3_47(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,47); END Long_insert_constants_3_47;
<*NOWARN*> PROCEDURE Long_insert_constants_3_48(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,48); END Long_insert_constants_3_48;
<*NOWARN*> PROCEDURE Long_insert_constants_3_49(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,49); END Long_insert_constants_3_49;
<*NOWARN*> PROCEDURE Long_insert_constants_3_50(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,50); END Long_insert_constants_3_50;
<*NOWARN*> PROCEDURE Long_insert_constants_3_51(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,51); END Long_insert_constants_3_51;
<*NOWARN*> PROCEDURE Long_insert_constants_3_52(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,52); END Long_insert_constants_3_52;
<*NOWARN*> PROCEDURE Long_insert_constants_3_53(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,53); END Long_insert_constants_3_53;
<*NOWARN*> PROCEDURE Long_insert_constants_3_54(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,54); END Long_insert_constants_3_54;
<*NOWARN*> PROCEDURE Long_insert_constants_3_55(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,55); END Long_insert_constants_3_55;
<*NOWARN*> PROCEDURE Long_insert_constants_3_56(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,56); END Long_insert_constants_3_56;
<*NOWARN*> PROCEDURE Long_insert_constants_3_57(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,57); END Long_insert_constants_3_57;
<*NOWARN*> PROCEDURE Long_insert_constants_3_58(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,58); END Long_insert_constants_3_58;
<*NOWARN*> PROCEDURE Long_insert_constants_3_59(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,59); END Long_insert_constants_3_59;
<*NOWARN*> PROCEDURE Long_insert_constants_3_60(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,60); END Long_insert_constants_3_60;
<*NOWARN*> PROCEDURE Long_insert_constants_3_61(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,3,61); END Long_insert_constants_3_61;
<*NOWARN*> PROCEDURE Long_insert_constants_4_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,0); END Long_insert_constants_4_0;
<*NOWARN*> PROCEDURE Long_insert_constants_4_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,1); END Long_insert_constants_4_1;
<*NOWARN*> PROCEDURE Long_insert_constants_4_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,2); END Long_insert_constants_4_2;
<*NOWARN*> PROCEDURE Long_insert_constants_4_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,3); END Long_insert_constants_4_3;
<*NOWARN*> PROCEDURE Long_insert_constants_4_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,4); END Long_insert_constants_4_4;
<*NOWARN*> PROCEDURE Long_insert_constants_4_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,5); END Long_insert_constants_4_5;
<*NOWARN*> PROCEDURE Long_insert_constants_4_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,6); END Long_insert_constants_4_6;
<*NOWARN*> PROCEDURE Long_insert_constants_4_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,7); END Long_insert_constants_4_7;
<*NOWARN*> PROCEDURE Long_insert_constants_4_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,8); END Long_insert_constants_4_8;
<*NOWARN*> PROCEDURE Long_insert_constants_4_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,9); END Long_insert_constants_4_9;
<*NOWARN*> PROCEDURE Long_insert_constants_4_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,10); END Long_insert_constants_4_10;
<*NOWARN*> PROCEDURE Long_insert_constants_4_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,11); END Long_insert_constants_4_11;
<*NOWARN*> PROCEDURE Long_insert_constants_4_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,12); END Long_insert_constants_4_12;
<*NOWARN*> PROCEDURE Long_insert_constants_4_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,13); END Long_insert_constants_4_13;
<*NOWARN*> PROCEDURE Long_insert_constants_4_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,14); END Long_insert_constants_4_14;
<*NOWARN*> PROCEDURE Long_insert_constants_4_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,15); END Long_insert_constants_4_15;
<*NOWARN*> PROCEDURE Long_insert_constants_4_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,16); END Long_insert_constants_4_16;
<*NOWARN*> PROCEDURE Long_insert_constants_4_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,17); END Long_insert_constants_4_17;
<*NOWARN*> PROCEDURE Long_insert_constants_4_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,18); END Long_insert_constants_4_18;
<*NOWARN*> PROCEDURE Long_insert_constants_4_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,19); END Long_insert_constants_4_19;
<*NOWARN*> PROCEDURE Long_insert_constants_4_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,20); END Long_insert_constants_4_20;
<*NOWARN*> PROCEDURE Long_insert_constants_4_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,21); END Long_insert_constants_4_21;
<*NOWARN*> PROCEDURE Long_insert_constants_4_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,22); END Long_insert_constants_4_22;
<*NOWARN*> PROCEDURE Long_insert_constants_4_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,23); END Long_insert_constants_4_23;
<*NOWARN*> PROCEDURE Long_insert_constants_4_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,24); END Long_insert_constants_4_24;
<*NOWARN*> PROCEDURE Long_insert_constants_4_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,25); END Long_insert_constants_4_25;
<*NOWARN*> PROCEDURE Long_insert_constants_4_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,26); END Long_insert_constants_4_26;
<*NOWARN*> PROCEDURE Long_insert_constants_4_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,27); END Long_insert_constants_4_27;
<*NOWARN*> PROCEDURE Long_insert_constants_4_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,28); END Long_insert_constants_4_28;
<*NOWARN*> PROCEDURE Long_insert_constants_4_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,29); END Long_insert_constants_4_29;
<*NOWARN*> PROCEDURE Long_insert_constants_4_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,30); END Long_insert_constants_4_30;
<*NOWARN*> PROCEDURE Long_insert_constants_4_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,31); END Long_insert_constants_4_31;
<*NOWARN*> PROCEDURE Long_insert_constants_4_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,32); END Long_insert_constants_4_32;
<*NOWARN*> PROCEDURE Long_insert_constants_4_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,33); END Long_insert_constants_4_33;
<*NOWARN*> PROCEDURE Long_insert_constants_4_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,34); END Long_insert_constants_4_34;
<*NOWARN*> PROCEDURE Long_insert_constants_4_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,35); END Long_insert_constants_4_35;
<*NOWARN*> PROCEDURE Long_insert_constants_4_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,36); END Long_insert_constants_4_36;
<*NOWARN*> PROCEDURE Long_insert_constants_4_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,37); END Long_insert_constants_4_37;
<*NOWARN*> PROCEDURE Long_insert_constants_4_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,38); END Long_insert_constants_4_38;
<*NOWARN*> PROCEDURE Long_insert_constants_4_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,39); END Long_insert_constants_4_39;
<*NOWARN*> PROCEDURE Long_insert_constants_4_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,40); END Long_insert_constants_4_40;
<*NOWARN*> PROCEDURE Long_insert_constants_4_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,41); END Long_insert_constants_4_41;
<*NOWARN*> PROCEDURE Long_insert_constants_4_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,42); END Long_insert_constants_4_42;
<*NOWARN*> PROCEDURE Long_insert_constants_4_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,43); END Long_insert_constants_4_43;
<*NOWARN*> PROCEDURE Long_insert_constants_4_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,44); END Long_insert_constants_4_44;
<*NOWARN*> PROCEDURE Long_insert_constants_4_45(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,45); END Long_insert_constants_4_45;
<*NOWARN*> PROCEDURE Long_insert_constants_4_46(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,46); END Long_insert_constants_4_46;
<*NOWARN*> PROCEDURE Long_insert_constants_4_47(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,47); END Long_insert_constants_4_47;
<*NOWARN*> PROCEDURE Long_insert_constants_4_48(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,48); END Long_insert_constants_4_48;
<*NOWARN*> PROCEDURE Long_insert_constants_4_49(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,49); END Long_insert_constants_4_49;
<*NOWARN*> PROCEDURE Long_insert_constants_4_50(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,50); END Long_insert_constants_4_50;
<*NOWARN*> PROCEDURE Long_insert_constants_4_51(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,51); END Long_insert_constants_4_51;
<*NOWARN*> PROCEDURE Long_insert_constants_4_52(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,52); END Long_insert_constants_4_52;
<*NOWARN*> PROCEDURE Long_insert_constants_4_53(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,53); END Long_insert_constants_4_53;
<*NOWARN*> PROCEDURE Long_insert_constants_4_54(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,54); END Long_insert_constants_4_54;
<*NOWARN*> PROCEDURE Long_insert_constants_4_55(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,55); END Long_insert_constants_4_55;
<*NOWARN*> PROCEDURE Long_insert_constants_4_56(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,56); END Long_insert_constants_4_56;
<*NOWARN*> PROCEDURE Long_insert_constants_4_57(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,57); END Long_insert_constants_4_57;
<*NOWARN*> PROCEDURE Long_insert_constants_4_58(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,58); END Long_insert_constants_4_58;
<*NOWARN*> PROCEDURE Long_insert_constants_4_59(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,59); END Long_insert_constants_4_59;
<*NOWARN*> PROCEDURE Long_insert_constants_4_60(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,4,60); END Long_insert_constants_4_60;
<*NOWARN*> PROCEDURE Long_insert_constants_5_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,0); END Long_insert_constants_5_0;
<*NOWARN*> PROCEDURE Long_insert_constants_5_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,1); END Long_insert_constants_5_1;
<*NOWARN*> PROCEDURE Long_insert_constants_5_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,2); END Long_insert_constants_5_2;
<*NOWARN*> PROCEDURE Long_insert_constants_5_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,3); END Long_insert_constants_5_3;
<*NOWARN*> PROCEDURE Long_insert_constants_5_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,4); END Long_insert_constants_5_4;
<*NOWARN*> PROCEDURE Long_insert_constants_5_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,5); END Long_insert_constants_5_5;
<*NOWARN*> PROCEDURE Long_insert_constants_5_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,6); END Long_insert_constants_5_6;
<*NOWARN*> PROCEDURE Long_insert_constants_5_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,7); END Long_insert_constants_5_7;
<*NOWARN*> PROCEDURE Long_insert_constants_5_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,8); END Long_insert_constants_5_8;
<*NOWARN*> PROCEDURE Long_insert_constants_5_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,9); END Long_insert_constants_5_9;
<*NOWARN*> PROCEDURE Long_insert_constants_5_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,10); END Long_insert_constants_5_10;
<*NOWARN*> PROCEDURE Long_insert_constants_5_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,11); END Long_insert_constants_5_11;
<*NOWARN*> PROCEDURE Long_insert_constants_5_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,12); END Long_insert_constants_5_12;
<*NOWARN*> PROCEDURE Long_insert_constants_5_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,13); END Long_insert_constants_5_13;
<*NOWARN*> PROCEDURE Long_insert_constants_5_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,14); END Long_insert_constants_5_14;
<*NOWARN*> PROCEDURE Long_insert_constants_5_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,15); END Long_insert_constants_5_15;
<*NOWARN*> PROCEDURE Long_insert_constants_5_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,16); END Long_insert_constants_5_16;
<*NOWARN*> PROCEDURE Long_insert_constants_5_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,17); END Long_insert_constants_5_17;
<*NOWARN*> PROCEDURE Long_insert_constants_5_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,18); END Long_insert_constants_5_18;
<*NOWARN*> PROCEDURE Long_insert_constants_5_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,19); END Long_insert_constants_5_19;
<*NOWARN*> PROCEDURE Long_insert_constants_5_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,20); END Long_insert_constants_5_20;
<*NOWARN*> PROCEDURE Long_insert_constants_5_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,21); END Long_insert_constants_5_21;
<*NOWARN*> PROCEDURE Long_insert_constants_5_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,22); END Long_insert_constants_5_22;
<*NOWARN*> PROCEDURE Long_insert_constants_5_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,23); END Long_insert_constants_5_23;
<*NOWARN*> PROCEDURE Long_insert_constants_5_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,24); END Long_insert_constants_5_24;
<*NOWARN*> PROCEDURE Long_insert_constants_5_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,25); END Long_insert_constants_5_25;
<*NOWARN*> PROCEDURE Long_insert_constants_5_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,26); END Long_insert_constants_5_26;
<*NOWARN*> PROCEDURE Long_insert_constants_5_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,27); END Long_insert_constants_5_27;
<*NOWARN*> PROCEDURE Long_insert_constants_5_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,28); END Long_insert_constants_5_28;
<*NOWARN*> PROCEDURE Long_insert_constants_5_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,29); END Long_insert_constants_5_29;
<*NOWARN*> PROCEDURE Long_insert_constants_5_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,30); END Long_insert_constants_5_30;
<*NOWARN*> PROCEDURE Long_insert_constants_5_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,31); END Long_insert_constants_5_31;
<*NOWARN*> PROCEDURE Long_insert_constants_5_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,32); END Long_insert_constants_5_32;
<*NOWARN*> PROCEDURE Long_insert_constants_5_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,33); END Long_insert_constants_5_33;
<*NOWARN*> PROCEDURE Long_insert_constants_5_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,34); END Long_insert_constants_5_34;
<*NOWARN*> PROCEDURE Long_insert_constants_5_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,35); END Long_insert_constants_5_35;
<*NOWARN*> PROCEDURE Long_insert_constants_5_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,36); END Long_insert_constants_5_36;
<*NOWARN*> PROCEDURE Long_insert_constants_5_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,37); END Long_insert_constants_5_37;
<*NOWARN*> PROCEDURE Long_insert_constants_5_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,38); END Long_insert_constants_5_38;
<*NOWARN*> PROCEDURE Long_insert_constants_5_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,39); END Long_insert_constants_5_39;
<*NOWARN*> PROCEDURE Long_insert_constants_5_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,40); END Long_insert_constants_5_40;
<*NOWARN*> PROCEDURE Long_insert_constants_5_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,41); END Long_insert_constants_5_41;
<*NOWARN*> PROCEDURE Long_insert_constants_5_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,42); END Long_insert_constants_5_42;
<*NOWARN*> PROCEDURE Long_insert_constants_5_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,43); END Long_insert_constants_5_43;
<*NOWARN*> PROCEDURE Long_insert_constants_5_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,44); END Long_insert_constants_5_44;
<*NOWARN*> PROCEDURE Long_insert_constants_5_45(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,45); END Long_insert_constants_5_45;
<*NOWARN*> PROCEDURE Long_insert_constants_5_46(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,46); END Long_insert_constants_5_46;
<*NOWARN*> PROCEDURE Long_insert_constants_5_47(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,47); END Long_insert_constants_5_47;
<*NOWARN*> PROCEDURE Long_insert_constants_5_48(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,48); END Long_insert_constants_5_48;
<*NOWARN*> PROCEDURE Long_insert_constants_5_49(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,49); END Long_insert_constants_5_49;
<*NOWARN*> PROCEDURE Long_insert_constants_5_50(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,50); END Long_insert_constants_5_50;
<*NOWARN*> PROCEDURE Long_insert_constants_5_51(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,51); END Long_insert_constants_5_51;
<*NOWARN*> PROCEDURE Long_insert_constants_5_52(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,52); END Long_insert_constants_5_52;
<*NOWARN*> PROCEDURE Long_insert_constants_5_53(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,53); END Long_insert_constants_5_53;
<*NOWARN*> PROCEDURE Long_insert_constants_5_54(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,54); END Long_insert_constants_5_54;
<*NOWARN*> PROCEDURE Long_insert_constants_5_55(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,55); END Long_insert_constants_5_55;
<*NOWARN*> PROCEDURE Long_insert_constants_5_56(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,56); END Long_insert_constants_5_56;
<*NOWARN*> PROCEDURE Long_insert_constants_5_57(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,57); END Long_insert_constants_5_57;
<*NOWARN*> PROCEDURE Long_insert_constants_5_58(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,58); END Long_insert_constants_5_58;
<*NOWARN*> PROCEDURE Long_insert_constants_5_59(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,5,59); END Long_insert_constants_5_59;
<*NOWARN*> PROCEDURE Long_insert_constants_6_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,0); END Long_insert_constants_6_0;
<*NOWARN*> PROCEDURE Long_insert_constants_6_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,1); END Long_insert_constants_6_1;
<*NOWARN*> PROCEDURE Long_insert_constants_6_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,2); END Long_insert_constants_6_2;
<*NOWARN*> PROCEDURE Long_insert_constants_6_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,3); END Long_insert_constants_6_3;
<*NOWARN*> PROCEDURE Long_insert_constants_6_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,4); END Long_insert_constants_6_4;
<*NOWARN*> PROCEDURE Long_insert_constants_6_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,5); END Long_insert_constants_6_5;
<*NOWARN*> PROCEDURE Long_insert_constants_6_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,6); END Long_insert_constants_6_6;
<*NOWARN*> PROCEDURE Long_insert_constants_6_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,7); END Long_insert_constants_6_7;
<*NOWARN*> PROCEDURE Long_insert_constants_6_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,8); END Long_insert_constants_6_8;
<*NOWARN*> PROCEDURE Long_insert_constants_6_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,9); END Long_insert_constants_6_9;
<*NOWARN*> PROCEDURE Long_insert_constants_6_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,10); END Long_insert_constants_6_10;
<*NOWARN*> PROCEDURE Long_insert_constants_6_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,11); END Long_insert_constants_6_11;
<*NOWARN*> PROCEDURE Long_insert_constants_6_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,12); END Long_insert_constants_6_12;
<*NOWARN*> PROCEDURE Long_insert_constants_6_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,13); END Long_insert_constants_6_13;
<*NOWARN*> PROCEDURE Long_insert_constants_6_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,14); END Long_insert_constants_6_14;
<*NOWARN*> PROCEDURE Long_insert_constants_6_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,15); END Long_insert_constants_6_15;
<*NOWARN*> PROCEDURE Long_insert_constants_6_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,16); END Long_insert_constants_6_16;
<*NOWARN*> PROCEDURE Long_insert_constants_6_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,17); END Long_insert_constants_6_17;
<*NOWARN*> PROCEDURE Long_insert_constants_6_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,18); END Long_insert_constants_6_18;
<*NOWARN*> PROCEDURE Long_insert_constants_6_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,19); END Long_insert_constants_6_19;
<*NOWARN*> PROCEDURE Long_insert_constants_6_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,20); END Long_insert_constants_6_20;
<*NOWARN*> PROCEDURE Long_insert_constants_6_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,21); END Long_insert_constants_6_21;
<*NOWARN*> PROCEDURE Long_insert_constants_6_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,22); END Long_insert_constants_6_22;
<*NOWARN*> PROCEDURE Long_insert_constants_6_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,23); END Long_insert_constants_6_23;
<*NOWARN*> PROCEDURE Long_insert_constants_6_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,24); END Long_insert_constants_6_24;
<*NOWARN*> PROCEDURE Long_insert_constants_6_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,25); END Long_insert_constants_6_25;
<*NOWARN*> PROCEDURE Long_insert_constants_6_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,26); END Long_insert_constants_6_26;
<*NOWARN*> PROCEDURE Long_insert_constants_6_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,27); END Long_insert_constants_6_27;
<*NOWARN*> PROCEDURE Long_insert_constants_6_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,28); END Long_insert_constants_6_28;
<*NOWARN*> PROCEDURE Long_insert_constants_6_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,29); END Long_insert_constants_6_29;
<*NOWARN*> PROCEDURE Long_insert_constants_6_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,30); END Long_insert_constants_6_30;
<*NOWARN*> PROCEDURE Long_insert_constants_6_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,31); END Long_insert_constants_6_31;
<*NOWARN*> PROCEDURE Long_insert_constants_6_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,32); END Long_insert_constants_6_32;
<*NOWARN*> PROCEDURE Long_insert_constants_6_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,33); END Long_insert_constants_6_33;
<*NOWARN*> PROCEDURE Long_insert_constants_6_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,34); END Long_insert_constants_6_34;
<*NOWARN*> PROCEDURE Long_insert_constants_6_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,35); END Long_insert_constants_6_35;
<*NOWARN*> PROCEDURE Long_insert_constants_6_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,36); END Long_insert_constants_6_36;
<*NOWARN*> PROCEDURE Long_insert_constants_6_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,37); END Long_insert_constants_6_37;
<*NOWARN*> PROCEDURE Long_insert_constants_6_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,38); END Long_insert_constants_6_38;
<*NOWARN*> PROCEDURE Long_insert_constants_6_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,39); END Long_insert_constants_6_39;
<*NOWARN*> PROCEDURE Long_insert_constants_6_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,40); END Long_insert_constants_6_40;
<*NOWARN*> PROCEDURE Long_insert_constants_6_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,41); END Long_insert_constants_6_41;
<*NOWARN*> PROCEDURE Long_insert_constants_6_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,42); END Long_insert_constants_6_42;
<*NOWARN*> PROCEDURE Long_insert_constants_6_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,43); END Long_insert_constants_6_43;
<*NOWARN*> PROCEDURE Long_insert_constants_6_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,44); END Long_insert_constants_6_44;
<*NOWARN*> PROCEDURE Long_insert_constants_6_45(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,45); END Long_insert_constants_6_45;
<*NOWARN*> PROCEDURE Long_insert_constants_6_46(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,46); END Long_insert_constants_6_46;
<*NOWARN*> PROCEDURE Long_insert_constants_6_47(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,47); END Long_insert_constants_6_47;
<*NOWARN*> PROCEDURE Long_insert_constants_6_48(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,48); END Long_insert_constants_6_48;
<*NOWARN*> PROCEDURE Long_insert_constants_6_49(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,49); END Long_insert_constants_6_49;
<*NOWARN*> PROCEDURE Long_insert_constants_6_50(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,50); END Long_insert_constants_6_50;
<*NOWARN*> PROCEDURE Long_insert_constants_6_51(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,51); END Long_insert_constants_6_51;
<*NOWARN*> PROCEDURE Long_insert_constants_6_52(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,52); END Long_insert_constants_6_52;
<*NOWARN*> PROCEDURE Long_insert_constants_6_53(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,53); END Long_insert_constants_6_53;
<*NOWARN*> PROCEDURE Long_insert_constants_6_54(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,54); END Long_insert_constants_6_54;
<*NOWARN*> PROCEDURE Long_insert_constants_6_55(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,55); END Long_insert_constants_6_55;
<*NOWARN*> PROCEDURE Long_insert_constants_6_56(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,56); END Long_insert_constants_6_56;
<*NOWARN*> PROCEDURE Long_insert_constants_6_57(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,57); END Long_insert_constants_6_57;
<*NOWARN*> PROCEDURE Long_insert_constants_6_58(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,6,58); END Long_insert_constants_6_58;
<*NOWARN*> PROCEDURE Long_insert_constants_7_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,0); END Long_insert_constants_7_0;
<*NOWARN*> PROCEDURE Long_insert_constants_7_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,1); END Long_insert_constants_7_1;
<*NOWARN*> PROCEDURE Long_insert_constants_7_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,2); END Long_insert_constants_7_2;
<*NOWARN*> PROCEDURE Long_insert_constants_7_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,3); END Long_insert_constants_7_3;
<*NOWARN*> PROCEDURE Long_insert_constants_7_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,4); END Long_insert_constants_7_4;
<*NOWARN*> PROCEDURE Long_insert_constants_7_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,5); END Long_insert_constants_7_5;
<*NOWARN*> PROCEDURE Long_insert_constants_7_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,6); END Long_insert_constants_7_6;
<*NOWARN*> PROCEDURE Long_insert_constants_7_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,7); END Long_insert_constants_7_7;
<*NOWARN*> PROCEDURE Long_insert_constants_7_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,8); END Long_insert_constants_7_8;
<*NOWARN*> PROCEDURE Long_insert_constants_7_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,9); END Long_insert_constants_7_9;
<*NOWARN*> PROCEDURE Long_insert_constants_7_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,10); END Long_insert_constants_7_10;
<*NOWARN*> PROCEDURE Long_insert_constants_7_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,11); END Long_insert_constants_7_11;
<*NOWARN*> PROCEDURE Long_insert_constants_7_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,12); END Long_insert_constants_7_12;
<*NOWARN*> PROCEDURE Long_insert_constants_7_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,13); END Long_insert_constants_7_13;
<*NOWARN*> PROCEDURE Long_insert_constants_7_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,14); END Long_insert_constants_7_14;
<*NOWARN*> PROCEDURE Long_insert_constants_7_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,15); END Long_insert_constants_7_15;
<*NOWARN*> PROCEDURE Long_insert_constants_7_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,16); END Long_insert_constants_7_16;
<*NOWARN*> PROCEDURE Long_insert_constants_7_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,17); END Long_insert_constants_7_17;
<*NOWARN*> PROCEDURE Long_insert_constants_7_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,18); END Long_insert_constants_7_18;
<*NOWARN*> PROCEDURE Long_insert_constants_7_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,19); END Long_insert_constants_7_19;
<*NOWARN*> PROCEDURE Long_insert_constants_7_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,20); END Long_insert_constants_7_20;
<*NOWARN*> PROCEDURE Long_insert_constants_7_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,21); END Long_insert_constants_7_21;
<*NOWARN*> PROCEDURE Long_insert_constants_7_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,22); END Long_insert_constants_7_22;
<*NOWARN*> PROCEDURE Long_insert_constants_7_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,23); END Long_insert_constants_7_23;
<*NOWARN*> PROCEDURE Long_insert_constants_7_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,24); END Long_insert_constants_7_24;
<*NOWARN*> PROCEDURE Long_insert_constants_7_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,25); END Long_insert_constants_7_25;
<*NOWARN*> PROCEDURE Long_insert_constants_7_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,26); END Long_insert_constants_7_26;
<*NOWARN*> PROCEDURE Long_insert_constants_7_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,27); END Long_insert_constants_7_27;
<*NOWARN*> PROCEDURE Long_insert_constants_7_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,28); END Long_insert_constants_7_28;
<*NOWARN*> PROCEDURE Long_insert_constants_7_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,29); END Long_insert_constants_7_29;
<*NOWARN*> PROCEDURE Long_insert_constants_7_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,30); END Long_insert_constants_7_30;
<*NOWARN*> PROCEDURE Long_insert_constants_7_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,31); END Long_insert_constants_7_31;
<*NOWARN*> PROCEDURE Long_insert_constants_7_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,32); END Long_insert_constants_7_32;
<*NOWARN*> PROCEDURE Long_insert_constants_7_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,33); END Long_insert_constants_7_33;
<*NOWARN*> PROCEDURE Long_insert_constants_7_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,34); END Long_insert_constants_7_34;
<*NOWARN*> PROCEDURE Long_insert_constants_7_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,35); END Long_insert_constants_7_35;
<*NOWARN*> PROCEDURE Long_insert_constants_7_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,36); END Long_insert_constants_7_36;
<*NOWARN*> PROCEDURE Long_insert_constants_7_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,37); END Long_insert_constants_7_37;
<*NOWARN*> PROCEDURE Long_insert_constants_7_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,38); END Long_insert_constants_7_38;
<*NOWARN*> PROCEDURE Long_insert_constants_7_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,39); END Long_insert_constants_7_39;
<*NOWARN*> PROCEDURE Long_insert_constants_7_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,40); END Long_insert_constants_7_40;
<*NOWARN*> PROCEDURE Long_insert_constants_7_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,41); END Long_insert_constants_7_41;
<*NOWARN*> PROCEDURE Long_insert_constants_7_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,42); END Long_insert_constants_7_42;
<*NOWARN*> PROCEDURE Long_insert_constants_7_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,43); END Long_insert_constants_7_43;
<*NOWARN*> PROCEDURE Long_insert_constants_7_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,44); END Long_insert_constants_7_44;
<*NOWARN*> PROCEDURE Long_insert_constants_7_45(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,45); END Long_insert_constants_7_45;
<*NOWARN*> PROCEDURE Long_insert_constants_7_46(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,46); END Long_insert_constants_7_46;
<*NOWARN*> PROCEDURE Long_insert_constants_7_47(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,47); END Long_insert_constants_7_47;
<*NOWARN*> PROCEDURE Long_insert_constants_7_48(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,48); END Long_insert_constants_7_48;
<*NOWARN*> PROCEDURE Long_insert_constants_7_49(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,49); END Long_insert_constants_7_49;
<*NOWARN*> PROCEDURE Long_insert_constants_7_50(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,50); END Long_insert_constants_7_50;
<*NOWARN*> PROCEDURE Long_insert_constants_7_51(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,51); END Long_insert_constants_7_51;
<*NOWARN*> PROCEDURE Long_insert_constants_7_52(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,52); END Long_insert_constants_7_52;
<*NOWARN*> PROCEDURE Long_insert_constants_7_53(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,53); END Long_insert_constants_7_53;
<*NOWARN*> PROCEDURE Long_insert_constants_7_54(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,54); END Long_insert_constants_7_54;
<*NOWARN*> PROCEDURE Long_insert_constants_7_55(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,55); END Long_insert_constants_7_55;
<*NOWARN*> PROCEDURE Long_insert_constants_7_56(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,56); END Long_insert_constants_7_56;
<*NOWARN*> PROCEDURE Long_insert_constants_7_57(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,7,57); END Long_insert_constants_7_57;
<*NOWARN*> PROCEDURE Long_insert_constants_8_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,0); END Long_insert_constants_8_0;
<*NOWARN*> PROCEDURE Long_insert_constants_8_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,1); END Long_insert_constants_8_1;
<*NOWARN*> PROCEDURE Long_insert_constants_8_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,2); END Long_insert_constants_8_2;
<*NOWARN*> PROCEDURE Long_insert_constants_8_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,3); END Long_insert_constants_8_3;
<*NOWARN*> PROCEDURE Long_insert_constants_8_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,4); END Long_insert_constants_8_4;
<*NOWARN*> PROCEDURE Long_insert_constants_8_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,5); END Long_insert_constants_8_5;
<*NOWARN*> PROCEDURE Long_insert_constants_8_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,6); END Long_insert_constants_8_6;
<*NOWARN*> PROCEDURE Long_insert_constants_8_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,7); END Long_insert_constants_8_7;
<*NOWARN*> PROCEDURE Long_insert_constants_8_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,8); END Long_insert_constants_8_8;
<*NOWARN*> PROCEDURE Long_insert_constants_8_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,9); END Long_insert_constants_8_9;
<*NOWARN*> PROCEDURE Long_insert_constants_8_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,10); END Long_insert_constants_8_10;
<*NOWARN*> PROCEDURE Long_insert_constants_8_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,11); END Long_insert_constants_8_11;
<*NOWARN*> PROCEDURE Long_insert_constants_8_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,12); END Long_insert_constants_8_12;
<*NOWARN*> PROCEDURE Long_insert_constants_8_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,13); END Long_insert_constants_8_13;
<*NOWARN*> PROCEDURE Long_insert_constants_8_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,14); END Long_insert_constants_8_14;
<*NOWARN*> PROCEDURE Long_insert_constants_8_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,15); END Long_insert_constants_8_15;
<*NOWARN*> PROCEDURE Long_insert_constants_8_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,16); END Long_insert_constants_8_16;
<*NOWARN*> PROCEDURE Long_insert_constants_8_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,17); END Long_insert_constants_8_17;
<*NOWARN*> PROCEDURE Long_insert_constants_8_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,18); END Long_insert_constants_8_18;
<*NOWARN*> PROCEDURE Long_insert_constants_8_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,19); END Long_insert_constants_8_19;
<*NOWARN*> PROCEDURE Long_insert_constants_8_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,20); END Long_insert_constants_8_20;
<*NOWARN*> PROCEDURE Long_insert_constants_8_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,21); END Long_insert_constants_8_21;
<*NOWARN*> PROCEDURE Long_insert_constants_8_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,22); END Long_insert_constants_8_22;
<*NOWARN*> PROCEDURE Long_insert_constants_8_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,23); END Long_insert_constants_8_23;
<*NOWARN*> PROCEDURE Long_insert_constants_8_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,24); END Long_insert_constants_8_24;
<*NOWARN*> PROCEDURE Long_insert_constants_8_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,25); END Long_insert_constants_8_25;
<*NOWARN*> PROCEDURE Long_insert_constants_8_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,26); END Long_insert_constants_8_26;
<*NOWARN*> PROCEDURE Long_insert_constants_8_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,27); END Long_insert_constants_8_27;
<*NOWARN*> PROCEDURE Long_insert_constants_8_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,28); END Long_insert_constants_8_28;
<*NOWARN*> PROCEDURE Long_insert_constants_8_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,29); END Long_insert_constants_8_29;
<*NOWARN*> PROCEDURE Long_insert_constants_8_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,30); END Long_insert_constants_8_30;
<*NOWARN*> PROCEDURE Long_insert_constants_8_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,31); END Long_insert_constants_8_31;
<*NOWARN*> PROCEDURE Long_insert_constants_8_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,32); END Long_insert_constants_8_32;
<*NOWARN*> PROCEDURE Long_insert_constants_8_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,33); END Long_insert_constants_8_33;
<*NOWARN*> PROCEDURE Long_insert_constants_8_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,34); END Long_insert_constants_8_34;
<*NOWARN*> PROCEDURE Long_insert_constants_8_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,35); END Long_insert_constants_8_35;
<*NOWARN*> PROCEDURE Long_insert_constants_8_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,36); END Long_insert_constants_8_36;
<*NOWARN*> PROCEDURE Long_insert_constants_8_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,37); END Long_insert_constants_8_37;
<*NOWARN*> PROCEDURE Long_insert_constants_8_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,38); END Long_insert_constants_8_38;
<*NOWARN*> PROCEDURE Long_insert_constants_8_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,39); END Long_insert_constants_8_39;
<*NOWARN*> PROCEDURE Long_insert_constants_8_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,40); END Long_insert_constants_8_40;
<*NOWARN*> PROCEDURE Long_insert_constants_8_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,41); END Long_insert_constants_8_41;
<*NOWARN*> PROCEDURE Long_insert_constants_8_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,42); END Long_insert_constants_8_42;
<*NOWARN*> PROCEDURE Long_insert_constants_8_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,43); END Long_insert_constants_8_43;
<*NOWARN*> PROCEDURE Long_insert_constants_8_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,44); END Long_insert_constants_8_44;
<*NOWARN*> PROCEDURE Long_insert_constants_8_45(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,45); END Long_insert_constants_8_45;
<*NOWARN*> PROCEDURE Long_insert_constants_8_46(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,46); END Long_insert_constants_8_46;
<*NOWARN*> PROCEDURE Long_insert_constants_8_47(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,47); END Long_insert_constants_8_47;
<*NOWARN*> PROCEDURE Long_insert_constants_8_48(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,48); END Long_insert_constants_8_48;
<*NOWARN*> PROCEDURE Long_insert_constants_8_49(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,49); END Long_insert_constants_8_49;
<*NOWARN*> PROCEDURE Long_insert_constants_8_50(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,50); END Long_insert_constants_8_50;
<*NOWARN*> PROCEDURE Long_insert_constants_8_51(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,51); END Long_insert_constants_8_51;
<*NOWARN*> PROCEDURE Long_insert_constants_8_52(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,52); END Long_insert_constants_8_52;
<*NOWARN*> PROCEDURE Long_insert_constants_8_53(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,53); END Long_insert_constants_8_53;
<*NOWARN*> PROCEDURE Long_insert_constants_8_54(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,54); END Long_insert_constants_8_54;
<*NOWARN*> PROCEDURE Long_insert_constants_8_55(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,55); END Long_insert_constants_8_55;
<*NOWARN*> PROCEDURE Long_insert_constants_8_56(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,8,56); END Long_insert_constants_8_56;
<*NOWARN*> PROCEDURE Long_insert_constants_9_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,0); END Long_insert_constants_9_0;
<*NOWARN*> PROCEDURE Long_insert_constants_9_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,1); END Long_insert_constants_9_1;
<*NOWARN*> PROCEDURE Long_insert_constants_9_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,2); END Long_insert_constants_9_2;
<*NOWARN*> PROCEDURE Long_insert_constants_9_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,3); END Long_insert_constants_9_3;
<*NOWARN*> PROCEDURE Long_insert_constants_9_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,4); END Long_insert_constants_9_4;
<*NOWARN*> PROCEDURE Long_insert_constants_9_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,5); END Long_insert_constants_9_5;
<*NOWARN*> PROCEDURE Long_insert_constants_9_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,6); END Long_insert_constants_9_6;
<*NOWARN*> PROCEDURE Long_insert_constants_9_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,7); END Long_insert_constants_9_7;
<*NOWARN*> PROCEDURE Long_insert_constants_9_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,8); END Long_insert_constants_9_8;
<*NOWARN*> PROCEDURE Long_insert_constants_9_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,9); END Long_insert_constants_9_9;
<*NOWARN*> PROCEDURE Long_insert_constants_9_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,10); END Long_insert_constants_9_10;
<*NOWARN*> PROCEDURE Long_insert_constants_9_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,11); END Long_insert_constants_9_11;
<*NOWARN*> PROCEDURE Long_insert_constants_9_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,12); END Long_insert_constants_9_12;
<*NOWARN*> PROCEDURE Long_insert_constants_9_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,13); END Long_insert_constants_9_13;
<*NOWARN*> PROCEDURE Long_insert_constants_9_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,14); END Long_insert_constants_9_14;
<*NOWARN*> PROCEDURE Long_insert_constants_9_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,15); END Long_insert_constants_9_15;
<*NOWARN*> PROCEDURE Long_insert_constants_9_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,16); END Long_insert_constants_9_16;
<*NOWARN*> PROCEDURE Long_insert_constants_9_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,17); END Long_insert_constants_9_17;
<*NOWARN*> PROCEDURE Long_insert_constants_9_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,18); END Long_insert_constants_9_18;
<*NOWARN*> PROCEDURE Long_insert_constants_9_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,19); END Long_insert_constants_9_19;
<*NOWARN*> PROCEDURE Long_insert_constants_9_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,20); END Long_insert_constants_9_20;
<*NOWARN*> PROCEDURE Long_insert_constants_9_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,21); END Long_insert_constants_9_21;
<*NOWARN*> PROCEDURE Long_insert_constants_9_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,22); END Long_insert_constants_9_22;
<*NOWARN*> PROCEDURE Long_insert_constants_9_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,23); END Long_insert_constants_9_23;
<*NOWARN*> PROCEDURE Long_insert_constants_9_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,24); END Long_insert_constants_9_24;
<*NOWARN*> PROCEDURE Long_insert_constants_9_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,25); END Long_insert_constants_9_25;
<*NOWARN*> PROCEDURE Long_insert_constants_9_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,26); END Long_insert_constants_9_26;
<*NOWARN*> PROCEDURE Long_insert_constants_9_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,27); END Long_insert_constants_9_27;
<*NOWARN*> PROCEDURE Long_insert_constants_9_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,28); END Long_insert_constants_9_28;
<*NOWARN*> PROCEDURE Long_insert_constants_9_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,29); END Long_insert_constants_9_29;
<*NOWARN*> PROCEDURE Long_insert_constants_9_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,30); END Long_insert_constants_9_30;
<*NOWARN*> PROCEDURE Long_insert_constants_9_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,31); END Long_insert_constants_9_31;
<*NOWARN*> PROCEDURE Long_insert_constants_9_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,32); END Long_insert_constants_9_32;
<*NOWARN*> PROCEDURE Long_insert_constants_9_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,33); END Long_insert_constants_9_33;
<*NOWARN*> PROCEDURE Long_insert_constants_9_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,34); END Long_insert_constants_9_34;
<*NOWARN*> PROCEDURE Long_insert_constants_9_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,35); END Long_insert_constants_9_35;
<*NOWARN*> PROCEDURE Long_insert_constants_9_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,36); END Long_insert_constants_9_36;
<*NOWARN*> PROCEDURE Long_insert_constants_9_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,37); END Long_insert_constants_9_37;
<*NOWARN*> PROCEDURE Long_insert_constants_9_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,38); END Long_insert_constants_9_38;
<*NOWARN*> PROCEDURE Long_insert_constants_9_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,39); END Long_insert_constants_9_39;
<*NOWARN*> PROCEDURE Long_insert_constants_9_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,40); END Long_insert_constants_9_40;
<*NOWARN*> PROCEDURE Long_insert_constants_9_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,41); END Long_insert_constants_9_41;
<*NOWARN*> PROCEDURE Long_insert_constants_9_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,42); END Long_insert_constants_9_42;
<*NOWARN*> PROCEDURE Long_insert_constants_9_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,43); END Long_insert_constants_9_43;
<*NOWARN*> PROCEDURE Long_insert_constants_9_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,44); END Long_insert_constants_9_44;
<*NOWARN*> PROCEDURE Long_insert_constants_9_45(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,45); END Long_insert_constants_9_45;
<*NOWARN*> PROCEDURE Long_insert_constants_9_46(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,46); END Long_insert_constants_9_46;
<*NOWARN*> PROCEDURE Long_insert_constants_9_47(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,47); END Long_insert_constants_9_47;
<*NOWARN*> PROCEDURE Long_insert_constants_9_48(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,48); END Long_insert_constants_9_48;
<*NOWARN*> PROCEDURE Long_insert_constants_9_49(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,49); END Long_insert_constants_9_49;
<*NOWARN*> PROCEDURE Long_insert_constants_9_50(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,50); END Long_insert_constants_9_50;
<*NOWARN*> PROCEDURE Long_insert_constants_9_51(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,51); END Long_insert_constants_9_51;
<*NOWARN*> PROCEDURE Long_insert_constants_9_52(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,52); END Long_insert_constants_9_52;
<*NOWARN*> PROCEDURE Long_insert_constants_9_53(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,53); END Long_insert_constants_9_53;
<*NOWARN*> PROCEDURE Long_insert_constants_9_54(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,54); END Long_insert_constants_9_54;
<*NOWARN*> PROCEDURE Long_insert_constants_9_55(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,9,55); END Long_insert_constants_9_55;
<*NOWARN*> PROCEDURE Long_insert_constants_10_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,0); END Long_insert_constants_10_0;
<*NOWARN*> PROCEDURE Long_insert_constants_10_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,1); END Long_insert_constants_10_1;
<*NOWARN*> PROCEDURE Long_insert_constants_10_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,2); END Long_insert_constants_10_2;
<*NOWARN*> PROCEDURE Long_insert_constants_10_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,3); END Long_insert_constants_10_3;
<*NOWARN*> PROCEDURE Long_insert_constants_10_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,4); END Long_insert_constants_10_4;
<*NOWARN*> PROCEDURE Long_insert_constants_10_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,5); END Long_insert_constants_10_5;
<*NOWARN*> PROCEDURE Long_insert_constants_10_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,6); END Long_insert_constants_10_6;
<*NOWARN*> PROCEDURE Long_insert_constants_10_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,7); END Long_insert_constants_10_7;
<*NOWARN*> PROCEDURE Long_insert_constants_10_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,8); END Long_insert_constants_10_8;
<*NOWARN*> PROCEDURE Long_insert_constants_10_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,9); END Long_insert_constants_10_9;
<*NOWARN*> PROCEDURE Long_insert_constants_10_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,10); END Long_insert_constants_10_10;
<*NOWARN*> PROCEDURE Long_insert_constants_10_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,11); END Long_insert_constants_10_11;
<*NOWARN*> PROCEDURE Long_insert_constants_10_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,12); END Long_insert_constants_10_12;
<*NOWARN*> PROCEDURE Long_insert_constants_10_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,13); END Long_insert_constants_10_13;
<*NOWARN*> PROCEDURE Long_insert_constants_10_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,14); END Long_insert_constants_10_14;
<*NOWARN*> PROCEDURE Long_insert_constants_10_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,15); END Long_insert_constants_10_15;
<*NOWARN*> PROCEDURE Long_insert_constants_10_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,16); END Long_insert_constants_10_16;
<*NOWARN*> PROCEDURE Long_insert_constants_10_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,17); END Long_insert_constants_10_17;
<*NOWARN*> PROCEDURE Long_insert_constants_10_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,18); END Long_insert_constants_10_18;
<*NOWARN*> PROCEDURE Long_insert_constants_10_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,19); END Long_insert_constants_10_19;
<*NOWARN*> PROCEDURE Long_insert_constants_10_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,20); END Long_insert_constants_10_20;
<*NOWARN*> PROCEDURE Long_insert_constants_10_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,21); END Long_insert_constants_10_21;
<*NOWARN*> PROCEDURE Long_insert_constants_10_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,22); END Long_insert_constants_10_22;
<*NOWARN*> PROCEDURE Long_insert_constants_10_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,23); END Long_insert_constants_10_23;
<*NOWARN*> PROCEDURE Long_insert_constants_10_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,24); END Long_insert_constants_10_24;
<*NOWARN*> PROCEDURE Long_insert_constants_10_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,25); END Long_insert_constants_10_25;
<*NOWARN*> PROCEDURE Long_insert_constants_10_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,26); END Long_insert_constants_10_26;
<*NOWARN*> PROCEDURE Long_insert_constants_10_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,27); END Long_insert_constants_10_27;
<*NOWARN*> PROCEDURE Long_insert_constants_10_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,28); END Long_insert_constants_10_28;
<*NOWARN*> PROCEDURE Long_insert_constants_10_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,29); END Long_insert_constants_10_29;
<*NOWARN*> PROCEDURE Long_insert_constants_10_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,30); END Long_insert_constants_10_30;
<*NOWARN*> PROCEDURE Long_insert_constants_10_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,31); END Long_insert_constants_10_31;
<*NOWARN*> PROCEDURE Long_insert_constants_10_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,32); END Long_insert_constants_10_32;
<*NOWARN*> PROCEDURE Long_insert_constants_10_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,33); END Long_insert_constants_10_33;
<*NOWARN*> PROCEDURE Long_insert_constants_10_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,34); END Long_insert_constants_10_34;
<*NOWARN*> PROCEDURE Long_insert_constants_10_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,35); END Long_insert_constants_10_35;
<*NOWARN*> PROCEDURE Long_insert_constants_10_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,36); END Long_insert_constants_10_36;
<*NOWARN*> PROCEDURE Long_insert_constants_10_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,37); END Long_insert_constants_10_37;
<*NOWARN*> PROCEDURE Long_insert_constants_10_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,38); END Long_insert_constants_10_38;
<*NOWARN*> PROCEDURE Long_insert_constants_10_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,39); END Long_insert_constants_10_39;
<*NOWARN*> PROCEDURE Long_insert_constants_10_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,40); END Long_insert_constants_10_40;
<*NOWARN*> PROCEDURE Long_insert_constants_10_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,41); END Long_insert_constants_10_41;
<*NOWARN*> PROCEDURE Long_insert_constants_10_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,42); END Long_insert_constants_10_42;
<*NOWARN*> PROCEDURE Long_insert_constants_10_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,43); END Long_insert_constants_10_43;
<*NOWARN*> PROCEDURE Long_insert_constants_10_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,44); END Long_insert_constants_10_44;
<*NOWARN*> PROCEDURE Long_insert_constants_10_45(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,45); END Long_insert_constants_10_45;
<*NOWARN*> PROCEDURE Long_insert_constants_10_46(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,46); END Long_insert_constants_10_46;
<*NOWARN*> PROCEDURE Long_insert_constants_10_47(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,47); END Long_insert_constants_10_47;
<*NOWARN*> PROCEDURE Long_insert_constants_10_48(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,48); END Long_insert_constants_10_48;
<*NOWARN*> PROCEDURE Long_insert_constants_10_49(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,49); END Long_insert_constants_10_49;
<*NOWARN*> PROCEDURE Long_insert_constants_10_50(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,50); END Long_insert_constants_10_50;
<*NOWARN*> PROCEDURE Long_insert_constants_10_51(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,51); END Long_insert_constants_10_51;
<*NOWARN*> PROCEDURE Long_insert_constants_10_52(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,52); END Long_insert_constants_10_52;
<*NOWARN*> PROCEDURE Long_insert_constants_10_53(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,53); END Long_insert_constants_10_53;
<*NOWARN*> PROCEDURE Long_insert_constants_10_54(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,10,54); END Long_insert_constants_10_54;
<*NOWARN*> PROCEDURE Long_insert_constants_11_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,0); END Long_insert_constants_11_0;
<*NOWARN*> PROCEDURE Long_insert_constants_11_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,1); END Long_insert_constants_11_1;
<*NOWARN*> PROCEDURE Long_insert_constants_11_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,2); END Long_insert_constants_11_2;
<*NOWARN*> PROCEDURE Long_insert_constants_11_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,3); END Long_insert_constants_11_3;
<*NOWARN*> PROCEDURE Long_insert_constants_11_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,4); END Long_insert_constants_11_4;
<*NOWARN*> PROCEDURE Long_insert_constants_11_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,5); END Long_insert_constants_11_5;
<*NOWARN*> PROCEDURE Long_insert_constants_11_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,6); END Long_insert_constants_11_6;
<*NOWARN*> PROCEDURE Long_insert_constants_11_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,7); END Long_insert_constants_11_7;
<*NOWARN*> PROCEDURE Long_insert_constants_11_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,8); END Long_insert_constants_11_8;
<*NOWARN*> PROCEDURE Long_insert_constants_11_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,9); END Long_insert_constants_11_9;
<*NOWARN*> PROCEDURE Long_insert_constants_11_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,10); END Long_insert_constants_11_10;
<*NOWARN*> PROCEDURE Long_insert_constants_11_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,11); END Long_insert_constants_11_11;
<*NOWARN*> PROCEDURE Long_insert_constants_11_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,12); END Long_insert_constants_11_12;
<*NOWARN*> PROCEDURE Long_insert_constants_11_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,13); END Long_insert_constants_11_13;
<*NOWARN*> PROCEDURE Long_insert_constants_11_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,14); END Long_insert_constants_11_14;
<*NOWARN*> PROCEDURE Long_insert_constants_11_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,15); END Long_insert_constants_11_15;
<*NOWARN*> PROCEDURE Long_insert_constants_11_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,16); END Long_insert_constants_11_16;
<*NOWARN*> PROCEDURE Long_insert_constants_11_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,17); END Long_insert_constants_11_17;
<*NOWARN*> PROCEDURE Long_insert_constants_11_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,18); END Long_insert_constants_11_18;
<*NOWARN*> PROCEDURE Long_insert_constants_11_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,19); END Long_insert_constants_11_19;
<*NOWARN*> PROCEDURE Long_insert_constants_11_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,20); END Long_insert_constants_11_20;
<*NOWARN*> PROCEDURE Long_insert_constants_11_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,21); END Long_insert_constants_11_21;
<*NOWARN*> PROCEDURE Long_insert_constants_11_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,22); END Long_insert_constants_11_22;
<*NOWARN*> PROCEDURE Long_insert_constants_11_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,23); END Long_insert_constants_11_23;
<*NOWARN*> PROCEDURE Long_insert_constants_11_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,24); END Long_insert_constants_11_24;
<*NOWARN*> PROCEDURE Long_insert_constants_11_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,25); END Long_insert_constants_11_25;
<*NOWARN*> PROCEDURE Long_insert_constants_11_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,26); END Long_insert_constants_11_26;
<*NOWARN*> PROCEDURE Long_insert_constants_11_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,27); END Long_insert_constants_11_27;
<*NOWARN*> PROCEDURE Long_insert_constants_11_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,28); END Long_insert_constants_11_28;
<*NOWARN*> PROCEDURE Long_insert_constants_11_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,29); END Long_insert_constants_11_29;
<*NOWARN*> PROCEDURE Long_insert_constants_11_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,30); END Long_insert_constants_11_30;
<*NOWARN*> PROCEDURE Long_insert_constants_11_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,31); END Long_insert_constants_11_31;
<*NOWARN*> PROCEDURE Long_insert_constants_11_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,32); END Long_insert_constants_11_32;
<*NOWARN*> PROCEDURE Long_insert_constants_11_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,33); END Long_insert_constants_11_33;
<*NOWARN*> PROCEDURE Long_insert_constants_11_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,34); END Long_insert_constants_11_34;
<*NOWARN*> PROCEDURE Long_insert_constants_11_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,35); END Long_insert_constants_11_35;
<*NOWARN*> PROCEDURE Long_insert_constants_11_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,36); END Long_insert_constants_11_36;
<*NOWARN*> PROCEDURE Long_insert_constants_11_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,37); END Long_insert_constants_11_37;
<*NOWARN*> PROCEDURE Long_insert_constants_11_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,38); END Long_insert_constants_11_38;
<*NOWARN*> PROCEDURE Long_insert_constants_11_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,39); END Long_insert_constants_11_39;
<*NOWARN*> PROCEDURE Long_insert_constants_11_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,40); END Long_insert_constants_11_40;
<*NOWARN*> PROCEDURE Long_insert_constants_11_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,41); END Long_insert_constants_11_41;
<*NOWARN*> PROCEDURE Long_insert_constants_11_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,42); END Long_insert_constants_11_42;
<*NOWARN*> PROCEDURE Long_insert_constants_11_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,43); END Long_insert_constants_11_43;
<*NOWARN*> PROCEDURE Long_insert_constants_11_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,44); END Long_insert_constants_11_44;
<*NOWARN*> PROCEDURE Long_insert_constants_11_45(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,45); END Long_insert_constants_11_45;
<*NOWARN*> PROCEDURE Long_insert_constants_11_46(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,46); END Long_insert_constants_11_46;
<*NOWARN*> PROCEDURE Long_insert_constants_11_47(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,47); END Long_insert_constants_11_47;
<*NOWARN*> PROCEDURE Long_insert_constants_11_48(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,48); END Long_insert_constants_11_48;
<*NOWARN*> PROCEDURE Long_insert_constants_11_49(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,49); END Long_insert_constants_11_49;
<*NOWARN*> PROCEDURE Long_insert_constants_11_50(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,50); END Long_insert_constants_11_50;
<*NOWARN*> PROCEDURE Long_insert_constants_11_51(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,51); END Long_insert_constants_11_51;
<*NOWARN*> PROCEDURE Long_insert_constants_11_52(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,52); END Long_insert_constants_11_52;
<*NOWARN*> PROCEDURE Long_insert_constants_11_53(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,11,53); END Long_insert_constants_11_53;
<*NOWARN*> PROCEDURE Long_insert_constants_12_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,0); END Long_insert_constants_12_0;
<*NOWARN*> PROCEDURE Long_insert_constants_12_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,1); END Long_insert_constants_12_1;
<*NOWARN*> PROCEDURE Long_insert_constants_12_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,2); END Long_insert_constants_12_2;
<*NOWARN*> PROCEDURE Long_insert_constants_12_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,3); END Long_insert_constants_12_3;
<*NOWARN*> PROCEDURE Long_insert_constants_12_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,4); END Long_insert_constants_12_4;
<*NOWARN*> PROCEDURE Long_insert_constants_12_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,5); END Long_insert_constants_12_5;
<*NOWARN*> PROCEDURE Long_insert_constants_12_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,6); END Long_insert_constants_12_6;
<*NOWARN*> PROCEDURE Long_insert_constants_12_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,7); END Long_insert_constants_12_7;
<*NOWARN*> PROCEDURE Long_insert_constants_12_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,8); END Long_insert_constants_12_8;
<*NOWARN*> PROCEDURE Long_insert_constants_12_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,9); END Long_insert_constants_12_9;
<*NOWARN*> PROCEDURE Long_insert_constants_12_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,10); END Long_insert_constants_12_10;
<*NOWARN*> PROCEDURE Long_insert_constants_12_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,11); END Long_insert_constants_12_11;
<*NOWARN*> PROCEDURE Long_insert_constants_12_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,12); END Long_insert_constants_12_12;
<*NOWARN*> PROCEDURE Long_insert_constants_12_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,13); END Long_insert_constants_12_13;
<*NOWARN*> PROCEDURE Long_insert_constants_12_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,14); END Long_insert_constants_12_14;
<*NOWARN*> PROCEDURE Long_insert_constants_12_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,15); END Long_insert_constants_12_15;
<*NOWARN*> PROCEDURE Long_insert_constants_12_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,16); END Long_insert_constants_12_16;
<*NOWARN*> PROCEDURE Long_insert_constants_12_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,17); END Long_insert_constants_12_17;
<*NOWARN*> PROCEDURE Long_insert_constants_12_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,18); END Long_insert_constants_12_18;
<*NOWARN*> PROCEDURE Long_insert_constants_12_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,19); END Long_insert_constants_12_19;
<*NOWARN*> PROCEDURE Long_insert_constants_12_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,20); END Long_insert_constants_12_20;
<*NOWARN*> PROCEDURE Long_insert_constants_12_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,21); END Long_insert_constants_12_21;
<*NOWARN*> PROCEDURE Long_insert_constants_12_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,22); END Long_insert_constants_12_22;
<*NOWARN*> PROCEDURE Long_insert_constants_12_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,23); END Long_insert_constants_12_23;
<*NOWARN*> PROCEDURE Long_insert_constants_12_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,24); END Long_insert_constants_12_24;
<*NOWARN*> PROCEDURE Long_insert_constants_12_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,25); END Long_insert_constants_12_25;
<*NOWARN*> PROCEDURE Long_insert_constants_12_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,26); END Long_insert_constants_12_26;
<*NOWARN*> PROCEDURE Long_insert_constants_12_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,27); END Long_insert_constants_12_27;
<*NOWARN*> PROCEDURE Long_insert_constants_12_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,28); END Long_insert_constants_12_28;
<*NOWARN*> PROCEDURE Long_insert_constants_12_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,29); END Long_insert_constants_12_29;
<*NOWARN*> PROCEDURE Long_insert_constants_12_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,30); END Long_insert_constants_12_30;
<*NOWARN*> PROCEDURE Long_insert_constants_12_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,31); END Long_insert_constants_12_31;
<*NOWARN*> PROCEDURE Long_insert_constants_12_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,32); END Long_insert_constants_12_32;
<*NOWARN*> PROCEDURE Long_insert_constants_12_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,33); END Long_insert_constants_12_33;
<*NOWARN*> PROCEDURE Long_insert_constants_12_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,34); END Long_insert_constants_12_34;
<*NOWARN*> PROCEDURE Long_insert_constants_12_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,35); END Long_insert_constants_12_35;
<*NOWARN*> PROCEDURE Long_insert_constants_12_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,36); END Long_insert_constants_12_36;
<*NOWARN*> PROCEDURE Long_insert_constants_12_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,37); END Long_insert_constants_12_37;
<*NOWARN*> PROCEDURE Long_insert_constants_12_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,38); END Long_insert_constants_12_38;
<*NOWARN*> PROCEDURE Long_insert_constants_12_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,39); END Long_insert_constants_12_39;
<*NOWARN*> PROCEDURE Long_insert_constants_12_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,40); END Long_insert_constants_12_40;
<*NOWARN*> PROCEDURE Long_insert_constants_12_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,41); END Long_insert_constants_12_41;
<*NOWARN*> PROCEDURE Long_insert_constants_12_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,42); END Long_insert_constants_12_42;
<*NOWARN*> PROCEDURE Long_insert_constants_12_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,43); END Long_insert_constants_12_43;
<*NOWARN*> PROCEDURE Long_insert_constants_12_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,44); END Long_insert_constants_12_44;
<*NOWARN*> PROCEDURE Long_insert_constants_12_45(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,45); END Long_insert_constants_12_45;
<*NOWARN*> PROCEDURE Long_insert_constants_12_46(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,46); END Long_insert_constants_12_46;
<*NOWARN*> PROCEDURE Long_insert_constants_12_47(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,47); END Long_insert_constants_12_47;
<*NOWARN*> PROCEDURE Long_insert_constants_12_48(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,48); END Long_insert_constants_12_48;
<*NOWARN*> PROCEDURE Long_insert_constants_12_49(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,49); END Long_insert_constants_12_49;
<*NOWARN*> PROCEDURE Long_insert_constants_12_50(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,50); END Long_insert_constants_12_50;
<*NOWARN*> PROCEDURE Long_insert_constants_12_51(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,51); END Long_insert_constants_12_51;
<*NOWARN*> PROCEDURE Long_insert_constants_12_52(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,12,52); END Long_insert_constants_12_52;
<*NOWARN*> PROCEDURE Long_insert_constants_13_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,0); END Long_insert_constants_13_0;
<*NOWARN*> PROCEDURE Long_insert_constants_13_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,1); END Long_insert_constants_13_1;
<*NOWARN*> PROCEDURE Long_insert_constants_13_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,2); END Long_insert_constants_13_2;
<*NOWARN*> PROCEDURE Long_insert_constants_13_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,3); END Long_insert_constants_13_3;
<*NOWARN*> PROCEDURE Long_insert_constants_13_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,4); END Long_insert_constants_13_4;
<*NOWARN*> PROCEDURE Long_insert_constants_13_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,5); END Long_insert_constants_13_5;
<*NOWARN*> PROCEDURE Long_insert_constants_13_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,6); END Long_insert_constants_13_6;
<*NOWARN*> PROCEDURE Long_insert_constants_13_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,7); END Long_insert_constants_13_7;
<*NOWARN*> PROCEDURE Long_insert_constants_13_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,8); END Long_insert_constants_13_8;
<*NOWARN*> PROCEDURE Long_insert_constants_13_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,9); END Long_insert_constants_13_9;
<*NOWARN*> PROCEDURE Long_insert_constants_13_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,10); END Long_insert_constants_13_10;
<*NOWARN*> PROCEDURE Long_insert_constants_13_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,11); END Long_insert_constants_13_11;
<*NOWARN*> PROCEDURE Long_insert_constants_13_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,12); END Long_insert_constants_13_12;
<*NOWARN*> PROCEDURE Long_insert_constants_13_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,13); END Long_insert_constants_13_13;
<*NOWARN*> PROCEDURE Long_insert_constants_13_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,14); END Long_insert_constants_13_14;
<*NOWARN*> PROCEDURE Long_insert_constants_13_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,15); END Long_insert_constants_13_15;
<*NOWARN*> PROCEDURE Long_insert_constants_13_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,16); END Long_insert_constants_13_16;
<*NOWARN*> PROCEDURE Long_insert_constants_13_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,17); END Long_insert_constants_13_17;
<*NOWARN*> PROCEDURE Long_insert_constants_13_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,18); END Long_insert_constants_13_18;
<*NOWARN*> PROCEDURE Long_insert_constants_13_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,19); END Long_insert_constants_13_19;
<*NOWARN*> PROCEDURE Long_insert_constants_13_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,20); END Long_insert_constants_13_20;
<*NOWARN*> PROCEDURE Long_insert_constants_13_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,21); END Long_insert_constants_13_21;
<*NOWARN*> PROCEDURE Long_insert_constants_13_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,22); END Long_insert_constants_13_22;
<*NOWARN*> PROCEDURE Long_insert_constants_13_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,23); END Long_insert_constants_13_23;
<*NOWARN*> PROCEDURE Long_insert_constants_13_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,24); END Long_insert_constants_13_24;
<*NOWARN*> PROCEDURE Long_insert_constants_13_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,25); END Long_insert_constants_13_25;
<*NOWARN*> PROCEDURE Long_insert_constants_13_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,26); END Long_insert_constants_13_26;
<*NOWARN*> PROCEDURE Long_insert_constants_13_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,27); END Long_insert_constants_13_27;
<*NOWARN*> PROCEDURE Long_insert_constants_13_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,28); END Long_insert_constants_13_28;
<*NOWARN*> PROCEDURE Long_insert_constants_13_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,29); END Long_insert_constants_13_29;
<*NOWARN*> PROCEDURE Long_insert_constants_13_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,30); END Long_insert_constants_13_30;
<*NOWARN*> PROCEDURE Long_insert_constants_13_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,31); END Long_insert_constants_13_31;
<*NOWARN*> PROCEDURE Long_insert_constants_13_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,32); END Long_insert_constants_13_32;
<*NOWARN*> PROCEDURE Long_insert_constants_13_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,33); END Long_insert_constants_13_33;
<*NOWARN*> PROCEDURE Long_insert_constants_13_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,34); END Long_insert_constants_13_34;
<*NOWARN*> PROCEDURE Long_insert_constants_13_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,35); END Long_insert_constants_13_35;
<*NOWARN*> PROCEDURE Long_insert_constants_13_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,36); END Long_insert_constants_13_36;
<*NOWARN*> PROCEDURE Long_insert_constants_13_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,37); END Long_insert_constants_13_37;
<*NOWARN*> PROCEDURE Long_insert_constants_13_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,38); END Long_insert_constants_13_38;
<*NOWARN*> PROCEDURE Long_insert_constants_13_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,39); END Long_insert_constants_13_39;
<*NOWARN*> PROCEDURE Long_insert_constants_13_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,40); END Long_insert_constants_13_40;
<*NOWARN*> PROCEDURE Long_insert_constants_13_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,41); END Long_insert_constants_13_41;
<*NOWARN*> PROCEDURE Long_insert_constants_13_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,42); END Long_insert_constants_13_42;
<*NOWARN*> PROCEDURE Long_insert_constants_13_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,43); END Long_insert_constants_13_43;
<*NOWARN*> PROCEDURE Long_insert_constants_13_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,44); END Long_insert_constants_13_44;
<*NOWARN*> PROCEDURE Long_insert_constants_13_45(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,45); END Long_insert_constants_13_45;
<*NOWARN*> PROCEDURE Long_insert_constants_13_46(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,46); END Long_insert_constants_13_46;
<*NOWARN*> PROCEDURE Long_insert_constants_13_47(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,47); END Long_insert_constants_13_47;
<*NOWARN*> PROCEDURE Long_insert_constants_13_48(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,48); END Long_insert_constants_13_48;
<*NOWARN*> PROCEDURE Long_insert_constants_13_49(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,49); END Long_insert_constants_13_49;
<*NOWARN*> PROCEDURE Long_insert_constants_13_50(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,50); END Long_insert_constants_13_50;
<*NOWARN*> PROCEDURE Long_insert_constants_13_51(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,13,51); END Long_insert_constants_13_51;
<*NOWARN*> PROCEDURE Long_insert_constants_14_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,0); END Long_insert_constants_14_0;
<*NOWARN*> PROCEDURE Long_insert_constants_14_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,1); END Long_insert_constants_14_1;
<*NOWARN*> PROCEDURE Long_insert_constants_14_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,2); END Long_insert_constants_14_2;
<*NOWARN*> PROCEDURE Long_insert_constants_14_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,3); END Long_insert_constants_14_3;
<*NOWARN*> PROCEDURE Long_insert_constants_14_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,4); END Long_insert_constants_14_4;
<*NOWARN*> PROCEDURE Long_insert_constants_14_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,5); END Long_insert_constants_14_5;
<*NOWARN*> PROCEDURE Long_insert_constants_14_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,6); END Long_insert_constants_14_6;
<*NOWARN*> PROCEDURE Long_insert_constants_14_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,7); END Long_insert_constants_14_7;
<*NOWARN*> PROCEDURE Long_insert_constants_14_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,8); END Long_insert_constants_14_8;
<*NOWARN*> PROCEDURE Long_insert_constants_14_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,9); END Long_insert_constants_14_9;
<*NOWARN*> PROCEDURE Long_insert_constants_14_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,10); END Long_insert_constants_14_10;
<*NOWARN*> PROCEDURE Long_insert_constants_14_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,11); END Long_insert_constants_14_11;
<*NOWARN*> PROCEDURE Long_insert_constants_14_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,12); END Long_insert_constants_14_12;
<*NOWARN*> PROCEDURE Long_insert_constants_14_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,13); END Long_insert_constants_14_13;
<*NOWARN*> PROCEDURE Long_insert_constants_14_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,14); END Long_insert_constants_14_14;
<*NOWARN*> PROCEDURE Long_insert_constants_14_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,15); END Long_insert_constants_14_15;
<*NOWARN*> PROCEDURE Long_insert_constants_14_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,16); END Long_insert_constants_14_16;
<*NOWARN*> PROCEDURE Long_insert_constants_14_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,17); END Long_insert_constants_14_17;
<*NOWARN*> PROCEDURE Long_insert_constants_14_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,18); END Long_insert_constants_14_18;
<*NOWARN*> PROCEDURE Long_insert_constants_14_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,19); END Long_insert_constants_14_19;
<*NOWARN*> PROCEDURE Long_insert_constants_14_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,20); END Long_insert_constants_14_20;
<*NOWARN*> PROCEDURE Long_insert_constants_14_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,21); END Long_insert_constants_14_21;
<*NOWARN*> PROCEDURE Long_insert_constants_14_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,22); END Long_insert_constants_14_22;
<*NOWARN*> PROCEDURE Long_insert_constants_14_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,23); END Long_insert_constants_14_23;
<*NOWARN*> PROCEDURE Long_insert_constants_14_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,24); END Long_insert_constants_14_24;
<*NOWARN*> PROCEDURE Long_insert_constants_14_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,25); END Long_insert_constants_14_25;
<*NOWARN*> PROCEDURE Long_insert_constants_14_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,26); END Long_insert_constants_14_26;
<*NOWARN*> PROCEDURE Long_insert_constants_14_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,27); END Long_insert_constants_14_27;
<*NOWARN*> PROCEDURE Long_insert_constants_14_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,28); END Long_insert_constants_14_28;
<*NOWARN*> PROCEDURE Long_insert_constants_14_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,29); END Long_insert_constants_14_29;
<*NOWARN*> PROCEDURE Long_insert_constants_14_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,30); END Long_insert_constants_14_30;
<*NOWARN*> PROCEDURE Long_insert_constants_14_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,31); END Long_insert_constants_14_31;
<*NOWARN*> PROCEDURE Long_insert_constants_14_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,32); END Long_insert_constants_14_32;
<*NOWARN*> PROCEDURE Long_insert_constants_14_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,33); END Long_insert_constants_14_33;
<*NOWARN*> PROCEDURE Long_insert_constants_14_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,34); END Long_insert_constants_14_34;
<*NOWARN*> PROCEDURE Long_insert_constants_14_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,35); END Long_insert_constants_14_35;
<*NOWARN*> PROCEDURE Long_insert_constants_14_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,36); END Long_insert_constants_14_36;
<*NOWARN*> PROCEDURE Long_insert_constants_14_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,37); END Long_insert_constants_14_37;
<*NOWARN*> PROCEDURE Long_insert_constants_14_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,38); END Long_insert_constants_14_38;
<*NOWARN*> PROCEDURE Long_insert_constants_14_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,39); END Long_insert_constants_14_39;
<*NOWARN*> PROCEDURE Long_insert_constants_14_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,40); END Long_insert_constants_14_40;
<*NOWARN*> PROCEDURE Long_insert_constants_14_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,41); END Long_insert_constants_14_41;
<*NOWARN*> PROCEDURE Long_insert_constants_14_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,42); END Long_insert_constants_14_42;
<*NOWARN*> PROCEDURE Long_insert_constants_14_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,43); END Long_insert_constants_14_43;
<*NOWARN*> PROCEDURE Long_insert_constants_14_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,44); END Long_insert_constants_14_44;
<*NOWARN*> PROCEDURE Long_insert_constants_14_45(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,45); END Long_insert_constants_14_45;
<*NOWARN*> PROCEDURE Long_insert_constants_14_46(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,46); END Long_insert_constants_14_46;
<*NOWARN*> PROCEDURE Long_insert_constants_14_47(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,47); END Long_insert_constants_14_47;
<*NOWARN*> PROCEDURE Long_insert_constants_14_48(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,48); END Long_insert_constants_14_48;
<*NOWARN*> PROCEDURE Long_insert_constants_14_49(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,49); END Long_insert_constants_14_49;
<*NOWARN*> PROCEDURE Long_insert_constants_14_50(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,14,50); END Long_insert_constants_14_50;
<*NOWARN*> PROCEDURE Long_insert_constants_15_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,0); END Long_insert_constants_15_0;
<*NOWARN*> PROCEDURE Long_insert_constants_15_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,1); END Long_insert_constants_15_1;
<*NOWARN*> PROCEDURE Long_insert_constants_15_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,2); END Long_insert_constants_15_2;
<*NOWARN*> PROCEDURE Long_insert_constants_15_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,3); END Long_insert_constants_15_3;
<*NOWARN*> PROCEDURE Long_insert_constants_15_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,4); END Long_insert_constants_15_4;
<*NOWARN*> PROCEDURE Long_insert_constants_15_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,5); END Long_insert_constants_15_5;
<*NOWARN*> PROCEDURE Long_insert_constants_15_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,6); END Long_insert_constants_15_6;
<*NOWARN*> PROCEDURE Long_insert_constants_15_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,7); END Long_insert_constants_15_7;
<*NOWARN*> PROCEDURE Long_insert_constants_15_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,8); END Long_insert_constants_15_8;
<*NOWARN*> PROCEDURE Long_insert_constants_15_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,9); END Long_insert_constants_15_9;
<*NOWARN*> PROCEDURE Long_insert_constants_15_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,10); END Long_insert_constants_15_10;
<*NOWARN*> PROCEDURE Long_insert_constants_15_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,11); END Long_insert_constants_15_11;
<*NOWARN*> PROCEDURE Long_insert_constants_15_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,12); END Long_insert_constants_15_12;
<*NOWARN*> PROCEDURE Long_insert_constants_15_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,13); END Long_insert_constants_15_13;
<*NOWARN*> PROCEDURE Long_insert_constants_15_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,14); END Long_insert_constants_15_14;
<*NOWARN*> PROCEDURE Long_insert_constants_15_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,15); END Long_insert_constants_15_15;
<*NOWARN*> PROCEDURE Long_insert_constants_15_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,16); END Long_insert_constants_15_16;
<*NOWARN*> PROCEDURE Long_insert_constants_15_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,17); END Long_insert_constants_15_17;
<*NOWARN*> PROCEDURE Long_insert_constants_15_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,18); END Long_insert_constants_15_18;
<*NOWARN*> PROCEDURE Long_insert_constants_15_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,19); END Long_insert_constants_15_19;
<*NOWARN*> PROCEDURE Long_insert_constants_15_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,20); END Long_insert_constants_15_20;
<*NOWARN*> PROCEDURE Long_insert_constants_15_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,21); END Long_insert_constants_15_21;
<*NOWARN*> PROCEDURE Long_insert_constants_15_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,22); END Long_insert_constants_15_22;
<*NOWARN*> PROCEDURE Long_insert_constants_15_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,23); END Long_insert_constants_15_23;
<*NOWARN*> PROCEDURE Long_insert_constants_15_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,24); END Long_insert_constants_15_24;
<*NOWARN*> PROCEDURE Long_insert_constants_15_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,25); END Long_insert_constants_15_25;
<*NOWARN*> PROCEDURE Long_insert_constants_15_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,26); END Long_insert_constants_15_26;
<*NOWARN*> PROCEDURE Long_insert_constants_15_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,27); END Long_insert_constants_15_27;
<*NOWARN*> PROCEDURE Long_insert_constants_15_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,28); END Long_insert_constants_15_28;
<*NOWARN*> PROCEDURE Long_insert_constants_15_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,29); END Long_insert_constants_15_29;
<*NOWARN*> PROCEDURE Long_insert_constants_15_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,30); END Long_insert_constants_15_30;
<*NOWARN*> PROCEDURE Long_insert_constants_15_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,31); END Long_insert_constants_15_31;
<*NOWARN*> PROCEDURE Long_insert_constants_15_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,32); END Long_insert_constants_15_32;
<*NOWARN*> PROCEDURE Long_insert_constants_15_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,33); END Long_insert_constants_15_33;
<*NOWARN*> PROCEDURE Long_insert_constants_15_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,34); END Long_insert_constants_15_34;
<*NOWARN*> PROCEDURE Long_insert_constants_15_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,35); END Long_insert_constants_15_35;
<*NOWARN*> PROCEDURE Long_insert_constants_15_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,36); END Long_insert_constants_15_36;
<*NOWARN*> PROCEDURE Long_insert_constants_15_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,37); END Long_insert_constants_15_37;
<*NOWARN*> PROCEDURE Long_insert_constants_15_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,38); END Long_insert_constants_15_38;
<*NOWARN*> PROCEDURE Long_insert_constants_15_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,39); END Long_insert_constants_15_39;
<*NOWARN*> PROCEDURE Long_insert_constants_15_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,40); END Long_insert_constants_15_40;
<*NOWARN*> PROCEDURE Long_insert_constants_15_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,41); END Long_insert_constants_15_41;
<*NOWARN*> PROCEDURE Long_insert_constants_15_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,42); END Long_insert_constants_15_42;
<*NOWARN*> PROCEDURE Long_insert_constants_15_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,43); END Long_insert_constants_15_43;
<*NOWARN*> PROCEDURE Long_insert_constants_15_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,44); END Long_insert_constants_15_44;
<*NOWARN*> PROCEDURE Long_insert_constants_15_45(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,45); END Long_insert_constants_15_45;
<*NOWARN*> PROCEDURE Long_insert_constants_15_46(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,46); END Long_insert_constants_15_46;
<*NOWARN*> PROCEDURE Long_insert_constants_15_47(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,47); END Long_insert_constants_15_47;
<*NOWARN*> PROCEDURE Long_insert_constants_15_48(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,48); END Long_insert_constants_15_48;
<*NOWARN*> PROCEDURE Long_insert_constants_15_49(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,15,49); END Long_insert_constants_15_49;
<*NOWARN*> PROCEDURE Long_insert_constants_16_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,0); END Long_insert_constants_16_0;
<*NOWARN*> PROCEDURE Long_insert_constants_16_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,1); END Long_insert_constants_16_1;
<*NOWARN*> PROCEDURE Long_insert_constants_16_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,2); END Long_insert_constants_16_2;
<*NOWARN*> PROCEDURE Long_insert_constants_16_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,3); END Long_insert_constants_16_3;
<*NOWARN*> PROCEDURE Long_insert_constants_16_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,4); END Long_insert_constants_16_4;
<*NOWARN*> PROCEDURE Long_insert_constants_16_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,5); END Long_insert_constants_16_5;
<*NOWARN*> PROCEDURE Long_insert_constants_16_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,6); END Long_insert_constants_16_6;
<*NOWARN*> PROCEDURE Long_insert_constants_16_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,7); END Long_insert_constants_16_7;
<*NOWARN*> PROCEDURE Long_insert_constants_16_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,8); END Long_insert_constants_16_8;
<*NOWARN*> PROCEDURE Long_insert_constants_16_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,9); END Long_insert_constants_16_9;
<*NOWARN*> PROCEDURE Long_insert_constants_16_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,10); END Long_insert_constants_16_10;
<*NOWARN*> PROCEDURE Long_insert_constants_16_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,11); END Long_insert_constants_16_11;
<*NOWARN*> PROCEDURE Long_insert_constants_16_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,12); END Long_insert_constants_16_12;
<*NOWARN*> PROCEDURE Long_insert_constants_16_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,13); END Long_insert_constants_16_13;
<*NOWARN*> PROCEDURE Long_insert_constants_16_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,14); END Long_insert_constants_16_14;
<*NOWARN*> PROCEDURE Long_insert_constants_16_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,15); END Long_insert_constants_16_15;
<*NOWARN*> PROCEDURE Long_insert_constants_16_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,16); END Long_insert_constants_16_16;
<*NOWARN*> PROCEDURE Long_insert_constants_16_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,17); END Long_insert_constants_16_17;
<*NOWARN*> PROCEDURE Long_insert_constants_16_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,18); END Long_insert_constants_16_18;
<*NOWARN*> PROCEDURE Long_insert_constants_16_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,19); END Long_insert_constants_16_19;
<*NOWARN*> PROCEDURE Long_insert_constants_16_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,20); END Long_insert_constants_16_20;
<*NOWARN*> PROCEDURE Long_insert_constants_16_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,21); END Long_insert_constants_16_21;
<*NOWARN*> PROCEDURE Long_insert_constants_16_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,22); END Long_insert_constants_16_22;
<*NOWARN*> PROCEDURE Long_insert_constants_16_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,23); END Long_insert_constants_16_23;
<*NOWARN*> PROCEDURE Long_insert_constants_16_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,24); END Long_insert_constants_16_24;
<*NOWARN*> PROCEDURE Long_insert_constants_16_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,25); END Long_insert_constants_16_25;
<*NOWARN*> PROCEDURE Long_insert_constants_16_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,26); END Long_insert_constants_16_26;
<*NOWARN*> PROCEDURE Long_insert_constants_16_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,27); END Long_insert_constants_16_27;
<*NOWARN*> PROCEDURE Long_insert_constants_16_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,28); END Long_insert_constants_16_28;
<*NOWARN*> PROCEDURE Long_insert_constants_16_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,29); END Long_insert_constants_16_29;
<*NOWARN*> PROCEDURE Long_insert_constants_16_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,30); END Long_insert_constants_16_30;
<*NOWARN*> PROCEDURE Long_insert_constants_16_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,31); END Long_insert_constants_16_31;
<*NOWARN*> PROCEDURE Long_insert_constants_16_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,32); END Long_insert_constants_16_32;
<*NOWARN*> PROCEDURE Long_insert_constants_16_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,33); END Long_insert_constants_16_33;
<*NOWARN*> PROCEDURE Long_insert_constants_16_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,34); END Long_insert_constants_16_34;
<*NOWARN*> PROCEDURE Long_insert_constants_16_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,35); END Long_insert_constants_16_35;
<*NOWARN*> PROCEDURE Long_insert_constants_16_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,36); END Long_insert_constants_16_36;
<*NOWARN*> PROCEDURE Long_insert_constants_16_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,37); END Long_insert_constants_16_37;
<*NOWARN*> PROCEDURE Long_insert_constants_16_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,38); END Long_insert_constants_16_38;
<*NOWARN*> PROCEDURE Long_insert_constants_16_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,39); END Long_insert_constants_16_39;
<*NOWARN*> PROCEDURE Long_insert_constants_16_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,40); END Long_insert_constants_16_40;
<*NOWARN*> PROCEDURE Long_insert_constants_16_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,41); END Long_insert_constants_16_41;
<*NOWARN*> PROCEDURE Long_insert_constants_16_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,42); END Long_insert_constants_16_42;
<*NOWARN*> PROCEDURE Long_insert_constants_16_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,43); END Long_insert_constants_16_43;
<*NOWARN*> PROCEDURE Long_insert_constants_16_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,44); END Long_insert_constants_16_44;
<*NOWARN*> PROCEDURE Long_insert_constants_16_45(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,45); END Long_insert_constants_16_45;
<*NOWARN*> PROCEDURE Long_insert_constants_16_46(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,46); END Long_insert_constants_16_46;
<*NOWARN*> PROCEDURE Long_insert_constants_16_47(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,47); END Long_insert_constants_16_47;
<*NOWARN*> PROCEDURE Long_insert_constants_16_48(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,16,48); END Long_insert_constants_16_48;
<*NOWARN*> PROCEDURE Long_insert_constants_17_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,0); END Long_insert_constants_17_0;
<*NOWARN*> PROCEDURE Long_insert_constants_17_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,1); END Long_insert_constants_17_1;
<*NOWARN*> PROCEDURE Long_insert_constants_17_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,2); END Long_insert_constants_17_2;
<*NOWARN*> PROCEDURE Long_insert_constants_17_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,3); END Long_insert_constants_17_3;
<*NOWARN*> PROCEDURE Long_insert_constants_17_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,4); END Long_insert_constants_17_4;
<*NOWARN*> PROCEDURE Long_insert_constants_17_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,5); END Long_insert_constants_17_5;
<*NOWARN*> PROCEDURE Long_insert_constants_17_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,6); END Long_insert_constants_17_6;
<*NOWARN*> PROCEDURE Long_insert_constants_17_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,7); END Long_insert_constants_17_7;
<*NOWARN*> PROCEDURE Long_insert_constants_17_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,8); END Long_insert_constants_17_8;
<*NOWARN*> PROCEDURE Long_insert_constants_17_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,9); END Long_insert_constants_17_9;
<*NOWARN*> PROCEDURE Long_insert_constants_17_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,10); END Long_insert_constants_17_10;
<*NOWARN*> PROCEDURE Long_insert_constants_17_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,11); END Long_insert_constants_17_11;
<*NOWARN*> PROCEDURE Long_insert_constants_17_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,12); END Long_insert_constants_17_12;
<*NOWARN*> PROCEDURE Long_insert_constants_17_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,13); END Long_insert_constants_17_13;
<*NOWARN*> PROCEDURE Long_insert_constants_17_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,14); END Long_insert_constants_17_14;
<*NOWARN*> PROCEDURE Long_insert_constants_17_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,15); END Long_insert_constants_17_15;
<*NOWARN*> PROCEDURE Long_insert_constants_17_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,16); END Long_insert_constants_17_16;
<*NOWARN*> PROCEDURE Long_insert_constants_17_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,17); END Long_insert_constants_17_17;
<*NOWARN*> PROCEDURE Long_insert_constants_17_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,18); END Long_insert_constants_17_18;
<*NOWARN*> PROCEDURE Long_insert_constants_17_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,19); END Long_insert_constants_17_19;
<*NOWARN*> PROCEDURE Long_insert_constants_17_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,20); END Long_insert_constants_17_20;
<*NOWARN*> PROCEDURE Long_insert_constants_17_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,21); END Long_insert_constants_17_21;
<*NOWARN*> PROCEDURE Long_insert_constants_17_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,22); END Long_insert_constants_17_22;
<*NOWARN*> PROCEDURE Long_insert_constants_17_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,23); END Long_insert_constants_17_23;
<*NOWARN*> PROCEDURE Long_insert_constants_17_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,24); END Long_insert_constants_17_24;
<*NOWARN*> PROCEDURE Long_insert_constants_17_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,25); END Long_insert_constants_17_25;
<*NOWARN*> PROCEDURE Long_insert_constants_17_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,26); END Long_insert_constants_17_26;
<*NOWARN*> PROCEDURE Long_insert_constants_17_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,27); END Long_insert_constants_17_27;
<*NOWARN*> PROCEDURE Long_insert_constants_17_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,28); END Long_insert_constants_17_28;
<*NOWARN*> PROCEDURE Long_insert_constants_17_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,29); END Long_insert_constants_17_29;
<*NOWARN*> PROCEDURE Long_insert_constants_17_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,30); END Long_insert_constants_17_30;
<*NOWARN*> PROCEDURE Long_insert_constants_17_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,31); END Long_insert_constants_17_31;
<*NOWARN*> PROCEDURE Long_insert_constants_17_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,32); END Long_insert_constants_17_32;
<*NOWARN*> PROCEDURE Long_insert_constants_17_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,33); END Long_insert_constants_17_33;
<*NOWARN*> PROCEDURE Long_insert_constants_17_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,34); END Long_insert_constants_17_34;
<*NOWARN*> PROCEDURE Long_insert_constants_17_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,35); END Long_insert_constants_17_35;
<*NOWARN*> PROCEDURE Long_insert_constants_17_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,36); END Long_insert_constants_17_36;
<*NOWARN*> PROCEDURE Long_insert_constants_17_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,37); END Long_insert_constants_17_37;
<*NOWARN*> PROCEDURE Long_insert_constants_17_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,38); END Long_insert_constants_17_38;
<*NOWARN*> PROCEDURE Long_insert_constants_17_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,39); END Long_insert_constants_17_39;
<*NOWARN*> PROCEDURE Long_insert_constants_17_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,40); END Long_insert_constants_17_40;
<*NOWARN*> PROCEDURE Long_insert_constants_17_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,41); END Long_insert_constants_17_41;
<*NOWARN*> PROCEDURE Long_insert_constants_17_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,42); END Long_insert_constants_17_42;
<*NOWARN*> PROCEDURE Long_insert_constants_17_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,43); END Long_insert_constants_17_43;
<*NOWARN*> PROCEDURE Long_insert_constants_17_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,44); END Long_insert_constants_17_44;
<*NOWARN*> PROCEDURE Long_insert_constants_17_45(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,45); END Long_insert_constants_17_45;
<*NOWARN*> PROCEDURE Long_insert_constants_17_46(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,46); END Long_insert_constants_17_46;
<*NOWARN*> PROCEDURE Long_insert_constants_17_47(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,17,47); END Long_insert_constants_17_47;
<*NOWARN*> PROCEDURE Long_insert_constants_18_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,0); END Long_insert_constants_18_0;
<*NOWARN*> PROCEDURE Long_insert_constants_18_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,1); END Long_insert_constants_18_1;
<*NOWARN*> PROCEDURE Long_insert_constants_18_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,2); END Long_insert_constants_18_2;
<*NOWARN*> PROCEDURE Long_insert_constants_18_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,3); END Long_insert_constants_18_3;
<*NOWARN*> PROCEDURE Long_insert_constants_18_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,4); END Long_insert_constants_18_4;
<*NOWARN*> PROCEDURE Long_insert_constants_18_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,5); END Long_insert_constants_18_5;
<*NOWARN*> PROCEDURE Long_insert_constants_18_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,6); END Long_insert_constants_18_6;
<*NOWARN*> PROCEDURE Long_insert_constants_18_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,7); END Long_insert_constants_18_7;
<*NOWARN*> PROCEDURE Long_insert_constants_18_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,8); END Long_insert_constants_18_8;
<*NOWARN*> PROCEDURE Long_insert_constants_18_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,9); END Long_insert_constants_18_9;
<*NOWARN*> PROCEDURE Long_insert_constants_18_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,10); END Long_insert_constants_18_10;
<*NOWARN*> PROCEDURE Long_insert_constants_18_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,11); END Long_insert_constants_18_11;
<*NOWARN*> PROCEDURE Long_insert_constants_18_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,12); END Long_insert_constants_18_12;
<*NOWARN*> PROCEDURE Long_insert_constants_18_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,13); END Long_insert_constants_18_13;
<*NOWARN*> PROCEDURE Long_insert_constants_18_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,14); END Long_insert_constants_18_14;
<*NOWARN*> PROCEDURE Long_insert_constants_18_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,15); END Long_insert_constants_18_15;
<*NOWARN*> PROCEDURE Long_insert_constants_18_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,16); END Long_insert_constants_18_16;
<*NOWARN*> PROCEDURE Long_insert_constants_18_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,17); END Long_insert_constants_18_17;
<*NOWARN*> PROCEDURE Long_insert_constants_18_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,18); END Long_insert_constants_18_18;
<*NOWARN*> PROCEDURE Long_insert_constants_18_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,19); END Long_insert_constants_18_19;
<*NOWARN*> PROCEDURE Long_insert_constants_18_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,20); END Long_insert_constants_18_20;
<*NOWARN*> PROCEDURE Long_insert_constants_18_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,21); END Long_insert_constants_18_21;
<*NOWARN*> PROCEDURE Long_insert_constants_18_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,22); END Long_insert_constants_18_22;
<*NOWARN*> PROCEDURE Long_insert_constants_18_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,23); END Long_insert_constants_18_23;
<*NOWARN*> PROCEDURE Long_insert_constants_18_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,24); END Long_insert_constants_18_24;
<*NOWARN*> PROCEDURE Long_insert_constants_18_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,25); END Long_insert_constants_18_25;
<*NOWARN*> PROCEDURE Long_insert_constants_18_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,26); END Long_insert_constants_18_26;
<*NOWARN*> PROCEDURE Long_insert_constants_18_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,27); END Long_insert_constants_18_27;
<*NOWARN*> PROCEDURE Long_insert_constants_18_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,28); END Long_insert_constants_18_28;
<*NOWARN*> PROCEDURE Long_insert_constants_18_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,29); END Long_insert_constants_18_29;
<*NOWARN*> PROCEDURE Long_insert_constants_18_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,30); END Long_insert_constants_18_30;
<*NOWARN*> PROCEDURE Long_insert_constants_18_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,31); END Long_insert_constants_18_31;
<*NOWARN*> PROCEDURE Long_insert_constants_18_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,32); END Long_insert_constants_18_32;
<*NOWARN*> PROCEDURE Long_insert_constants_18_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,33); END Long_insert_constants_18_33;
<*NOWARN*> PROCEDURE Long_insert_constants_18_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,34); END Long_insert_constants_18_34;
<*NOWARN*> PROCEDURE Long_insert_constants_18_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,35); END Long_insert_constants_18_35;
<*NOWARN*> PROCEDURE Long_insert_constants_18_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,36); END Long_insert_constants_18_36;
<*NOWARN*> PROCEDURE Long_insert_constants_18_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,37); END Long_insert_constants_18_37;
<*NOWARN*> PROCEDURE Long_insert_constants_18_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,38); END Long_insert_constants_18_38;
<*NOWARN*> PROCEDURE Long_insert_constants_18_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,39); END Long_insert_constants_18_39;
<*NOWARN*> PROCEDURE Long_insert_constants_18_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,40); END Long_insert_constants_18_40;
<*NOWARN*> PROCEDURE Long_insert_constants_18_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,41); END Long_insert_constants_18_41;
<*NOWARN*> PROCEDURE Long_insert_constants_18_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,42); END Long_insert_constants_18_42;
<*NOWARN*> PROCEDURE Long_insert_constants_18_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,43); END Long_insert_constants_18_43;
<*NOWARN*> PROCEDURE Long_insert_constants_18_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,44); END Long_insert_constants_18_44;
<*NOWARN*> PROCEDURE Long_insert_constants_18_45(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,45); END Long_insert_constants_18_45;
<*NOWARN*> PROCEDURE Long_insert_constants_18_46(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,18,46); END Long_insert_constants_18_46;
<*NOWARN*> PROCEDURE Long_insert_constants_19_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,0); END Long_insert_constants_19_0;
<*NOWARN*> PROCEDURE Long_insert_constants_19_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,1); END Long_insert_constants_19_1;
<*NOWARN*> PROCEDURE Long_insert_constants_19_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,2); END Long_insert_constants_19_2;
<*NOWARN*> PROCEDURE Long_insert_constants_19_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,3); END Long_insert_constants_19_3;
<*NOWARN*> PROCEDURE Long_insert_constants_19_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,4); END Long_insert_constants_19_4;
<*NOWARN*> PROCEDURE Long_insert_constants_19_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,5); END Long_insert_constants_19_5;
<*NOWARN*> PROCEDURE Long_insert_constants_19_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,6); END Long_insert_constants_19_6;
<*NOWARN*> PROCEDURE Long_insert_constants_19_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,7); END Long_insert_constants_19_7;
<*NOWARN*> PROCEDURE Long_insert_constants_19_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,8); END Long_insert_constants_19_8;
<*NOWARN*> PROCEDURE Long_insert_constants_19_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,9); END Long_insert_constants_19_9;
<*NOWARN*> PROCEDURE Long_insert_constants_19_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,10); END Long_insert_constants_19_10;
<*NOWARN*> PROCEDURE Long_insert_constants_19_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,11); END Long_insert_constants_19_11;
<*NOWARN*> PROCEDURE Long_insert_constants_19_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,12); END Long_insert_constants_19_12;
<*NOWARN*> PROCEDURE Long_insert_constants_19_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,13); END Long_insert_constants_19_13;
<*NOWARN*> PROCEDURE Long_insert_constants_19_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,14); END Long_insert_constants_19_14;
<*NOWARN*> PROCEDURE Long_insert_constants_19_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,15); END Long_insert_constants_19_15;
<*NOWARN*> PROCEDURE Long_insert_constants_19_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,16); END Long_insert_constants_19_16;
<*NOWARN*> PROCEDURE Long_insert_constants_19_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,17); END Long_insert_constants_19_17;
<*NOWARN*> PROCEDURE Long_insert_constants_19_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,18); END Long_insert_constants_19_18;
<*NOWARN*> PROCEDURE Long_insert_constants_19_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,19); END Long_insert_constants_19_19;
<*NOWARN*> PROCEDURE Long_insert_constants_19_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,20); END Long_insert_constants_19_20;
<*NOWARN*> PROCEDURE Long_insert_constants_19_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,21); END Long_insert_constants_19_21;
<*NOWARN*> PROCEDURE Long_insert_constants_19_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,22); END Long_insert_constants_19_22;
<*NOWARN*> PROCEDURE Long_insert_constants_19_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,23); END Long_insert_constants_19_23;
<*NOWARN*> PROCEDURE Long_insert_constants_19_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,24); END Long_insert_constants_19_24;
<*NOWARN*> PROCEDURE Long_insert_constants_19_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,25); END Long_insert_constants_19_25;
<*NOWARN*> PROCEDURE Long_insert_constants_19_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,26); END Long_insert_constants_19_26;
<*NOWARN*> PROCEDURE Long_insert_constants_19_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,27); END Long_insert_constants_19_27;
<*NOWARN*> PROCEDURE Long_insert_constants_19_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,28); END Long_insert_constants_19_28;
<*NOWARN*> PROCEDURE Long_insert_constants_19_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,29); END Long_insert_constants_19_29;
<*NOWARN*> PROCEDURE Long_insert_constants_19_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,30); END Long_insert_constants_19_30;
<*NOWARN*> PROCEDURE Long_insert_constants_19_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,31); END Long_insert_constants_19_31;
<*NOWARN*> PROCEDURE Long_insert_constants_19_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,32); END Long_insert_constants_19_32;
<*NOWARN*> PROCEDURE Long_insert_constants_19_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,33); END Long_insert_constants_19_33;
<*NOWARN*> PROCEDURE Long_insert_constants_19_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,34); END Long_insert_constants_19_34;
<*NOWARN*> PROCEDURE Long_insert_constants_19_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,35); END Long_insert_constants_19_35;
<*NOWARN*> PROCEDURE Long_insert_constants_19_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,36); END Long_insert_constants_19_36;
<*NOWARN*> PROCEDURE Long_insert_constants_19_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,37); END Long_insert_constants_19_37;
<*NOWARN*> PROCEDURE Long_insert_constants_19_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,38); END Long_insert_constants_19_38;
<*NOWARN*> PROCEDURE Long_insert_constants_19_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,39); END Long_insert_constants_19_39;
<*NOWARN*> PROCEDURE Long_insert_constants_19_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,40); END Long_insert_constants_19_40;
<*NOWARN*> PROCEDURE Long_insert_constants_19_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,41); END Long_insert_constants_19_41;
<*NOWARN*> PROCEDURE Long_insert_constants_19_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,42); END Long_insert_constants_19_42;
<*NOWARN*> PROCEDURE Long_insert_constants_19_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,43); END Long_insert_constants_19_43;
<*NOWARN*> PROCEDURE Long_insert_constants_19_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,44); END Long_insert_constants_19_44;
<*NOWARN*> PROCEDURE Long_insert_constants_19_45(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,19,45); END Long_insert_constants_19_45;
<*NOWARN*> PROCEDURE Long_insert_constants_20_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,0); END Long_insert_constants_20_0;
<*NOWARN*> PROCEDURE Long_insert_constants_20_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,1); END Long_insert_constants_20_1;
<*NOWARN*> PROCEDURE Long_insert_constants_20_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,2); END Long_insert_constants_20_2;
<*NOWARN*> PROCEDURE Long_insert_constants_20_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,3); END Long_insert_constants_20_3;
<*NOWARN*> PROCEDURE Long_insert_constants_20_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,4); END Long_insert_constants_20_4;
<*NOWARN*> PROCEDURE Long_insert_constants_20_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,5); END Long_insert_constants_20_5;
<*NOWARN*> PROCEDURE Long_insert_constants_20_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,6); END Long_insert_constants_20_6;
<*NOWARN*> PROCEDURE Long_insert_constants_20_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,7); END Long_insert_constants_20_7;
<*NOWARN*> PROCEDURE Long_insert_constants_20_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,8); END Long_insert_constants_20_8;
<*NOWARN*> PROCEDURE Long_insert_constants_20_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,9); END Long_insert_constants_20_9;
<*NOWARN*> PROCEDURE Long_insert_constants_20_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,10); END Long_insert_constants_20_10;
<*NOWARN*> PROCEDURE Long_insert_constants_20_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,11); END Long_insert_constants_20_11;
<*NOWARN*> PROCEDURE Long_insert_constants_20_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,12); END Long_insert_constants_20_12;
<*NOWARN*> PROCEDURE Long_insert_constants_20_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,13); END Long_insert_constants_20_13;
<*NOWARN*> PROCEDURE Long_insert_constants_20_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,14); END Long_insert_constants_20_14;
<*NOWARN*> PROCEDURE Long_insert_constants_20_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,15); END Long_insert_constants_20_15;
<*NOWARN*> PROCEDURE Long_insert_constants_20_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,16); END Long_insert_constants_20_16;
<*NOWARN*> PROCEDURE Long_insert_constants_20_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,17); END Long_insert_constants_20_17;
<*NOWARN*> PROCEDURE Long_insert_constants_20_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,18); END Long_insert_constants_20_18;
<*NOWARN*> PROCEDURE Long_insert_constants_20_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,19); END Long_insert_constants_20_19;
<*NOWARN*> PROCEDURE Long_insert_constants_20_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,20); END Long_insert_constants_20_20;
<*NOWARN*> PROCEDURE Long_insert_constants_20_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,21); END Long_insert_constants_20_21;
<*NOWARN*> PROCEDURE Long_insert_constants_20_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,22); END Long_insert_constants_20_22;
<*NOWARN*> PROCEDURE Long_insert_constants_20_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,23); END Long_insert_constants_20_23;
<*NOWARN*> PROCEDURE Long_insert_constants_20_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,24); END Long_insert_constants_20_24;
<*NOWARN*> PROCEDURE Long_insert_constants_20_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,25); END Long_insert_constants_20_25;
<*NOWARN*> PROCEDURE Long_insert_constants_20_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,26); END Long_insert_constants_20_26;
<*NOWARN*> PROCEDURE Long_insert_constants_20_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,27); END Long_insert_constants_20_27;
<*NOWARN*> PROCEDURE Long_insert_constants_20_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,28); END Long_insert_constants_20_28;
<*NOWARN*> PROCEDURE Long_insert_constants_20_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,29); END Long_insert_constants_20_29;
<*NOWARN*> PROCEDURE Long_insert_constants_20_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,30); END Long_insert_constants_20_30;
<*NOWARN*> PROCEDURE Long_insert_constants_20_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,31); END Long_insert_constants_20_31;
<*NOWARN*> PROCEDURE Long_insert_constants_20_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,32); END Long_insert_constants_20_32;
<*NOWARN*> PROCEDURE Long_insert_constants_20_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,33); END Long_insert_constants_20_33;
<*NOWARN*> PROCEDURE Long_insert_constants_20_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,34); END Long_insert_constants_20_34;
<*NOWARN*> PROCEDURE Long_insert_constants_20_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,35); END Long_insert_constants_20_35;
<*NOWARN*> PROCEDURE Long_insert_constants_20_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,36); END Long_insert_constants_20_36;
<*NOWARN*> PROCEDURE Long_insert_constants_20_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,37); END Long_insert_constants_20_37;
<*NOWARN*> PROCEDURE Long_insert_constants_20_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,38); END Long_insert_constants_20_38;
<*NOWARN*> PROCEDURE Long_insert_constants_20_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,39); END Long_insert_constants_20_39;
<*NOWARN*> PROCEDURE Long_insert_constants_20_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,40); END Long_insert_constants_20_40;
<*NOWARN*> PROCEDURE Long_insert_constants_20_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,41); END Long_insert_constants_20_41;
<*NOWARN*> PROCEDURE Long_insert_constants_20_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,42); END Long_insert_constants_20_42;
<*NOWARN*> PROCEDURE Long_insert_constants_20_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,43); END Long_insert_constants_20_43;
<*NOWARN*> PROCEDURE Long_insert_constants_20_44(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,20,44); END Long_insert_constants_20_44;
<*NOWARN*> PROCEDURE Long_insert_constants_21_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,0); END Long_insert_constants_21_0;
<*NOWARN*> PROCEDURE Long_insert_constants_21_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,1); END Long_insert_constants_21_1;
<*NOWARN*> PROCEDURE Long_insert_constants_21_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,2); END Long_insert_constants_21_2;
<*NOWARN*> PROCEDURE Long_insert_constants_21_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,3); END Long_insert_constants_21_3;
<*NOWARN*> PROCEDURE Long_insert_constants_21_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,4); END Long_insert_constants_21_4;
<*NOWARN*> PROCEDURE Long_insert_constants_21_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,5); END Long_insert_constants_21_5;
<*NOWARN*> PROCEDURE Long_insert_constants_21_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,6); END Long_insert_constants_21_6;
<*NOWARN*> PROCEDURE Long_insert_constants_21_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,7); END Long_insert_constants_21_7;
<*NOWARN*> PROCEDURE Long_insert_constants_21_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,8); END Long_insert_constants_21_8;
<*NOWARN*> PROCEDURE Long_insert_constants_21_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,9); END Long_insert_constants_21_9;
<*NOWARN*> PROCEDURE Long_insert_constants_21_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,10); END Long_insert_constants_21_10;
<*NOWARN*> PROCEDURE Long_insert_constants_21_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,11); END Long_insert_constants_21_11;
<*NOWARN*> PROCEDURE Long_insert_constants_21_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,12); END Long_insert_constants_21_12;
<*NOWARN*> PROCEDURE Long_insert_constants_21_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,13); END Long_insert_constants_21_13;
<*NOWARN*> PROCEDURE Long_insert_constants_21_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,14); END Long_insert_constants_21_14;
<*NOWARN*> PROCEDURE Long_insert_constants_21_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,15); END Long_insert_constants_21_15;
<*NOWARN*> PROCEDURE Long_insert_constants_21_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,16); END Long_insert_constants_21_16;
<*NOWARN*> PROCEDURE Long_insert_constants_21_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,17); END Long_insert_constants_21_17;
<*NOWARN*> PROCEDURE Long_insert_constants_21_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,18); END Long_insert_constants_21_18;
<*NOWARN*> PROCEDURE Long_insert_constants_21_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,19); END Long_insert_constants_21_19;
<*NOWARN*> PROCEDURE Long_insert_constants_21_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,20); END Long_insert_constants_21_20;
<*NOWARN*> PROCEDURE Long_insert_constants_21_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,21); END Long_insert_constants_21_21;
<*NOWARN*> PROCEDURE Long_insert_constants_21_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,22); END Long_insert_constants_21_22;
<*NOWARN*> PROCEDURE Long_insert_constants_21_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,23); END Long_insert_constants_21_23;
<*NOWARN*> PROCEDURE Long_insert_constants_21_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,24); END Long_insert_constants_21_24;
<*NOWARN*> PROCEDURE Long_insert_constants_21_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,25); END Long_insert_constants_21_25;
<*NOWARN*> PROCEDURE Long_insert_constants_21_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,26); END Long_insert_constants_21_26;
<*NOWARN*> PROCEDURE Long_insert_constants_21_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,27); END Long_insert_constants_21_27;
<*NOWARN*> PROCEDURE Long_insert_constants_21_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,28); END Long_insert_constants_21_28;
<*NOWARN*> PROCEDURE Long_insert_constants_21_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,29); END Long_insert_constants_21_29;
<*NOWARN*> PROCEDURE Long_insert_constants_21_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,30); END Long_insert_constants_21_30;
<*NOWARN*> PROCEDURE Long_insert_constants_21_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,31); END Long_insert_constants_21_31;
<*NOWARN*> PROCEDURE Long_insert_constants_21_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,32); END Long_insert_constants_21_32;
<*NOWARN*> PROCEDURE Long_insert_constants_21_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,33); END Long_insert_constants_21_33;
<*NOWARN*> PROCEDURE Long_insert_constants_21_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,34); END Long_insert_constants_21_34;
<*NOWARN*> PROCEDURE Long_insert_constants_21_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,35); END Long_insert_constants_21_35;
<*NOWARN*> PROCEDURE Long_insert_constants_21_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,36); END Long_insert_constants_21_36;
<*NOWARN*> PROCEDURE Long_insert_constants_21_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,37); END Long_insert_constants_21_37;
<*NOWARN*> PROCEDURE Long_insert_constants_21_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,38); END Long_insert_constants_21_38;
<*NOWARN*> PROCEDURE Long_insert_constants_21_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,39); END Long_insert_constants_21_39;
<*NOWARN*> PROCEDURE Long_insert_constants_21_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,40); END Long_insert_constants_21_40;
<*NOWARN*> PROCEDURE Long_insert_constants_21_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,41); END Long_insert_constants_21_41;
<*NOWARN*> PROCEDURE Long_insert_constants_21_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,42); END Long_insert_constants_21_42;
<*NOWARN*> PROCEDURE Long_insert_constants_21_43(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,21,43); END Long_insert_constants_21_43;
<*NOWARN*> PROCEDURE Long_insert_constants_22_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,0); END Long_insert_constants_22_0;
<*NOWARN*> PROCEDURE Long_insert_constants_22_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,1); END Long_insert_constants_22_1;
<*NOWARN*> PROCEDURE Long_insert_constants_22_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,2); END Long_insert_constants_22_2;
<*NOWARN*> PROCEDURE Long_insert_constants_22_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,3); END Long_insert_constants_22_3;
<*NOWARN*> PROCEDURE Long_insert_constants_22_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,4); END Long_insert_constants_22_4;
<*NOWARN*> PROCEDURE Long_insert_constants_22_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,5); END Long_insert_constants_22_5;
<*NOWARN*> PROCEDURE Long_insert_constants_22_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,6); END Long_insert_constants_22_6;
<*NOWARN*> PROCEDURE Long_insert_constants_22_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,7); END Long_insert_constants_22_7;
<*NOWARN*> PROCEDURE Long_insert_constants_22_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,8); END Long_insert_constants_22_8;
<*NOWARN*> PROCEDURE Long_insert_constants_22_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,9); END Long_insert_constants_22_9;
<*NOWARN*> PROCEDURE Long_insert_constants_22_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,10); END Long_insert_constants_22_10;
<*NOWARN*> PROCEDURE Long_insert_constants_22_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,11); END Long_insert_constants_22_11;
<*NOWARN*> PROCEDURE Long_insert_constants_22_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,12); END Long_insert_constants_22_12;
<*NOWARN*> PROCEDURE Long_insert_constants_22_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,13); END Long_insert_constants_22_13;
<*NOWARN*> PROCEDURE Long_insert_constants_22_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,14); END Long_insert_constants_22_14;
<*NOWARN*> PROCEDURE Long_insert_constants_22_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,15); END Long_insert_constants_22_15;
<*NOWARN*> PROCEDURE Long_insert_constants_22_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,16); END Long_insert_constants_22_16;
<*NOWARN*> PROCEDURE Long_insert_constants_22_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,17); END Long_insert_constants_22_17;
<*NOWARN*> PROCEDURE Long_insert_constants_22_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,18); END Long_insert_constants_22_18;
<*NOWARN*> PROCEDURE Long_insert_constants_22_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,19); END Long_insert_constants_22_19;
<*NOWARN*> PROCEDURE Long_insert_constants_22_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,20); END Long_insert_constants_22_20;
<*NOWARN*> PROCEDURE Long_insert_constants_22_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,21); END Long_insert_constants_22_21;
<*NOWARN*> PROCEDURE Long_insert_constants_22_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,22); END Long_insert_constants_22_22;
<*NOWARN*> PROCEDURE Long_insert_constants_22_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,23); END Long_insert_constants_22_23;
<*NOWARN*> PROCEDURE Long_insert_constants_22_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,24); END Long_insert_constants_22_24;
<*NOWARN*> PROCEDURE Long_insert_constants_22_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,25); END Long_insert_constants_22_25;
<*NOWARN*> PROCEDURE Long_insert_constants_22_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,26); END Long_insert_constants_22_26;
<*NOWARN*> PROCEDURE Long_insert_constants_22_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,27); END Long_insert_constants_22_27;
<*NOWARN*> PROCEDURE Long_insert_constants_22_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,28); END Long_insert_constants_22_28;
<*NOWARN*> PROCEDURE Long_insert_constants_22_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,29); END Long_insert_constants_22_29;
<*NOWARN*> PROCEDURE Long_insert_constants_22_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,30); END Long_insert_constants_22_30;
<*NOWARN*> PROCEDURE Long_insert_constants_22_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,31); END Long_insert_constants_22_31;
<*NOWARN*> PROCEDURE Long_insert_constants_22_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,32); END Long_insert_constants_22_32;
<*NOWARN*> PROCEDURE Long_insert_constants_22_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,33); END Long_insert_constants_22_33;
<*NOWARN*> PROCEDURE Long_insert_constants_22_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,34); END Long_insert_constants_22_34;
<*NOWARN*> PROCEDURE Long_insert_constants_22_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,35); END Long_insert_constants_22_35;
<*NOWARN*> PROCEDURE Long_insert_constants_22_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,36); END Long_insert_constants_22_36;
<*NOWARN*> PROCEDURE Long_insert_constants_22_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,37); END Long_insert_constants_22_37;
<*NOWARN*> PROCEDURE Long_insert_constants_22_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,38); END Long_insert_constants_22_38;
<*NOWARN*> PROCEDURE Long_insert_constants_22_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,39); END Long_insert_constants_22_39;
<*NOWARN*> PROCEDURE Long_insert_constants_22_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,40); END Long_insert_constants_22_40;
<*NOWARN*> PROCEDURE Long_insert_constants_22_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,41); END Long_insert_constants_22_41;
<*NOWARN*> PROCEDURE Long_insert_constants_22_42(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,22,42); END Long_insert_constants_22_42;
<*NOWARN*> PROCEDURE Long_insert_constants_23_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,0); END Long_insert_constants_23_0;
<*NOWARN*> PROCEDURE Long_insert_constants_23_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,1); END Long_insert_constants_23_1;
<*NOWARN*> PROCEDURE Long_insert_constants_23_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,2); END Long_insert_constants_23_2;
<*NOWARN*> PROCEDURE Long_insert_constants_23_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,3); END Long_insert_constants_23_3;
<*NOWARN*> PROCEDURE Long_insert_constants_23_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,4); END Long_insert_constants_23_4;
<*NOWARN*> PROCEDURE Long_insert_constants_23_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,5); END Long_insert_constants_23_5;
<*NOWARN*> PROCEDURE Long_insert_constants_23_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,6); END Long_insert_constants_23_6;
<*NOWARN*> PROCEDURE Long_insert_constants_23_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,7); END Long_insert_constants_23_7;
<*NOWARN*> PROCEDURE Long_insert_constants_23_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,8); END Long_insert_constants_23_8;
<*NOWARN*> PROCEDURE Long_insert_constants_23_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,9); END Long_insert_constants_23_9;
<*NOWARN*> PROCEDURE Long_insert_constants_23_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,10); END Long_insert_constants_23_10;
<*NOWARN*> PROCEDURE Long_insert_constants_23_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,11); END Long_insert_constants_23_11;
<*NOWARN*> PROCEDURE Long_insert_constants_23_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,12); END Long_insert_constants_23_12;
<*NOWARN*> PROCEDURE Long_insert_constants_23_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,13); END Long_insert_constants_23_13;
<*NOWARN*> PROCEDURE Long_insert_constants_23_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,14); END Long_insert_constants_23_14;
<*NOWARN*> PROCEDURE Long_insert_constants_23_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,15); END Long_insert_constants_23_15;
<*NOWARN*> PROCEDURE Long_insert_constants_23_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,16); END Long_insert_constants_23_16;
<*NOWARN*> PROCEDURE Long_insert_constants_23_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,17); END Long_insert_constants_23_17;
<*NOWARN*> PROCEDURE Long_insert_constants_23_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,18); END Long_insert_constants_23_18;
<*NOWARN*> PROCEDURE Long_insert_constants_23_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,19); END Long_insert_constants_23_19;
<*NOWARN*> PROCEDURE Long_insert_constants_23_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,20); END Long_insert_constants_23_20;
<*NOWARN*> PROCEDURE Long_insert_constants_23_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,21); END Long_insert_constants_23_21;
<*NOWARN*> PROCEDURE Long_insert_constants_23_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,22); END Long_insert_constants_23_22;
<*NOWARN*> PROCEDURE Long_insert_constants_23_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,23); END Long_insert_constants_23_23;
<*NOWARN*> PROCEDURE Long_insert_constants_23_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,24); END Long_insert_constants_23_24;
<*NOWARN*> PROCEDURE Long_insert_constants_23_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,25); END Long_insert_constants_23_25;
<*NOWARN*> PROCEDURE Long_insert_constants_23_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,26); END Long_insert_constants_23_26;
<*NOWARN*> PROCEDURE Long_insert_constants_23_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,27); END Long_insert_constants_23_27;
<*NOWARN*> PROCEDURE Long_insert_constants_23_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,28); END Long_insert_constants_23_28;
<*NOWARN*> PROCEDURE Long_insert_constants_23_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,29); END Long_insert_constants_23_29;
<*NOWARN*> PROCEDURE Long_insert_constants_23_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,30); END Long_insert_constants_23_30;
<*NOWARN*> PROCEDURE Long_insert_constants_23_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,31); END Long_insert_constants_23_31;
<*NOWARN*> PROCEDURE Long_insert_constants_23_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,32); END Long_insert_constants_23_32;
<*NOWARN*> PROCEDURE Long_insert_constants_23_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,33); END Long_insert_constants_23_33;
<*NOWARN*> PROCEDURE Long_insert_constants_23_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,34); END Long_insert_constants_23_34;
<*NOWARN*> PROCEDURE Long_insert_constants_23_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,35); END Long_insert_constants_23_35;
<*NOWARN*> PROCEDURE Long_insert_constants_23_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,36); END Long_insert_constants_23_36;
<*NOWARN*> PROCEDURE Long_insert_constants_23_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,37); END Long_insert_constants_23_37;
<*NOWARN*> PROCEDURE Long_insert_constants_23_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,38); END Long_insert_constants_23_38;
<*NOWARN*> PROCEDURE Long_insert_constants_23_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,39); END Long_insert_constants_23_39;
<*NOWARN*> PROCEDURE Long_insert_constants_23_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,40); END Long_insert_constants_23_40;
<*NOWARN*> PROCEDURE Long_insert_constants_23_41(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,23,41); END Long_insert_constants_23_41;
<*NOWARN*> PROCEDURE Long_insert_constants_24_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,0); END Long_insert_constants_24_0;
<*NOWARN*> PROCEDURE Long_insert_constants_24_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,1); END Long_insert_constants_24_1;
<*NOWARN*> PROCEDURE Long_insert_constants_24_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,2); END Long_insert_constants_24_2;
<*NOWARN*> PROCEDURE Long_insert_constants_24_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,3); END Long_insert_constants_24_3;
<*NOWARN*> PROCEDURE Long_insert_constants_24_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,4); END Long_insert_constants_24_4;
<*NOWARN*> PROCEDURE Long_insert_constants_24_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,5); END Long_insert_constants_24_5;
<*NOWARN*> PROCEDURE Long_insert_constants_24_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,6); END Long_insert_constants_24_6;
<*NOWARN*> PROCEDURE Long_insert_constants_24_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,7); END Long_insert_constants_24_7;
<*NOWARN*> PROCEDURE Long_insert_constants_24_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,8); END Long_insert_constants_24_8;
<*NOWARN*> PROCEDURE Long_insert_constants_24_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,9); END Long_insert_constants_24_9;
<*NOWARN*> PROCEDURE Long_insert_constants_24_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,10); END Long_insert_constants_24_10;
<*NOWARN*> PROCEDURE Long_insert_constants_24_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,11); END Long_insert_constants_24_11;
<*NOWARN*> PROCEDURE Long_insert_constants_24_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,12); END Long_insert_constants_24_12;
<*NOWARN*> PROCEDURE Long_insert_constants_24_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,13); END Long_insert_constants_24_13;
<*NOWARN*> PROCEDURE Long_insert_constants_24_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,14); END Long_insert_constants_24_14;
<*NOWARN*> PROCEDURE Long_insert_constants_24_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,15); END Long_insert_constants_24_15;
<*NOWARN*> PROCEDURE Long_insert_constants_24_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,16); END Long_insert_constants_24_16;
<*NOWARN*> PROCEDURE Long_insert_constants_24_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,17); END Long_insert_constants_24_17;
<*NOWARN*> PROCEDURE Long_insert_constants_24_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,18); END Long_insert_constants_24_18;
<*NOWARN*> PROCEDURE Long_insert_constants_24_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,19); END Long_insert_constants_24_19;
<*NOWARN*> PROCEDURE Long_insert_constants_24_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,20); END Long_insert_constants_24_20;
<*NOWARN*> PROCEDURE Long_insert_constants_24_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,21); END Long_insert_constants_24_21;
<*NOWARN*> PROCEDURE Long_insert_constants_24_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,22); END Long_insert_constants_24_22;
<*NOWARN*> PROCEDURE Long_insert_constants_24_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,23); END Long_insert_constants_24_23;
<*NOWARN*> PROCEDURE Long_insert_constants_24_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,24); END Long_insert_constants_24_24;
<*NOWARN*> PROCEDURE Long_insert_constants_24_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,25); END Long_insert_constants_24_25;
<*NOWARN*> PROCEDURE Long_insert_constants_24_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,26); END Long_insert_constants_24_26;
<*NOWARN*> PROCEDURE Long_insert_constants_24_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,27); END Long_insert_constants_24_27;
<*NOWARN*> PROCEDURE Long_insert_constants_24_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,28); END Long_insert_constants_24_28;
<*NOWARN*> PROCEDURE Long_insert_constants_24_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,29); END Long_insert_constants_24_29;
<*NOWARN*> PROCEDURE Long_insert_constants_24_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,30); END Long_insert_constants_24_30;
<*NOWARN*> PROCEDURE Long_insert_constants_24_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,31); END Long_insert_constants_24_31;
<*NOWARN*> PROCEDURE Long_insert_constants_24_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,32); END Long_insert_constants_24_32;
<*NOWARN*> PROCEDURE Long_insert_constants_24_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,33); END Long_insert_constants_24_33;
<*NOWARN*> PROCEDURE Long_insert_constants_24_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,34); END Long_insert_constants_24_34;
<*NOWARN*> PROCEDURE Long_insert_constants_24_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,35); END Long_insert_constants_24_35;
<*NOWARN*> PROCEDURE Long_insert_constants_24_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,36); END Long_insert_constants_24_36;
<*NOWARN*> PROCEDURE Long_insert_constants_24_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,37); END Long_insert_constants_24_37;
<*NOWARN*> PROCEDURE Long_insert_constants_24_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,38); END Long_insert_constants_24_38;
<*NOWARN*> PROCEDURE Long_insert_constants_24_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,39); END Long_insert_constants_24_39;
<*NOWARN*> PROCEDURE Long_insert_constants_24_40(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,24,40); END Long_insert_constants_24_40;
<*NOWARN*> PROCEDURE Long_insert_constants_25_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,0); END Long_insert_constants_25_0;
<*NOWARN*> PROCEDURE Long_insert_constants_25_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,1); END Long_insert_constants_25_1;
<*NOWARN*> PROCEDURE Long_insert_constants_25_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,2); END Long_insert_constants_25_2;
<*NOWARN*> PROCEDURE Long_insert_constants_25_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,3); END Long_insert_constants_25_3;
<*NOWARN*> PROCEDURE Long_insert_constants_25_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,4); END Long_insert_constants_25_4;
<*NOWARN*> PROCEDURE Long_insert_constants_25_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,5); END Long_insert_constants_25_5;
<*NOWARN*> PROCEDURE Long_insert_constants_25_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,6); END Long_insert_constants_25_6;
<*NOWARN*> PROCEDURE Long_insert_constants_25_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,7); END Long_insert_constants_25_7;
<*NOWARN*> PROCEDURE Long_insert_constants_25_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,8); END Long_insert_constants_25_8;
<*NOWARN*> PROCEDURE Long_insert_constants_25_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,9); END Long_insert_constants_25_9;
<*NOWARN*> PROCEDURE Long_insert_constants_25_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,10); END Long_insert_constants_25_10;
<*NOWARN*> PROCEDURE Long_insert_constants_25_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,11); END Long_insert_constants_25_11;
<*NOWARN*> PROCEDURE Long_insert_constants_25_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,12); END Long_insert_constants_25_12;
<*NOWARN*> PROCEDURE Long_insert_constants_25_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,13); END Long_insert_constants_25_13;
<*NOWARN*> PROCEDURE Long_insert_constants_25_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,14); END Long_insert_constants_25_14;
<*NOWARN*> PROCEDURE Long_insert_constants_25_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,15); END Long_insert_constants_25_15;
<*NOWARN*> PROCEDURE Long_insert_constants_25_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,16); END Long_insert_constants_25_16;
<*NOWARN*> PROCEDURE Long_insert_constants_25_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,17); END Long_insert_constants_25_17;
<*NOWARN*> PROCEDURE Long_insert_constants_25_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,18); END Long_insert_constants_25_18;
<*NOWARN*> PROCEDURE Long_insert_constants_25_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,19); END Long_insert_constants_25_19;
<*NOWARN*> PROCEDURE Long_insert_constants_25_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,20); END Long_insert_constants_25_20;
<*NOWARN*> PROCEDURE Long_insert_constants_25_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,21); END Long_insert_constants_25_21;
<*NOWARN*> PROCEDURE Long_insert_constants_25_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,22); END Long_insert_constants_25_22;
<*NOWARN*> PROCEDURE Long_insert_constants_25_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,23); END Long_insert_constants_25_23;
<*NOWARN*> PROCEDURE Long_insert_constants_25_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,24); END Long_insert_constants_25_24;
<*NOWARN*> PROCEDURE Long_insert_constants_25_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,25); END Long_insert_constants_25_25;
<*NOWARN*> PROCEDURE Long_insert_constants_25_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,26); END Long_insert_constants_25_26;
<*NOWARN*> PROCEDURE Long_insert_constants_25_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,27); END Long_insert_constants_25_27;
<*NOWARN*> PROCEDURE Long_insert_constants_25_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,28); END Long_insert_constants_25_28;
<*NOWARN*> PROCEDURE Long_insert_constants_25_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,29); END Long_insert_constants_25_29;
<*NOWARN*> PROCEDURE Long_insert_constants_25_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,30); END Long_insert_constants_25_30;
<*NOWARN*> PROCEDURE Long_insert_constants_25_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,31); END Long_insert_constants_25_31;
<*NOWARN*> PROCEDURE Long_insert_constants_25_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,32); END Long_insert_constants_25_32;
<*NOWARN*> PROCEDURE Long_insert_constants_25_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,33); END Long_insert_constants_25_33;
<*NOWARN*> PROCEDURE Long_insert_constants_25_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,34); END Long_insert_constants_25_34;
<*NOWARN*> PROCEDURE Long_insert_constants_25_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,35); END Long_insert_constants_25_35;
<*NOWARN*> PROCEDURE Long_insert_constants_25_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,36); END Long_insert_constants_25_36;
<*NOWARN*> PROCEDURE Long_insert_constants_25_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,37); END Long_insert_constants_25_37;
<*NOWARN*> PROCEDURE Long_insert_constants_25_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,38); END Long_insert_constants_25_38;
<*NOWARN*> PROCEDURE Long_insert_constants_25_39(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,25,39); END Long_insert_constants_25_39;
<*NOWARN*> PROCEDURE Long_insert_constants_26_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,0); END Long_insert_constants_26_0;
<*NOWARN*> PROCEDURE Long_insert_constants_26_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,1); END Long_insert_constants_26_1;
<*NOWARN*> PROCEDURE Long_insert_constants_26_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,2); END Long_insert_constants_26_2;
<*NOWARN*> PROCEDURE Long_insert_constants_26_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,3); END Long_insert_constants_26_3;
<*NOWARN*> PROCEDURE Long_insert_constants_26_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,4); END Long_insert_constants_26_4;
<*NOWARN*> PROCEDURE Long_insert_constants_26_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,5); END Long_insert_constants_26_5;
<*NOWARN*> PROCEDURE Long_insert_constants_26_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,6); END Long_insert_constants_26_6;
<*NOWARN*> PROCEDURE Long_insert_constants_26_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,7); END Long_insert_constants_26_7;
<*NOWARN*> PROCEDURE Long_insert_constants_26_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,8); END Long_insert_constants_26_8;
<*NOWARN*> PROCEDURE Long_insert_constants_26_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,9); END Long_insert_constants_26_9;
<*NOWARN*> PROCEDURE Long_insert_constants_26_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,10); END Long_insert_constants_26_10;
<*NOWARN*> PROCEDURE Long_insert_constants_26_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,11); END Long_insert_constants_26_11;
<*NOWARN*> PROCEDURE Long_insert_constants_26_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,12); END Long_insert_constants_26_12;
<*NOWARN*> PROCEDURE Long_insert_constants_26_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,13); END Long_insert_constants_26_13;
<*NOWARN*> PROCEDURE Long_insert_constants_26_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,14); END Long_insert_constants_26_14;
<*NOWARN*> PROCEDURE Long_insert_constants_26_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,15); END Long_insert_constants_26_15;
<*NOWARN*> PROCEDURE Long_insert_constants_26_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,16); END Long_insert_constants_26_16;
<*NOWARN*> PROCEDURE Long_insert_constants_26_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,17); END Long_insert_constants_26_17;
<*NOWARN*> PROCEDURE Long_insert_constants_26_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,18); END Long_insert_constants_26_18;
<*NOWARN*> PROCEDURE Long_insert_constants_26_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,19); END Long_insert_constants_26_19;
<*NOWARN*> PROCEDURE Long_insert_constants_26_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,20); END Long_insert_constants_26_20;
<*NOWARN*> PROCEDURE Long_insert_constants_26_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,21); END Long_insert_constants_26_21;
<*NOWARN*> PROCEDURE Long_insert_constants_26_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,22); END Long_insert_constants_26_22;
<*NOWARN*> PROCEDURE Long_insert_constants_26_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,23); END Long_insert_constants_26_23;
<*NOWARN*> PROCEDURE Long_insert_constants_26_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,24); END Long_insert_constants_26_24;
<*NOWARN*> PROCEDURE Long_insert_constants_26_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,25); END Long_insert_constants_26_25;
<*NOWARN*> PROCEDURE Long_insert_constants_26_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,26); END Long_insert_constants_26_26;
<*NOWARN*> PROCEDURE Long_insert_constants_26_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,27); END Long_insert_constants_26_27;
<*NOWARN*> PROCEDURE Long_insert_constants_26_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,28); END Long_insert_constants_26_28;
<*NOWARN*> PROCEDURE Long_insert_constants_26_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,29); END Long_insert_constants_26_29;
<*NOWARN*> PROCEDURE Long_insert_constants_26_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,30); END Long_insert_constants_26_30;
<*NOWARN*> PROCEDURE Long_insert_constants_26_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,31); END Long_insert_constants_26_31;
<*NOWARN*> PROCEDURE Long_insert_constants_26_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,32); END Long_insert_constants_26_32;
<*NOWARN*> PROCEDURE Long_insert_constants_26_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,33); END Long_insert_constants_26_33;
<*NOWARN*> PROCEDURE Long_insert_constants_26_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,34); END Long_insert_constants_26_34;
<*NOWARN*> PROCEDURE Long_insert_constants_26_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,35); END Long_insert_constants_26_35;
<*NOWARN*> PROCEDURE Long_insert_constants_26_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,36); END Long_insert_constants_26_36;
<*NOWARN*> PROCEDURE Long_insert_constants_26_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,37); END Long_insert_constants_26_37;
<*NOWARN*> PROCEDURE Long_insert_constants_26_38(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,26,38); END Long_insert_constants_26_38;
<*NOWARN*> PROCEDURE Long_insert_constants_27_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,0); END Long_insert_constants_27_0;
<*NOWARN*> PROCEDURE Long_insert_constants_27_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,1); END Long_insert_constants_27_1;
<*NOWARN*> PROCEDURE Long_insert_constants_27_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,2); END Long_insert_constants_27_2;
<*NOWARN*> PROCEDURE Long_insert_constants_27_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,3); END Long_insert_constants_27_3;
<*NOWARN*> PROCEDURE Long_insert_constants_27_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,4); END Long_insert_constants_27_4;
<*NOWARN*> PROCEDURE Long_insert_constants_27_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,5); END Long_insert_constants_27_5;
<*NOWARN*> PROCEDURE Long_insert_constants_27_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,6); END Long_insert_constants_27_6;
<*NOWARN*> PROCEDURE Long_insert_constants_27_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,7); END Long_insert_constants_27_7;
<*NOWARN*> PROCEDURE Long_insert_constants_27_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,8); END Long_insert_constants_27_8;
<*NOWARN*> PROCEDURE Long_insert_constants_27_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,9); END Long_insert_constants_27_9;
<*NOWARN*> PROCEDURE Long_insert_constants_27_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,10); END Long_insert_constants_27_10;
<*NOWARN*> PROCEDURE Long_insert_constants_27_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,11); END Long_insert_constants_27_11;
<*NOWARN*> PROCEDURE Long_insert_constants_27_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,12); END Long_insert_constants_27_12;
<*NOWARN*> PROCEDURE Long_insert_constants_27_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,13); END Long_insert_constants_27_13;
<*NOWARN*> PROCEDURE Long_insert_constants_27_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,14); END Long_insert_constants_27_14;
<*NOWARN*> PROCEDURE Long_insert_constants_27_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,15); END Long_insert_constants_27_15;
<*NOWARN*> PROCEDURE Long_insert_constants_27_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,16); END Long_insert_constants_27_16;
<*NOWARN*> PROCEDURE Long_insert_constants_27_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,17); END Long_insert_constants_27_17;
<*NOWARN*> PROCEDURE Long_insert_constants_27_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,18); END Long_insert_constants_27_18;
<*NOWARN*> PROCEDURE Long_insert_constants_27_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,19); END Long_insert_constants_27_19;
<*NOWARN*> PROCEDURE Long_insert_constants_27_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,20); END Long_insert_constants_27_20;
<*NOWARN*> PROCEDURE Long_insert_constants_27_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,21); END Long_insert_constants_27_21;
<*NOWARN*> PROCEDURE Long_insert_constants_27_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,22); END Long_insert_constants_27_22;
<*NOWARN*> PROCEDURE Long_insert_constants_27_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,23); END Long_insert_constants_27_23;
<*NOWARN*> PROCEDURE Long_insert_constants_27_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,24); END Long_insert_constants_27_24;
<*NOWARN*> PROCEDURE Long_insert_constants_27_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,25); END Long_insert_constants_27_25;
<*NOWARN*> PROCEDURE Long_insert_constants_27_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,26); END Long_insert_constants_27_26;
<*NOWARN*> PROCEDURE Long_insert_constants_27_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,27); END Long_insert_constants_27_27;
<*NOWARN*> PROCEDURE Long_insert_constants_27_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,28); END Long_insert_constants_27_28;
<*NOWARN*> PROCEDURE Long_insert_constants_27_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,29); END Long_insert_constants_27_29;
<*NOWARN*> PROCEDURE Long_insert_constants_27_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,30); END Long_insert_constants_27_30;
<*NOWARN*> PROCEDURE Long_insert_constants_27_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,31); END Long_insert_constants_27_31;
<*NOWARN*> PROCEDURE Long_insert_constants_27_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,32); END Long_insert_constants_27_32;
<*NOWARN*> PROCEDURE Long_insert_constants_27_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,33); END Long_insert_constants_27_33;
<*NOWARN*> PROCEDURE Long_insert_constants_27_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,34); END Long_insert_constants_27_34;
<*NOWARN*> PROCEDURE Long_insert_constants_27_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,35); END Long_insert_constants_27_35;
<*NOWARN*> PROCEDURE Long_insert_constants_27_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,36); END Long_insert_constants_27_36;
<*NOWARN*> PROCEDURE Long_insert_constants_27_37(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,27,37); END Long_insert_constants_27_37;
<*NOWARN*> PROCEDURE Long_insert_constants_28_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,0); END Long_insert_constants_28_0;
<*NOWARN*> PROCEDURE Long_insert_constants_28_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,1); END Long_insert_constants_28_1;
<*NOWARN*> PROCEDURE Long_insert_constants_28_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,2); END Long_insert_constants_28_2;
<*NOWARN*> PROCEDURE Long_insert_constants_28_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,3); END Long_insert_constants_28_3;
<*NOWARN*> PROCEDURE Long_insert_constants_28_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,4); END Long_insert_constants_28_4;
<*NOWARN*> PROCEDURE Long_insert_constants_28_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,5); END Long_insert_constants_28_5;
<*NOWARN*> PROCEDURE Long_insert_constants_28_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,6); END Long_insert_constants_28_6;
<*NOWARN*> PROCEDURE Long_insert_constants_28_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,7); END Long_insert_constants_28_7;
<*NOWARN*> PROCEDURE Long_insert_constants_28_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,8); END Long_insert_constants_28_8;
<*NOWARN*> PROCEDURE Long_insert_constants_28_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,9); END Long_insert_constants_28_9;
<*NOWARN*> PROCEDURE Long_insert_constants_28_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,10); END Long_insert_constants_28_10;
<*NOWARN*> PROCEDURE Long_insert_constants_28_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,11); END Long_insert_constants_28_11;
<*NOWARN*> PROCEDURE Long_insert_constants_28_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,12); END Long_insert_constants_28_12;
<*NOWARN*> PROCEDURE Long_insert_constants_28_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,13); END Long_insert_constants_28_13;
<*NOWARN*> PROCEDURE Long_insert_constants_28_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,14); END Long_insert_constants_28_14;
<*NOWARN*> PROCEDURE Long_insert_constants_28_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,15); END Long_insert_constants_28_15;
<*NOWARN*> PROCEDURE Long_insert_constants_28_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,16); END Long_insert_constants_28_16;
<*NOWARN*> PROCEDURE Long_insert_constants_28_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,17); END Long_insert_constants_28_17;
<*NOWARN*> PROCEDURE Long_insert_constants_28_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,18); END Long_insert_constants_28_18;
<*NOWARN*> PROCEDURE Long_insert_constants_28_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,19); END Long_insert_constants_28_19;
<*NOWARN*> PROCEDURE Long_insert_constants_28_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,20); END Long_insert_constants_28_20;
<*NOWARN*> PROCEDURE Long_insert_constants_28_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,21); END Long_insert_constants_28_21;
<*NOWARN*> PROCEDURE Long_insert_constants_28_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,22); END Long_insert_constants_28_22;
<*NOWARN*> PROCEDURE Long_insert_constants_28_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,23); END Long_insert_constants_28_23;
<*NOWARN*> PROCEDURE Long_insert_constants_28_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,24); END Long_insert_constants_28_24;
<*NOWARN*> PROCEDURE Long_insert_constants_28_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,25); END Long_insert_constants_28_25;
<*NOWARN*> PROCEDURE Long_insert_constants_28_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,26); END Long_insert_constants_28_26;
<*NOWARN*> PROCEDURE Long_insert_constants_28_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,27); END Long_insert_constants_28_27;
<*NOWARN*> PROCEDURE Long_insert_constants_28_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,28); END Long_insert_constants_28_28;
<*NOWARN*> PROCEDURE Long_insert_constants_28_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,29); END Long_insert_constants_28_29;
<*NOWARN*> PROCEDURE Long_insert_constants_28_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,30); END Long_insert_constants_28_30;
<*NOWARN*> PROCEDURE Long_insert_constants_28_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,31); END Long_insert_constants_28_31;
<*NOWARN*> PROCEDURE Long_insert_constants_28_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,32); END Long_insert_constants_28_32;
<*NOWARN*> PROCEDURE Long_insert_constants_28_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,33); END Long_insert_constants_28_33;
<*NOWARN*> PROCEDURE Long_insert_constants_28_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,34); END Long_insert_constants_28_34;
<*NOWARN*> PROCEDURE Long_insert_constants_28_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,35); END Long_insert_constants_28_35;
<*NOWARN*> PROCEDURE Long_insert_constants_28_36(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,28,36); END Long_insert_constants_28_36;
<*NOWARN*> PROCEDURE Long_insert_constants_29_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,0); END Long_insert_constants_29_0;
<*NOWARN*> PROCEDURE Long_insert_constants_29_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,1); END Long_insert_constants_29_1;
<*NOWARN*> PROCEDURE Long_insert_constants_29_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,2); END Long_insert_constants_29_2;
<*NOWARN*> PROCEDURE Long_insert_constants_29_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,3); END Long_insert_constants_29_3;
<*NOWARN*> PROCEDURE Long_insert_constants_29_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,4); END Long_insert_constants_29_4;
<*NOWARN*> PROCEDURE Long_insert_constants_29_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,5); END Long_insert_constants_29_5;
<*NOWARN*> PROCEDURE Long_insert_constants_29_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,6); END Long_insert_constants_29_6;
<*NOWARN*> PROCEDURE Long_insert_constants_29_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,7); END Long_insert_constants_29_7;
<*NOWARN*> PROCEDURE Long_insert_constants_29_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,8); END Long_insert_constants_29_8;
<*NOWARN*> PROCEDURE Long_insert_constants_29_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,9); END Long_insert_constants_29_9;
<*NOWARN*> PROCEDURE Long_insert_constants_29_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,10); END Long_insert_constants_29_10;
<*NOWARN*> PROCEDURE Long_insert_constants_29_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,11); END Long_insert_constants_29_11;
<*NOWARN*> PROCEDURE Long_insert_constants_29_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,12); END Long_insert_constants_29_12;
<*NOWARN*> PROCEDURE Long_insert_constants_29_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,13); END Long_insert_constants_29_13;
<*NOWARN*> PROCEDURE Long_insert_constants_29_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,14); END Long_insert_constants_29_14;
<*NOWARN*> PROCEDURE Long_insert_constants_29_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,15); END Long_insert_constants_29_15;
<*NOWARN*> PROCEDURE Long_insert_constants_29_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,16); END Long_insert_constants_29_16;
<*NOWARN*> PROCEDURE Long_insert_constants_29_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,17); END Long_insert_constants_29_17;
<*NOWARN*> PROCEDURE Long_insert_constants_29_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,18); END Long_insert_constants_29_18;
<*NOWARN*> PROCEDURE Long_insert_constants_29_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,19); END Long_insert_constants_29_19;
<*NOWARN*> PROCEDURE Long_insert_constants_29_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,20); END Long_insert_constants_29_20;
<*NOWARN*> PROCEDURE Long_insert_constants_29_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,21); END Long_insert_constants_29_21;
<*NOWARN*> PROCEDURE Long_insert_constants_29_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,22); END Long_insert_constants_29_22;
<*NOWARN*> PROCEDURE Long_insert_constants_29_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,23); END Long_insert_constants_29_23;
<*NOWARN*> PROCEDURE Long_insert_constants_29_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,24); END Long_insert_constants_29_24;
<*NOWARN*> PROCEDURE Long_insert_constants_29_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,25); END Long_insert_constants_29_25;
<*NOWARN*> PROCEDURE Long_insert_constants_29_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,26); END Long_insert_constants_29_26;
<*NOWARN*> PROCEDURE Long_insert_constants_29_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,27); END Long_insert_constants_29_27;
<*NOWARN*> PROCEDURE Long_insert_constants_29_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,28); END Long_insert_constants_29_28;
<*NOWARN*> PROCEDURE Long_insert_constants_29_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,29); END Long_insert_constants_29_29;
<*NOWARN*> PROCEDURE Long_insert_constants_29_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,30); END Long_insert_constants_29_30;
<*NOWARN*> PROCEDURE Long_insert_constants_29_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,31); END Long_insert_constants_29_31;
<*NOWARN*> PROCEDURE Long_insert_constants_29_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,32); END Long_insert_constants_29_32;
<*NOWARN*> PROCEDURE Long_insert_constants_29_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,33); END Long_insert_constants_29_33;
<*NOWARN*> PROCEDURE Long_insert_constants_29_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,34); END Long_insert_constants_29_34;
<*NOWARN*> PROCEDURE Long_insert_constants_29_35(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,29,35); END Long_insert_constants_29_35;
<*NOWARN*> PROCEDURE Long_insert_constants_30_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,0); END Long_insert_constants_30_0;
<*NOWARN*> PROCEDURE Long_insert_constants_30_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,1); END Long_insert_constants_30_1;
<*NOWARN*> PROCEDURE Long_insert_constants_30_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,2); END Long_insert_constants_30_2;
<*NOWARN*> PROCEDURE Long_insert_constants_30_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,3); END Long_insert_constants_30_3;
<*NOWARN*> PROCEDURE Long_insert_constants_30_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,4); END Long_insert_constants_30_4;
<*NOWARN*> PROCEDURE Long_insert_constants_30_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,5); END Long_insert_constants_30_5;
<*NOWARN*> PROCEDURE Long_insert_constants_30_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,6); END Long_insert_constants_30_6;
<*NOWARN*> PROCEDURE Long_insert_constants_30_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,7); END Long_insert_constants_30_7;
<*NOWARN*> PROCEDURE Long_insert_constants_30_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,8); END Long_insert_constants_30_8;
<*NOWARN*> PROCEDURE Long_insert_constants_30_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,9); END Long_insert_constants_30_9;
<*NOWARN*> PROCEDURE Long_insert_constants_30_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,10); END Long_insert_constants_30_10;
<*NOWARN*> PROCEDURE Long_insert_constants_30_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,11); END Long_insert_constants_30_11;
<*NOWARN*> PROCEDURE Long_insert_constants_30_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,12); END Long_insert_constants_30_12;
<*NOWARN*> PROCEDURE Long_insert_constants_30_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,13); END Long_insert_constants_30_13;
<*NOWARN*> PROCEDURE Long_insert_constants_30_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,14); END Long_insert_constants_30_14;
<*NOWARN*> PROCEDURE Long_insert_constants_30_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,15); END Long_insert_constants_30_15;
<*NOWARN*> PROCEDURE Long_insert_constants_30_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,16); END Long_insert_constants_30_16;
<*NOWARN*> PROCEDURE Long_insert_constants_30_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,17); END Long_insert_constants_30_17;
<*NOWARN*> PROCEDURE Long_insert_constants_30_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,18); END Long_insert_constants_30_18;
<*NOWARN*> PROCEDURE Long_insert_constants_30_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,19); END Long_insert_constants_30_19;
<*NOWARN*> PROCEDURE Long_insert_constants_30_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,20); END Long_insert_constants_30_20;
<*NOWARN*> PROCEDURE Long_insert_constants_30_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,21); END Long_insert_constants_30_21;
<*NOWARN*> PROCEDURE Long_insert_constants_30_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,22); END Long_insert_constants_30_22;
<*NOWARN*> PROCEDURE Long_insert_constants_30_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,23); END Long_insert_constants_30_23;
<*NOWARN*> PROCEDURE Long_insert_constants_30_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,24); END Long_insert_constants_30_24;
<*NOWARN*> PROCEDURE Long_insert_constants_30_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,25); END Long_insert_constants_30_25;
<*NOWARN*> PROCEDURE Long_insert_constants_30_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,26); END Long_insert_constants_30_26;
<*NOWARN*> PROCEDURE Long_insert_constants_30_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,27); END Long_insert_constants_30_27;
<*NOWARN*> PROCEDURE Long_insert_constants_30_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,28); END Long_insert_constants_30_28;
<*NOWARN*> PROCEDURE Long_insert_constants_30_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,29); END Long_insert_constants_30_29;
<*NOWARN*> PROCEDURE Long_insert_constants_30_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,30); END Long_insert_constants_30_30;
<*NOWARN*> PROCEDURE Long_insert_constants_30_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,31); END Long_insert_constants_30_31;
<*NOWARN*> PROCEDURE Long_insert_constants_30_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,32); END Long_insert_constants_30_32;
<*NOWARN*> PROCEDURE Long_insert_constants_30_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,33); END Long_insert_constants_30_33;
<*NOWARN*> PROCEDURE Long_insert_constants_30_34(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,30,34); END Long_insert_constants_30_34;
<*NOWARN*> PROCEDURE Long_insert_constants_31_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,0); END Long_insert_constants_31_0;
<*NOWARN*> PROCEDURE Long_insert_constants_31_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,1); END Long_insert_constants_31_1;
<*NOWARN*> PROCEDURE Long_insert_constants_31_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,2); END Long_insert_constants_31_2;
<*NOWARN*> PROCEDURE Long_insert_constants_31_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,3); END Long_insert_constants_31_3;
<*NOWARN*> PROCEDURE Long_insert_constants_31_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,4); END Long_insert_constants_31_4;
<*NOWARN*> PROCEDURE Long_insert_constants_31_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,5); END Long_insert_constants_31_5;
<*NOWARN*> PROCEDURE Long_insert_constants_31_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,6); END Long_insert_constants_31_6;
<*NOWARN*> PROCEDURE Long_insert_constants_31_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,7); END Long_insert_constants_31_7;
<*NOWARN*> PROCEDURE Long_insert_constants_31_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,8); END Long_insert_constants_31_8;
<*NOWARN*> PROCEDURE Long_insert_constants_31_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,9); END Long_insert_constants_31_9;
<*NOWARN*> PROCEDURE Long_insert_constants_31_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,10); END Long_insert_constants_31_10;
<*NOWARN*> PROCEDURE Long_insert_constants_31_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,11); END Long_insert_constants_31_11;
<*NOWARN*> PROCEDURE Long_insert_constants_31_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,12); END Long_insert_constants_31_12;
<*NOWARN*> PROCEDURE Long_insert_constants_31_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,13); END Long_insert_constants_31_13;
<*NOWARN*> PROCEDURE Long_insert_constants_31_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,14); END Long_insert_constants_31_14;
<*NOWARN*> PROCEDURE Long_insert_constants_31_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,15); END Long_insert_constants_31_15;
<*NOWARN*> PROCEDURE Long_insert_constants_31_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,16); END Long_insert_constants_31_16;
<*NOWARN*> PROCEDURE Long_insert_constants_31_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,17); END Long_insert_constants_31_17;
<*NOWARN*> PROCEDURE Long_insert_constants_31_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,18); END Long_insert_constants_31_18;
<*NOWARN*> PROCEDURE Long_insert_constants_31_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,19); END Long_insert_constants_31_19;
<*NOWARN*> PROCEDURE Long_insert_constants_31_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,20); END Long_insert_constants_31_20;
<*NOWARN*> PROCEDURE Long_insert_constants_31_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,21); END Long_insert_constants_31_21;
<*NOWARN*> PROCEDURE Long_insert_constants_31_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,22); END Long_insert_constants_31_22;
<*NOWARN*> PROCEDURE Long_insert_constants_31_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,23); END Long_insert_constants_31_23;
<*NOWARN*> PROCEDURE Long_insert_constants_31_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,24); END Long_insert_constants_31_24;
<*NOWARN*> PROCEDURE Long_insert_constants_31_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,25); END Long_insert_constants_31_25;
<*NOWARN*> PROCEDURE Long_insert_constants_31_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,26); END Long_insert_constants_31_26;
<*NOWARN*> PROCEDURE Long_insert_constants_31_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,27); END Long_insert_constants_31_27;
<*NOWARN*> PROCEDURE Long_insert_constants_31_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,28); END Long_insert_constants_31_28;
<*NOWARN*> PROCEDURE Long_insert_constants_31_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,29); END Long_insert_constants_31_29;
<*NOWARN*> PROCEDURE Long_insert_constants_31_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,30); END Long_insert_constants_31_30;
<*NOWARN*> PROCEDURE Long_insert_constants_31_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,31); END Long_insert_constants_31_31;
<*NOWARN*> PROCEDURE Long_insert_constants_31_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,32); END Long_insert_constants_31_32;
<*NOWARN*> PROCEDURE Long_insert_constants_31_33(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,31,33); END Long_insert_constants_31_33;
<*NOWARN*> PROCEDURE Long_insert_constants_32_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,0); END Long_insert_constants_32_0;
<*NOWARN*> PROCEDURE Long_insert_constants_32_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,1); END Long_insert_constants_32_1;
<*NOWARN*> PROCEDURE Long_insert_constants_32_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,2); END Long_insert_constants_32_2;
<*NOWARN*> PROCEDURE Long_insert_constants_32_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,3); END Long_insert_constants_32_3;
<*NOWARN*> PROCEDURE Long_insert_constants_32_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,4); END Long_insert_constants_32_4;
<*NOWARN*> PROCEDURE Long_insert_constants_32_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,5); END Long_insert_constants_32_5;
<*NOWARN*> PROCEDURE Long_insert_constants_32_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,6); END Long_insert_constants_32_6;
<*NOWARN*> PROCEDURE Long_insert_constants_32_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,7); END Long_insert_constants_32_7;
<*NOWARN*> PROCEDURE Long_insert_constants_32_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,8); END Long_insert_constants_32_8;
<*NOWARN*> PROCEDURE Long_insert_constants_32_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,9); END Long_insert_constants_32_9;
<*NOWARN*> PROCEDURE Long_insert_constants_32_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,10); END Long_insert_constants_32_10;
<*NOWARN*> PROCEDURE Long_insert_constants_32_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,11); END Long_insert_constants_32_11;
<*NOWARN*> PROCEDURE Long_insert_constants_32_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,12); END Long_insert_constants_32_12;
<*NOWARN*> PROCEDURE Long_insert_constants_32_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,13); END Long_insert_constants_32_13;
<*NOWARN*> PROCEDURE Long_insert_constants_32_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,14); END Long_insert_constants_32_14;
<*NOWARN*> PROCEDURE Long_insert_constants_32_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,15); END Long_insert_constants_32_15;
<*NOWARN*> PROCEDURE Long_insert_constants_32_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,16); END Long_insert_constants_32_16;
<*NOWARN*> PROCEDURE Long_insert_constants_32_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,17); END Long_insert_constants_32_17;
<*NOWARN*> PROCEDURE Long_insert_constants_32_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,18); END Long_insert_constants_32_18;
<*NOWARN*> PROCEDURE Long_insert_constants_32_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,19); END Long_insert_constants_32_19;
<*NOWARN*> PROCEDURE Long_insert_constants_32_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,20); END Long_insert_constants_32_20;
<*NOWARN*> PROCEDURE Long_insert_constants_32_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,21); END Long_insert_constants_32_21;
<*NOWARN*> PROCEDURE Long_insert_constants_32_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,22); END Long_insert_constants_32_22;
<*NOWARN*> PROCEDURE Long_insert_constants_32_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,23); END Long_insert_constants_32_23;
<*NOWARN*> PROCEDURE Long_insert_constants_32_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,24); END Long_insert_constants_32_24;
<*NOWARN*> PROCEDURE Long_insert_constants_32_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,25); END Long_insert_constants_32_25;
<*NOWARN*> PROCEDURE Long_insert_constants_32_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,26); END Long_insert_constants_32_26;
<*NOWARN*> PROCEDURE Long_insert_constants_32_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,27); END Long_insert_constants_32_27;
<*NOWARN*> PROCEDURE Long_insert_constants_32_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,28); END Long_insert_constants_32_28;
<*NOWARN*> PROCEDURE Long_insert_constants_32_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,29); END Long_insert_constants_32_29;
<*NOWARN*> PROCEDURE Long_insert_constants_32_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,30); END Long_insert_constants_32_30;
<*NOWARN*> PROCEDURE Long_insert_constants_32_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,31); END Long_insert_constants_32_31;
<*NOWARN*> PROCEDURE Long_insert_constants_32_32(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,32,32); END Long_insert_constants_32_32;
<*NOWARN*> PROCEDURE Long_insert_constants_33_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,0); END Long_insert_constants_33_0;
<*NOWARN*> PROCEDURE Long_insert_constants_33_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,1); END Long_insert_constants_33_1;
<*NOWARN*> PROCEDURE Long_insert_constants_33_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,2); END Long_insert_constants_33_2;
<*NOWARN*> PROCEDURE Long_insert_constants_33_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,3); END Long_insert_constants_33_3;
<*NOWARN*> PROCEDURE Long_insert_constants_33_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,4); END Long_insert_constants_33_4;
<*NOWARN*> PROCEDURE Long_insert_constants_33_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,5); END Long_insert_constants_33_5;
<*NOWARN*> PROCEDURE Long_insert_constants_33_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,6); END Long_insert_constants_33_6;
<*NOWARN*> PROCEDURE Long_insert_constants_33_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,7); END Long_insert_constants_33_7;
<*NOWARN*> PROCEDURE Long_insert_constants_33_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,8); END Long_insert_constants_33_8;
<*NOWARN*> PROCEDURE Long_insert_constants_33_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,9); END Long_insert_constants_33_9;
<*NOWARN*> PROCEDURE Long_insert_constants_33_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,10); END Long_insert_constants_33_10;
<*NOWARN*> PROCEDURE Long_insert_constants_33_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,11); END Long_insert_constants_33_11;
<*NOWARN*> PROCEDURE Long_insert_constants_33_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,12); END Long_insert_constants_33_12;
<*NOWARN*> PROCEDURE Long_insert_constants_33_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,13); END Long_insert_constants_33_13;
<*NOWARN*> PROCEDURE Long_insert_constants_33_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,14); END Long_insert_constants_33_14;
<*NOWARN*> PROCEDURE Long_insert_constants_33_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,15); END Long_insert_constants_33_15;
<*NOWARN*> PROCEDURE Long_insert_constants_33_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,16); END Long_insert_constants_33_16;
<*NOWARN*> PROCEDURE Long_insert_constants_33_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,17); END Long_insert_constants_33_17;
<*NOWARN*> PROCEDURE Long_insert_constants_33_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,18); END Long_insert_constants_33_18;
<*NOWARN*> PROCEDURE Long_insert_constants_33_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,19); END Long_insert_constants_33_19;
<*NOWARN*> PROCEDURE Long_insert_constants_33_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,20); END Long_insert_constants_33_20;
<*NOWARN*> PROCEDURE Long_insert_constants_33_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,21); END Long_insert_constants_33_21;
<*NOWARN*> PROCEDURE Long_insert_constants_33_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,22); END Long_insert_constants_33_22;
<*NOWARN*> PROCEDURE Long_insert_constants_33_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,23); END Long_insert_constants_33_23;
<*NOWARN*> PROCEDURE Long_insert_constants_33_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,24); END Long_insert_constants_33_24;
<*NOWARN*> PROCEDURE Long_insert_constants_33_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,25); END Long_insert_constants_33_25;
<*NOWARN*> PROCEDURE Long_insert_constants_33_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,26); END Long_insert_constants_33_26;
<*NOWARN*> PROCEDURE Long_insert_constants_33_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,27); END Long_insert_constants_33_27;
<*NOWARN*> PROCEDURE Long_insert_constants_33_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,28); END Long_insert_constants_33_28;
<*NOWARN*> PROCEDURE Long_insert_constants_33_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,29); END Long_insert_constants_33_29;
<*NOWARN*> PROCEDURE Long_insert_constants_33_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,30); END Long_insert_constants_33_30;
<*NOWARN*> PROCEDURE Long_insert_constants_33_31(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,33,31); END Long_insert_constants_33_31;
<*NOWARN*> PROCEDURE Long_insert_constants_34_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,0); END Long_insert_constants_34_0;
<*NOWARN*> PROCEDURE Long_insert_constants_34_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,1); END Long_insert_constants_34_1;
<*NOWARN*> PROCEDURE Long_insert_constants_34_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,2); END Long_insert_constants_34_2;
<*NOWARN*> PROCEDURE Long_insert_constants_34_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,3); END Long_insert_constants_34_3;
<*NOWARN*> PROCEDURE Long_insert_constants_34_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,4); END Long_insert_constants_34_4;
<*NOWARN*> PROCEDURE Long_insert_constants_34_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,5); END Long_insert_constants_34_5;
<*NOWARN*> PROCEDURE Long_insert_constants_34_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,6); END Long_insert_constants_34_6;
<*NOWARN*> PROCEDURE Long_insert_constants_34_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,7); END Long_insert_constants_34_7;
<*NOWARN*> PROCEDURE Long_insert_constants_34_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,8); END Long_insert_constants_34_8;
<*NOWARN*> PROCEDURE Long_insert_constants_34_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,9); END Long_insert_constants_34_9;
<*NOWARN*> PROCEDURE Long_insert_constants_34_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,10); END Long_insert_constants_34_10;
<*NOWARN*> PROCEDURE Long_insert_constants_34_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,11); END Long_insert_constants_34_11;
<*NOWARN*> PROCEDURE Long_insert_constants_34_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,12); END Long_insert_constants_34_12;
<*NOWARN*> PROCEDURE Long_insert_constants_34_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,13); END Long_insert_constants_34_13;
<*NOWARN*> PROCEDURE Long_insert_constants_34_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,14); END Long_insert_constants_34_14;
<*NOWARN*> PROCEDURE Long_insert_constants_34_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,15); END Long_insert_constants_34_15;
<*NOWARN*> PROCEDURE Long_insert_constants_34_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,16); END Long_insert_constants_34_16;
<*NOWARN*> PROCEDURE Long_insert_constants_34_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,17); END Long_insert_constants_34_17;
<*NOWARN*> PROCEDURE Long_insert_constants_34_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,18); END Long_insert_constants_34_18;
<*NOWARN*> PROCEDURE Long_insert_constants_34_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,19); END Long_insert_constants_34_19;
<*NOWARN*> PROCEDURE Long_insert_constants_34_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,20); END Long_insert_constants_34_20;
<*NOWARN*> PROCEDURE Long_insert_constants_34_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,21); END Long_insert_constants_34_21;
<*NOWARN*> PROCEDURE Long_insert_constants_34_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,22); END Long_insert_constants_34_22;
<*NOWARN*> PROCEDURE Long_insert_constants_34_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,23); END Long_insert_constants_34_23;
<*NOWARN*> PROCEDURE Long_insert_constants_34_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,24); END Long_insert_constants_34_24;
<*NOWARN*> PROCEDURE Long_insert_constants_34_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,25); END Long_insert_constants_34_25;
<*NOWARN*> PROCEDURE Long_insert_constants_34_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,26); END Long_insert_constants_34_26;
<*NOWARN*> PROCEDURE Long_insert_constants_34_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,27); END Long_insert_constants_34_27;
<*NOWARN*> PROCEDURE Long_insert_constants_34_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,28); END Long_insert_constants_34_28;
<*NOWARN*> PROCEDURE Long_insert_constants_34_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,29); END Long_insert_constants_34_29;
<*NOWARN*> PROCEDURE Long_insert_constants_34_30(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,34,30); END Long_insert_constants_34_30;
<*NOWARN*> PROCEDURE Long_insert_constants_35_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,0); END Long_insert_constants_35_0;
<*NOWARN*> PROCEDURE Long_insert_constants_35_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,1); END Long_insert_constants_35_1;
<*NOWARN*> PROCEDURE Long_insert_constants_35_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,2); END Long_insert_constants_35_2;
<*NOWARN*> PROCEDURE Long_insert_constants_35_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,3); END Long_insert_constants_35_3;
<*NOWARN*> PROCEDURE Long_insert_constants_35_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,4); END Long_insert_constants_35_4;
<*NOWARN*> PROCEDURE Long_insert_constants_35_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,5); END Long_insert_constants_35_5;
<*NOWARN*> PROCEDURE Long_insert_constants_35_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,6); END Long_insert_constants_35_6;
<*NOWARN*> PROCEDURE Long_insert_constants_35_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,7); END Long_insert_constants_35_7;
<*NOWARN*> PROCEDURE Long_insert_constants_35_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,8); END Long_insert_constants_35_8;
<*NOWARN*> PROCEDURE Long_insert_constants_35_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,9); END Long_insert_constants_35_9;
<*NOWARN*> PROCEDURE Long_insert_constants_35_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,10); END Long_insert_constants_35_10;
<*NOWARN*> PROCEDURE Long_insert_constants_35_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,11); END Long_insert_constants_35_11;
<*NOWARN*> PROCEDURE Long_insert_constants_35_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,12); END Long_insert_constants_35_12;
<*NOWARN*> PROCEDURE Long_insert_constants_35_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,13); END Long_insert_constants_35_13;
<*NOWARN*> PROCEDURE Long_insert_constants_35_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,14); END Long_insert_constants_35_14;
<*NOWARN*> PROCEDURE Long_insert_constants_35_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,15); END Long_insert_constants_35_15;
<*NOWARN*> PROCEDURE Long_insert_constants_35_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,16); END Long_insert_constants_35_16;
<*NOWARN*> PROCEDURE Long_insert_constants_35_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,17); END Long_insert_constants_35_17;
<*NOWARN*> PROCEDURE Long_insert_constants_35_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,18); END Long_insert_constants_35_18;
<*NOWARN*> PROCEDURE Long_insert_constants_35_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,19); END Long_insert_constants_35_19;
<*NOWARN*> PROCEDURE Long_insert_constants_35_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,20); END Long_insert_constants_35_20;
<*NOWARN*> PROCEDURE Long_insert_constants_35_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,21); END Long_insert_constants_35_21;
<*NOWARN*> PROCEDURE Long_insert_constants_35_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,22); END Long_insert_constants_35_22;
<*NOWARN*> PROCEDURE Long_insert_constants_35_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,23); END Long_insert_constants_35_23;
<*NOWARN*> PROCEDURE Long_insert_constants_35_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,24); END Long_insert_constants_35_24;
<*NOWARN*> PROCEDURE Long_insert_constants_35_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,25); END Long_insert_constants_35_25;
<*NOWARN*> PROCEDURE Long_insert_constants_35_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,26); END Long_insert_constants_35_26;
<*NOWARN*> PROCEDURE Long_insert_constants_35_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,27); END Long_insert_constants_35_27;
<*NOWARN*> PROCEDURE Long_insert_constants_35_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,28); END Long_insert_constants_35_28;
<*NOWARN*> PROCEDURE Long_insert_constants_35_29(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,35,29); END Long_insert_constants_35_29;
<*NOWARN*> PROCEDURE Long_insert_constants_36_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,0); END Long_insert_constants_36_0;
<*NOWARN*> PROCEDURE Long_insert_constants_36_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,1); END Long_insert_constants_36_1;
<*NOWARN*> PROCEDURE Long_insert_constants_36_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,2); END Long_insert_constants_36_2;
<*NOWARN*> PROCEDURE Long_insert_constants_36_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,3); END Long_insert_constants_36_3;
<*NOWARN*> PROCEDURE Long_insert_constants_36_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,4); END Long_insert_constants_36_4;
<*NOWARN*> PROCEDURE Long_insert_constants_36_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,5); END Long_insert_constants_36_5;
<*NOWARN*> PROCEDURE Long_insert_constants_36_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,6); END Long_insert_constants_36_6;
<*NOWARN*> PROCEDURE Long_insert_constants_36_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,7); END Long_insert_constants_36_7;
<*NOWARN*> PROCEDURE Long_insert_constants_36_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,8); END Long_insert_constants_36_8;
<*NOWARN*> PROCEDURE Long_insert_constants_36_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,9); END Long_insert_constants_36_9;
<*NOWARN*> PROCEDURE Long_insert_constants_36_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,10); END Long_insert_constants_36_10;
<*NOWARN*> PROCEDURE Long_insert_constants_36_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,11); END Long_insert_constants_36_11;
<*NOWARN*> PROCEDURE Long_insert_constants_36_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,12); END Long_insert_constants_36_12;
<*NOWARN*> PROCEDURE Long_insert_constants_36_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,13); END Long_insert_constants_36_13;
<*NOWARN*> PROCEDURE Long_insert_constants_36_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,14); END Long_insert_constants_36_14;
<*NOWARN*> PROCEDURE Long_insert_constants_36_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,15); END Long_insert_constants_36_15;
<*NOWARN*> PROCEDURE Long_insert_constants_36_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,16); END Long_insert_constants_36_16;
<*NOWARN*> PROCEDURE Long_insert_constants_36_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,17); END Long_insert_constants_36_17;
<*NOWARN*> PROCEDURE Long_insert_constants_36_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,18); END Long_insert_constants_36_18;
<*NOWARN*> PROCEDURE Long_insert_constants_36_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,19); END Long_insert_constants_36_19;
<*NOWARN*> PROCEDURE Long_insert_constants_36_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,20); END Long_insert_constants_36_20;
<*NOWARN*> PROCEDURE Long_insert_constants_36_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,21); END Long_insert_constants_36_21;
<*NOWARN*> PROCEDURE Long_insert_constants_36_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,22); END Long_insert_constants_36_22;
<*NOWARN*> PROCEDURE Long_insert_constants_36_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,23); END Long_insert_constants_36_23;
<*NOWARN*> PROCEDURE Long_insert_constants_36_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,24); END Long_insert_constants_36_24;
<*NOWARN*> PROCEDURE Long_insert_constants_36_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,25); END Long_insert_constants_36_25;
<*NOWARN*> PROCEDURE Long_insert_constants_36_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,26); END Long_insert_constants_36_26;
<*NOWARN*> PROCEDURE Long_insert_constants_36_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,27); END Long_insert_constants_36_27;
<*NOWARN*> PROCEDURE Long_insert_constants_36_28(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,36,28); END Long_insert_constants_36_28;
<*NOWARN*> PROCEDURE Long_insert_constants_37_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,0); END Long_insert_constants_37_0;
<*NOWARN*> PROCEDURE Long_insert_constants_37_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,1); END Long_insert_constants_37_1;
<*NOWARN*> PROCEDURE Long_insert_constants_37_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,2); END Long_insert_constants_37_2;
<*NOWARN*> PROCEDURE Long_insert_constants_37_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,3); END Long_insert_constants_37_3;
<*NOWARN*> PROCEDURE Long_insert_constants_37_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,4); END Long_insert_constants_37_4;
<*NOWARN*> PROCEDURE Long_insert_constants_37_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,5); END Long_insert_constants_37_5;
<*NOWARN*> PROCEDURE Long_insert_constants_37_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,6); END Long_insert_constants_37_6;
<*NOWARN*> PROCEDURE Long_insert_constants_37_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,7); END Long_insert_constants_37_7;
<*NOWARN*> PROCEDURE Long_insert_constants_37_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,8); END Long_insert_constants_37_8;
<*NOWARN*> PROCEDURE Long_insert_constants_37_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,9); END Long_insert_constants_37_9;
<*NOWARN*> PROCEDURE Long_insert_constants_37_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,10); END Long_insert_constants_37_10;
<*NOWARN*> PROCEDURE Long_insert_constants_37_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,11); END Long_insert_constants_37_11;
<*NOWARN*> PROCEDURE Long_insert_constants_37_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,12); END Long_insert_constants_37_12;
<*NOWARN*> PROCEDURE Long_insert_constants_37_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,13); END Long_insert_constants_37_13;
<*NOWARN*> PROCEDURE Long_insert_constants_37_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,14); END Long_insert_constants_37_14;
<*NOWARN*> PROCEDURE Long_insert_constants_37_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,15); END Long_insert_constants_37_15;
<*NOWARN*> PROCEDURE Long_insert_constants_37_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,16); END Long_insert_constants_37_16;
<*NOWARN*> PROCEDURE Long_insert_constants_37_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,17); END Long_insert_constants_37_17;
<*NOWARN*> PROCEDURE Long_insert_constants_37_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,18); END Long_insert_constants_37_18;
<*NOWARN*> PROCEDURE Long_insert_constants_37_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,19); END Long_insert_constants_37_19;
<*NOWARN*> PROCEDURE Long_insert_constants_37_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,20); END Long_insert_constants_37_20;
<*NOWARN*> PROCEDURE Long_insert_constants_37_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,21); END Long_insert_constants_37_21;
<*NOWARN*> PROCEDURE Long_insert_constants_37_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,22); END Long_insert_constants_37_22;
<*NOWARN*> PROCEDURE Long_insert_constants_37_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,23); END Long_insert_constants_37_23;
<*NOWARN*> PROCEDURE Long_insert_constants_37_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,24); END Long_insert_constants_37_24;
<*NOWARN*> PROCEDURE Long_insert_constants_37_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,25); END Long_insert_constants_37_25;
<*NOWARN*> PROCEDURE Long_insert_constants_37_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,26); END Long_insert_constants_37_26;
<*NOWARN*> PROCEDURE Long_insert_constants_37_27(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,37,27); END Long_insert_constants_37_27;
<*NOWARN*> PROCEDURE Long_insert_constants_38_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,0); END Long_insert_constants_38_0;
<*NOWARN*> PROCEDURE Long_insert_constants_38_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,1); END Long_insert_constants_38_1;
<*NOWARN*> PROCEDURE Long_insert_constants_38_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,2); END Long_insert_constants_38_2;
<*NOWARN*> PROCEDURE Long_insert_constants_38_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,3); END Long_insert_constants_38_3;
<*NOWARN*> PROCEDURE Long_insert_constants_38_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,4); END Long_insert_constants_38_4;
<*NOWARN*> PROCEDURE Long_insert_constants_38_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,5); END Long_insert_constants_38_5;
<*NOWARN*> PROCEDURE Long_insert_constants_38_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,6); END Long_insert_constants_38_6;
<*NOWARN*> PROCEDURE Long_insert_constants_38_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,7); END Long_insert_constants_38_7;
<*NOWARN*> PROCEDURE Long_insert_constants_38_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,8); END Long_insert_constants_38_8;
<*NOWARN*> PROCEDURE Long_insert_constants_38_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,9); END Long_insert_constants_38_9;
<*NOWARN*> PROCEDURE Long_insert_constants_38_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,10); END Long_insert_constants_38_10;
<*NOWARN*> PROCEDURE Long_insert_constants_38_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,11); END Long_insert_constants_38_11;
<*NOWARN*> PROCEDURE Long_insert_constants_38_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,12); END Long_insert_constants_38_12;
<*NOWARN*> PROCEDURE Long_insert_constants_38_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,13); END Long_insert_constants_38_13;
<*NOWARN*> PROCEDURE Long_insert_constants_38_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,14); END Long_insert_constants_38_14;
<*NOWARN*> PROCEDURE Long_insert_constants_38_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,15); END Long_insert_constants_38_15;
<*NOWARN*> PROCEDURE Long_insert_constants_38_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,16); END Long_insert_constants_38_16;
<*NOWARN*> PROCEDURE Long_insert_constants_38_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,17); END Long_insert_constants_38_17;
<*NOWARN*> PROCEDURE Long_insert_constants_38_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,18); END Long_insert_constants_38_18;
<*NOWARN*> PROCEDURE Long_insert_constants_38_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,19); END Long_insert_constants_38_19;
<*NOWARN*> PROCEDURE Long_insert_constants_38_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,20); END Long_insert_constants_38_20;
<*NOWARN*> PROCEDURE Long_insert_constants_38_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,21); END Long_insert_constants_38_21;
<*NOWARN*> PROCEDURE Long_insert_constants_38_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,22); END Long_insert_constants_38_22;
<*NOWARN*> PROCEDURE Long_insert_constants_38_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,23); END Long_insert_constants_38_23;
<*NOWARN*> PROCEDURE Long_insert_constants_38_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,24); END Long_insert_constants_38_24;
<*NOWARN*> PROCEDURE Long_insert_constants_38_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,25); END Long_insert_constants_38_25;
<*NOWARN*> PROCEDURE Long_insert_constants_38_26(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,38,26); END Long_insert_constants_38_26;
<*NOWARN*> PROCEDURE Long_insert_constants_39_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,0); END Long_insert_constants_39_0;
<*NOWARN*> PROCEDURE Long_insert_constants_39_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,1); END Long_insert_constants_39_1;
<*NOWARN*> PROCEDURE Long_insert_constants_39_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,2); END Long_insert_constants_39_2;
<*NOWARN*> PROCEDURE Long_insert_constants_39_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,3); END Long_insert_constants_39_3;
<*NOWARN*> PROCEDURE Long_insert_constants_39_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,4); END Long_insert_constants_39_4;
<*NOWARN*> PROCEDURE Long_insert_constants_39_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,5); END Long_insert_constants_39_5;
<*NOWARN*> PROCEDURE Long_insert_constants_39_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,6); END Long_insert_constants_39_6;
<*NOWARN*> PROCEDURE Long_insert_constants_39_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,7); END Long_insert_constants_39_7;
<*NOWARN*> PROCEDURE Long_insert_constants_39_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,8); END Long_insert_constants_39_8;
<*NOWARN*> PROCEDURE Long_insert_constants_39_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,9); END Long_insert_constants_39_9;
<*NOWARN*> PROCEDURE Long_insert_constants_39_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,10); END Long_insert_constants_39_10;
<*NOWARN*> PROCEDURE Long_insert_constants_39_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,11); END Long_insert_constants_39_11;
<*NOWARN*> PROCEDURE Long_insert_constants_39_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,12); END Long_insert_constants_39_12;
<*NOWARN*> PROCEDURE Long_insert_constants_39_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,13); END Long_insert_constants_39_13;
<*NOWARN*> PROCEDURE Long_insert_constants_39_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,14); END Long_insert_constants_39_14;
<*NOWARN*> PROCEDURE Long_insert_constants_39_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,15); END Long_insert_constants_39_15;
<*NOWARN*> PROCEDURE Long_insert_constants_39_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,16); END Long_insert_constants_39_16;
<*NOWARN*> PROCEDURE Long_insert_constants_39_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,17); END Long_insert_constants_39_17;
<*NOWARN*> PROCEDURE Long_insert_constants_39_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,18); END Long_insert_constants_39_18;
<*NOWARN*> PROCEDURE Long_insert_constants_39_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,19); END Long_insert_constants_39_19;
<*NOWARN*> PROCEDURE Long_insert_constants_39_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,20); END Long_insert_constants_39_20;
<*NOWARN*> PROCEDURE Long_insert_constants_39_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,21); END Long_insert_constants_39_21;
<*NOWARN*> PROCEDURE Long_insert_constants_39_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,22); END Long_insert_constants_39_22;
<*NOWARN*> PROCEDURE Long_insert_constants_39_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,23); END Long_insert_constants_39_23;
<*NOWARN*> PROCEDURE Long_insert_constants_39_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,24); END Long_insert_constants_39_24;
<*NOWARN*> PROCEDURE Long_insert_constants_39_25(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,39,25); END Long_insert_constants_39_25;
<*NOWARN*> PROCEDURE Long_insert_constants_40_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,0); END Long_insert_constants_40_0;
<*NOWARN*> PROCEDURE Long_insert_constants_40_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,1); END Long_insert_constants_40_1;
<*NOWARN*> PROCEDURE Long_insert_constants_40_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,2); END Long_insert_constants_40_2;
<*NOWARN*> PROCEDURE Long_insert_constants_40_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,3); END Long_insert_constants_40_3;
<*NOWARN*> PROCEDURE Long_insert_constants_40_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,4); END Long_insert_constants_40_4;
<*NOWARN*> PROCEDURE Long_insert_constants_40_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,5); END Long_insert_constants_40_5;
<*NOWARN*> PROCEDURE Long_insert_constants_40_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,6); END Long_insert_constants_40_6;
<*NOWARN*> PROCEDURE Long_insert_constants_40_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,7); END Long_insert_constants_40_7;
<*NOWARN*> PROCEDURE Long_insert_constants_40_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,8); END Long_insert_constants_40_8;
<*NOWARN*> PROCEDURE Long_insert_constants_40_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,9); END Long_insert_constants_40_9;
<*NOWARN*> PROCEDURE Long_insert_constants_40_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,10); END Long_insert_constants_40_10;
<*NOWARN*> PROCEDURE Long_insert_constants_40_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,11); END Long_insert_constants_40_11;
<*NOWARN*> PROCEDURE Long_insert_constants_40_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,12); END Long_insert_constants_40_12;
<*NOWARN*> PROCEDURE Long_insert_constants_40_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,13); END Long_insert_constants_40_13;
<*NOWARN*> PROCEDURE Long_insert_constants_40_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,14); END Long_insert_constants_40_14;
<*NOWARN*> PROCEDURE Long_insert_constants_40_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,15); END Long_insert_constants_40_15;
<*NOWARN*> PROCEDURE Long_insert_constants_40_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,16); END Long_insert_constants_40_16;
<*NOWARN*> PROCEDURE Long_insert_constants_40_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,17); END Long_insert_constants_40_17;
<*NOWARN*> PROCEDURE Long_insert_constants_40_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,18); END Long_insert_constants_40_18;
<*NOWARN*> PROCEDURE Long_insert_constants_40_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,19); END Long_insert_constants_40_19;
<*NOWARN*> PROCEDURE Long_insert_constants_40_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,20); END Long_insert_constants_40_20;
<*NOWARN*> PROCEDURE Long_insert_constants_40_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,21); END Long_insert_constants_40_21;
<*NOWARN*> PROCEDURE Long_insert_constants_40_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,22); END Long_insert_constants_40_22;
<*NOWARN*> PROCEDURE Long_insert_constants_40_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,23); END Long_insert_constants_40_23;
<*NOWARN*> PROCEDURE Long_insert_constants_40_24(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,40,24); END Long_insert_constants_40_24;
<*NOWARN*> PROCEDURE Long_insert_constants_41_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,0); END Long_insert_constants_41_0;
<*NOWARN*> PROCEDURE Long_insert_constants_41_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,1); END Long_insert_constants_41_1;
<*NOWARN*> PROCEDURE Long_insert_constants_41_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,2); END Long_insert_constants_41_2;
<*NOWARN*> PROCEDURE Long_insert_constants_41_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,3); END Long_insert_constants_41_3;
<*NOWARN*> PROCEDURE Long_insert_constants_41_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,4); END Long_insert_constants_41_4;
<*NOWARN*> PROCEDURE Long_insert_constants_41_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,5); END Long_insert_constants_41_5;
<*NOWARN*> PROCEDURE Long_insert_constants_41_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,6); END Long_insert_constants_41_6;
<*NOWARN*> PROCEDURE Long_insert_constants_41_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,7); END Long_insert_constants_41_7;
<*NOWARN*> PROCEDURE Long_insert_constants_41_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,8); END Long_insert_constants_41_8;
<*NOWARN*> PROCEDURE Long_insert_constants_41_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,9); END Long_insert_constants_41_9;
<*NOWARN*> PROCEDURE Long_insert_constants_41_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,10); END Long_insert_constants_41_10;
<*NOWARN*> PROCEDURE Long_insert_constants_41_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,11); END Long_insert_constants_41_11;
<*NOWARN*> PROCEDURE Long_insert_constants_41_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,12); END Long_insert_constants_41_12;
<*NOWARN*> PROCEDURE Long_insert_constants_41_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,13); END Long_insert_constants_41_13;
<*NOWARN*> PROCEDURE Long_insert_constants_41_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,14); END Long_insert_constants_41_14;
<*NOWARN*> PROCEDURE Long_insert_constants_41_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,15); END Long_insert_constants_41_15;
<*NOWARN*> PROCEDURE Long_insert_constants_41_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,16); END Long_insert_constants_41_16;
<*NOWARN*> PROCEDURE Long_insert_constants_41_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,17); END Long_insert_constants_41_17;
<*NOWARN*> PROCEDURE Long_insert_constants_41_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,18); END Long_insert_constants_41_18;
<*NOWARN*> PROCEDURE Long_insert_constants_41_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,19); END Long_insert_constants_41_19;
<*NOWARN*> PROCEDURE Long_insert_constants_41_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,20); END Long_insert_constants_41_20;
<*NOWARN*> PROCEDURE Long_insert_constants_41_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,21); END Long_insert_constants_41_21;
<*NOWARN*> PROCEDURE Long_insert_constants_41_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,22); END Long_insert_constants_41_22;
<*NOWARN*> PROCEDURE Long_insert_constants_41_23(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,41,23); END Long_insert_constants_41_23;
<*NOWARN*> PROCEDURE Long_insert_constants_42_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,0); END Long_insert_constants_42_0;
<*NOWARN*> PROCEDURE Long_insert_constants_42_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,1); END Long_insert_constants_42_1;
<*NOWARN*> PROCEDURE Long_insert_constants_42_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,2); END Long_insert_constants_42_2;
<*NOWARN*> PROCEDURE Long_insert_constants_42_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,3); END Long_insert_constants_42_3;
<*NOWARN*> PROCEDURE Long_insert_constants_42_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,4); END Long_insert_constants_42_4;
<*NOWARN*> PROCEDURE Long_insert_constants_42_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,5); END Long_insert_constants_42_5;
<*NOWARN*> PROCEDURE Long_insert_constants_42_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,6); END Long_insert_constants_42_6;
<*NOWARN*> PROCEDURE Long_insert_constants_42_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,7); END Long_insert_constants_42_7;
<*NOWARN*> PROCEDURE Long_insert_constants_42_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,8); END Long_insert_constants_42_8;
<*NOWARN*> PROCEDURE Long_insert_constants_42_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,9); END Long_insert_constants_42_9;
<*NOWARN*> PROCEDURE Long_insert_constants_42_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,10); END Long_insert_constants_42_10;
<*NOWARN*> PROCEDURE Long_insert_constants_42_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,11); END Long_insert_constants_42_11;
<*NOWARN*> PROCEDURE Long_insert_constants_42_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,12); END Long_insert_constants_42_12;
<*NOWARN*> PROCEDURE Long_insert_constants_42_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,13); END Long_insert_constants_42_13;
<*NOWARN*> PROCEDURE Long_insert_constants_42_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,14); END Long_insert_constants_42_14;
<*NOWARN*> PROCEDURE Long_insert_constants_42_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,15); END Long_insert_constants_42_15;
<*NOWARN*> PROCEDURE Long_insert_constants_42_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,16); END Long_insert_constants_42_16;
<*NOWARN*> PROCEDURE Long_insert_constants_42_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,17); END Long_insert_constants_42_17;
<*NOWARN*> PROCEDURE Long_insert_constants_42_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,18); END Long_insert_constants_42_18;
<*NOWARN*> PROCEDURE Long_insert_constants_42_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,19); END Long_insert_constants_42_19;
<*NOWARN*> PROCEDURE Long_insert_constants_42_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,20); END Long_insert_constants_42_20;
<*NOWARN*> PROCEDURE Long_insert_constants_42_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,21); END Long_insert_constants_42_21;
<*NOWARN*> PROCEDURE Long_insert_constants_42_22(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,42,22); END Long_insert_constants_42_22;
<*NOWARN*> PROCEDURE Long_insert_constants_43_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,0); END Long_insert_constants_43_0;
<*NOWARN*> PROCEDURE Long_insert_constants_43_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,1); END Long_insert_constants_43_1;
<*NOWARN*> PROCEDURE Long_insert_constants_43_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,2); END Long_insert_constants_43_2;
<*NOWARN*> PROCEDURE Long_insert_constants_43_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,3); END Long_insert_constants_43_3;
<*NOWARN*> PROCEDURE Long_insert_constants_43_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,4); END Long_insert_constants_43_4;
<*NOWARN*> PROCEDURE Long_insert_constants_43_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,5); END Long_insert_constants_43_5;
<*NOWARN*> PROCEDURE Long_insert_constants_43_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,6); END Long_insert_constants_43_6;
<*NOWARN*> PROCEDURE Long_insert_constants_43_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,7); END Long_insert_constants_43_7;
<*NOWARN*> PROCEDURE Long_insert_constants_43_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,8); END Long_insert_constants_43_8;
<*NOWARN*> PROCEDURE Long_insert_constants_43_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,9); END Long_insert_constants_43_9;
<*NOWARN*> PROCEDURE Long_insert_constants_43_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,10); END Long_insert_constants_43_10;
<*NOWARN*> PROCEDURE Long_insert_constants_43_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,11); END Long_insert_constants_43_11;
<*NOWARN*> PROCEDURE Long_insert_constants_43_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,12); END Long_insert_constants_43_12;
<*NOWARN*> PROCEDURE Long_insert_constants_43_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,13); END Long_insert_constants_43_13;
<*NOWARN*> PROCEDURE Long_insert_constants_43_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,14); END Long_insert_constants_43_14;
<*NOWARN*> PROCEDURE Long_insert_constants_43_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,15); END Long_insert_constants_43_15;
<*NOWARN*> PROCEDURE Long_insert_constants_43_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,16); END Long_insert_constants_43_16;
<*NOWARN*> PROCEDURE Long_insert_constants_43_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,17); END Long_insert_constants_43_17;
<*NOWARN*> PROCEDURE Long_insert_constants_43_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,18); END Long_insert_constants_43_18;
<*NOWARN*> PROCEDURE Long_insert_constants_43_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,19); END Long_insert_constants_43_19;
<*NOWARN*> PROCEDURE Long_insert_constants_43_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,20); END Long_insert_constants_43_20;
<*NOWARN*> PROCEDURE Long_insert_constants_43_21(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,43,21); END Long_insert_constants_43_21;
<*NOWARN*> PROCEDURE Long_insert_constants_44_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,0); END Long_insert_constants_44_0;
<*NOWARN*> PROCEDURE Long_insert_constants_44_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,1); END Long_insert_constants_44_1;
<*NOWARN*> PROCEDURE Long_insert_constants_44_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,2); END Long_insert_constants_44_2;
<*NOWARN*> PROCEDURE Long_insert_constants_44_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,3); END Long_insert_constants_44_3;
<*NOWARN*> PROCEDURE Long_insert_constants_44_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,4); END Long_insert_constants_44_4;
<*NOWARN*> PROCEDURE Long_insert_constants_44_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,5); END Long_insert_constants_44_5;
<*NOWARN*> PROCEDURE Long_insert_constants_44_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,6); END Long_insert_constants_44_6;
<*NOWARN*> PROCEDURE Long_insert_constants_44_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,7); END Long_insert_constants_44_7;
<*NOWARN*> PROCEDURE Long_insert_constants_44_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,8); END Long_insert_constants_44_8;
<*NOWARN*> PROCEDURE Long_insert_constants_44_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,9); END Long_insert_constants_44_9;
<*NOWARN*> PROCEDURE Long_insert_constants_44_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,10); END Long_insert_constants_44_10;
<*NOWARN*> PROCEDURE Long_insert_constants_44_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,11); END Long_insert_constants_44_11;
<*NOWARN*> PROCEDURE Long_insert_constants_44_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,12); END Long_insert_constants_44_12;
<*NOWARN*> PROCEDURE Long_insert_constants_44_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,13); END Long_insert_constants_44_13;
<*NOWARN*> PROCEDURE Long_insert_constants_44_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,14); END Long_insert_constants_44_14;
<*NOWARN*> PROCEDURE Long_insert_constants_44_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,15); END Long_insert_constants_44_15;
<*NOWARN*> PROCEDURE Long_insert_constants_44_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,16); END Long_insert_constants_44_16;
<*NOWARN*> PROCEDURE Long_insert_constants_44_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,17); END Long_insert_constants_44_17;
<*NOWARN*> PROCEDURE Long_insert_constants_44_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,18); END Long_insert_constants_44_18;
<*NOWARN*> PROCEDURE Long_insert_constants_44_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,19); END Long_insert_constants_44_19;
<*NOWARN*> PROCEDURE Long_insert_constants_44_20(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,44,20); END Long_insert_constants_44_20;
<*NOWARN*> PROCEDURE Long_insert_constants_45_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,45,0); END Long_insert_constants_45_0;
<*NOWARN*> PROCEDURE Long_insert_constants_45_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,45,1); END Long_insert_constants_45_1;
<*NOWARN*> PROCEDURE Long_insert_constants_45_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,45,2); END Long_insert_constants_45_2;
<*NOWARN*> PROCEDURE Long_insert_constants_45_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,45,3); END Long_insert_constants_45_3;
<*NOWARN*> PROCEDURE Long_insert_constants_45_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,45,4); END Long_insert_constants_45_4;
<*NOWARN*> PROCEDURE Long_insert_constants_45_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,45,5); END Long_insert_constants_45_5;
<*NOWARN*> PROCEDURE Long_insert_constants_45_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,45,6); END Long_insert_constants_45_6;
<*NOWARN*> PROCEDURE Long_insert_constants_45_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,45,7); END Long_insert_constants_45_7;
<*NOWARN*> PROCEDURE Long_insert_constants_45_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,45,8); END Long_insert_constants_45_8;
<*NOWARN*> PROCEDURE Long_insert_constants_45_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,45,9); END Long_insert_constants_45_9;
<*NOWARN*> PROCEDURE Long_insert_constants_45_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,45,10); END Long_insert_constants_45_10;
<*NOWARN*> PROCEDURE Long_insert_constants_45_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,45,11); END Long_insert_constants_45_11;
<*NOWARN*> PROCEDURE Long_insert_constants_45_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,45,12); END Long_insert_constants_45_12;
<*NOWARN*> PROCEDURE Long_insert_constants_45_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,45,13); END Long_insert_constants_45_13;
<*NOWARN*> PROCEDURE Long_insert_constants_45_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,45,14); END Long_insert_constants_45_14;
<*NOWARN*> PROCEDURE Long_insert_constants_45_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,45,15); END Long_insert_constants_45_15;
<*NOWARN*> PROCEDURE Long_insert_constants_45_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,45,16); END Long_insert_constants_45_16;
<*NOWARN*> PROCEDURE Long_insert_constants_45_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,45,17); END Long_insert_constants_45_17;
<*NOWARN*> PROCEDURE Long_insert_constants_45_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,45,18); END Long_insert_constants_45_18;
<*NOWARN*> PROCEDURE Long_insert_constants_45_19(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,45,19); END Long_insert_constants_45_19;
<*NOWARN*> PROCEDURE Long_insert_constants_46_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,46,0); END Long_insert_constants_46_0;
<*NOWARN*> PROCEDURE Long_insert_constants_46_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,46,1); END Long_insert_constants_46_1;
<*NOWARN*> PROCEDURE Long_insert_constants_46_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,46,2); END Long_insert_constants_46_2;
<*NOWARN*> PROCEDURE Long_insert_constants_46_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,46,3); END Long_insert_constants_46_3;
<*NOWARN*> PROCEDURE Long_insert_constants_46_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,46,4); END Long_insert_constants_46_4;
<*NOWARN*> PROCEDURE Long_insert_constants_46_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,46,5); END Long_insert_constants_46_5;
<*NOWARN*> PROCEDURE Long_insert_constants_46_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,46,6); END Long_insert_constants_46_6;
<*NOWARN*> PROCEDURE Long_insert_constants_46_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,46,7); END Long_insert_constants_46_7;
<*NOWARN*> PROCEDURE Long_insert_constants_46_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,46,8); END Long_insert_constants_46_8;
<*NOWARN*> PROCEDURE Long_insert_constants_46_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,46,9); END Long_insert_constants_46_9;
<*NOWARN*> PROCEDURE Long_insert_constants_46_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,46,10); END Long_insert_constants_46_10;
<*NOWARN*> PROCEDURE Long_insert_constants_46_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,46,11); END Long_insert_constants_46_11;
<*NOWARN*> PROCEDURE Long_insert_constants_46_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,46,12); END Long_insert_constants_46_12;
<*NOWARN*> PROCEDURE Long_insert_constants_46_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,46,13); END Long_insert_constants_46_13;
<*NOWARN*> PROCEDURE Long_insert_constants_46_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,46,14); END Long_insert_constants_46_14;
<*NOWARN*> PROCEDURE Long_insert_constants_46_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,46,15); END Long_insert_constants_46_15;
<*NOWARN*> PROCEDURE Long_insert_constants_46_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,46,16); END Long_insert_constants_46_16;
<*NOWARN*> PROCEDURE Long_insert_constants_46_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,46,17); END Long_insert_constants_46_17;
<*NOWARN*> PROCEDURE Long_insert_constants_46_18(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,46,18); END Long_insert_constants_46_18;
<*NOWARN*> PROCEDURE Long_insert_constants_47_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,47,0); END Long_insert_constants_47_0;
<*NOWARN*> PROCEDURE Long_insert_constants_47_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,47,1); END Long_insert_constants_47_1;
<*NOWARN*> PROCEDURE Long_insert_constants_47_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,47,2); END Long_insert_constants_47_2;
<*NOWARN*> PROCEDURE Long_insert_constants_47_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,47,3); END Long_insert_constants_47_3;
<*NOWARN*> PROCEDURE Long_insert_constants_47_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,47,4); END Long_insert_constants_47_4;
<*NOWARN*> PROCEDURE Long_insert_constants_47_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,47,5); END Long_insert_constants_47_5;
<*NOWARN*> PROCEDURE Long_insert_constants_47_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,47,6); END Long_insert_constants_47_6;
<*NOWARN*> PROCEDURE Long_insert_constants_47_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,47,7); END Long_insert_constants_47_7;
<*NOWARN*> PROCEDURE Long_insert_constants_47_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,47,8); END Long_insert_constants_47_8;
<*NOWARN*> PROCEDURE Long_insert_constants_47_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,47,9); END Long_insert_constants_47_9;
<*NOWARN*> PROCEDURE Long_insert_constants_47_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,47,10); END Long_insert_constants_47_10;
<*NOWARN*> PROCEDURE Long_insert_constants_47_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,47,11); END Long_insert_constants_47_11;
<*NOWARN*> PROCEDURE Long_insert_constants_47_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,47,12); END Long_insert_constants_47_12;
<*NOWARN*> PROCEDURE Long_insert_constants_47_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,47,13); END Long_insert_constants_47_13;
<*NOWARN*> PROCEDURE Long_insert_constants_47_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,47,14); END Long_insert_constants_47_14;
<*NOWARN*> PROCEDURE Long_insert_constants_47_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,47,15); END Long_insert_constants_47_15;
<*NOWARN*> PROCEDURE Long_insert_constants_47_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,47,16); END Long_insert_constants_47_16;
<*NOWARN*> PROCEDURE Long_insert_constants_47_17(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,47,17); END Long_insert_constants_47_17;
<*NOWARN*> PROCEDURE Long_insert_constants_48_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,48,0); END Long_insert_constants_48_0;
<*NOWARN*> PROCEDURE Long_insert_constants_48_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,48,1); END Long_insert_constants_48_1;
<*NOWARN*> PROCEDURE Long_insert_constants_48_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,48,2); END Long_insert_constants_48_2;
<*NOWARN*> PROCEDURE Long_insert_constants_48_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,48,3); END Long_insert_constants_48_3;
<*NOWARN*> PROCEDURE Long_insert_constants_48_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,48,4); END Long_insert_constants_48_4;
<*NOWARN*> PROCEDURE Long_insert_constants_48_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,48,5); END Long_insert_constants_48_5;
<*NOWARN*> PROCEDURE Long_insert_constants_48_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,48,6); END Long_insert_constants_48_6;
<*NOWARN*> PROCEDURE Long_insert_constants_48_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,48,7); END Long_insert_constants_48_7;
<*NOWARN*> PROCEDURE Long_insert_constants_48_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,48,8); END Long_insert_constants_48_8;
<*NOWARN*> PROCEDURE Long_insert_constants_48_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,48,9); END Long_insert_constants_48_9;
<*NOWARN*> PROCEDURE Long_insert_constants_48_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,48,10); END Long_insert_constants_48_10;
<*NOWARN*> PROCEDURE Long_insert_constants_48_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,48,11); END Long_insert_constants_48_11;
<*NOWARN*> PROCEDURE Long_insert_constants_48_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,48,12); END Long_insert_constants_48_12;
<*NOWARN*> PROCEDURE Long_insert_constants_48_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,48,13); END Long_insert_constants_48_13;
<*NOWARN*> PROCEDURE Long_insert_constants_48_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,48,14); END Long_insert_constants_48_14;
<*NOWARN*> PROCEDURE Long_insert_constants_48_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,48,15); END Long_insert_constants_48_15;
<*NOWARN*> PROCEDURE Long_insert_constants_48_16(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,48,16); END Long_insert_constants_48_16;
<*NOWARN*> PROCEDURE Long_insert_constants_49_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,49,0); END Long_insert_constants_49_0;
<*NOWARN*> PROCEDURE Long_insert_constants_49_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,49,1); END Long_insert_constants_49_1;
<*NOWARN*> PROCEDURE Long_insert_constants_49_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,49,2); END Long_insert_constants_49_2;
<*NOWARN*> PROCEDURE Long_insert_constants_49_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,49,3); END Long_insert_constants_49_3;
<*NOWARN*> PROCEDURE Long_insert_constants_49_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,49,4); END Long_insert_constants_49_4;
<*NOWARN*> PROCEDURE Long_insert_constants_49_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,49,5); END Long_insert_constants_49_5;
<*NOWARN*> PROCEDURE Long_insert_constants_49_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,49,6); END Long_insert_constants_49_6;
<*NOWARN*> PROCEDURE Long_insert_constants_49_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,49,7); END Long_insert_constants_49_7;
<*NOWARN*> PROCEDURE Long_insert_constants_49_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,49,8); END Long_insert_constants_49_8;
<*NOWARN*> PROCEDURE Long_insert_constants_49_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,49,9); END Long_insert_constants_49_9;
<*NOWARN*> PROCEDURE Long_insert_constants_49_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,49,10); END Long_insert_constants_49_10;
<*NOWARN*> PROCEDURE Long_insert_constants_49_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,49,11); END Long_insert_constants_49_11;
<*NOWARN*> PROCEDURE Long_insert_constants_49_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,49,12); END Long_insert_constants_49_12;
<*NOWARN*> PROCEDURE Long_insert_constants_49_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,49,13); END Long_insert_constants_49_13;
<*NOWARN*> PROCEDURE Long_insert_constants_49_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,49,14); END Long_insert_constants_49_14;
<*NOWARN*> PROCEDURE Long_insert_constants_49_15(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,49,15); END Long_insert_constants_49_15;
<*NOWARN*> PROCEDURE Long_insert_constants_50_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,50,0); END Long_insert_constants_50_0;
<*NOWARN*> PROCEDURE Long_insert_constants_50_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,50,1); END Long_insert_constants_50_1;
<*NOWARN*> PROCEDURE Long_insert_constants_50_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,50,2); END Long_insert_constants_50_2;
<*NOWARN*> PROCEDURE Long_insert_constants_50_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,50,3); END Long_insert_constants_50_3;
<*NOWARN*> PROCEDURE Long_insert_constants_50_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,50,4); END Long_insert_constants_50_4;
<*NOWARN*> PROCEDURE Long_insert_constants_50_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,50,5); END Long_insert_constants_50_5;
<*NOWARN*> PROCEDURE Long_insert_constants_50_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,50,6); END Long_insert_constants_50_6;
<*NOWARN*> PROCEDURE Long_insert_constants_50_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,50,7); END Long_insert_constants_50_7;
<*NOWARN*> PROCEDURE Long_insert_constants_50_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,50,8); END Long_insert_constants_50_8;
<*NOWARN*> PROCEDURE Long_insert_constants_50_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,50,9); END Long_insert_constants_50_9;
<*NOWARN*> PROCEDURE Long_insert_constants_50_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,50,10); END Long_insert_constants_50_10;
<*NOWARN*> PROCEDURE Long_insert_constants_50_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,50,11); END Long_insert_constants_50_11;
<*NOWARN*> PROCEDURE Long_insert_constants_50_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,50,12); END Long_insert_constants_50_12;
<*NOWARN*> PROCEDURE Long_insert_constants_50_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,50,13); END Long_insert_constants_50_13;
<*NOWARN*> PROCEDURE Long_insert_constants_50_14(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,50,14); END Long_insert_constants_50_14;
<*NOWARN*> PROCEDURE Long_insert_constants_51_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,51,0); END Long_insert_constants_51_0;
<*NOWARN*> PROCEDURE Long_insert_constants_51_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,51,1); END Long_insert_constants_51_1;
<*NOWARN*> PROCEDURE Long_insert_constants_51_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,51,2); END Long_insert_constants_51_2;
<*NOWARN*> PROCEDURE Long_insert_constants_51_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,51,3); END Long_insert_constants_51_3;
<*NOWARN*> PROCEDURE Long_insert_constants_51_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,51,4); END Long_insert_constants_51_4;
<*NOWARN*> PROCEDURE Long_insert_constants_51_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,51,5); END Long_insert_constants_51_5;
<*NOWARN*> PROCEDURE Long_insert_constants_51_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,51,6); END Long_insert_constants_51_6;
<*NOWARN*> PROCEDURE Long_insert_constants_51_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,51,7); END Long_insert_constants_51_7;
<*NOWARN*> PROCEDURE Long_insert_constants_51_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,51,8); END Long_insert_constants_51_8;
<*NOWARN*> PROCEDURE Long_insert_constants_51_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,51,9); END Long_insert_constants_51_9;
<*NOWARN*> PROCEDURE Long_insert_constants_51_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,51,10); END Long_insert_constants_51_10;
<*NOWARN*> PROCEDURE Long_insert_constants_51_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,51,11); END Long_insert_constants_51_11;
<*NOWARN*> PROCEDURE Long_insert_constants_51_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,51,12); END Long_insert_constants_51_12;
<*NOWARN*> PROCEDURE Long_insert_constants_51_13(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,51,13); END Long_insert_constants_51_13;
<*NOWARN*> PROCEDURE Long_insert_constants_52_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,52,0); END Long_insert_constants_52_0;
<*NOWARN*> PROCEDURE Long_insert_constants_52_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,52,1); END Long_insert_constants_52_1;
<*NOWARN*> PROCEDURE Long_insert_constants_52_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,52,2); END Long_insert_constants_52_2;
<*NOWARN*> PROCEDURE Long_insert_constants_52_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,52,3); END Long_insert_constants_52_3;
<*NOWARN*> PROCEDURE Long_insert_constants_52_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,52,4); END Long_insert_constants_52_4;
<*NOWARN*> PROCEDURE Long_insert_constants_52_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,52,5); END Long_insert_constants_52_5;
<*NOWARN*> PROCEDURE Long_insert_constants_52_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,52,6); END Long_insert_constants_52_6;
<*NOWARN*> PROCEDURE Long_insert_constants_52_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,52,7); END Long_insert_constants_52_7;
<*NOWARN*> PROCEDURE Long_insert_constants_52_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,52,8); END Long_insert_constants_52_8;
<*NOWARN*> PROCEDURE Long_insert_constants_52_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,52,9); END Long_insert_constants_52_9;
<*NOWARN*> PROCEDURE Long_insert_constants_52_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,52,10); END Long_insert_constants_52_10;
<*NOWARN*> PROCEDURE Long_insert_constants_52_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,52,11); END Long_insert_constants_52_11;
<*NOWARN*> PROCEDURE Long_insert_constants_52_12(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,52,12); END Long_insert_constants_52_12;
<*NOWARN*> PROCEDURE Long_insert_constants_53_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,53,0); END Long_insert_constants_53_0;
<*NOWARN*> PROCEDURE Long_insert_constants_53_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,53,1); END Long_insert_constants_53_1;
<*NOWARN*> PROCEDURE Long_insert_constants_53_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,53,2); END Long_insert_constants_53_2;
<*NOWARN*> PROCEDURE Long_insert_constants_53_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,53,3); END Long_insert_constants_53_3;
<*NOWARN*> PROCEDURE Long_insert_constants_53_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,53,4); END Long_insert_constants_53_4;
<*NOWARN*> PROCEDURE Long_insert_constants_53_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,53,5); END Long_insert_constants_53_5;
<*NOWARN*> PROCEDURE Long_insert_constants_53_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,53,6); END Long_insert_constants_53_6;
<*NOWARN*> PROCEDURE Long_insert_constants_53_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,53,7); END Long_insert_constants_53_7;
<*NOWARN*> PROCEDURE Long_insert_constants_53_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,53,8); END Long_insert_constants_53_8;
<*NOWARN*> PROCEDURE Long_insert_constants_53_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,53,9); END Long_insert_constants_53_9;
<*NOWARN*> PROCEDURE Long_insert_constants_53_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,53,10); END Long_insert_constants_53_10;
<*NOWARN*> PROCEDURE Long_insert_constants_53_11(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,53,11); END Long_insert_constants_53_11;
<*NOWARN*> PROCEDURE Long_insert_constants_54_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,54,0); END Long_insert_constants_54_0;
<*NOWARN*> PROCEDURE Long_insert_constants_54_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,54,1); END Long_insert_constants_54_1;
<*NOWARN*> PROCEDURE Long_insert_constants_54_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,54,2); END Long_insert_constants_54_2;
<*NOWARN*> PROCEDURE Long_insert_constants_54_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,54,3); END Long_insert_constants_54_3;
<*NOWARN*> PROCEDURE Long_insert_constants_54_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,54,4); END Long_insert_constants_54_4;
<*NOWARN*> PROCEDURE Long_insert_constants_54_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,54,5); END Long_insert_constants_54_5;
<*NOWARN*> PROCEDURE Long_insert_constants_54_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,54,6); END Long_insert_constants_54_6;
<*NOWARN*> PROCEDURE Long_insert_constants_54_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,54,7); END Long_insert_constants_54_7;
<*NOWARN*> PROCEDURE Long_insert_constants_54_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,54,8); END Long_insert_constants_54_8;
<*NOWARN*> PROCEDURE Long_insert_constants_54_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,54,9); END Long_insert_constants_54_9;
<*NOWARN*> PROCEDURE Long_insert_constants_54_10(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,54,10); END Long_insert_constants_54_10;
<*NOWARN*> PROCEDURE Long_insert_constants_55_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,55,0); END Long_insert_constants_55_0;
<*NOWARN*> PROCEDURE Long_insert_constants_55_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,55,1); END Long_insert_constants_55_1;
<*NOWARN*> PROCEDURE Long_insert_constants_55_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,55,2); END Long_insert_constants_55_2;
<*NOWARN*> PROCEDURE Long_insert_constants_55_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,55,3); END Long_insert_constants_55_3;
<*NOWARN*> PROCEDURE Long_insert_constants_55_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,55,4); END Long_insert_constants_55_4;
<*NOWARN*> PROCEDURE Long_insert_constants_55_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,55,5); END Long_insert_constants_55_5;
<*NOWARN*> PROCEDURE Long_insert_constants_55_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,55,6); END Long_insert_constants_55_6;
<*NOWARN*> PROCEDURE Long_insert_constants_55_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,55,7); END Long_insert_constants_55_7;
<*NOWARN*> PROCEDURE Long_insert_constants_55_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,55,8); END Long_insert_constants_55_8;
<*NOWARN*> PROCEDURE Long_insert_constants_55_9(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,55,9); END Long_insert_constants_55_9;
<*NOWARN*> PROCEDURE Long_insert_constants_56_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,56,0); END Long_insert_constants_56_0;
<*NOWARN*> PROCEDURE Long_insert_constants_56_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,56,1); END Long_insert_constants_56_1;
<*NOWARN*> PROCEDURE Long_insert_constants_56_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,56,2); END Long_insert_constants_56_2;
<*NOWARN*> PROCEDURE Long_insert_constants_56_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,56,3); END Long_insert_constants_56_3;
<*NOWARN*> PROCEDURE Long_insert_constants_56_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,56,4); END Long_insert_constants_56_4;
<*NOWARN*> PROCEDURE Long_insert_constants_56_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,56,5); END Long_insert_constants_56_5;
<*NOWARN*> PROCEDURE Long_insert_constants_56_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,56,6); END Long_insert_constants_56_6;
<*NOWARN*> PROCEDURE Long_insert_constants_56_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,56,7); END Long_insert_constants_56_7;
<*NOWARN*> PROCEDURE Long_insert_constants_56_8(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,56,8); END Long_insert_constants_56_8;
<*NOWARN*> PROCEDURE Long_insert_constants_57_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,57,0); END Long_insert_constants_57_0;
<*NOWARN*> PROCEDURE Long_insert_constants_57_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,57,1); END Long_insert_constants_57_1;
<*NOWARN*> PROCEDURE Long_insert_constants_57_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,57,2); END Long_insert_constants_57_2;
<*NOWARN*> PROCEDURE Long_insert_constants_57_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,57,3); END Long_insert_constants_57_3;
<*NOWARN*> PROCEDURE Long_insert_constants_57_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,57,4); END Long_insert_constants_57_4;
<*NOWARN*> PROCEDURE Long_insert_constants_57_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,57,5); END Long_insert_constants_57_5;
<*NOWARN*> PROCEDURE Long_insert_constants_57_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,57,6); END Long_insert_constants_57_6;
<*NOWARN*> PROCEDURE Long_insert_constants_57_7(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,57,7); END Long_insert_constants_57_7;
<*NOWARN*> PROCEDURE Long_insert_constants_58_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,58,0); END Long_insert_constants_58_0;
<*NOWARN*> PROCEDURE Long_insert_constants_58_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,58,1); END Long_insert_constants_58_1;
<*NOWARN*> PROCEDURE Long_insert_constants_58_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,58,2); END Long_insert_constants_58_2;
<*NOWARN*> PROCEDURE Long_insert_constants_58_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,58,3); END Long_insert_constants_58_3;
<*NOWARN*> PROCEDURE Long_insert_constants_58_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,58,4); END Long_insert_constants_58_4;
<*NOWARN*> PROCEDURE Long_insert_constants_58_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,58,5); END Long_insert_constants_58_5;
<*NOWARN*> PROCEDURE Long_insert_constants_58_6(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,58,6); END Long_insert_constants_58_6;
<*NOWARN*> PROCEDURE Long_insert_constants_59_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,59,0); END Long_insert_constants_59_0;
<*NOWARN*> PROCEDURE Long_insert_constants_59_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,59,1); END Long_insert_constants_59_1;
<*NOWARN*> PROCEDURE Long_insert_constants_59_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,59,2); END Long_insert_constants_59_2;
<*NOWARN*> PROCEDURE Long_insert_constants_59_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,59,3); END Long_insert_constants_59_3;
<*NOWARN*> PROCEDURE Long_insert_constants_59_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,59,4); END Long_insert_constants_59_4;
<*NOWARN*> PROCEDURE Long_insert_constants_59_5(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,59,5); END Long_insert_constants_59_5;
<*NOWARN*> PROCEDURE Long_insert_constants_60_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,60,0); END Long_insert_constants_60_0;
<*NOWARN*> PROCEDURE Long_insert_constants_60_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,60,1); END Long_insert_constants_60_1;
<*NOWARN*> PROCEDURE Long_insert_constants_60_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,60,2); END Long_insert_constants_60_2;
<*NOWARN*> PROCEDURE Long_insert_constants_60_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,60,3); END Long_insert_constants_60_3;
<*NOWARN*> PROCEDURE Long_insert_constants_60_4(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,60,4); END Long_insert_constants_60_4;
<*NOWARN*> PROCEDURE Long_insert_constants_61_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,61,0); END Long_insert_constants_61_0;
<*NOWARN*> PROCEDURE Long_insert_constants_61_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,61,1); END Long_insert_constants_61_1;
<*NOWARN*> PROCEDURE Long_insert_constants_61_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,61,2); END Long_insert_constants_61_2;
<*NOWARN*> PROCEDURE Long_insert_constants_61_3(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,61,3); END Long_insert_constants_61_3;
<*NOWARN*> PROCEDURE Long_insert_constants_62_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,62,0); END Long_insert_constants_62_0;
<*NOWARN*> PROCEDURE Long_insert_constants_62_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,62,1); END Long_insert_constants_62_1;
<*NOWARN*> PROCEDURE Long_insert_constants_62_2(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,62,2); END Long_insert_constants_62_2;
<*NOWARN*> PROCEDURE Long_insert_constants_63_0(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,63,0); END Long_insert_constants_63_0;
<*NOWARN*> PROCEDURE Long_insert_constants_63_1(a,b:Long.T):Long.T=
           BEGIN RETURN Long.Insert(a,b,63,1); END Long_insert_constants_63_1;
BEGIN
END insert_constant_both.
