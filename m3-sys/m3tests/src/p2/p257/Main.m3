(* Test shift_right with a signed integer in the IR.
   Specifically m3front/src/misc/cg/Setrange
   This is machine-generated. Do not edit
*)
UNSAFE MODULE Main;
IMPORT Dump;

PROCEDURE F0() =
TYPE T = SET OF[0..0];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 0 + 1) DO
       FOR offset := 0 TO MAX(0, 0 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(0, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F0;

PROCEDURE F1() =
TYPE T = SET OF[0..1];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 1 + 1) DO
       FOR offset := 0 TO MAX(0, 1 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(1, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F1;

PROCEDURE F2() =
TYPE T = SET OF[0..2];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 2 + 1) DO
       FOR offset := 0 TO MAX(0, 2 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(2, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F2;

PROCEDURE F3() =
TYPE T = SET OF[0..3];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 3 + 1) DO
       FOR offset := 0 TO MAX(0, 3 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(3, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F3;

PROCEDURE F4() =
TYPE T = SET OF[0..4];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 4 + 1) DO
       FOR offset := 0 TO MAX(0, 4 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(4, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F4;

PROCEDURE F5() =
TYPE T = SET OF[0..5];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 5 + 1) DO
       FOR offset := 0 TO MAX(0, 5 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(5, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F5;

PROCEDURE F6() =
TYPE T = SET OF[0..6];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 6 + 1) DO
       FOR offset := 0 TO MAX(0, 6 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(6, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F6;

PROCEDURE F7() =
TYPE T = SET OF[0..7];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 7 + 1) DO
       FOR offset := 0 TO MAX(0, 7 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(7, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F7;

PROCEDURE F8() =
TYPE T = SET OF[0..8];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 8 + 1) DO
       FOR offset := 0 TO MAX(0, 8 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(8, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F8;

PROCEDURE F9() =
TYPE T = SET OF[0..9];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 9 + 1) DO
       FOR offset := 0 TO MAX(0, 9 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(9, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F9;

PROCEDURE F10() =
TYPE T = SET OF[0..10];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 10 + 1) DO
       FOR offset := 0 TO MAX(0, 10 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(10, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F10;

PROCEDURE F11() =
TYPE T = SET OF[0..11];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 11 + 1) DO
       FOR offset := 0 TO MAX(0, 11 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(11, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F11;

PROCEDURE F12() =
TYPE T = SET OF[0..12];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 12 + 1) DO
       FOR offset := 0 TO MAX(0, 12 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(12, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F12;

PROCEDURE F13() =
TYPE T = SET OF[0..13];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 13 + 1) DO
       FOR offset := 0 TO MAX(0, 13 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(13, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F13;

PROCEDURE F14() =
TYPE T = SET OF[0..14];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 14 + 1) DO
       FOR offset := 0 TO MAX(0, 14 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(14, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F14;

PROCEDURE F15() =
TYPE T = SET OF[0..15];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 15 + 1) DO
       FOR offset := 0 TO MAX(0, 15 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(15, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F15;

PROCEDURE F16() =
TYPE T = SET OF[0..16];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 16 + 1) DO
       FOR offset := 0 TO MAX(0, 16 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(16, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F16;

PROCEDURE F17() =
TYPE T = SET OF[0..17];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 17 + 1) DO
       FOR offset := 0 TO MAX(0, 17 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(17, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F17;

PROCEDURE F18() =
TYPE T = SET OF[0..18];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 18 + 1) DO
       FOR offset := 0 TO MAX(0, 18 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(18, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F18;

PROCEDURE F19() =
TYPE T = SET OF[0..19];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 19 + 1) DO
       FOR offset := 0 TO MAX(0, 19 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(19, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F19;

PROCEDURE F20() =
TYPE T = SET OF[0..20];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 20 + 1) DO
       FOR offset := 0 TO MAX(0, 20 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(20, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F20;

PROCEDURE F21() =
TYPE T = SET OF[0..21];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 21 + 1) DO
       FOR offset := 0 TO MAX(0, 21 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(21, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F21;

PROCEDURE F22() =
TYPE T = SET OF[0..22];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 22 + 1) DO
       FOR offset := 0 TO MAX(0, 22 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(22, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F22;

PROCEDURE F23() =
TYPE T = SET OF[0..23];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 23 + 1) DO
       FOR offset := 0 TO MAX(0, 23 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(23, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F23;

PROCEDURE F24() =
TYPE T = SET OF[0..24];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 24 + 1) DO
       FOR offset := 0 TO MAX(0, 24 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(24, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F24;

PROCEDURE F25() =
TYPE T = SET OF[0..25];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 25 + 1) DO
       FOR offset := 0 TO MAX(0, 25 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(25, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F25;

PROCEDURE F26() =
TYPE T = SET OF[0..26];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 26 + 1) DO
       FOR offset := 0 TO MAX(0, 26 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(26, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F26;

PROCEDURE F27() =
TYPE T = SET OF[0..27];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 27 + 1) DO
       FOR offset := 0 TO MAX(0, 27 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(27, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F27;

PROCEDURE F28() =
TYPE T = SET OF[0..28];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 28 + 1) DO
       FOR offset := 0 TO MAX(0, 28 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(28, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F28;

PROCEDURE F29() =
TYPE T = SET OF[0..29];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 29 + 1) DO
       FOR offset := 0 TO MAX(0, 29 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(29, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F29;

PROCEDURE F30() =
TYPE T = SET OF[0..30];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 30 + 1) DO
       FOR offset := 0 TO MAX(0, 30 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(30, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F30;

PROCEDURE F31() =
TYPE T = SET OF[0..31];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 31 + 1) DO
       FOR offset := 0 TO MAX(0, 31 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(31, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F31;

PROCEDURE F32() =
TYPE T = SET OF[0..32];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 32 + 1) DO
       FOR offset := 0 TO MAX(0, 32 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(32, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F32;

PROCEDURE F33() =
TYPE T = SET OF[0..33];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 33 + 1) DO
       FOR offset := 0 TO MAX(0, 33 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(33, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F33;

PROCEDURE F34() =
TYPE T = SET OF[0..34];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 34 + 1) DO
       FOR offset := 0 TO MAX(0, 34 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(34, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F34;

PROCEDURE F35() =
TYPE T = SET OF[0..35];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 35 + 1) DO
       FOR offset := 0 TO MAX(0, 35 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(35, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F35;

PROCEDURE F36() =
TYPE T = SET OF[0..36];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 36 + 1) DO
       FOR offset := 0 TO MAX(0, 36 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(36, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F36;

PROCEDURE F37() =
TYPE T = SET OF[0..37];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 37 + 1) DO
       FOR offset := 0 TO MAX(0, 37 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(37, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F37;

PROCEDURE F38() =
TYPE T = SET OF[0..38];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 38 + 1) DO
       FOR offset := 0 TO MAX(0, 38 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(38, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F38;

PROCEDURE F39() =
TYPE T = SET OF[0..39];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 39 + 1) DO
       FOR offset := 0 TO MAX(0, 39 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(39, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F39;

PROCEDURE F40() =
TYPE T = SET OF[0..40];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 40 + 1) DO
       FOR offset := 0 TO MAX(0, 40 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(40, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F40;

PROCEDURE F41() =
TYPE T = SET OF[0..41];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 41 + 1) DO
       FOR offset := 0 TO MAX(0, 41 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(41, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F41;

PROCEDURE F42() =
TYPE T = SET OF[0..42];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 42 + 1) DO
       FOR offset := 0 TO MAX(0, 42 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(42, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F42;

PROCEDURE F43() =
TYPE T = SET OF[0..43];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 43 + 1) DO
       FOR offset := 0 TO MAX(0, 43 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(43, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F43;

PROCEDURE F44() =
TYPE T = SET OF[0..44];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 44 + 1) DO
       FOR offset := 0 TO MAX(0, 44 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(44, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F44;

PROCEDURE F45() =
TYPE T = SET OF[0..45];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 45 + 1) DO
       FOR offset := 0 TO MAX(0, 45 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(45, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F45;

PROCEDURE F46() =
TYPE T = SET OF[0..46];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 46 + 1) DO
       FOR offset := 0 TO MAX(0, 46 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(46, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F46;

PROCEDURE F47() =
TYPE T = SET OF[0..47];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 47 + 1) DO
       FOR offset := 0 TO MAX(0, 47 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(47, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F47;

PROCEDURE F48() =
TYPE T = SET OF[0..48];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 48 + 1) DO
       FOR offset := 0 TO MAX(0, 48 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(48, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F48;

PROCEDURE F49() =
TYPE T = SET OF[0..49];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 49 + 1) DO
       FOR offset := 0 TO MAX(0, 49 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(49, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F49;

PROCEDURE F50() =
TYPE T = SET OF[0..50];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 50 + 1) DO
       FOR offset := 0 TO MAX(0, 50 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(50, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F50;

PROCEDURE F51() =
TYPE T = SET OF[0..51];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 51 + 1) DO
       FOR offset := 0 TO MAX(0, 51 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(51, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F51;

PROCEDURE F52() =
TYPE T = SET OF[0..52];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 52 + 1) DO
       FOR offset := 0 TO MAX(0, 52 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(52, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F52;

PROCEDURE F53() =
TYPE T = SET OF[0..53];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 53 + 1) DO
       FOR offset := 0 TO MAX(0, 53 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(53, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F53;

PROCEDURE F54() =
TYPE T = SET OF[0..54];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 54 + 1) DO
       FOR offset := 0 TO MAX(0, 54 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(54, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F54;

PROCEDURE F55() =
TYPE T = SET OF[0..55];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 55 + 1) DO
       FOR offset := 0 TO MAX(0, 55 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(55, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F55;

PROCEDURE F56() =
TYPE T = SET OF[0..56];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 56 + 1) DO
       FOR offset := 0 TO MAX(0, 56 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(56, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F56;

PROCEDURE F57() =
TYPE T = SET OF[0..57];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 57 + 1) DO
       FOR offset := 0 TO MAX(0, 57 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(57, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F57;

PROCEDURE F58() =
TYPE T = SET OF[0..58];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 58 + 1) DO
       FOR offset := 0 TO MAX(0, 58 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(58, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F58;

PROCEDURE F59() =
TYPE T = SET OF[0..59];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 59 + 1) DO
       FOR offset := 0 TO MAX(0, 59 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(59, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F59;

PROCEDURE F60() =
TYPE T = SET OF[0..60];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 60 + 1) DO
       FOR offset := 0 TO MAX(0, 60 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(60, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F60;

PROCEDURE F61() =
TYPE T = SET OF[0..61];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 61 + 1) DO
       FOR offset := 0 TO MAX(0, 61 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(61, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F61;

PROCEDURE F62() =
TYPE T = SET OF[0..62];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 62 + 1) DO
       FOR offset := 0 TO MAX(0, 62 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(62, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F62;

PROCEDURE F63() =
TYPE T = SET OF[0..63];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 63 + 1) DO
       FOR offset := 0 TO MAX(0, 63 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(63, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F63;

PROCEDURE F64() =
TYPE T = SET OF[0..64];
VAR a:T;
BEGIN
   FOR count := 0 TO MIN(2, 64 + 1) DO
       FOR offset := 0 TO MAX(0, 64 - count) DO
           a := T{};
           IF count > 0 THEN
               a := T{offset..offset + count - 1};
           END;
           Dump.Dump(64, offset, count, BITSIZE(T), BYTESIZE(T), ADR(a));
       END;
   END;
END F64;

BEGIN
F0();
F1();
F2();
F3();
F4();
F5();
F6();
F7();
F8();
F9();
F10();
F11();
F12();
F13();
F14();
F15();
F16();
F17();
F18();
F19();
F20();
F21();
F22();
F23();
F24();
F25();
F26();
F27();
F28();
F29();
F30();
F31();
F32();
F33();
F34();
F35();
F36();
F37();
F38();
F39();
F40();
F41();
F42();
F43();
F44();
F45();
F46();
F47();
F48();
F49();
F50();
F51();
F52();
F53();
F54();
F55();
F56();
F57();
F58();
F59();
F60();
F61();
F62();
F63();
F64();
END Main.
