MODULE Main;

FROM Test IMPORT checkI, checkN;

TYPE U1 = [ 0 .. 16_7F ];
TYPE S1 = [ 0 .. 16_FF ];
TYPE U2 = [ 0 .. 16_7FFF ];
TYPE S2 = [ 0 .. 16_FFFF ];
TYPE U3 = [ 0 .. 16_7FFFFF ];
TYPE S3 = [ 0 .. 16_FFFFFF ];
TYPE U4 = [ 0 .. 16_7FFFFFFF ];
TYPE S4 = [ 0 .. 16_7FFFFFFF ];

TYPE LU1 = [ 0L .. 16_7FL ];
TYPE LS1 = [ 0L .. 16_FFL ];
TYPE LU2 = [ 0L .. 16_7FFFL ];
TYPE LS2 = [ 0L .. 16_FFFFL ];
TYPE LU3 = [ 0L .. 16_7FFFFFL ];
TYPE LS3 = [ 0L .. 16_FFFFFFL ];
TYPE LU4 = [ 0L .. 16_7FFFFFFFL ];
TYPE LS4 = [ 0L .. 16_FFFFFFFFL ];
TYPE LU5 = [ 0L .. 16_7FFFFFFFFFL ];
TYPE LS5 = [ 0L .. 16_FFFFFFFFFFL ];
TYPE LU6 = [ 0L .. 16_7FFFFFFFFFFFL ];
TYPE LS6 = [ 0L .. 16_FFFFFFFFFFFFL ];
TYPE LU7 = [ 0L .. 16_7FFFFFFFFFFFFFL ];
TYPE LS7 = [ 0L .. 16_FFFFFFFFFFFFFFL ];
TYPE LU8 = [ 0L .. 16_7FFFFFFFFFFFFFFFL ];
TYPE LS8 = [ 0L .. 16_7FFFFFFFFFFFFFFFL ];

PROCEDURE Work ( ) =
  VAR VU1 : U1 := 0;
  VAR VS1 : S1 := 0;
  VAR VU2 : U2 := 0;
  VAR VS2 : S2 := 0;
  VAR VU3 : U3 := 0;
  VAR VS3 : S3 := 0;
  VAR VU4 : U4 := 0;
  VAR VS4 : S4 := 0;
  VAR VLU1 : LU1 := 0L;
  VAR VLS1 : LS1 := 0L;
  VAR VLU2 : LU2 := 0L;
  VAR VLS2 : LS2 := 0L;
  VAR VLU3 : LU3 := 0L;
  VAR VLS3 : LS3 := 0L;
  VAR VLU4 : LU4 := 0L;
  VAR VLS4 : LS4 := 0L;
  VAR VLU5 : LU5 := 0L;
  VAR VLS5 : LS5 := 0L;
  VAR VLU6 : LU6 := 0L;
  VAR VLS6 : LS6 := 0L;
  VAR VLU7 : LU7 := 0L;
  VAR VLS7 : LS7 := 0L;
  VAR VLU8 : LU8 := 0L;
  VAR VLS8 : LS8 := 0L;
  VAR X : INTEGER;

BEGIN
  checkI(VU1, 0);
  checkI(VS1, 0);
  checkI(VU2, 0);
  checkI(VS2, 0);
  checkI(VU3, 0);
  checkI(VS3, 0);
  checkI(VU4, 0);
  checkI(VS4, 0);

  checkN (VLU1, 0L);
  checkN (VLS1, 0L);
  checkN (VLU2, 0L);
  checkN (VLS2, 0L);
  checkN (VLU3, 0L);
  checkN (VLS3, 0L);
  checkN (VLU4, 0L);
  checkN (VLS4, 0L);
  checkN (VLU5, 0L);
  checkN (VLS5, 0L);
  checkN (VLU6, 0L);
  checkN (VLS6, 0L);
  checkN (VLU7, 0L);
  checkN (VLS7, 0L);
  checkN (VLU8, 0L);
  checkN (VLS8, 0L);

  X := VU1
  END Work;

BEGIN
  Work ()
END Main.
