(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
INTERFACE Test;

IMPORT Text;
IMPORT Cstdint;

VAR
  errors:   INTEGER := 0;
  warnings: INTEGER := 0;

TYPE
  T = RECORD
    d := ARRAY [1..10] OF LONGREAL {0.0d0, 0.5d0, 1.0d0, 2.0d0, -1.0d0, -3.0d0, 12.34d0, -124.456d0, 1000.0d0, -10000.0d0};
    f := ARRAY [1..10] OF     REAL {0.0e0, 0.5e0, 1.0e0, 2.0e0, -1.0e0, -3.5e0, 12.34e0, -124.456e0, 1000.0e0, -10000.0e0};
    align := 0.0d0;
  END;

<*EXTERNAL "Test__CheckFloatsAndTypes"*> PROCEDURE CheckFloatsAndTypes(READONLY t:T; size := BYTESIZE(T));

PROCEDURE msg (t: Text.T);
PROCEDURE msgB (b: BOOLEAN);
PROCEDURE msgI (i: INTEGER);
PROCEDURE msgC (c: CHAR);
PROCEDURE msgR (r: REAL);

PROCEDURE check (b: BOOLEAN);
PROCEDURE checkM (b: BOOLEAN; msg: TEXT);
PROCEDURE checkB (b, shouldBe: BOOLEAN);
PROCEDURE checkI (i, shouldBe: INTEGER);
PROCEDURE checkN (i, shouldBe: LONGINT);
PROCEDURE checkC (i, shouldBe: CHAR);
PROCEDURE checkR (r, shouldBe: REAL);
PROCEDURE checkL (r, shouldBe: LONGREAL);
PROCEDURE checkX (r, shouldBe: EXTENDED);

PROCEDURE warn (b: BOOLEAN);

PROCEDURE Err (a, b, c, d: TEXT := NIL);
PROCEDURE Out (a, b, c, d, e: TEXT := NIL);

PROCEDURE done ();

(* Test sign/zero extension of
 * the return value of functions returning types
 * smaller than 32bits. It is most effective
 * to test it on NT386, esp. with older versions of
 * Visual C++, or hand written assembly.
 *)

<*EXTERNAL*>PROCEDURE  NegativeInt8():    Cstdint.int8_t;
<*EXTERNAL*>PROCEDURE NegativeUInt8():   Cstdint.uint8_t;
<*EXTERNAL*>PROCEDURE  NegativeInt16():  Cstdint.int16_t;
<*EXTERNAL*>PROCEDURE NegativeUInt16(): Cstdint.uint16_t;
<*EXTERNAL*>PROCEDURE  NegativeInt32():  Cstdint.int32_t;
<*EXTERNAL*>PROCEDURE NegativeUInt32(): Cstdint.uint32_t;
<*EXTERNAL*>PROCEDURE  NegativeInt64():  Cstdint.int64_t;
<*EXTERNAL*>PROCEDURE NegativeUInt64(): Cstdint.uint64_t;

<*EXTERNAL*>PROCEDURE  PositiveInt8():    Cstdint.int8_t;
<*EXTERNAL*>PROCEDURE PositiveUInt8():   Cstdint.uint8_t;
<*EXTERNAL*>PROCEDURE  PositiveInt16():  Cstdint.int16_t;
<*EXTERNAL*>PROCEDURE PositiveUInt16(): Cstdint.uint16_t;
<*EXTERNAL*>PROCEDURE  PositiveInt32():  Cstdint.int32_t;
<*EXTERNAL*>PROCEDURE PositiveUInt32(): Cstdint.uint32_t;
<*EXTERNAL*>PROCEDURE  PositiveInt64():  Cstdint.int64_t;
<*EXTERNAL*>PROCEDURE PositiveUInt64(): Cstdint.uint64_t;

END Test.
