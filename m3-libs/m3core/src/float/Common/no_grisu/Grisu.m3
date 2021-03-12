(* Copyright (C) 2016 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

MODULE Grisu;

(* temporarily disable Grisu for Win32 and C backend until problems investigated
 * NT386 fails to compile, C fails at runtime
 *)
<*NOWARN*>
PROCEDURE FastDtoa(v : LONGREAL;
                   mode : FastDtoaMode;
                   requestedDigits : INTEGER;
                   VAR buffer : DigitArr;
                   VAR length : CARDINAL;
                   VAR decimalPoint : INTEGER) : BOOLEAN =
  BEGIN
    RETURN FALSE;
  END FastDtoa;

BEGIN
END Grisu.
