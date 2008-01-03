(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: M3FP.i3                                               *)
(* Last modified on Fri Jul 15 11:55:54 PDT 1994 by kalsow     *)
(*      modified on Tue May 25 14:16:06 PDT 1993 by muller     *)

INTERFACE M3FP;

IMPORT Fingerprint;

TYPE T = Fingerprint.T;
     Int = BITS 32 FOR [-16_7fffffff - 1 .. 16_7fffffff];
     CharBuf = ARRAY [0..2*BYTESIZE(T)-1] OF CHAR;

CONST Zero = Fingerprint.Zero;
VAR(*CONST*) OfEmpty: T;

PROCEDURE FromText (t: TEXT): T;
PROCEDURE FromChars (READONLY buf: ARRAY OF CHAR;  READONLY t: T): T;
PROCEDURE Combine (a, b: T): T;
PROCEDURE ExtendByInt (READONLY a: T;  i: Int): T;

PROCEDURE FromInt (a: INTEGER;  VAR t: T);
PROCEDURE ToInt   (READONLY t: T): INTEGER;
PROCEDURE ToChars (READONLY t: T;  VAR buf: CharBuf);

END M3FP.

