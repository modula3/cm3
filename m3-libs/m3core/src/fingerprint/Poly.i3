(* Copyright (C) 1989, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Thu Feb 10 15:13:13 PST 1994 by kalsow    *)
(*      modified on Mon May 10 09:16:09 PDT 1993 by muller    *)

INTERFACE Poly;

(* This module provides polynomials of degree [0..63] with
    coefficients in the field Z(2).
*)

TYPE
  T      = ARRAY [0..1] OF Int32;
  Int32  = [-16_7fffffff-1 .. 16_7fffffff];
  Card32 = [0 .. LAST (Int32)];
  Byte   = BITS 8 FOR [0..255];
  Bytes  = ARRAY [0..7] OF Byte;

CONST
  ZERO = T{ 0, 0 };
  ONE  = T{ 0, FIRST (Int32) };
  X    = T{ 0, 16_40000000 };

PROCEDURE Sum     (READONLY p, q: T): T; (* returns (p+q) *)
PROCEDURE Product (READONLY p, q: T): T; (* returns (p * q MOD PolyBasis.P) *)
PROCEDURE TimesX  (READONLY p : T)  : T; (* returns (p * X^1) *)
PROCEDURE Power   (d: Card32)       : T; (* returns (x^d MOD PolyBasis.P) *)

PROCEDURE ComputeMod (READONLY init: T;  addr: ADDRESS;  len: INTEGER): T;
(* This procedure assumes that the 'len' bytes beginning at address
   'addr' define a polynomial, A(x), of degree '(8*len)'.
   The procedure returns '(init*x^(8*len) + A(x)) MOD PolyBasis.P' *)

PROCEDURE ToBytes   (READONLY t: T;  VAR b: Bytes);
PROCEDURE FromBytes (READONLY b: Bytes;  VAR t: T);

END Poly.
