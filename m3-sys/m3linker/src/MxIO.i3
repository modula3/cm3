(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: MxIO.i3                                               *)
(* Last Modified On Mon Sep 19 14:20:52 PDT 1994 By kalsow     *)

INTERFACE MxIO;

IMPORT M3FP, M3Buf, Mx;

PROCEDURE PutTxt (wr: M3Buf.T;  a, b, c, d, e: TEXT := NIL);
PROCEDURE PutCh  (wr: M3Buf.T;  ch: CHAR);
PROCEDURE PutInt (wr: M3Buf.T;  i: INTEGER;  ch: TEXT);
PROCEDURE PutFP  (wr: M3Buf.T;  READONLY x: M3FP.T;  ch: TEXT);
PROCEDURE PutHex (wr: M3Buf.T;  x: Mx.Int32;  ch: TEXT);

END MxIO.

