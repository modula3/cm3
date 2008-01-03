(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: TypeFP.i3                                             *)
(* Last modified on Fri Jul 22 10:12:14 PDT 1994 by kalsow     *)

INTERFACE TypeFP;

IMPORT Type, M3FP, M3Buf;

PROCEDURE FromType (t: Type.T): M3FP.T;
(* Computes and returns 't's fingerprint, and sets 't.fp' and 't.uid'. *)

PROCEDURE FromText (txt: TEXT): M3FP.T;
PROCEDURE FromPair (READONLY a, b: M3FP.T): M3FP.T;
PROCEDURE FromBuf  (buf: M3Buf.T): M3FP.T;

PROCEDURE Initialize ();
PROCEDURE Reset ();

END TypeFP.
