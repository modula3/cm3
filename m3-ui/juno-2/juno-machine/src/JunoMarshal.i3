(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Feb  8 09:43:52 PST 1995 by heydon                   *)

INTERFACE JunoMarshal;

IMPORT JunoRT, JunoValue;

(* Encode bytecodes into a ByteStream, or decode them from it. All of these
   procedures move their "a" argument past the read or written value. *)

TYPE
  BytePtr = UNTRACED REF JunoRT.ByteCode;
  Short  = BITS 16 FOR [-16_8000 .. 16_7fff];
  UShort = BITS 16 FOR [0 .. 16_ffff];
  ULong  = BITS 32 FOR [0 .. 16_7fffffff];

CONST
  ShortSize = BYTESIZE(Short);
  UShortSize = BYTESIZE(UShort);
  ULongSize = BYTESIZE(ULong);
  RealSize = BYTESIZE(JunoValue.Real);

PROCEDURE ReadShort (VAR a: BytePtr): Short;
PROCEDURE WriteShort (VAR a: BytePtr; i: Short);
(* Read/write a signed 16-bit value. *)

PROCEDURE ReadUShort (VAR a: BytePtr): UShort;
PROCEDURE WriteUShort (VAR a: BytePtr; i: UShort);
(* Read/write an unsigned 16-bit value. *)

PROCEDURE ReadULong (VAR a: BytePtr): ULong;
PROCEDURE WriteULong (VAR a: BytePtr; i: ULong);
(* Read/write an unsigned 32-bit value. *)

PROCEDURE ReadReal(VAR a: BytePtr): JunoValue.Real;
PROCEDURE WriteReal(VAR a: BytePtr; r: JunoValue.Real);
(* Read/write a real number. *)

END JunoMarshal.
