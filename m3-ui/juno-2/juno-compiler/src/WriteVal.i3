(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jun  8 18:58:11 PDT 1994 by heydon                   *)

INTERFACE WriteVal;

IMPORT JunoRT, JunoValue;

(* The procedures in this interface write values of various types into a byte
   stream. All procedures take to VAR (*INOUT*) variables: a byte stream
   "code" and an offset in that bytestream. The instructions first test if the
   bytestream is long enough; if not, they set "code" to a new bytesteam with
   the same prefix (up to location "loc") that is large enough. These
   procedures also advance "loc" to point to the byte in the stream after the
   last one written for the value. *)

TYPE
  Code = JunoRT.ByteStream;
  JVReal = JunoValue.Real;

PROCEDURE UShort(VAR (*IO*) code: Code; VAR (*IO*) loc: CARDINAL; v: CARDINAL);
(* Write "v" as an unsigned 16-bit value to "code" at location "loc". *)

PROCEDURE Short(VAR (*IO*) code: Code; VAR (*IO*) loc: CARDINAL; v: INTEGER);
(* Write "v" as a signed 16-bit value to "code" at location "loc". *)

PROCEDURE ULong(VAR (*IO*) code: Code; VAR (*IO*) loc: CARDINAL; v: CARDINAL);
(* Write "v" as an unsigned 32-bit value to "code" at location "loc". *)

PROCEDURE Real(VAR (*IO*) code: Code; VAR (*IO*) loc: CARDINAL; v: JVReal);
(* Write "v" as a real number to "code" at location "loc". *)

END WriteVal.
