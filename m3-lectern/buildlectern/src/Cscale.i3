(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: a user interface for viewing documents stored as images *)
(* C optimizations for LGM module *)

(* Last modified on Wed Feb  8 12:17:50 PST 1995 by birrell  *)
(*      modified on Tue Nov 15 14:53:26 PST 1994 by wobber   *)

UNSAFE INTERFACE Cscale;

IMPORT Ctypes;

<*EXTERNAL*> PROCEDURE ScaleInit();

(* for LSBFirst pixmaps *)

<*EXTERNAL*> PROCEDURE By2(
    line: Ctypes.long_star; pixels: Ctypes.int_star; width: Ctypes.int);

<*EXTERNAL*> PROCEDURE By3(
    line: Ctypes.long_star; pixels: Ctypes.int_star; width: Ctypes.int);

<*EXTERNAL*> PROCEDURE By4(
    line: Ctypes.long_star; pixels: Ctypes.int_star; width: Ctypes.int);

<*EXTERNAL*> PROCEDURE By48(
    line: Ctypes.long_star; pixels: Ctypes.int_star; width: Ctypes.int);

<*EXTERNAL*> PROCEDURE ByN(
    line: Ctypes.long_star; pixels: Ctypes.int_star;
    width: Ctypes.int; n: Ctypes.int);

(* for MSBFirst pixmaps *)

<*EXTERNAL*> PROCEDURE By2R(
    line: Ctypes.long_star; pixels: Ctypes.int_star; width: Ctypes.int);

<*EXTERNAL*> PROCEDURE By4R(
    line: Ctypes.long_star; pixels: Ctypes.int_star; width: Ctypes.int);

<*EXTERNAL*> PROCEDURE By48R(
    line: Ctypes.long_star; pixels: Ctypes.int_star; width: Ctypes.int);

<*EXTERNAL*> PROCEDURE ByNR(
    line: Ctypes.long_star; pixels: Ctypes.int_star;
    width: Ctypes.int; n: Ctypes.int);


<*EXTERNAL*> PROCEDURE FastZero(l: Ctypes.char_star; n: Ctypes.int);

<*EXTERNAL*> PROCEDURE FastMap(
    l: Ctypes.long_star; nl: Ctypes.int; map: Ctypes.long_star);

<*EXTERNAL*> PROCEDURE ScaleRGB(
    l: Ctypes.long_star; nl: Ctypes.int; unscaled: Ctypes.char_star;
    scale: Ctypes.int);

<*EXTERNAL*> PROCEDURE FastMapToChar(
    l: Ctypes.long_star; nl: Ctypes.int; map: Ctypes.char_star;
    destChars: Ctypes.char_star);

<*EXTERNAL*> PROCEDURE FastPack(
    src: Ctypes.long_star; dest: Ctypes.char_star; n: Ctypes.int);

<*EXTERNAL*> PROCEDURE FinishPRun(
    src: Ctypes.long_star; dest: Ctypes.char_star;
    lim: Ctypes.int): Ctypes.int;

<*EXTERNAL*> PROCEDURE FinishZRun(
    src: Ctypes.long_star; lim: Ctypes.int): Ctypes.int;

END Cscale.
