(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Tue Nov  8 14:59:28 PST 1994 by kalsow   *)
(*      modified on Wed Feb 10 14:51:58 PST 1993 by harrison *)

UNSAFE MODULE WinNT;

FROM M3toC IMPORT TtoS;
FROM Word IMPORT And, Or, Not, Shift;
FROM WinDef IMPORT WORD;

(* Basic Type of x *)
(* #define BTYPE(x) ((x) & N_BTMASK) *)

<* INLINE *>
PROCEDURE BTYPE (x: WORD): WORD =
  BEGIN
    RETURN And(x, N_BTMASK);
  END BTYPE;

(* Is x a pointer? *)
(* #define ISPTR(x) (((x) & N_TMASK) == (IMAGE_SYM_DTYPE_POINTER << N_BTSHFT)) *)

<* INLINE *>
PROCEDURE ISPTR (x: WORD): BOOLEAN =
  BEGIN
    RETURN And(x, N_TMASK) = Shift(IMAGE_SYM_DTYPE_POINTER, N_BTSHFT);
  END ISPTR;

(* Is x a function? *)
(* #define ISFCN(x) (((x) & N_TMASK) == (IMAGE_SYM_DTYPE_FUNCTION << N_BTSHFT)) *)
<* INLINE *>
PROCEDURE ISFCN (x: WORD): BOOLEAN =
  BEGIN
    RETURN And(x, N_TMASK) = Shift(IMAGE_SYM_DTYPE_FUNCTION, N_BTSHFT);
  END ISFCN;

(* Is x an array? *)
(* #define ISARY(x) (((x) & N_TMASK) == (IMAGE_SYM_DTYPE_ARRAY << N_BTSHFT)) *)

<* INLINE *>
PROCEDURE ISARY (x: WORD): BOOLEAN =
  BEGIN
    RETURN And(x, N_TMASK) = Shift(IMAGE_SYM_DTYPE_ARRAY, N_BTSHFT);
  END ISARY;

(* Is x a structure, union, or enumeration TAG? *)
(* #define ISTAG(x) ((x)==IMAGE_SYM_CLASS_STRUCT_TAG || (x)==IMAGE_SYM_CLASS_UNION_TAG || (x)==IMAGE_SYM_CLASS_ENUM_TAG) *)

<* INLINE *>
PROCEDURE ISTAG (x: WORD): BOOLEAN =
  BEGIN
    RETURN x = IMAGE_SYM_CLASS_STRUCT_TAG OR x = IMAGE_SYM_CLASS_UNION_TAG
             OR x = IMAGE_SYM_CLASS_ENUM_TAG;
  END ISTAG;

(* #define INCREF(x) ((((x)&~N_BTMASK)<<N_TSHIFT)|(IMAGE_SYM_DTYPE_POINTER<<N_BTSHFT)|(x&N_BTMASK)) *)

<* INLINE *>
PROCEDURE INCREF (x: WORD): WORD =
  BEGIN
    RETURN
      Or(Or(Shift(And(x, Not(N_BTMASK)), N_TSHIFT),
            Shift(IMAGE_SYM_DTYPE_POINTER, N_BTSHFT)), And(x, N_BTSHFT));
  END INCREF;

(* #define DECREF(x) ((((x)>>N_TSHIFT)&~N_BTMASK)|((x)&N_BTMASK)) *)

<* INLINE *>
PROCEDURE DECREF (x: WORD): WORD =
  BEGIN
    RETURN Or(And(Shift(x, -N_TSHIFT), Not(N_BTMASK)), And(x, N_BTMASK));
  END DECREF;


<* INLINE *>
PROCEDURE IMAGE_SNAP_BY_ORDINAL (Ordinal: WORD): BOOLEAN =
  BEGIN
    RETURN And(Ordinal, IMAGE_ORDINAL_FLAG) # 0;
  END IMAGE_SNAP_BY_ORDINAL;

<* INLINE *>
PROCEDURE IMAGE_ORDINAL (Ordinal: WORD): WORD =
  BEGIN
    RETURN And(Ordinal, 16_ffff);
  END IMAGE_ORDINAL;

BEGIN
  IMAGE_ARCHIVE_START            := TtoS("!<arch>\n");
  IMAGE_ARCHIVE_END              := TtoS("`\n");
  IMAGE_ARCHIVE_PAD              := TtoS("\n");
  IMAGE_ARCHIVE_LINKER_MEMBER    := TtoS("/               ");
  IMAGE_ARCHIVE_LONGNAMES_MEMBER := TtoS("//              ");
END WinNT.
