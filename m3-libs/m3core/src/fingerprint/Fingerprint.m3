(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Jul 18 08:11:31 PDT 1994 by kalsow     *)
(*      modified on Thu Apr 29 16:34:14 1993 by gnelson        *)
(*      modified on Fri Apr 16 15:20:04 PDT 1993 by wobber     *)
(*      modified on Thu Jan 28 21:51:31 PST 1993 by mjordan    *)

UNSAFE MODULE Fingerprint;

(* This module contains the routines for computing fingerprints for strings.
   Externally a fingerprint is an opaque object of 64 bits. Internally a
   fingerprint is a polynomial of degree 64 over Z[2]. *)

IMPORT Text, TextF, Poly, Word;

PROCEDURE FromText (t: Text.T): T =
  (* returns the fingerprint of t *)
  VAR result: T;  poly: Poly.T;
  BEGIN
    poly := Poly.ComputeMod (Poly.ONE, ADR (t[0]), Text.Length (t));
    Poly.ToBytes (poly, result.byte);
    RETURN result;
  END FromText;

PROCEDURE FromChars (READONLY buff: ARRAY OF CHAR;  READONLY fp: T): T =
  VAR n := NUMBER (buff);  result: T;  init, poly: Poly.T;
  BEGIN
    IF n <= 0 THEN RETURN fp; END;
    Poly.FromBytes (fp.byte, init);
    poly := Poly.ComputeMod (init, ADR (buff[0]), n);
    Poly.ToBytes (poly, result.byte);
    RETURN result;
  END FromChars;

CONST
  A = 16_ff208489;
  B = 16_f4872e10;
  C = 16_402d619b;
  D = 16_0bf359a7;

CONST
  Perm = ARRAY [0..255] OF [0..255] { 
    255, 254, 252, 251, 250, 248, 240, 245, 246, 238, 237, 244, 7, 189,
    214, 236, 235, 20, 33, 8, 227, 14, 233, 178, 172, 60, 229, 133, 152,
    19, 210, 203, 221, 208, 76, 18, 13, 199, 113, 62, 40, 190, 213, 194,
    43, 181, 21, 15, 201, 162, 90, 186, 71, 117, 107, 70, 191, 5, 173, 44,
    39, 12, 174, 183, 99, 11, 176, 163, 161, 72, 86, 105, 2, 83, 42, 52,
    179, 135, 103, 110, 151, 58, 108, 96, 166, 25, 115, 66, 142, 10, 141,
    48, 104, 34, 159, 120, 22, 140, 64, 82, 78, 68, 207, 125, 123, 150,
    144, 138, 128, 139, 136, 114, 119, 53, 148, 185, 41, 124, 216, 143,
    49, 92, 98, 51, 112, 73, 50, 63, 16, 46, 158, 126, 206, 122, 94, 132,
    88, 184, 28, 84, 127, 156, 167, 223, 118, 89, 116, 17, 111, 121, 109,
    77, 146, 61, 224, 101, 81, 218, 97, 188, 243, 155, 57, 102, 54, 129,
    93, 192, 153, 106, 36, 145, 79, 31, 137, 26, 67, 85, 175, 80, 168, 65,
    91, 1, 147, 149, 6, 29, 37, 69, 182, 165, 4, 74, 55, 47, 171, 169, 75,
    134, 193, 195, 198, 131, 38, 180, 56, 196, 23, 154, 177, 200, 205, 27,
    209, 95, 204, 160, 3, 30, 157, 32, 9, 212, 211, 45, 202, 170, 0, 219,
    187, 87, 35, 100, 217, 232, 164, 228, 220, 197, 231, 215, 226, 130,
    225, 234, 241, 239, 59, 230, 247, 24, 249, 242, 222, 253
  };

PROCEDURE Combine (READONLY fp1, fp2: T): T =
  VAR poly1, poly2: Poly.T;   buf: ARRAY [0..1] OF T;  res: T;
  BEGIN
    <*ASSERT BYTESIZE(buf) = 2 * BYTESIZE(T) *>
    buf[0] := fp1;  buf[1] := fp2;
    poly1 := Poly.ComputeMod (Poly.ONE, ADR (buf[0]), BYTESIZE (buf));

    poly2[0] := Fix32 (Word.Plus (Word.Times (poly1[0], A),
                                  Word.Times (poly1[1], B)));
    poly2[1] := Fix32 (Word.Plus (Word.Times (poly1[0], C),
                                  Word.Times (poly1[1], D)));
    Poly.ToBytes (poly2, res.byte);

    FOR i := FIRST (res.byte) TO LAST (res.byte) DO
      res.byte[i] := Perm[res.byte[i]];
    END;
    RETURN res;
  END Combine;

PROCEDURE Fix32 (x: Word.T): Poly.Int32 =
  (* return the sign-extended bottom 32 bits of 'x' *)
  CONST
    SigBits = 16_ffffffff; (* mask to grab 32 significant bits *)
    Sign = 16_80000000;
    SignExtend = Word.LeftShift (Word.Not (0), 31);
  BEGIN
    IF Word.And (x, Sign) = 0
      THEN RETURN Word.And (x, SigBits);
      ELSE RETURN Word.Or (SignExtend, Word.And (x, SigBits));
    END;
  END Fix32;

PROCEDURE Equal (READONLY fp1, fp2: T): BOOLEAN =
  BEGIN
    RETURN fp1 = fp2;
  END Equal;

PROCEDURE Hash (READONLY fp: T): INTEGER =
  VAR x: Poly.T;
  BEGIN
    Poly.FromBytes (fp.byte, x);
    RETURN Word.Xor (x[0], x[1]);
  END Hash;

BEGIN
  Poly.ToBytes (Poly.ONE, OfEmpty.byte);
END Fingerprint.
