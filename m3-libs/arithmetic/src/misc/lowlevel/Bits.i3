INTERFACE Bits;
(*Copyright (c) 1996, m3na project

Abstract: Bits and Bytes

2/17/96  Harry George    Initial version
*)

IMPORT Word;

(*============================*)
(* Handy collectors           *)
(*============================*)
TYPE
  NYBBLE = BITS 4 FOR [0..16_F];
  BYTE   = BITS 8 FOR [0..16_FF];

(*Declare bit arrays (i.e. bit maps) as needed.
  BitArray = ARRAY [0..N] OF BITS 1 FOR [0..1];
*)

  ByteArray= REF ARRAY OF BYTE;

(*============================*)
(* Which Endian?              *)
(*============================*)
TYPE
(*e.g., LOOPHOLE(my_cardinal,BigEndian).b1*)
  AnyEndian=
    RECORD data:ARRAY [0..3] OF BYTE; END;

  BigEndian=
    RECORD b4,b3,b2,b1:BYTE; END;

  LittleEndian=
    RECORD b1,b2,b3,b4:BYTE; END;

PROCEDURE WhichEndian():[-1..+1];
(*-1 means little-endian, +1 means big-endian.*)

(*============================*)
(* Other Functions            *)
(*============================*)
PROCEDURE Fmt(x:Word.T;
              nbits:[1..Word.Size]:=32;
              base :CARDINAL:=2   (*typically 2 or 16*)
              ):TEXT;
(*returns text for x, left padded
to nbits length by "0" if necessary.
*)


PROCEDURE Reverse(x:Word.T;               (*given this number*)
                  nbits:[1..Word.Size];   (*using the low n bits*)
                  ):Word.T;               (*return reversed bit pattern*)
(*E.g., nbits = 4 ==> 00001011 becomes 00001101
NOTE: There are better ways to do this if you need to do a whole array,
as in FFT.  See xFFT.m3.*)

PROCEDURE HashPJW(READONLY str: ARRAY OF CHAR; (*given this string*)
                          n1,nn:CARDINAL        (*using n1..nn*)
                              ):CARDINAL;       (*return hash value*)
(*P. Weinberger's hash.
From the str buffer, using str[n1]..str[nn], make a hash value
*)

PROCEDURE HashELF(READONLY str: ARRAY OF CHAR; (*given this string*)
                          n1,nn:CARDINAL        (*using n1..nn*)
                              ):CARDINAL;       (*return hash value*)
(*ELF hash
From the str buffer, using str[n1]..str[nn], make a hash value
*)


(*==========================*)
END Bits.
