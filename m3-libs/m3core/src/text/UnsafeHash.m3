(* Copyright (C) 1989, 1990, Digital Equipment Corporation     *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified On Tue Jun 21 11:54:04 PDT 1994 by kalsow         *)
(*      modified On Thu Feb 17 11:53:19 1994 by gnelson        *)
(*      Modified On Tue Feb 15 10:00:03 PST 1994 By perl       *)

UNSAFE MODULE UnsafeHash EXPORTS Text;

IMPORT TextF, Word;

(* "Hash" is the only unsafe procedure in the "Text" interface,
   so me move it into its own module.
*)

(*
We will derive an efficient version of "Text.Hash" starting from the
following simple version:

|    res := 0;
|    i := 0;
|    DO i # M -> 
|      res[i MOD N] := res[i MOD N] XOR t[i];
|      i := i + 1 
|    OD

where

|    t is the text to be hashed
|    M is the number of bytes in t
|    t[i] is byte i of t
|    res is the computed result
|    N is the number of bytes per word
|    res[i] is byte i of res

The numeric value of "res" will depend on whether the
machine is big-endian or little-endian; but the value
of "res" regarded as a sequence of "N" bytes will not.

We would like to derive a more efficient version that 
uses word operations.  We write

|    rotup(w, k)  

to indicate the word "w" (regarded as an array of 
bytes) shifted up (towards increasing indexes) by "k" places, circularly.

|    rotup(w, k)[(i + k) MOD N] = w[i] for all i

We also write

|    rotdn(w, k)

to indicate "up(w, -k)". 

We will also need the corresponding shift operators:
"shiftup" is like "rotup" except it shifts instead of rotates, that is:

|  shiftup(w, k)[i + k] = w[i] 
|      for all i such that i and i+k are both in [0..N-1] and all other
|      bytes of shiftup(w, k) are 0.

and "shiftdn(w, k) = shiftup(w, -k)".


We begin by transforming the simple loop by a change of
coordinates.  We "temp", which is "res" rotated
so that "temp[0]" corresponds to "res[i MOD N]", that is,

|    rotup(temp, i) = res            (Q)

(Note that "rotup(temp, i) = rotup(temp, i MOD N)"; in general the
second argument to "rotup" and "rotdn" only matters modulo "N".)

This allows us to transform the simple loop into:

|    res := 0;
|    temp := 0;
|    {Q}
|    i := 0;
|    DO i # M -> 
|      {Q}
|      res[i MOD N] := res[i MOD N] XOR t[i];
|      temp[0] := temp[0] XOR t[i];
|      i := i + 1;
|      temp := rotdn(temp, 1)
|    OD

Proof that "rotdn(temp, 1)" is correct: 

|    {Q} i := i + 1; temp := rotdn(temp, 1) {Q}
| == {Hoare Logic}
|    Q => Q(i := i+1, temp := rotdn(temp, 1))
| == {Carry out the substitution}
|    Q => rotup(rotdn(temp,1), i+1) = res
| == {Since rotup(rotup(x, a), b) = rotup(x, a+b)}
| == Q => rotup(temp, i) = res
| == Q => Q
| == TRUE
    
Now we can eliminate the work on "res", and do it only at the end:

|    temp := 0;
|    i := 0;
|    DO i # M -> 
|      temp[0] := temp[0] XOR t[i];
|      i := i + 1;
|      temp := rotdn(temp, 1)
|    OD;
|    {Q}
|    res := rotup(temp, M)

Next, we break this loop into three pieces, the first of which processes the
unaligned prefix of the text, the second of which processes the aligned full
words of the text, and the last of which processes trailing subword fragment:

|    temp := 0;
|    i := 0;
|    DO i # M AND (ADR(t[i]) MOD N) # 0 -> 
|      temp[0] := t[i];
|      i := i + 1;
|      temp := rotdn(temp, 1)
|    OD;
|    DO i + N <= M -> 
|      VAR j := i IN
|        DO j # i + N ->
|          temp[0] := temp[0] XOR t[j];
|          j := j + 1;
|          temp := rotdn(temp, 1)
|        OD
|      END;
|      i := i + N
|    OD;
|    DO i # M -> 
|      temp[0] := temp[0] XOR t[i];
|      i := i + 1;
|      temp := rotdn(temp, 1)
|    OD;
|    {Q}
|    res := rotup(temp, M)

Now we will change the first loop to use word operations.  This loop copies
into "temp" some number of bytes from a single word of memory, preserving the
order of the bytes, and leaving the bytes in "temp" so that the last byte
copied is in "temp[N-1]".  We can achieve this with word operations by loading
the appropriate word into "temp", shifting down to eliminate any junk bytes
that preceed the relevant bytes, and then shifting up to eliminate any junk
bytes that follow the relevant bytes, if any.  In our case, the number of
preceeding junk bytes ("jpre") is just "ADR(t[0]) MOD N", and the number of
following junk bytes ("jpost") is zero if "M > N - jpre", otherwise it is 
"N -jpre - M".  Thus the first loop above can be replaced by:

|    jpre := ADR(t[0]) MOD N;
|    IF jpre # 0 ->
|         jpost := MAX(0, N - jpre - M);
|         temp := Mem[ADR(t[0])-jpre];
|         temp := shiftdn(temp, jpre);
|         temp := shiftup(temp, jpost+jpre);
|         i := N - jpre - jpost
|    [] jpre = 0 -> SKIP
|    FI

Similarly, we can change the last loop to use word operations.

|    IF i # M ->
|        jpost := N - (M - i);
|        VAR w := Mem[ADR(t[i])] IN
|          w := shiftup(w, jpost);
|          temp := rotup(temp, jpost);
|          temp := temp XOR w;
|        END
|    [] i = M -> SKIP
|    FI

(Note that the rotation of "temp" to "rotup(temp, jpost)" could equally well
have been written "rotdn(temp, M-i)".  The same rotation that brings "temp"
into alignment with "shiftup(w, jpost)" also matches the rotation performed by
the loop we are refining.)

Finally we change the middle loop to use word operations.  Its
inner loop rotates "temp" by one "N" times, and consequently
has no net rotation.  The inner loop also XORs

|   t[i], ..., t[i+N-1]

into

|   temp[0], ..., temp[N-1].

respectively.  Since "ADR(t[i]) MOD N = 0", this can be 
accomplished by a single word operation.  The new version
of the middle loop is therefore:

|    DO i + N <= M -> 
|      temp := temp XOR Mem[ADR(t[i])];
|      i := i + N
|    OD;

In the above we write "Mem[addr]" to indicate the word whose byte's addresses
range from "addr" to "addr+N-1", regarding that word as an array of bytes.  
We have also (for the first time) used XOR on words instead of bytes.

Now we can translate the program into Modula-3.  Here is the collected guarded
command version:

|    temp := 0;
|    i := 0;
|    jpre := ADR(t[0]) MOD N;
|    IF jpre # 0 ->
|         jpost := MAX(0, N - jpre - M);
|         temp := Mem[ADR(t[0])-jpre];
|         temp := shiftdn(temp, jpre);
|         temp := shiftup(temp, jpost+jpre);
|         i := N - jpre - jpost
|    [] jpre = 0 -> SKIP
|    FI;
|    DO i + N <= M -> 
|      temp := temp XOR Mem[ADR(t[i])];
|      i := i + N
|    OD;
|    IF i # M ->
|        jpost := N - (M - i);
|        VAR w := Mem[ADR(t[i])] IN
|          w := shiftup(w, jpost);
|          temp := rotup(temp, jpost);
|          temp := temp XOR w;
|        END
|    [] i = M -> SKIP
|    FI;
|    res := rotup(temp, M)

Which in Modula-3 becomes:
*)
        
PROCEDURE Hash(t: TEXT): INTEGER =
  CONST
    N = BYTESIZE(INTEGER);
  VAR
    temp := 0;
    p    := LOOPHOLE (ADR(t[0]), UNTRACED REF INTEGER);
    m    := NUMBER(t^) - 1;
    endp := p + m;
  BEGIN
    VAR jpre := Word.And(LOOPHOLE(p, INTEGER), N-1);
        jpost: INTEGER;
    BEGIN
      IF jpre # 0 THEN
        jpost := MAX(0, N - jpre - m);
        temp := LOOPHOLE(p - jpre, UNTRACED REF INTEGER)^;
        temp := Word.Shift(Word.Shift(temp, jpre * -up1), (jpost+jpre) * up1);
        INC(p, N - jpre - jpost)
      END
    END;
    WHILE p + N < endp DO
      temp := Word.Xor(temp, p^);
      INC(p, N)
    END;
     IF littleEndian THEN
      IF p # endp THEN
        VAR jpost := N - (endp - p);
            w := Word.Shift(p^, Word.Shift(jpost, lgUp1));
        BEGIN
          temp := Word.Xor(Word.Rotate(temp, Word.Shift(jpost, lgUp1)), w)
        END
      END;
      RETURN Word.Plus(Word.Rotate(temp, Word.Shift(m, lgUp1)), m)
    ELSE
      IF p # endp THEN
        VAR jpost := N - (endp - p);
            w := Word.Shift(p^, -Word.Shift(jpost, lgUp1));
        BEGIN
          temp := Word.Xor(Word.Rotate(temp, -Word.Shift(jpost, lgUp1)), w)
        END
      END;
      RETURN Word.Plus(Word.Rotate(temp, -Word.Shift(m, lgUp1)), m)
    END
  END Hash;

(* In the Modula-3 version we have added the text length into
   the result before returning it, in order to get a better
   hash function for texts that contain long strings of
   repeated characters.  Also, instead of multiplying
   by "up1" we have shifted by its base two logarithm
  "lg2Up1".  These constants are computed below: *)

VAR  
  littleEndian: BOOLEAN;
  ref := NEW(UNTRACED REF INTEGER);
  up1: INTEGER;
  lgUp1: INTEGER;

BEGIN
  <* ASSERT 1 = ADRSIZE(CHAR) *>
  <* ASSERT 0 = Word.And(BYTESIZE(INTEGER), BYTESIZE(INTEGER)-1) *>
  ref^ := 1;
  littleEndian := 1 = LOOPHOLE(ref, UNTRACED REF [0..255])^;
  IF littleEndian THEN up1 := BITSIZE(CHAR) ELSE up1 := -BITSIZE(CHAR) END;
  lgUp1 := 0;
  VAR k := 1; BEGIN
    WHILE k # ABS(up1) DO
      INC(lgUp1); k := k + k
    END
  END
END UnsafeHash.
