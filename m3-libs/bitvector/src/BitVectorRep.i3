(* Copyright (C) 1997, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Created on Thu Mar 27 10:21:25 PST 1997 by heydon        *)
(* Last modified on Sat Nov 29 18:19:49 PST 1997 by heydon  *)

(* This interface reveals the representations of the types
   "BitVector.T" and "BitVector.Iterator". It may be useful to
   clients who wish to implement subtypes of "BitVector.T". *)

INTERFACE BitVectorRep;

IMPORT Word, BitVector;

REVEAL
  BitVector.T <: T;

TYPE
  T = BitVector.Public OBJECT
    word: REF ARRAY OF Word.T := NIL;
    firstAvailWd: CARDINAL := 0;
    sz: CARDINAL := 0;
  END;

REVEAL
  BitVector.Iterator <: Iterator;

TYPE
  Iterator = BitVector.PublicIter OBJECT
    bv: BitVector.T;     (* the associated bit vector *)
    bitIndex: CARDINAL;  (* the next bit index *)
    wordIndex: CARDINAL; (* index into the bv's "word" array *)
    mask: Word.T;        (* current bit mask *)
  END;

END BitVectorRep.

(*
\subsection{BitVector.T Invariants}

A new, empty bit vector "bv := NEW(BitVector.T).init(sizeHint)"
has "bv.sz = 0". The following invariants hold for an initialized
bit vector, where "numWords" denotes "NUMBER(word^)" if "word # NIL"
or 0 if "word = NIL", "bitsPerWord" denotes "BITSIZE(Word.T)", and
"bit(word, i)" denotes the state of the i'th bit of the "word" array:

| I0. sz > 0 ==> word # NIL
| I1. sz IN [0, numWords * bitsPerWd]
| I2. (forall i: i IN [sz, numWords * bitsPerWd) => NOT bit(word, i))
| I3. (forall i: i IN [0, firstAvailWd * bitsPerWd) => bit(word, i))
| I4. firstAvailWd IN [0, numWords]

I1 says that the "sz" field is at most the total number of bits in
the "word" array. Hence, at its maximum value, "sz" is the index of
the non-existent bit just past the end of the "word" array.

I2 says that all bits in the "word" array with index at least "sz"
are reset. Hence, "sz" is a strict upper-bound on the index of the
bit vector's most significant bit.

I3 says that all of the bits in the first "firstAvailWd" words of the
"word" array are set. Hence, "firstAvailWd" is a lower bound on
the index of the first word in the "word" array that contains any
unset bits.

I4 says that "firstAvailWd" is an index into the "word" array, or
may be the index of the non-existent word just past the end of the
"word" array.

Note that I2 and I3 together imply that:

| I5. firstAvailWd * bitsPerWd <= sz
*)

(*
\subsection{BitVector.Iterator Invariants}

An "Iterator" works by checking the corresponding bit vector's
bits in increasing order. Its state records the index of the
next bit to test. In particular, if "iter" is of type "Iterator",
then:

\begin{itemize}
\item
"iter.bv" is the bit vector on which "iter" was created.

\item
"iter.bitIndex" is the index of the next bit to test.

\item
"iter.wordIndex" is the index of the word in which bit
"iter.bitIndex" occurs.

\item
"iter.mask" is "Word.LeftShift(1, iter.bitIndex MOD Word.Size)".
\end{itemize}
*)
