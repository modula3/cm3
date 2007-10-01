INTERFACE ProcessInOut;
(* author: thielema *)

IMPORT TextSeq;

TYPE WordSize = {Bits8, Bits16, Bits32};

CONST WordSizeToText = ARRAY WordSize OF TEXT{"b", "w", "l"};

PROCEDURE TextSeqToArr (seq: TextSeq.T; ): REF ARRAY OF TEXT;

END ProcessInOut.
