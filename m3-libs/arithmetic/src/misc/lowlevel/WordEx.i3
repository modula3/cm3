INTERFACE WordEx;
(* Arithmetic for Modula-3, see doc for details

   Abstract: Extensions to the Word module

   [These ought to be available in Word, and compiled in-line as with other
   Word functions.  Doing them as coded implementations is a stopgap.]

   3/16/96 Warren D.  Smith Initial version *)

IMPORT Word;


PROCEDURE PlusWithCarry (VALUE x, y: Word.T; VAR carry: BOOLEAN; ): Word.T;

PROCEDURE MinusWithBorrow (VALUE x, y: Word.T; VAR borrow: BOOLEAN; ):
  Word.T;


PROCEDURE LeftShiftWithProbscosis
  (VALUE x: Word.T; VALUE sh: CARDINAL; VAR probscosis: Word.T; ): Word.T;
(* Perhaps also a version LeftShiftWithCarry, specializing this to sh=1,
   should be provided, to allow extra closeness to the hardware. *)

PROCEDURE RightShiftWithProbscosis
  (VALUE x: Word.T; VALUE sh: CARDINAL; VAR probscosis: Word.T; ): Word.T;
(* Perhaps also a version RightShiftWithCarry, specializing to sh=1, should
   be provided, to allow extra closeness to the hardware. *)

PROCEDURE DoubleLengthMultiply (VALUE x, y: Word.T; VAR lo, hi: Word.T; );

PROCEDURE HighTimes (VALUE x, y: Word.T; ): Word.T;
(* Returns the "hi" word in DoubleLengthMultiply(x,y, lo,hi), which was
   just implemented as lo := Word.Times(x,y); hi := HighTimes(x,y). *)

(* I have not written a DoubleLengthDivide workaround, but this absence is
   perhaps not so serious since many bignum packages, whose authors are
   lazy, implement division via multiplication and Newton algorithm, and
   modulus via division, multiplication and subtraction.  Similar remarks
   apply to sqrt.  Still, I think, since hardware provides this, the
   routine ought to be accessible to modula-3 programmers! *)

PROCEDURE PopCount (x: Word.T; ): [0 .. Word.Size];
(* Returns number of 1s in binary representation of x *)

PROCEDURE FindLeastSignifBit (x: Word.T; ): [-1 .. Word.Size - 1];
(* Returns the index (in [0..Word.Size-1]) of the least significant bit of
   x that is 1.  But if x=0, returns -1. *)

PROCEDURE FindMostSignifBit (x: Word.T; ): [0 .. Word.Size];
(* Returns the index (in [0..Word.Size-1]) of the most significant bit of x
   that is 1.  But if x=0, returns Word.Size. *)


PROCEDURE Test ();
(* Performs a series of assertions *)

END WordEx.
