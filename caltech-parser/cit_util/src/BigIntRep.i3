INTERFACE BigIntRep;
IMPORT BigInt;

IMPORT Word;

REVEAL 
  BigInt.T = BigInt.Public BRANDED BigInt.Brand OBJECT
    sign : [ -1 .. 1 ];
    rep  : NSeq;
  END;

CONST Chunk = 4; (* make it easy to debug in hex *)
      BaseLog2 =   ((BITSIZE(Word.T) DIV 2 - 1) DIV Chunk) * Chunk;
      (* (*was*) 10 *)
      
      Base     =  Word.Shift(1, BaseLog2); 
      (* must be less than or equal to sqrt(LAST(CARDINAL)) *)
      (* must be power of 2 *)

      WordMask   =  Base - 1;
      WordUnmask =  Word.Not(WordMask);

(********************* sequence of cardinals *********************)

(* other sequence impl *)

TYPE 
  NArry = REF ARRAY OF Word.T;  (* hmm... *)

  NSeq = RECORD
    siz : CARDINAL; (* # of significant digits *)
    a   : NArry;
  END;

  (*
  METHODS
    init(hintSize : CARDINAL := 5) : NSeq := InitN;
    shift(sa : CARDINAL) := ShiftLeftInternalN;
    extend(toBits : CARDINAL) := ExtendN;
    clearTop() := ClearTop;
    copy() : NSeq := CopyN;
    size() : CARDINAL := SizeN;
  END;
  *)

PROCEDURE InitN(VAR s : NSeq; hintSize : CARDINAL) ;
PROCEDURE ShiftLeftInternalN(VAR s : NSeq; sa : CARDINAL) ;
PROCEDURE ShiftRightInternalN(VAR s : NSeq; sa : CARDINAL) ;
PROCEDURE ExtendN(VAR s : NSeq; toDigits : CARDINAL) ;
PROCEDURE CopyN(READONLY s : NSeq) : NSeq ;
PROCEDURE ClearTop(VAR s : NSeq) ; 

PROCEDURE StuffTheBits((*MODIFIES*)VAR wa : ARRAY OF Word.T) : BigInt.T ;

PROCEDURE GetTheBits(a : BigInt.T; VAR res : ARRAY OF Word.T) ;

END BigIntRep.
