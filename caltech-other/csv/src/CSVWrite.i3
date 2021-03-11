(* $Id$ *)

INTERFACE CSVWrite;
IMPORT Wr;
IMPORT FinDate;

TYPE Array1 = ARRAY OF LONGREAL;
     Array2 = ARRAY OF Array1;
     Array3 = ARRAY OF Array2;
     Array4 = ARRAY OF Array3;

CONST Empty2D = Array2 { };

PROCEDURE Write2D(         wr : Wr.T; 
                           rl : Labeler; (* row labeler *)
                  READONLY xx : Array2;
                           xl : Labeler;
                  READONLY yy : Array2 := Empty2D;
                           yl : Labeler := NIL;
                  READONLY zz : Array2 := Empty2D;
                           zl : Labeler := NIL;
                  READONLY tt : Array2 := Empty2D;
                           tl : Labeler := NIL;

                  transpose := TRUE
  ) 
  RAISES { Wr.Failure };

PROCEDURE Write1D(         wr : Wr.T; 
                           rl : Labeler; (* row labeler *)
                  READONLY xx : Array1) 
  RAISES { Wr.Failure };

PROCEDURE WriteCovariances(wr          : Wr.T;
                           READONLY cv : Array4;
                           cl : Labeler;
                           READONLY da : ARRAY OF FinDate.T;
                           s           : CARDINAL) 
  RAISES { Wr.Failure };

PROCEDURE WriteSCovariances(wr          : Wr.T;
                           READONLY cv : Array3;
                           cl : Labeler;
                           READONLY da : ARRAY OF FinDate.T)
  RAISES { Wr.Failure };

VAR writeTrailingChanges : BOOLEAN;

TYPE
  Labeler <: PublicLabeler; (* implements setTopLabel *)

  PublicLabeler = OBJECT METHODS
    label(i : CARDINAL) : TEXT;
    n() : CARDINAL;
    setTopLabel(to : TEXT);
  END;

(* the Arr labelers have to copy, but it avoids rep exposure *)

PROCEDURE TextArrLabeler(READONLY t : ARRAY OF TEXT) : Labeler;

PROCEDURE FinDateArrLabeler(READONLY t : ARRAY OF FinDate.T) : Labeler;

PROCEDURE CardinalLabeler(n : CARDINAL) : Labeler;

PROCEDURE EmptyLabeler(n : CARDINAL) : Labeler;

END CSVWrite.
