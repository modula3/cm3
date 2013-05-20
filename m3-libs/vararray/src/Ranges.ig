
(* -----------------------------------------------------------------------1- *)
(* File Ranges.ig                                                            *)
(* Modula-3 source code.                                                     *)
(* Copyright 2013, Rodney M. Bates.                                          *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the Gnu Public License, version 2 or later.                *)
(* -----------------------------------------------------------------------2- *)

GENERIC INTERFACE Ranges ( Base )

(* Some utilities for ranges of values over a base ordinal type. 
   These are designed to be overflow-free.  
   All procedures herein are pure functions, so thread safety is 
   not an issue. *) 

(* Instantiating interface Base must contain:
     TYPE T          An ordinal type, satisfying:
                       FIRST (INTEGER) <= ORD (FIRST (T)) 
                       AND ORD (LAST (T)) <= LAST (INTEGER)
                       AND NUMBER (T) >= 2 (So empty range is representable.)
                     The provided ranges will be over this type. 

*) 

; IMPORT Word   

; TYPE BaseTyp = Base . T 

; TYPE RangeTyp = RECORD Lo , Hi : BaseTyp END 

; CONST EmptyRange 
    = RangeTyp { Lo := LAST ( BaseTyp ) , Hi := FIRST ( BaseTyp ) }  
  (* A canonical empty range.  An empty range computed by any procedure 
     herein will be thus canonicalized. *) 

; CONST FullRange 
    = RangeTyp { Lo := FIRST ( BaseTyp ) , Hi := LAST ( BaseTyp ) }  
  (* The range containing all of BaseTyp. *) 

; PROCEDURE RangeIsEmpty ( R : RangeTyp ) : BOOLEAN 
  (* Need I belabor this one?  But note that EmptyRange is not the only
     range that is empty. 
  *) 

; PROCEDURE CanonicalRange ( R : RangeTyp ) : RangeTyp 
  (* Equivalent to IF RangeIsEmpty ( R ) THEN EmptyRange ELSE R. *) 

; PROCEDURE InRange ( Val : BaseTyp ; Range : RangeTyp ) : BOOLEAN 
  (* Val is a member of Range. *) 

; PROCEDURE IsSubrange ( Left , Right : RangeTyp ) : BOOLEAN 
  (* Left is a subrange of Right. *) 

; PROCEDURE RangeIsMaxRepresentable ( R : RangeTyp ) : BOOLEAN 
  (* R is the maximum internally representable range, 
     i.e., ORD ( R . Lo ) = FIRST ( INTEGER ) 
           AND ORD ( R . Hi ) = LAST ( INTEGER ) )
  *) 

; CONST UnsignedLastOfWordT = - 1  
  (* ^Largest unsigned value representable by Word.T. *) 

; PROCEDURE NumberOfRange ( R : RangeTyp ) : Word . T 
  (* Number of values in R.  Treat as unsigned. 
     WARNING: If RangeIsMaxRepresentable(R), the correct result of 
       NumberOfRange is one greater than UnsignedLastOfWordT. In this 
       case, NumberOfRange returns UnsignedLastOfWordT, one too low. 
       You probably want to check this case separately, if 
       RangeIsMaxRepresentable(FullRange).
  *) 

; PROCEDURE IntersectionRange ( R1 , R2 : RangeTyp ) : RangeTyp 
  (* The range of values that each belong to both R1 and R2. *)  
 
; PROCEDURE EnclosingRange ( R1 , R2 : RangeTyp ) : RangeTyp 
  (* Smallest range that contains all members of R1 and all members of R2. *) 
 
; PROCEDURE HiOfLoPlusNumber ( Lo : BaseTyp ; Number : Word . T ) : BaseTyp 
  (* Upper bound of a range with lower bound Lo and Number values.
     WARNING: This procedure treats Number as UNSIGNED.  
     NOTE: Will saturate at the limit of BaseTyp.  
     NOTE: The (unsigned) value range of Word.T is one too short for the 
           max representable range.  Clients must handle this case separately. 
  *) 

; PROCEDURE RangeOfLoPlusNumber 
    ( Lo : BaseTyp ; Number : Word . T ) : RangeTyp 
  (* Range with lower bound Lo and Number values. 
     WARNING: This procedure treats Number as UNSIGNED.  
     NOTE: Will saturate at the limit of BaseTyp.  
     NOTE: The (unsigned) value range of Word.T is one too short for the 
           max representable range.  Clients must handle this case separately. 
  *) 

; PROCEDURE RangeIsAllocatable ( R : RangeTyp ) : BOOLEAN 
  (* NumberOfRange(R) does not exceed LAST(CARDINAL) 
     (Otherwise, an open array of this size would be impossible. *) 

; END Ranges 
. 
