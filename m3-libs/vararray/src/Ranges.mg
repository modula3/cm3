
(* -----------------------------------------------------------------------1- *)
(* File Ranges.mg                                                            *)
(* Modula-3 source code.                                                     *)
(* Copyright 2013, Rodney M. Bates.                                          *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the Gnu Public License, version 2 or later.                *)
(* -----------------------------------------------------------------------2- *)

GENERIC MODULE Ranges ( ) 

(* The only reasons to instantiate this are to give each instantiation a unique
   name and to name the interface it exports.  Thus no generic parameters. 

   We use the following from the exported interface (an instantiation of
   Ranges.ig), as a rename from its generic actual interface Base:

   Name in instantiated Ranges.ig       Name in its generic actual interfaces

   BaseTyp                              Base . T 

*)

; IMPORT Word 

(* VISIBLE: *) 
; PROCEDURE RangeIsEmpty ( R : RangeTyp ) : BOOLEAN 
  (* Need I belabor this one?  But note that EmptyRange is not the only
     range that is empty. 
  *) 

  = BEGIN (* RangeIsEmpty *) 
      RETURN ORD ( R . Lo ) > ORD ( R . Hi ) 
    END RangeIsEmpty 

(* VISIBLE: *) 
; PROCEDURE CanonicalRange ( R : RangeTyp ) : RangeTyp 
  (* Equivalent to IF RangeIsEmpty ( R ) THEN EmptyRange ELSE R. *) 

  = BEGIN 
      IF ORD ( R . Lo ) > ORD ( R . Hi ) 
      THEN RETURN EmptyRange 
      ELSE RETURN R 
      END (* IF *) 
    END CanonicalRange 

(* VISIBLE: *) 
; PROCEDURE InRange ( Val : BaseTyp ; Range : RangeTyp ) : BOOLEAN 
  (* Val is a member of Range. *) 

  = BEGIN (* InRange *) 
      IF RangeIsEmpty ( Range ) 
      THEN RETURN FALSE 
      ELSE 
        RETURN Range . Lo <= Val AND Val <= Range . Hi  
      END (* IF *) 
    END InRange 

(* VISIBLE: *) 
; PROCEDURE IsSubrange ( Left , Right : RangeTyp ) : BOOLEAN 
  (* Left is a subrange of Right. *) 

  = BEGIN (* IsSubrange *) 
      IF RangeIsEmpty ( Left ) 
      THEN RETURN TRUE
      ELSIF RangeIsEmpty ( Right ) 
      THEN RETURN FALSE 
      ELSE (* Both ranges are nonempty. *) 
        RETURN ORD ( Right . Lo ) <= ORD ( Left . Lo ) 
               AND ORD ( Left . Hi ) <= ORD ( Right . Hi )  
      END (* IF *) 
    END IsSubrange 

(* VISIBLE: *) 
; PROCEDURE RangeIsMaxRepresentable ( R : RangeTyp ) : BOOLEAN 
  (* R is the maximum internally representable range, 
     i.e., ORD ( R . Lo ) = FIRST ( INTEGER ) 
           AND ORD ( R . Hi ) = LAST ( INTEGER ) )
  *) 

  = BEGIN (* RangeIsMaxRepresentable *) 
      RETURN ORD ( R . Lo ) = FIRST ( INTEGER ) 
             AND ORD ( R . Hi ) = LAST ( INTEGER )
    END RangeIsMaxRepresentable  

(* VISIBLE: *) 
; PROCEDURE NumberOfRange ( R : RangeTyp ) : Word . T 
  (* Number of values in R.  Treat as unsigned. 
     WARNING: If RangeIsMaxRepresentable(R), the correct result of 
       NumberOfRange is one greater than UnsignedLastOfWordT. In this 
       case, NumberOfRange returns UnsignedLastOfWordT, one too low. 
       You probably want to check this case separately, if 
       RangeIsMaxRepresentable(FullRange).
  *) 

  = BEGIN (* NumberOfRange *) 
      IF RangeIsEmpty ( R ) 
      THEN RETURN 0 
      ELSIF RangeIsMaxRepresentable ( R ) 
      THEN RETURN UnsignedLastOfWordT 
      ELSE 
        RETURN 
          Word . Plus ( Word . Minus ( ORD ( R . Hi ) , ORD ( R . Lo ) ) , 1 ) 
      END (* IF *) 
    END NumberOfRange 

(* VISIBLE: *) 
; PROCEDURE IntersectionRange ( R1 , R2 : RangeTyp ) : RangeTyp 
  (* The range of values that each belong to both R1 and R2. *)  
 
  = VAR LResult : RangeTyp 

  ; BEGIN (* IntersectionRange *) 
      IF RangeIsEmpty ( R1 ) OR RangeIsEmpty ( R2 )  
      THEN RETURN EmptyRange 
      ELSE (* Both ranges are nonempty. *)
        LResult 
          := RangeTyp 
               { Lo 
                  := VAL ( MAX ( ORD ( R1 . Lo ) , ORD ( R2 . Lo ) ) , BaseTyp )
               , Hi 
                  := VAL ( MIN ( ORD ( R1 . Hi ) , ORD ( R2 . Hi ) ) , BaseTyp )
               } 
      ; IF RangeIsEmpty ( LResult ) 
        THEN RETURN EmptyRange 
        ELSE RETURN LResult 
        END 
      END 
    END IntersectionRange  

(* VISIBLE: *) 
; PROCEDURE EnclosingRange ( R1 , R2 : RangeTyp ) : RangeTyp 
  (* Smallest range that contains all members of R1 and all members of R2. *) 
 
  = VAR LResult : RangeTyp 

  ; BEGIN (* EnclosingRange *) 
      IF RangeIsEmpty ( R1 ) 
      THEN RETURN R2 
      ELSIF RangeIsEmpty ( R2 ) 
      THEN RETURN R1 
      ELSE (* Both ranges are nonempty. *)
        LResult 
          := RangeTyp 
               { Lo := VAL 
                         ( MIN ( ORD ( R1 . Lo ) , ORD ( R2 . Lo ) ) , BaseTyp )
               , Hi := VAL 
                         ( MAX ( ORD ( R1 . Hi ) , ORD ( R2 . Hi ) ) , BaseTyp )
               } 
      ; RETURN LResult 
      END 
    END EnclosingRange  

(* VISIBLE: *) 
; PROCEDURE HiOfLoPlusNumber ( Lo : BaseTyp ; Number : Word . T ) : BaseTyp 
  (* Upper bound of a range with lower bound Lo and Number values.
     WARNING: This procedure treats Number as UNSIGNED.  
     NOTE: Will saturate at the limit of BaseTyp.  
     NOTE: The (unsigned) value range of Word.T is one too short for the 
           max representable range.  Clients must handle this case separately. 
  *) 

  = VAR LLoI : INTEGER 

  ;  BEGIN (* HiOfLoPlusNumber *) 
       LLoI := ORD ( Lo ) 
     ; IF Word . Minus ( ORD (  LAST ( BaseTyp ) ) , Number ) < LLoI 
       THEN (* Saturate. *) 
         RETURN LAST ( BaseTyp ) 
       ELSE 
         RETURN 
           VAL ( Word . Minus ( Word . Plus ( LLoI , Number ) , 1 ) , BaseTyp ) 
       END (* IF *) 
    END HiOfLoPlusNumber

(* VISIBLE: *) 
; PROCEDURE RangeOfLoPlusNumber 
    ( Lo : BaseTyp ; Number : Word . T ) : RangeTyp 
  (* Range with lower bound Lo and Number values. 
     WARNING: This procedure treats Number as UNSIGNED.  
     NOTE: Will saturate at the limit of BaseTyp.  
     NOTE: The (unsigned) value range of Word.T is one too short for the 
           max representable range.  Clients must handle this case separately. 
  *) 

  = VAR LResult : RangeTyp 

  ; BEGIN (* RangeOfLoPlusNumber *) 
      IF Number = 0 
      THEN LResult := EmptyRange 
      ELSE 
        LResult := RangeTyp { Lo , HiOfLoPlusNumber ( Lo , Number ) } 
      END (* IF *) 
    ; RETURN LResult 
    END RangeOfLoPlusNumber

(* VISIBLE: *) 
; PROCEDURE RangeIsAllocatable ( R : RangeTyp ) : BOOLEAN 
  (* NumberOfRange(R) does not exceed LAST(CARDINAL) 
     (Which would mean an open array of this size is impossible. *) 

  = BEGIN (* RangeIsAllocatable *) 
      IF RangeIsEmpty ( R ) 
      THEN RETURN TRUE 
      ELSIF RangeIsMaxRepresentable ( R )  
      THEN RETURN FALSE 
      ELSE 
        RETURN 
          Word . LT 
            ( Word . Minus ( ORD ( R . Hi ) , ORD ( R . Lo ) ) 
            , LAST ( CARDINAL ) 
            )
      END 
    END RangeIsAllocatable 

; BEGIN (* Ranges *) 
  END Ranges 
. 

