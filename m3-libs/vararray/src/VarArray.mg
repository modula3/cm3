
(* -----------------------------------------------------------------------1- *)
(* File VarArray.mg                                                          *)
(* Modula-3 source code.                                                     *)
(* Copyright 2013, Rodney M. Bates.                                          *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *) 
(* -----------------------------------------------------------------------2- *)


GENERIC MODULE VarArray ( Ranges ) 

(* Interfaces Subscript and Element, generic formals to the corresponding
   exported interface, are not needed as generic formals here.  Instead, we
   use the following from the exported interface (an instantiation of
   VarArray.ig), as renames of things from its generic actual interfaces
   Subscript and Element:

   Name in instantiated VarArray.ig   Name in its generic actual interfaces

   SsTyp                              Subscript . T 
   ElemTyp                            Element . T 

   Generic formal Ranges must provide a number of things that can be 
   conveniently provided by making it an instantiation of generic Ranges,
   with its Base formal equal to Subscript here. 

*)

(* Some identifier naming conventions: 
     Local variables start with 'L'

     There is a lot of mixed arithmetic on subscripts.  It is important to 
     distinguish values of Type SsTyp from their ORD equivalents.
     It is also important to keep track of signed and unsigned values.
     The latter are consistently kept in either CARDINAL or Word.T, and 
     use Word operations.  Word.T is used where possible, to support 
     full range unsigned values.  However, for open array NUMBERs and 
     subscripts, the upper part of the range can't happen, because Modula-3 
     uses CARDINAL for NUMBER, NEW, and open array subscripts.  Variables that
     are unsigned but whose use depends on their being in the CARDINAL range
     are declared CARDINAL.  

     There is also some REAL arithmetic involving subscript values, to handle
     allocation expansion.  

     So the naming conventions for all variations on subscript values are below.
     These are used only for identifiers that are not visible outside this 
     module. 

     SsTyp values have no suffix. 
     INTEGER variables end with 'I' 
     CARDINAL variables end with 'C' 
     Word.T variables end with 'W' 
     REAL values end with 'R'
*) 

; IMPORT Word 

; FROM Ranges 
  IMPORT RangeTyp , EmptyRange , FullRange , RangeIsEmpty , UnsignedLastOfWordT 

(* VarArray data structures. *) 

; CONST AbsMaxSizeIncrC : CARDINAL = 10000 
    (* Never expand by more than this.  
       Must be in CARDINAL to avoid overflows. *) 

; TYPE ElemsTyp = ARRAY OF ElemTyp 
; TYPE ElemsRefTyp = REF ElemsTyp 

; REVEAL T = Public BRANDED Brand OBJECT 
(* CONSIDER: Is is worth it to make INTEGER fields with value ranges
             corresponding to subscript ranges into subranges of INTEGER? 
             This would sometimes allow smaller objects here, if the subscripts
             were instantiated with smaller range ordinal types.
*) 
      Elems : ElemsRefTyp := NIL 
    ; BiasI : INTEGER := 0 
      (* Abstract array element with subscript VAL(BiasI,SsTyp) is stored in 
         Elems ^ [ 0 ] *)  
    ; BiasedLoTouchedW : CARDINAL := 0 
    ; BiasedHiTouchedW : CARDINAL := 0 
      (* INVARIANT: Word.LT ( BiasedHiTouchedW , NUMBER ( Elems ^ ) ). *) 
    ; InitElemValue : ElemTyp  
    END  

(* TODO: Surely, we can find a more widely accessible place to put 
         WordMin and WordMax. *) 
 
; PROCEDURE WordMin ( LeftW , RightW : Word . T ) : Word . T 
  (* Unsigned MIN on Word.T. *) 

  = BEGIN (* WordMin *) 
      IF Word . LE ( LeftW , RightW ) 
      THEN RETURN LeftW 
      ELSE RETURN RightW 
      END (* IF *) 
    END WordMin 

; PROCEDURE WordMax ( LeftW , RightW : Word . T ) : Word . T 
  (* Unsigned MAX on Word.T. *) 

  = BEGIN (* WordMax *) 
      IF Word . GE ( LeftW , RightW ) 
      THEN RETURN LeftW 
      ELSE RETURN RightW 
      END (* IF *) 
    END WordMax 

; CONST MinExpansionFactorR = 1.0 (* Never try to shrink when expanding. *) 

(* VISIBLE: *) 
; PROCEDURE New 
    ( InitElemValue : ElemTyp 
    ; InitialAlloc : RangeTyp := EmptyRange 
    ; ExpansionFactor : REAL := DefaultExpansionFactor 
    ) 
  : T
  RAISES { AllocationFailure } 
  (* Abstract view: A new array with subscripts in the full range SsTyp,
       all elements initialized to InitElemValue, and an empty touched range.
     Concrete view:  Create the new T with ExpansionFactor and with initial 
       allocation for exactly the range InitialAlloc.  If this is any empty
       range, postpone initial allocation, which will then happen later,
       the first time an element is touched. 
     Performance: O(1). 
  *) 

  = VAR LExpansionFactorR : REAL 
  ; VAR LLoI , LHiI : INTEGER 
  ; VAR LNumberW , LLastW : Word . T 
  ; VAR LResult : T 

  ; BEGIN (* New *) 
      LExpansionFactorR := MAX ( ExpansionFactor , MinExpansionFactorR ) 
    ; LResult 
        := NEW ( T 
               , ExpansionFactor := LExpansionFactorR 
               , InitElemValue := InitElemValue 
               ) 
    ; IF LResult = NIL 
      THEN RAISE AllocationFailure 
      ELSE 
        IF RangeIsEmpty ( InitialAlloc ) 
        THEN (* Allocate no space for now. *) 
          LResult . BiasI := 0 (* Defensive.  Meaningless for now. *) 
        ; LResult . Elems := NIL 
        ; LResult . BiasedLoTouchedW := LAST ( CARDINAL ) 
        ; LResult . BiasedHiTouchedW := 0 
        ELSE 
          LLoI := ORD ( InitialAlloc . Lo ) 
        ; LHiI := ORD ( InitialAlloc . Hi ) 
        ; LLastW := Word . Minus ( LHiI , LLoI )  
        ; IF Word . GE ( LLastW , LAST ( CARDINAL ) ) 
          THEN RAISE AllocationFailure 
          ELSE 
            LNumberW := Word . Plus ( LLastW , 1 ) 
          ; LResult . Elems := NEW ( ElemsRefTyp , LNumberW ) 
          ; IF LResult . Elems = NIL 
            THEN RAISE AllocationFailure 
            ELSE 
              LResult . BiasI := LLoI 
            ; LResult . BiasedLoTouchedW := LNumberW 
            ; LResult . BiasedHiTouchedW := 0 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      ; RETURN LResult 
      END (* IF *) 
    END New 

(* VISIBLE: *) 
; PROCEDURE TouchedRange ( Array : T ) : RangeTyp 
  (* PRE: Array # NIL *) 
  (* Abstract view: 
     If touched(Array) is empty, EmptyRange 
     Otherwise, touched(Array). 
     Performance: O(1). 
  *) 

  = BEGIN (* TouchedRange *) 
      IF Array . Elems = NIL 
         OR Word . GT ( Array . BiasedLoTouchedW , Array . BiasedHiTouchedW )
      THEN (* Return EmptyRange.  SsTyp is big enough for this. *)  
        RETURN EmptyRange 
      ELSE 
        RETURN RangeTyp 
          { VAL ( Word . Plus ( Array . BiasedLoTouchedW , Array . BiasI ) 
                , SsTyp 
                ) 
          , VAL ( Word . Plus ( Array . BiasedHiTouchedW , Array . BiasI ) 
                , SsTyp 
                ) 
          }
      END (* IF *) 
    END TouchedRange 

(* VISIBLE: *)
; PROCEDURE Touch ( Array : T ; Range : RangeTyp ) 
  RAISES { AllocationFailure } 
  (* PRE: Array # NIL *) 
  (* Abstract view: Expand touched(Array) to include Range. 
     Concrete view: Expand allocation if needed to cover Range. 
     Performance: If expansion is necessary, O(NUMBER((new)touched(Array))). 
       Otherwise O(1). 
  *)     

  = BEGIN (* Touch *)  
      AllocateAndTouchRange 
        ( Array , ORD ( Range . Lo ) , ORD ( Range . Hi ) , DoInit := TRUE ) 
    END Touch 

(* VISIBLE: *) 
; PROCEDURE InitElemValue ( Array : T ) : ElemTyp 
  (* PRE: Array # NIL *) 
  (* Abstract view: The value that was passed to the call on New that 
       created Array. 
     Performance: O(1). 
  *)

  = BEGIN (* InitElemValue *) 
      RETURN Array . InitElemValue
    END InitElemValue 

; PROCEDURE Expansion ( CurrentSizeC : CARDINAL ; ExpansionFactorR : REAL ) 
  : CARDINAL 
  (* The *additional* size to be *added*, by virtue of expansion rules alone. 
     POST: CurrentSizeC + Result <= MaxArrayNumberC. *) 

  = VAR LIncrFactorR : REAL 
  ; VAR LCurrentSizeR : REAL 
  ; VAR LSizeIncrR : REAL 
  ; VAR LSizeIncrC : Word . T 
  ; VAR LResult : Word . T 

  ; BEGIN (* Expansion *) 
      IF ExpansionFactorR <= 1.0 
      THEN RETURN 0 
      ELSE 
        IF CurrentSizeC >= MaxSizeIncrC 
        THEN LSizeIncrC := MaxSizeIncrC
        ELSE 
          LIncrFactorR := ExpansionFactorR - 1.0 
        ; LCurrentSizeR := FLOAT ( CurrentSizeC ) 
        ; LSizeIncrR := LCurrentSizeR * LIncrFactorR 
        ; IF LSizeIncrR >= MaxSizeIncrR 
          THEN LSizeIncrC := MaxSizeIncrC
          ELSE LSizeIncrC := TRUNC ( LSizeIncrR )  
          END (* IF *)   
        END (* IF *)   
      ; LResult 
          := WordMin 
               ( LSizeIncrC , Word . Minus ( MaxArrayNumberC , CurrentSizeC ) )
      ; RETURN LResult 
      END (* IF *)   
    END Expansion  

; PROCEDURE AllocateAndTouchRange 
    ( Array : T ; LoI , HiI : INTEGER ; DoInit : BOOLEAN ) 
  RAISES { AllocationFailure } 
  (* PRE: Array # NIL *) 
  (* 1) Expand allocated element space of Array as necessary to contain elements
        in [LoI..HiI], preserving the contents of old touched(Array). 
     2) Expand the touched range to include [LoI..HiI]. 
     3) If DoInit, initialize any elements of Array in the new touched range 
        but not in the old touched range, to InitElemValue. 
        Otherwise, initialize any elements of Array in the new touched range 
        but neither in the old touched range nor [LoI..HiI], to initElemValue. 
  *) 

  = VAR LMinGrowthMinus1W : Word . T 
  ; VAR LMinGrowthLoW , LMinGrowthHiW , LMinGrowthW : Word . T 
  ; VAR LMaxGrowthHiW : Word . T 
  ; VAR LMaxGrowthW : Word . T 
  ; VAR LGrowthLoW , LGrowthHiW , LGrowthW : Word . T 
  ; VAR LArrayNumberC : CARDINAL 
  ; VAR LArrayLastC : CARDINAL 
  ; VAR LAllocHiI : INTEGER 
  ; VAR LOldTouchedNumberC : CARDINAL 
  ; VAR LOldBiasedLoTouchedW , LOldBiasedHiTouchedW : Word . T 
  ; VAR LOldRebiasedLoTouchedW , LOldRebiasedHiTouchedW : Word . T 
  ; VAR LBiasedLoW , LBiasedHiW : Word . T 
  ; VAR LBiasedLoNoInitW , LBiasedHiNoInitW : Word . T 
  ; VAR LNewElems : ElemsRefTyp 

  ; BEGIN (* AllocateAndTouchRange *) 
      IF  LoI > HiI 
      THEN (* Empty range to be touched.  Do nothing. *) 
      ELSE 
        IF Array . Elems = NIL 
        THEN LArrayNumberC := 0 
        ELSE LArrayNumberC := NUMBER ( Array . Elems ^ ) 
        END (* IF *) 
      ; IF LArrayNumberC = 0 
        THEN (* No array is allocated, or an empty array is allocated. *) 

        (* Allocate a new array. *) 

          LMinGrowthMinus1W := Word . Minus ( HiI , LoI )  
        ; IF LMinGrowthMinus1W 
             = Word . Minus ( LAST ( INTEGER ) , FIRST ( INTEGER ) ) 
          THEN (* Too big for open array and would overflow below. *) 
            RAISE AllocationFailure 
          END (* IF *) 
        ; LMinGrowthW := Word . Plus ( LMinGrowthMinus1W , 1 ) 
        ; IF Word . GT ( LMinGrowthW , LAST ( CARDINAL ) )  
          THEN (* Still too big for an open array. *) 
            RAISE AllocationFailure 
          END (* IF *) 
        ; LGrowthW 
            := WordMax 
                 ( LMinGrowthW 
                 , WordMin ( DefaultInitialSize , MaxArrayNumberC ) 
                 ) 
        ; Array . Elems := NEW ( ElemsRefTyp , LGrowthW ) 
        ; IF Array . Elems = NIL THEN RAISE AllocationFailure END 
        ; Array . BiasI 
            := MIN ( LoI 
                   , Word . Minus 
                       ( ORD ( LAST ( SsTyp ) ) 
                       , Word . Minus ( LGrowthW , 1 ) 
                       )
                   ) 
          (* ^Assume future growth will be upwards, except when allocated Elems
             is up against the top of the unbiased subscript range. *) 
        (* No previously allocated array => previously empty touched range. *) 
        ; Array . BiasedLoTouchedW := Word . Minus ( LoI , Array . BiasI )  
        ; Array . BiasedHiTouchedW := Word . Minus ( HiI , Array . BiasI )  
        ; IF DoInit 
          THEN 
            FOR RI := Array . BiasedLoTouchedW TO Array . BiasedHiTouchedW  
            DO Array . Elems ^ [ RI ] := Array . InitElemValue 
            END (* FOR *) 
          END (* IF *) 

        ELSE (* An array is allocated, and it is nonempty. *) 
          LArrayLastC := Word . Minus ( LArrayNumberC , 1 ) 
        ; LAllocHiI := Array . BiasI + LArrayLastC 
        ; LOldBiasedLoTouchedW := Array . BiasedLoTouchedW 
        ; LOldBiasedHiTouchedW := Array . BiasedHiTouchedW 
        ; IF Word . GT ( LOldBiasedLoTouchedW , LOldBiasedHiTouchedW )  
          THEN LOldTouchedNumberC := 0 
          ELSE (* Cannot exceed LAST(CARDINAL). *) 
            LOldTouchedNumberC 
              := Word . Minus 
                   ( LOldBiasedHiTouchedW  , LOldBiasedLoTouchedW )
                 + 1 
          END (* IF *) 
        ; IF  LoI < Array . BiasI 
          THEN (* Must expand downward. *) 
            IF HiI > LAllocHiI  
            THEN (* Must expand upward. *) 

            (* Expand array in both directions, down to Lo and up to Hi. *) 

              LMinGrowthLoW (* Enough to hold Lo. *)  
                := Word . Minus ( Array . BiasI ,  LoI )   
            ; LMinGrowthHiW (* Enough to hold Hi. *) 
                := Word . Minus ( HiI , LAllocHiI )  
            ; LMinGrowthW := Word . Plus ( LMinGrowthLoW , LMinGrowthHiW ) 
              (* Word . LE ( LMinGrowthW , LAST ( Word . T ), since Elems ^ 
                 is nonempty. *)
            ; IF Word . GT ( LMinGrowthW , LAST ( CARDINAL ) )  
              THEN (* Even minimum *growth* is too much for an open array. *) 
                RAISE AllocationFailure 
              ELSIF Word . GT  
                   ( Word . Plus ( LArrayNumberC , LMinGrowthW ) 
                   , MaxArrayNumberC 
                   )  
              THEN (* Minimum final size is too big. *) 
                RAISE AllocationFailure 
              END (* IF *) 
              (* LMinGrowthW + NUMBER ( Array . Elems ^ ) <= MaxArrayNumberC. *)
            ; LGrowthW 
                := WordMax 
                     ( LMinGrowthW 
                     , Expansion ( LArrayNumberC , Array . ExpansionFactor ) 
                     ) 
              (* LGrowthW + NUMBER ( Array . Elems ^ ) <= MaxArrayNumberC. *) 
            ; LMaxGrowthHiW 
                := Word . Minus 
                     ( Word . Minus ( ORD ( LAST ( SsTyp ) ) , Array . BiasI ) 
                     , LArrayLastC 
                     ) 
              (* ^Keep within the abstract subscript space at the hi end. *)
            ; LGrowthHiW 
                := WordMin 
                     ( Word . Minus ( LGrowthW , LMinGrowthLoW ) 
                     , LMaxGrowthHiW 
                     ) 
              (* ^Bias growth to the upward direction as much as possible.  
                 Lo end of abstract subscript space will take care of itself. *)
            ; LGrowthLoW := Word . Minus ( LGrowthW , LGrowthHiW ) 
            ; LNewElems 
                := NEW ( ElemsRefTyp 
                       , Word . Plus ( LArrayNumberC , LGrowthW )
                       ) 
            ; IF LNewElems = NIL THEN RAISE AllocationFailure END (* IF *) 
            ; IF LOldTouchedNumberC = 0 
              THEN 
                LOldRebiasedLoTouchedW := LOldBiasedLoTouchedW 
              ; LOldRebiasedHiTouchedW := LOldBiasedHiTouchedW 
              ELSE 
                LOldRebiasedLoTouchedW 
                  := Word . Plus ( LOldBiasedLoTouchedW , LGrowthLoW ) 
              ; LOldRebiasedHiTouchedW 
                  := Word . Plus ( LOldBiasedHiTouchedW , LGrowthLoW )
              (* ^Old touched range, but rebiased to LNewElems. *)  
              ; SUBARRAY 
                  ( LNewElems ^ , LOldRebiasedLoTouchedW , LOldTouchedNumberC ) 
                  := SUBARRAY 
                       ( Array . Elems ^ 
                       , LOldBiasedLoTouchedW 
                       , LOldTouchedNumberC 
                       )
              END (* IF *) 
            ; Array . Elems := LNewElems 
            ; Array . BiasI := Array . BiasI - LGrowthLoW  
            (* ^Update bias. *) 
            ELSE 

            (* Expand array downward only, to Lo. *) 

              LMinGrowthW (* Enough to hold Lo. *)  
                := Word . Minus ( Array . BiasI ,  LoI )   
            ; IF Word . GT ( LMinGrowthW , LAST ( CARDINAL ) )  
              THEN (* Even minimum *growth* is too much for an open array. *) 
                RAISE AllocationFailure 
              ELSIF Word . GT  
                   ( Word . Plus ( LArrayNumberC , LMinGrowthW ) 
                   , MaxArrayNumberC 
                   )  
              THEN (* Minimum final size is too big. *) 
                RAISE AllocationFailure 
              END (* IF *) 
              (* LMinGrowthW + NUMBER ( Array . Elems ^ ) <= MaxArrayNumberC. *)
            ; LGrowthW 
                := WordMax 
                     ( LMinGrowthW 
                     , Expansion ( LArrayNumberC , Array . ExpansionFactor ) 
                     ) 
              (* LGrowthW + NUMBER ( Array . Elems ^ ) <= MaxArrayNumberC. *) 
            ; LMaxGrowthW (* To stay within the abstract subscript space. *) 
                := Word . Minus ( Array . BiasI , ORD ( FIRST ( SsTyp ) ) )
            ; LGrowthW := WordMin ( LGrowthW , LMaxGrowthW ) 
            ; LNewElems 
                := NEW ( ElemsRefTyp 
                       , Word . Plus ( LArrayNumberC , LGrowthW ) 
                       )
            ; IF LNewElems = NIL THEN RAISE AllocationFailure END (* IF *) 
            ; IF LOldTouchedNumberC = 0 
              THEN 
                LOldRebiasedLoTouchedW := LOldBiasedLoTouchedW 
              ; LOldRebiasedHiTouchedW := LOldBiasedHiTouchedW 
              ELSE 
                LOldRebiasedLoTouchedW 
                  := Word . Plus ( LOldBiasedLoTouchedW , LGrowthW ) 
              ; LOldRebiasedHiTouchedW 
                  := Word . Plus ( LOldBiasedHiTouchedW , LGrowthW )
              (* ^Old touched range, but rebiased to new Elems array. *)  
              ; SUBARRAY 
                  ( LNewElems ^ , LOldRebiasedLoTouchedW , LOldTouchedNumberC ) 
                  := SUBARRAY 
                       ( Array . Elems ^ 
                       , LOldBiasedLoTouchedW 
                       , LOldTouchedNumberC 
                       )
              END (* IF *) 
            ; Array . Elems := LNewElems 
            ; Array . BiasI := Array . BiasI - LGrowthW  
            (* ^Update bias. *) 
            END (* IF *)  

          ELSIF HiI > LAllocHiI 
          THEN (* Need not expand downward. Must expand upward. *) 

          (* Expand array upward only, to Hi. *) 

            LMinGrowthW (* Enough to hold Hi. *) 
              := Word . Minus ( HiI , LAllocHiI ) 
          ; IF Word . GT ( LMinGrowthW , LAST ( CARDINAL ) )  
            THEN (* Even minimum *growth* is too much for an open array. *) 
              RAISE AllocationFailure 
            ELSIF Word . GT 
                 ( Word . Plus ( LArrayNumberC , LMinGrowthW  ) 
                 , MaxArrayNumberC 
                 )  
            THEN (* Minimum final size is too big. *) 
              RAISE AllocationFailure 
            END (* IF *) 
          (* LMinGrowthW + NUMBER ( Array . Elems ^ ) <= MaxArrayNumberC. *) 
          ; LGrowthW 
              := WordMax 
                   ( LMinGrowthW 
                   , Expansion ( LArrayNumberC , Array . ExpansionFactor ) 
                   ) 
          (* LGrowthW + NUMBER ( Array . Elems ^ ) <= MaxArrayNumberC. *) 
          ; LMaxGrowthW 
              := Word . Minus 
                   ( Word . Minus ( ORD ( LAST ( SsTyp ) ) , Array . BiasI ) 
                   , LArrayLastC 
                   ) 
              (* ^Keep within the abstract subscript space at the hi end. *)
          ; LGrowthW := WordMin ( LGrowthW , LMaxGrowthW ) 
          ; LNewElems 
              := NEW ( ElemsRefTyp 
                     , Word . Plus ( LArrayNumberC , LGrowthW )
                     ) 
          ; IF LNewElems = NIL THEN RAISE AllocationFailure END (* IF *) 
          ; LOldRebiasedLoTouchedW := LOldBiasedLoTouchedW 
          ; LOldRebiasedHiTouchedW := LOldBiasedHiTouchedW 
          (* ^Old touched range, also relative to LNewElems. *)  
          ; IF LOldTouchedNumberC # 0 
            THEN 
              SUBARRAY 
                ( LNewElems ^ , LOldRebiasedLoTouchedW , LOldTouchedNumberC ) 
                := SUBARRAY 
                     ( Array . Elems ^ 
                     , LOldBiasedLoTouchedW 
                     , LOldTouchedNumberC 
                     )
            END (* IF *) 
          ; Array . Elems := LNewElems
          (* No change to bias. *)  

          ELSE (* Range lies within the allocated array; no expansion needed. *)
            LOldRebiasedLoTouchedW := LOldBiasedLoTouchedW 
          ; LOldRebiasedHiTouchedW := LOldBiasedHiTouchedW 
          END (* IF *) 

        (* Compute new touched values and do any needed initialization. *) 
        ; LBiasedLoW := Word . Minus ( LoI , Array . BiasI (* New value. *) ) 
        ; LBiasedHiW := Word . Minus ( HiI , Array . BiasI (* New value. *) ) 
        ; IF LOldTouchedNumberC = 0 
          THEN (* Old touched(Array) was empty. *)  
            Array . BiasedLoTouchedW := LBiasedLoW
          ; Array . BiasedHiTouchedW := LBiasedHiW
          ; IF DoInit 
            THEN 
              FOR RI := LBiasedLoW TO LBiasedHiW 
              DO Array . Elems ^ [ RI ] := Array . InitElemValue 
              END (* FOR *) 
            END (* IF *) 
          ELSE (* Old touched range was nonempty. *)  
            Array . BiasedLoTouchedW 
              := WordMin ( LBiasedLoW , LOldRebiasedLoTouchedW ) 
          ; Array . BiasedHiTouchedW 
              := WordMax ( LBiasedHiW , LOldRebiasedHiTouchedW ) 

          (* Quite a few cases for initializing.  Here, the total new touched 
             range (Array.Biased[LoHi]TouchedW) is nonempty, and both old 
             LOldRebiased[Lo|Hi]TouchedW) and newly touched (LBiased[Lo|Hi]W)
             ranges are subranges of total touched range. *) 

          ; IF DoInit
            THEN (* Initialize total touched minus old touched range. *) 
              IF Word . GT ( LOldRebiasedLoTouchedW , LOldRebiasedHiTouchedW )  
              THEN (* Initialize total touched range. *) 
                FOR RI := Array . BiasedLoTouchedW 
                       TO Array . BiasedHiTouchedW 
                DO Array . Elems ^ [ RI ] := Array . InitElemValue 
                END (* FOR *) 
              ELSE (* Omit only old touched range. *) 
                IF Word . GT ( LOldRebiasedLoTouchedW , 0 ) 
                THEN 
                  FOR RI := Array . BiasedLoTouchedW 
                         TO Word . Minus ( LOldRebiasedLoTouchedW , 1 )     
                  DO Array . Elems ^ [ RI ] := Array . InitElemValue 
                  END (* FOR *) 
                END (* IF *) 
              ; IF Word . LT ( LOldRebiasedHiTouchedW , UnsignedLastOfWordT ) 
                THEN 
                  FOR RI := Word . Plus ( LOldRebiasedHiTouchedW , 1 )  
                         TO Array . BiasedHiTouchedW      
                  DO Array . Elems ^ [ RI ] := Array . InitElemValue 
                  END (* FOR *) 
                END (* IF *) 
              END (* IF *) 
            ELSE (* Initialize total touched minus both old touched and 
                    newly touched ranges. *) 
              IF Word . GT ( LOldRebiasedLoTouchedW , LOldRebiasedHiTouchedW )  
              THEN (* Only newly touched range needs to be omitted. *) 
                IF Word . GT ( LBiasedLoW , 0 ) 
                THEN 
                  FOR RI := Array . BiasedLoTouchedW 
                         TO Word . Minus ( LBiasedLoW , 1 )     
                  DO Array . Elems ^ [ RI ] := Array . InitElemValue 
                  END (* FOR *) 
                END (* IF *) 
              ; IF Word . LT ( LBiasedHiW , UnsignedLastOfWordT ) 
                THEN 
                  FOR RI := Word . Plus ( LBiasedHiW , 1 )  
                         TO Array . BiasedHiTouchedW      
                  DO Array . Elems ^ [ RI ] := Array . InitElemValue 
                  END (* FOR *) 
                END (* IF *) 
              ELSIF Word . LT ( LOldRebiasedHiTouchedW , LBiasedLoW ) 
              THEN (* Old range is lower and doesn't meet newly touched. *) 
                IF Word . GT ( LOldRebiasedLoTouchedW , 0 ) 
                THEN 
                  FOR RI := Array . BiasedLoTouchedW 
                         TO Word . Minus ( LOldRebiasedLoTouchedW , 1 )     
                  DO Array . Elems ^ [ RI ] := Array . InitElemValue 
                  END (* FOR *) 
                END (* IF *) 
              ; FOR RI := Word . Plus ( LOldRebiasedHiTouchedW , 1 ) 
                       TO Word . Minus ( LBiasedLoW , 1 ) 
                DO Array . Elems ^ [ RI ] := Array . InitElemValue 
                END (* FOR *) 
              ; IF Word . LT ( LBiasedHiW , UnsignedLastOfWordT ) 
                THEN 
                  FOR RI := Word . Plus ( LBiasedHiW , 1 )  
                         TO Array . BiasedHiTouchedW      
                  DO Array . Elems ^ [ RI ] := Array . InitElemValue 
                  END (* FOR *) 
                END (* IF *) 
              ELSIF Word . LT ( LBiasedHiW , LOldRebiasedLoTouchedW ) 
              THEN (* Newly touched is lower and doesn't meet old touched. *) 
                IF Word . GT ( LBiasedLoW , 0 ) 
                THEN 
                  FOR RI := Array . BiasedLoTouchedW 
                         TO Word . Minus ( LBiasedLoW , 1 )     
                  DO Array . Elems ^ [ RI ] := Array . InitElemValue 
                  END (* FOR *) 
                END (* IF *) 
              ; FOR RI := Word . Plus ( LBiasedHiW , 1 ) 
                       TO Word . Minus ( LOldRebiasedLoTouchedW , 1 ) 
                DO Array . Elems ^ [ RI ] := Array . InitElemValue 
                END (* FOR *) 
              ; IF Word . LT ( LOldRebiasedHiTouchedW , UnsignedLastOfWordT ) 
                THEN 
                  FOR RI := Word . Plus ( LOldRebiasedHiTouchedW , 1 )  
                         TO Array . BiasedHiTouchedW      
                  DO Array . Elems ^ [ RI ] := Array . InitElemValue 
                  END (* FOR *) 
                END (* IF *) 
              ELSE (* Old and newly touched ranges meet or overlap. *) 
                LBiasedLoNoInitW 
                  := WordMin ( LOldRebiasedLoTouchedW , LBiasedLoW ) 
              ; LBiasedHiNoInitW 
                  := WordMax ( LOldRebiasedHiTouchedW , LBiasedHiW ) 
              ; IF Word . GT ( LBiasedLoNoInitW , 0 ) 
                THEN 
                  FOR RI := Array . BiasedLoTouchedW 
                         TO Word . Minus ( LBiasedLoNoInitW , 1 )     
                  DO Array . Elems ^ [ RI ] := Array . InitElemValue 
                  END (* FOR *) 
                END (* IF *) 
              ; IF Word . LT ( LBiasedHiNoInitW , UnsignedLastOfWordT ) 
                THEN 
                  FOR RI := Word . Plus ( LBiasedHiNoInitW , 1 )
                         TO Array . BiasedHiTouchedW      
                  DO Array . Elems ^ [ RI ] := Array . InitElemValue 
                  END (* FOR *) 
                END (* IF *) 
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        END (* IF No allocated array. *)  
      END (* IF Empty range. *)  
    END AllocateAndTouchRange  

(* VISIBLE: *) 
; PROCEDURE Assign ( Array : T ; Ss : SsTyp ; Value : ElemTyp ) 
  RAISES { AllocationFailure } 
  (* PRE: Array # NIL *) 
  (* Abstract view: 
       1) Expand touched(Array) to include Ss. 
       2) Array[Ss] := Value. 
     Concrete view: Expand allocation if needed to cover Ss. 
     Performance: If expansion is necessary, O(NUMBER((new)touched(Array))). 
       Otherwise O(1). 
  *)     

  = VAR LSsI : INTEGER := ORD ( Ss )      
  ; VAR LBiasedSsW : Word . T 

  ; BEGIN (* Assign *) 
      AllocateAndTouchRange ( Array , LSsI , LSsI , DoInit := FALSE ) 
    ; LBiasedSsW := Word . Minus ( LSsI , Array . BiasI )   
    ; Array . Elems ^ [ LBiasedSsW ] := Value   
    END Assign  

(* VISIBLE: *) 
; PROCEDURE Fetch ( Array : T ; Ss : SsTyp ) : ElemTyp  
  (* PRE: Array # NIL *) 
  (* Abstract view: Return Array[Ss]. 
      Ss need not be in touched(A).
      Do not touch Array[Ss].
        (Untouched elements implicitly have InitElemValue(Array)) 
     Concrete view: (Make no change of allocation. )
     Performance: O(1). 
  *)  

  = VAR LSsI : INTEGER 
  ; VAR LBiasedSsW : Word . T 

  ; BEGIN (* Fetch *) 
      IF Array . Elems = NIL 
      THEN 
        RETURN Array . InitElemValue 
      ELSE 
        LSsI := ORD ( Ss )
      ; IF LSsI < Array . BiasI 
        THEN RETURN Array . InitElemValue 
        ELSE  
          LBiasedSsW := Word . Minus ( LSsI , Array . BiasI )  
        ; IF Word . LT ( LBiasedSsW , Array . BiasedLoTouchedW ) 
             OR Word . GT ( LBiasedSsW , Array . BiasedHiTouchedW ) 
          THEN RETURN Array . InitElemValue 
          ELSE RETURN Array . Elems ^ [ LBiasedSsW ]  
          END (* IF *)  
        END (* IF *)  
      END (* IF *)  
    END Fetch 
 
(* VISIBLE: *) 
; PROCEDURE AssignSubarray 
    ( Array : T ; Lo : SsTyp ; READONLY Elems : ARRAY OF ElemTyp ) 
  (* PRE: Array # NIL *) 
  RAISES { AllocationFailure } 
  (* Let N=MIN(NUMBER(Elems),ORD(LAST(SsTyp))-ORD(Lo)+1). 
     Abstract view: 
       1) Expand touched(Array) to cover SUBARRAY(Array,Lo,N).
          N=0, which can happen when Elems is empty, does not touch Lo. 
       2) SUBARRAY(Array,Lo,N) := SUBARRAY(Elems,0,N).
     Concrete view: Expand allocation if needed to cover the assigned elements. 
     Performance: If expansion is necessary, O(NUMBER((new)touched(Array))). 
       Otherwise O(N) 
  *) 

  = VAR LElemsNumberC : CARDINAL 
  ; VAR LMaxNumberMinus1W : Word . T 
  ; VAR LNumberMinus1C : CARDINAL 
  ; VAR LAssignNumberC : CARDINAL 
  ; VAR LLoI , LHiI : INTEGER 
  ; VAR LBiasedLoW : Word . T 

  ; BEGIN (* AssignSubarray *)
      LElemsNumberC := NUMBER ( Elems ) 
    ; IF LElemsNumberC > 0 
      THEN (* Nonempty Elems. *) 
        LLoI := ORD ( Lo ) 
      ; LMaxNumberMinus1W := Word . Minus ( ORD ( LAST ( SsTyp ) ) , LLoI )  
      ; LNumberMinus1C := WordMin ( LElemsNumberC - 1 , LMaxNumberMinus1W ) 
      ; LAssignNumberC := LNumberMinus1C + 1 
      ; LHiI := LLoI + LNumberMinus1C  
      ; AllocateAndTouchRange ( Array , LLoI , LHiI , DoInit := FALSE ) 
      ; LBiasedLoW := Word . Minus ( LLoI , Array . BiasI )   
      ; SUBARRAY ( Array . Elems ^ , LBiasedLoW , LAssignNumberC ) 
          := SUBARRAY ( Elems , 0 , LAssignNumberC ) 
      END (* IF *) 
    END AssignSubarray 

(* VISIBLE: *) 
; PROCEDURE FetchSubarray 
    ( Array : T ; Lo : SsTyp ; VAR Elems : ARRAY OF ElemTyp )
  (* PRE: Array # NIL *) 
  (* Let N=MIN(NUMBER(Elems),ORD(LAST(SsTyp))-ORD(Lo)+1). 
     Abstract view: 
       SUBARRAY(Elems,0,N) := SUBARRAY(Array,Lo,N) 
       Do not touch any additional array elements.  
       The SUBARRAY need not be in touched(Array). 
         (Untouched elements are implicitly InitElemValue(Array)) 
     Concrete view: (Do no allocation.)  
     Performance: O(N) 
  *) 

  = VAR LElemsNumberC : CARDINAL 
  ; VAR LMaxNumberMinus1W : Word . T 
  ; VAR LResultLastC : CARDINAL 
  ; VAR LLoI , LHiI : INTEGER 
  ; VAR LBiasedLoW : Word . T 
  ; VAR LLoTouchedI , LHiTouchedI : INTEGER  
  ; VAR LResultLoTouchedW : Word . T   
  ; VAR LResultHiTouchedW : Word . T   
  ; VAR LResultCtC : CARDINAL 

  ; BEGIN (* FetchSubarray *) 
      LElemsNumberC := NUMBER ( Elems ) 
    ; IF LElemsNumberC > 0 
      THEN (* Nonempty Elems, do something. *) 
        LLoI := ORD ( Lo ) 
      ; LMaxNumberMinus1W := Word . Minus ( ORD ( LAST ( SsTyp ) ) , LLoI )  
      ; LResultLastC := WordMin ( LElemsNumberC - 1 , LMaxNumberMinus1W ) 
      ; LHiI := Word . Plus ( LLoI , LResultLastC ) 
      ; IF Array . Elems = NIL 
           OR Word . GT ( Array . BiasedLoTouchedW , Array . BiasedHiTouchedW )
        THEN (* Nothing is previously touched. *) 
          FOR RI := 0 TO LResultLastC 
          DO Elems [ RI ] := Array . InitElemValue 
          END (* FOR *) 
        ELSE (* Previously touched elements exist. *)   
          LLoTouchedI 
            := Word . Plus ( Array . BiasedLoTouchedW , Array . BiasI ) 
        ; LHiTouchedI 
            := Word . Plus ( Array . BiasedHiTouchedW , Array . BiasI ) 
        ; IF LLoTouchedI <= LLoI 
          THEN (* Nothing untouched on low end of result. *) 
            LResultLoTouchedW := 0 
          ; LBiasedLoW := Word . Minus ( LLoI , Array . BiasI )   
          ELSE
            LResultLoTouchedW := Word . Minus ( LLoTouchedI , LLoI ) 
          ; IF Word . GT ( LResultLoTouchedW , LResultLastC ) 
            THEN (* Touched region lies entirely above result. *) 
              FOR RI := 0 TO LResultLastC   
              DO Elems [ RI ] := Array . InitElemValue 
              END (* FOR *) 
            ; RETURN 
            ELSE (* Low part of result is untouched. *) 
              IF Word . GT ( LResultLoTouchedW , 0 )
              THEN (* Can this fail? *) 
                FOR RI := 0 TO Word . Minus ( LResultLoTouchedW , 1 )  
                DO Elems [ RI ] := Array . InitElemValue 
                END (* FOR *) 
              END (* IF *) 
            ; LBiasedLoW := Array . BiasedLoTouchedW 
            END (* IF *) 
          END (* IF *) 

        (* We are ready to copy in the touched part, and it is nonempty. *) 
        ; LResultHiTouchedW := Word . Minus ( LHiTouchedI , LLoI ) 
        ; IF Word . GE ( LResultHiTouchedW , LResultLastC ) 
          THEN (* Entire remainder of result is touched. *) 
            LResultCtC 
              := Word . Plus 
                   ( Word . Minus ( LResultLastC , LResultLoTouchedW ) 
                   , 1 
                   ) 
          ; SUBARRAY ( Elems , LResultLoTouchedW , LResultCtC ) 
              := SUBARRAY ( Array . Elems ^ , LBiasedLoW , LResultCtC ) 
          ELSE (* Some touched result, then some untouched above it. *) 
            LResultCtC 
              := Word . Plus 
                   ( Word . Minus ( LResultHiTouchedW , LResultLoTouchedW ) 
                   , 1 
                   )  
          ; SUBARRAY ( Elems , LResultLoTouchedW , LResultCtC ) 
              := SUBARRAY ( Array . Elems ^ , LBiasedLoW , LResultCtC ) 
          ; IF Word . LT ( LResultHiTouchedW , UnsignedLastOfWordT )
            THEN 
              FOR RI := Word . Plus ( LResultHiTouchedW , 1 ) TO LResultLastC 
              DO Elems [ RI ] := Array . InitElemValue 
              END (* FOR *) 
            END (* IF *) 
          END (* IF *) 
        END (* IF *)       
      END (* IF *)       
    END FetchSubarray 

(* VISIBLE: *) 
; PROCEDURE CallbackWithElem 
    ( Array : T ; Ss : SsTyp ; Callback : ProcOfSsTypVARElemTyp ) 
  RAISES ANY (* AllocationFailure from CallbackWithElem, ANY from Callback. *) 
  (* PRE: Array # NIL *) 
  (* Abstract view: 
       1) Expand touched(Array) to include Ss.  
       2) Call back Callback, passing Ss and Array[Ss].  
     Concrete view: Expand allocation if needed to cover Ss. 
     Performance: If expansion is necessary, O(NUMBER((new)touched(Array))). 
       Otherwise O(1). 
       If ElemTyp is large, this likely has better constant-time efficiency 
       than calls on Assign/Fetch, by virtue of a single pass-by-reference 
       of the element.  Call overhead for the callback works against this.
  *) 

  = VAR LSsI : INTEGER := ORD ( Ss )      
  ; VAR LBiasedSsW : Word . T 

  ; BEGIN (* CallbackWithElem *) 
      AllocateAndTouchRange ( Array , LSsI , LSsI , DoInit := TRUE ) 
    ; LBiasedSsW := Word . Minus ( LSsI , Array . BiasI )   
    ; Callback ( Ss , Array . Elems ^ [ LBiasedSsW ] ) 
    END CallbackWithElem 

; VAR GEmptyArrayOfElem := ARRAY [ 1 .. 0 ] OF ElemTyp { } 
 
(* VISIBLE: *) 
; PROCEDURE CallbackWithSubarray 
    ( Array : T 
    ; Lo : SsTyp 
    ; Number : CARDINAL 
    ; Callback : ProcOfSsTypVARArrayTyp 
    ) 
  RAISES ANY 
    (* ^AllocationFailure from CallbackWithSubarray, ANY from Callback. *) 
  (* PRE: Array # NIL *) 
  (* Let N = MIN (Number,ORD(LAST(SsTyp))-ORD(Lo)+1) 
         Hi = VAL(ORD(Lo)+N-1,SsTyp)
     Abstract view: 
       1) Touch [Lo..Hi]. 
       2) Call back Callback, passing Lo and SUBARRAY(Array,Lo,N)
       The range is given in the style of SUBARRAY.   
     Concrete view: Expand allocation if needed to cover [Lo..Hi]. 
     Performance: If allocation occurs, O(NUMBER((new)touched(Array)))
       Otherwise, O(1).
  *) 

  = VAR LLoI : INTEGER 
  ; VAR LHiI : INTEGER 
  ; VAR LMaxNumberMinus1W : Word . T 
  ; VAR LNumberC : CARDINAL 
  ; VAR LNumberMinus1C : CARDINAL 
  ; VAR LBiasedLoW : Word . T 

  ; BEGIN (* CallbackWithSubarray *) 
      IF Number = 0 
      THEN (* Empty range. *) 
        Callback ( Lo , GEmptyArrayOfElem ) 
      ELSE (* Number > 0 *) 
        LLoI := ORD ( Lo ) 
      ; LMaxNumberMinus1W := Word . Minus ( ORD ( LAST ( SsTyp ) ) , LLoI )  
      ; LNumberMinus1C := WordMin ( Number - 1 , LMaxNumberMinus1W ) 
      ; LNumberC := LNumberMinus1C + 1 
      ; LHiI := LLoI + LNumberMinus1C 
      ; AllocateAndTouchRange ( Array , LLoI , LHiI , DoInit := TRUE ) 
      ; LBiasedLoW := Word . Minus ( LLoI , Array . BiasI )   
      ; Callback 
          ( Lo 
          , SUBARRAY ( Array . Elems ^ , LBiasedLoW , LNumberC )
          ) 
      END (* IF *) 
    END CallbackWithSubarray 

; PROCEDURE InnerFor 
    ( Array : T 
    ; FromI , ToI , ByI : INTEGER 
    ; Do : ProcOfSsTypVARElemTyp 
    ) 
  RAISES ANY (* ANY from Do. *) 
  (* PRE: Array # NIL *) 
  (* PRE: Needed elements of Array are allocated, initialized, and touched. *) 

  = VAR LSsI , LPenultimateSsI : INTEGER 
  ; VAR LBiasedSsW : Word . T 

  ; BEGIN (* InnerFor *) 
      IF ByI >= 0 
      THEN (* Ascending subscripts. *) 
        IF FromI > ToI 
        THEN (* Zero interations. *) 
        ELSE (* At least one iteration. *) 
          LBiasedSsW := Word . Minus ( FromI , Array . BiasI ) 
        ; IF Word . LT ( Word . Minus ( ToI , FromI ) , ByI ) 
          THEN (* One iteration. *) 
            Do ( VAL ( FromI , SsTyp ) , Array . Elems ^ [ LBiasedSsW ] ) 
          ELSE (* Plural iterations AND ToI - ByI won't underflow. *) 
            LPenultimateSsI := ToI - ByI 
          ; LSsI := FromI 
          ; LOOP 
            (* INVARIANT: LSsI <= ToI *) 
              Do ( VAL ( LSsI , SsTyp ) , Array . Elems ^ [ LBiasedSsW ] ) 
            ; IF LSsI > LPenultimateSsI 
              THEN (* LSsI + ByI > ToI *)  
                EXIT 
              ELSE 
                INC ( LSsI , ByI ) 
              ; LBiasedSsW := Word . Plus ( LBiasedSsW , ByI ) 
              END (* IF *) 
            END (* LOOP *) 
          END (* IF *)  
        END (* IF *) 
      ELSE (* Descending subscripts. *) 
        IF FromI < ToI 
        THEN (* Zero interations. *) 
        ELSE (* At least one iteration. *)
          LBiasedSsW := Word . Minus ( FromI , Array . BiasI ) 
        ; IF Word . LT ( Word . Minus ( FromI , ToI ) , - ByI ) 
          THEN (* One iteration. *) 
            Do ( VAL ( FromI , SsTyp ) , Array . Elems ^ [ LBiasedSsW ] ) 
          ELSE (* Plural iterations AND ToI - ByI won't overflow. *) 
            LPenultimateSsI := ToI - ByI 
          ; LSsI := FromI 
          ; LOOP 
            (* INVARIANT: LSsI >= ToI *) 
              Do ( VAL ( LSsI , SsTyp ) , Array . Elems ^ [ LBiasedSsW ] ) 
            ; IF LSsI < LPenultimateSsI 
              THEN (* LSsI + ByI < ToI *)  
                EXIT 
              ELSE 
                INC ( LSsI , ByI ) 
              ; LBiasedSsW := Word . Plus ( LBiasedSsW , ByI ) 
              END (* IF *) 
            END (* LOOP *) 
          END (* IF *)  
        END (* IF *) 
      END (* IF *) 
    END InnerFor

(* VISIBLE: *) 
; PROCEDURE ForAllTouchedInRange
    ( Array : T 
    ; From : SsTyp := FIRST ( SsTyp )  
    ; To : SsTyp := LAST ( SsTyp ) 
    ; By : INTEGER := 1 
    ; Do : ProcOfSsTypVARElemTyp 
    ) 
  RAISES ANY (* ANY from Do. *) 
  (* PRE: Array # NIL *) 
  (* Let R be the range [From..To], if By>=0, or [To..From], if By<0. 
     Let I be the intersection range of R and touched(Array).
     Let [F..T] be I, if By>=0, or [I.Hi..I.Lo], of By<0. 
     Abstract view: 
       Call back Do, passing Ss and Array[Ss], for the sequence 
       Ss=F, F+By, F+2*By, ... so long as Ss<=T, if By>=0, or Ss>=T, if By<0. 
       Like a FOR statement, if By=0, this will loop forever.  
       Unlike a FOR statement, this will work right up against the lower 
       and upper bounds of SsTyp, both as starting and ending subscript.   
       The range is given in the style of a FOR-loop.   
     Concrete view: (Make no change of allocation.) 
     Performance: O(NUMBER(R)). 
       More efficient than a client-written loop making repeated
       calls on other single-element-accessing procedures in this interface.
  *)  

  = VAR LLoI , LHiI : INTEGER 
  ; VAR LTouchedLoI , LTouchedHiI : INTEGER 
  ; VAR LIntersectionLoI , LIntersectionHiI : INTEGER 

  ; BEGIN (* ForAllTouchedInRange *) 
      IF Array . Elems # NIL 
         AND Word . LE ( Array . BiasedLoTouchedW , Array . BiasedHiTouchedW ) 
      THEN (* Touched range is nonempty *) 
        IF By >= 0 
        THEN 
          LLoI := ORD ( From ) 
        ; LHiI := ORD ( To ) 
        ELSE 
          LLoI := ORD ( To ) 
        ; LHiI := ORD ( From ) 
        END (* IF *) 
      ; IF LLoI <= LHiI 
        THEN (* Both ranges are nonempty. *) 
        (* We will do this bounds arithmetic in unbiased form. *) 
          LTouchedLoI 
            := Word . Plus ( Array . BiasedLoTouchedW , Array . BiasI )
        ; LTouchedHiI 
            := Word . Plus ( Array . BiasedHiTouchedW , Array . BiasI )
        ; IF LTouchedHiI < LLoI OR LHiI < LTouchedLoI 
          THEN (* Empty intersection, do nothing. *) 
          ELSE (* Intersection is nonempty *) 
            LIntersectionLoI := MAX ( LTouchedLoI , LLoI ) 
          ; LIntersectionHiI := MIN ( LTouchedHiI , LHiI ) 
          ; IF By >= 0 
            THEN 
              InnerFor ( Array , LIntersectionLoI , LIntersectionHiI , By , Do )
            ELSE 
              InnerFor ( Array , LIntersectionHiI , LIntersectionLoI , By , Do )
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END ForAllTouchedInRange

(* VISIBLE: *) 
; PROCEDURE ForAllInRange
    ( Array : T 
    ; From : SsTyp 
    ; To : SsTyp 
    ; By : INTEGER := 1 
    ; Do : ProcOfSsTypVARElemTyp 
    ) 
  RAISES ANY (* AllocationFailure from ForAllInRange, ANY from Do. *) 
  (* PRE: Array # NIL *) 
  (* Let R be the range [From..To], if By >= 0, or [To..From], if By<0. 
     Abstract view: 
       1) Expand touched(Array) to cover R.  
       2) Call back Do, passing Ss and Array[Ss], for the sequence 
          Ss=From, From+By, From+2*By, ... so long as Ss<=To, if By>=0,
          or Ss>=To, if By<0. 
       Like a FOR statement, if By = 0, this will loop forever.  
       Unlike a FOR statement, this will work right up against the lower 
       and upper bounds of SsTyp, both as starting and ending subscript.   
       The range is given in the style of a FOR-loop.   
     Concrete view: Expand allocation if needed to cover R. 
       More efficient than a client-written loop making repeated
       calls on other single-element-accessing procedures in this interface.
     Performance: O(NUMBER(R)). 
  *)  

  = VAR LFromI , LToI : INTEGER 

  ; BEGIN (* ForAllInRange *) 
      LFromI := ORD ( From ) 
    ; LToI := ORD ( To )  
    ; IF By >= 0 
      THEN 
        AllocateAndTouchRange ( Array , LFromI , LToI , DoInit := TRUE ) 
      ELSE 
        AllocateAndTouchRange ( Array , LToI , LFromI , DoInit := TRUE ) 
      END (* IF *) 
    ; InnerFor ( Array , LFromI , LToI , By , Do ) 
    END ForAllInRange

(* VISIBLE: *) 
; PROCEDURE Project ( Array : T ; Range : RangeTyp ) 
  (* PRE: Array # NIL *) 
  (* Let PRange be the intersection of Range with touched(Array). 
     Abstract view: 
       1) Set all elements of Array outside PRange to InitElemValue.  
       2) Make touched(Array) equal to PRange. 
     Concrete view: (Make no change of allocation.) 
     Performance: O(1).  
  *) 

  = VAR LLoI : INTEGER 
  ; VAR LHiI : INTEGER 
  ; VAR LBiasedIntersectionLoW : Word . T  
  ; VAR LBiasedIntersectionHiW : Word . T  

  ; BEGIN (* Project *) 
      IF Word . LE ( Array . BiasedLoTouchedW , Array . BiasedHiTouchedW )  
      THEN (* touched(Array) is nonempty. *) 
        LLoI := ORD ( Range . Lo )  
      ; LHiI := ORD ( Range . Hi )  
      ; IF LLoI > LHiI (* Empty project range *) 
           OR LHiI < Array . BiasI (* Project range below allocated. *) 
        THEN (* Make touched range empty also, but in a standard way. *) 
          Array . BiasedLoTouchedW := LAST ( CARDINAL ) 
        ; Array . BiasedHiTouchedW := 0  
        ELSE (* Neither touched(Array) nor [LLoI..LHiI] is empty.
                We can safely bias LHiI w/o overflow. *) 
          IF LLoI < Array . BiasI 
          THEN LBiasedIntersectionLoW := Array . BiasedLoTouchedW 
          ELSE (* We can safely bias LLoI w/o overflow. *)
            LBiasedIntersectionLoW 
              := WordMax 
                   ( Word . Minus ( LLoI , Array . BiasI ) 
                   , Array . BiasedLoTouchedW 
                   ) 
          END (* IF *) 
        ; LBiasedIntersectionHiW 
            := WordMin
                 ( Word . Minus ( LHiI , Array . BiasI )  
                 , Array . BiasedHiTouchedW 
                 ) 
        ; IF Word . GT ( LBiasedIntersectionLoW , LBiasedIntersectionHiW ) 
          THEN (* Empty intersection.  Make touched range empty. *) 
            Array . BiasedLoTouchedW := LAST ( CARDINAL ) 
          ; Array . BiasedHiTouchedW := 0  
          ELSE (* [Lo..Hi] overlaps touched(Array). *) 
            Array . BiasedLoTouchedW := LBiasedIntersectionLoW 
          ; Array . BiasedHiTouchedW := LBiasedIntersectionHiW 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END Project 

(* VISIBLE: *) 
; PROCEDURE Copy 
  (* PRE: Array # NIL *) 
    ( Array : T 
    ; ProjectRange : RangeTyp := FullRange 
      (* ^Default results in no projection. *) 
    ; AllocRange : RangeTyp := EmptyRange 
      (* ^Default results in no change to allocation. *) 
    ) 
  : T 
  RAISES { AllocationFailure } 
  (* PRE: Array # NIL *) 
  (* Abstract view: 
       1. A copy of Array.  This means mutations to Array will 
          not affect the copy, and vice versa.  
       2. Apply a Project operation to the copy, using ProjectRange. 
          (See PROCEDURE Project for precise definition.) 
          (It is more efficient to Project and copy at the same time.)
          Note that the default gives a noop Project.  
     Concrete view: 
       If AllocRange=EmptyRange (not just any empty range), give the copy 
       the same allocated space as the original.  Otherwise, give the copy 
       allocated space for the smallest range that covers both 
       (new)touched(Copy) and AllocRange. 
       (It is more efficient to adjust allocation and copy at the same time.)
  *) 

  = VAR LResult : T 
  ; VAR LAllocLoI , LAllocHiI : INTEGER 
  ; VAR LNewTouchedLoI , LNewTouchedHiI : INTEGER 
  ; VAR LArrayNumberW : Word . T 
  ; VAR LCopyNumberW : Word . T 
  ; VAR LNewLastW : Word . T 
  ; VAR LNewNumberW : Word . T 

  ; BEGIN (* Copy *) 
      LResult 
        := NEW ( T 
               , ExpansionFactor := Array . ExpansionFactor 
               , InitElemValue := Array . InitElemValue 
               ) 
    ; IF LResult = NIL THEN RAISE AllocationFailure END (* IF *) 
    ; IF Array . Elems = NIL 
      THEN LArrayNumberW := 0 
      ELSE LArrayNumberW := NUMBER ( Array . Elems ^ ) 
      END (* IF *) 
    ; IF LArrayNumberW = 0 
      THEN (* No elements allocated => touched is empty & BiasI meaningless. *)
        IF AllocRange . Lo > AllocRange . Hi 
        THEN (* Give the copy an empty allocation. *) 
          LResult . BiasI := 0 (* Defensive.  Meaningless for now. *) 
        ; LResult . Elems := NIL 
        ; LResult . BiasedLoTouchedW := LAST ( CARDINAL ) 
        ; LResult . BiasedHiTouchedW := 0 
        ELSE (* touched is empty, but allocate some space for the copy. *) 
          LAllocLoI := ORD ( AllocRange . Lo ) 
        ; LAllocHiI := ORD ( AllocRange . Hi ) 
        ; LNewLastW := Word . Minus ( LAllocHiI , LAllocLoI ) 
        ; IF Word . GE ( LNewLastW , LAST ( CARDINAL ) ) 
          THEN RAISE AllocationFailure 
          END (* IF *) 
        ; LNewNumberW := Word . Plus ( LNewLastW , 1 ) 
        ; LResult . BiasI := LAllocLoI 
        ; LResult . Elems := NEW ( ElemsRefTyp , LNewNumberW ) 
        ; IF LResult . Elems = NIL THEN RAISE AllocationFailure END (* IF *) 
        ; LResult . BiasedLoTouchedW := LNewNumberW 
        ; LResult . BiasedHiTouchedW := 0 
        END (* IF *) 
      ELSE (* The original has an allocated array. *)  
      (* Unbiased arithmetic will stay in range here. *) 
        IF Word . GT ( Array . BiasedLoTouchedW , Array . BiasedHiTouchedW )
        THEN (* Old touched range is empty. *) 
          LNewTouchedLoI := ORD ( EmptyRange . Lo ) 
        ; LNewTouchedHiI := ORD ( EmptyRange . Hi ) 
        ELSE 
          LNewTouchedLoI 
            := MAX ( ORD ( ProjectRange . Lo ) 
                   , Word . Plus ( Array . BiasI , Array . BiasedLoTouchedW )
                   ) 
        ; LNewTouchedHiI 
            := MIN ( ORD ( ProjectRange . Hi ) 
                   , Word . Plus ( Array . BiasI , Array . BiasedHiTouchedW )
                   ) 
        END (* IF *) 
      ; IF AllocRange = EmptyRange (* Not just any empty range. *)  
        THEN (* Preserve the coverage of existing allocation. *) 
          LNewNumberW := LArrayNumberW 
        ; LResult . BiasI := Array . BiasI 
        ELSE (* Change to the requested allocation. *) 
          IF LNewTouchedLoI > LNewTouchedHiI 
          THEN (* New touched range is empty. *) 
            IF AllocRange . Lo > AllocRange . Hi 
            THEN (* Give the copy an empty allocation. *) 
              LResult . BiasI := 0 (* Defensive.  Meaningless for now. *) 
            ; LResult . Elems := NIL 
            ; LResult . BiasedLoTouchedW := LAST ( CARDINAL ) 
            ; LResult . BiasedHiTouchedW := 0 
            ; RETURN LResult (* Avoid allocation below. *) 
            ELSE (* New touched range is empty, AllocRange is not. *) 
              LAllocLoI := ORD ( AllocRange . Lo ) 
            ; LAllocHiI := ORD ( AllocRange . Hi ) 
            END (* IF *) 
          ELSE (* New touched range is nonempty. *)  
            IF AllocRange . Lo > AllocRange . Hi 
            THEN (* Just allocate for the touched range. *) 
              LAllocLoI := LNewTouchedLoI  
            ; LAllocHiI := LNewTouchedHiI  
            ELSE  
              LAllocLoI := MIN ( ORD ( AllocRange . Lo ) , LNewTouchedLoI ) 
            ; LAllocHiI := MAX ( ORD ( AllocRange . Hi ) , LNewTouchedHiI ) 
            END (* IF *) 
          END (* IF *) 
        ; LNewLastW := Word . Minus ( LAllocHiI , LAllocLoI ) 
        ; IF Word . GE ( LNewLastW , LAST ( CARDINAL ) ) 
          THEN RAISE AllocationFailure 
          END (* IF *) 
        ; LNewNumberW := Word . Plus ( LNewLastW , 1 ) 
        ; LResult . BiasI := LAllocLoI 
        END (* IF *) 
      ; LResult . Elems := NEW ( ElemsRefTyp , LNewNumberW ) 
      ; IF LResult . Elems = NIL THEN RAISE AllocationFailure END  (* IF *) 
      ; IF LNewTouchedLoI > LNewTouchedHiI 
        THEN (* Make touched empty. *) 
          LResult . BiasedLoTouchedW := LNewNumberW 
        ; LResult . BiasedHiTouchedW := 0 
        ELSE 
          LResult . BiasedLoTouchedW 
            := Word . Minus ( LNewTouchedLoI , LResult . BiasI ) 
        ; LResult . BiasedHiTouchedW 
            := Word . Minus ( LNewTouchedHiI , LResult . BiasI ) 
        ; LCopyNumberW 
            := Word . Plus 
                 ( Word . Minus ( LNewTouchedHiI , LNewTouchedLoI ) , 1 )
        ; SUBARRAY 
            ( LResult . Elems ^ , LResult . BiasedLoTouchedW , LCopyNumberW ) 
            := SUBARRAY 
                 ( Array . Elems ^ 
                 , Word . Minus ( LNewTouchedLoI , Array . BiasI ) 
                 , LCopyNumberW 
                 ) 
        END (* IF *) 
      END (* IF *) 
    ; RETURN LResult 
    END Copy 

(* VISIBLE: *) 
; PROCEDURE AllocatedRange ( Array : T ) : RangeTyp 
  (* PRE: Array # NIL *) 
  (* Abstract view: A noop.
     Concrete view: The currently allocated space of Array. 
     Performance: O(1). 
  *) 

  = VAR LArrayNumberC , LArrayLastC : CARDINAL 

  ; BEGIN (* AllocatedRange *) 
      IF Array . Elems = NIL 
      THEN RETURN EmptyRange 
      ELSE 
        LArrayNumberC := NUMBER ( Array . Elems ^ ) 
      ; IF LArrayNumberC = 0 
        THEN (* Empty array allocated.  (Can this happen? Return empty. *) 
          RETURN EmptyRange
        ELSE 
          LArrayLastC := LArrayNumberC - 1 
        ; RETURN 
            RangeTyp 
              { Lo := VAL ( Array . BiasI , SsTyp ) 
              , Hi := VAL ( Array . BiasI + LArrayLastC , SsTyp )
              }
        END (* IF *) 
      END (* IF *) 
    END AllocatedRange 

(* VISIBLE: *) 
; PROCEDURE Compact ( Array : T ) 
  RAISES { AllocationFailure } 
  (* PRE: Array # NIL *) 
  (* Abstract view: A noop. 
     Concrete view: Deallocate space for everything outside touched(Array). 
     Performance: If any deallocation is done, O(NUMBER(touched(Array))), 
       otherwise, O(1).  
  *) 

  = VAR LArrayNumberW : Word . T 
  ; VAR LNewLastW : Word . T 
  ; VAR LNewNumberW : Word . T 
  ; VAR LNewElems : ElemsRefTyp 

  ; BEGIN (* Compact *) 
      IF Array . Elems # NIL 
      THEN (* There is an allocated array to compact. *)  
        LArrayNumberW := NUMBER ( Array . Elems ^ ) 
      ; IF LArrayNumberW = 0 (* Zero size allocated array.  Can this happen? *)
           OR Word . GT ( Array . BiasedLoTouchedW , Array . BiasedHiTouchedW )
              (* ^Empty touched range. *) 
        THEN (* Deallocate the array. *)
          Array . BiasI := 0 
        ; Array . Elems := NIL 
        ELSE 
          LNewLastW 
            := Word . Minus 
                 ( Array . BiasedHiTouchedW , Array . BiasedLoTouchedW ) 
        ; LNewNumberW := Word . Plus ( LNewLastW , 1 ) 
        ; IF Word . LT ( LNewNumberW , LArrayNumberW )  
          THEN (* There is shrinkage. *)  
            LNewElems := NEW ( ElemsRefTyp , LNewNumberW ) 
          ; IF LNewElems = NIL THEN RAISE AllocationFailure END (* IF *) 
          ; LNewElems ^ 
              := SUBARRAY 
                   ( Array . Elems ^ , Array . BiasedLoTouchedW , LNewNumberW )
          ; Array . BiasI 
              := Word . Plus ( Array . BiasI , Array . BiasedLoTouchedW ) 
          ; Array . Elems := LNewElems 
          ; Array . BiasedLoTouchedW := 0 
          ; Array . BiasedHiTouchedW := LNewLastW  
          END (* IF *)   
        END (* IF *)   
      END (* IF *) 
    END Compact 

(* VISIBLE: *) 
; PROCEDURE RawInfo ( Array : T ) : RawInfoTyp  
  (* Return a record of information that allows fast, direct access to the
     underlying allocated array used by the implementation, at the risk of 
     creating chaos, unless the client follows these rules: 

     Assume a client has executed RI := RawInfo ( Array ). 

     If RI.Touched.Lo <= I AND I <= RI.Touched.Hi,
     then element Array[T] is found in 
       RI.ArrayRef^[Word.Minus(ORD(I),RI.BiasInt)].
     The client may read or write it.  

     Attempts to use an I outside RI.Touched could result in invalid data,
     runtime errors, or unpredictable damage to Array. 

     After any call is made to a procedure in this interface that mutates Array
     (Touch, Project, Assign, AssignSubarray, CallbackWithElem, 
     CallbackWithSubarray, ForAllInRange, and Compact), the client must 
     assume RI is invalid and must be refetched.

     If the client validly assigns to any elements of RI.ArrayRef^,
     the changes will be visible immediately to any procedure in this
     interface that allows element values to be accessed but does not
     invalidate RI (Fetch, FetchSubarray, Copy, ForAllTouchedInRange).

     Client calls to ForAllTouchedInRange do not invalidate any part of RI,
     even though they may assign to array element values.  Such assignments
     are immediately visible via valid uses of RI.     
  *)    

  = VAR LResult : RawInfoTyp 

  ; BEGIN
      LResult . Touched . Lo 
        := VAL ( Word . Plus ( Array . BiasedLoTouchedW , Array . BiasI ) 
               , SsTyp 
               ) 
    ; LResult . Touched . Hi 
        := VAL ( Word . Plus ( Array . BiasedHiTouchedW , Array . BiasI ) 
               , SsTyp 
               ) 
    ; LResult . BiasInt := Array . BiasI 
    ; LResult . ArrayRef := Array . Elems 
    ; RETURN LResult 
    END RawInfo 

(* Variables below are initialized at startup.  Don't change them later. 
     It would be nice if Word had Min and Max functions so these could
     be constants.  We would use Word.Min and Word.Max a lot for many
     other non-constant values in here, too. *) 

; VAR MaxArrayNumberC : CARDINAL 
      (* ^Max NUMBER of an allocated array, limited by both the abstract 
         subscript type and the CARDINAL limit on open arrays. *) 
; VAR MaxSizeIncrC : CARDINAL 
      (* ^Limited by both AbsMaxSizeIncrC and MaxArrayNumberC. *)  
; VAR MaxSizeIncrR : REAL 
      (* ^REAL equivalent of MaxSizeIncrC *) 

; BEGIN (* VarArray *) 
    IF ORD ( FIRST ( SsTyp ) ) >= ORD ( LAST ( SsTyp ) )  
    THEN (* Not a plural range. *) 
      MaxArrayNumberC := 0 
    ELSIF ORD ( FIRST ( SsTyp ) ) = FIRST ( INTEGER ) 
       AND ORD ( LAST ( SsTyp ) ) = LAST ( INTEGER ) 
    THEN (* SsTyp has the full range of INTEGER. *) 
      MaxArrayNumberC := LAST ( CARDINAL ) 
    ELSE 
      MaxArrayNumberC 
        := WordMin 
             ( Word . Plus 
                 ( Word . Minus 
                     ( ORD ( LAST ( SsTyp ) ) , ORD ( FIRST ( SsTyp ) ) ) 
                 , 1
                 ) 
             , LAST ( CARDINAL ) 
             )
    END (* IF *) 
  ; MaxSizeIncrC := MIN ( MaxArrayNumberC , AbsMaxSizeIncrC ) 
  ; MaxSizeIncrR := FLOAT ( MaxSizeIncrC ) 
  END VarArray 
. 

