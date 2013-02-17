
(* -----------------------------------------------------------------------1- *)
(* File OrdSets.mg  Modula-3 source code.                                    *)
(* Copyright 2010 .. 2012, Rodney M. Bates.                                  *)
(* rbates@acm.org                                                            *)
(* Licensed under the Gnu Public License, version 2 or later.                *)
(* -----------------------------------------------------------------------2- *)

GENERIC MODULE OrdSets ( ) 

(* The only reasons to instantiate this are to give each instantiation a unique
   module name and to name the interface it exports.  Thus it has no generic 
   parameters. 

   Instead, we use following from the exported interface (an instantiation of
   OrdSets.ig), as renames of things from its generic parameter Element:

   Name in instantiated OrdSets.ig    Name in its generic actual interface

   ElemT                              Element . T
   ValidElemT                         Element . ValidElemT
   NullElem                           Element . NullElem

*) 

; IMPORT Fmt
; IMPORT Text 
; IMPORT TextWr 
; IMPORT Thread 
; IMPORT Word 
; IMPORT Wr 

; IMPORT BitNoTable 
; IMPORT UnsafeUtils 


(* NOTE on compiler WARNING messages on 32-bit machines:  
    Expect two "warning: value out of range" messages when compiling on 
    a 32-bit machine.  These are harmless because the cited code will not
    be executed. 
*) 

(* Change DoPseudoPointers to control whether misaligned
   pseudo-pointers are used to save heap space for small sets.  If
   this is FALSE, a compiler might, hopefully, optimize out
   unnecessary runtime tests.  If TRUE, you will need a later Modula-3
   compiler and garbage collector that tolerate misaligned pointers.
   CM3 5.8 is sufficient.  SRC M3, PM3, EZM3, and earlier CM3 versions
   are not.  

   Up through 2013-02-17, Pickles will not tolerate this misaligned
   pointers.  Also, up through the same date, even without misaligned
   pointers, pickles will not correctly convert these sets between different
   word sizes.     
*)
; CONST DoPseudoPointers = FALSE  

(* Thread safety: 

   I believe this module to be thread-safe, without needing mutual
   exclusion around calls to its visible procedures, except for a
   small chance of a performance bug involving simultaneous calls to
   Hash or simultaneous calls to Card.  This has had no testing, but
   here is a rationale:

   All heap-allocated objects are mostly immutable.  During initial
   creation, they of course, have assignments being made to various
   fields.  But until a set value is returned to the client world, no
   other thread would have a way of accessing it.  As for
   pseudo-pointers, they get copied during passing in and returning.

   The exception is partially lazy evaluation of cardinalities and
   hash codes of heap-allocated Bitsets.  These are computed and
   cached in the object at set creation time only if it is
   computationally easy.  Otherwise, the fields are set to an
   "unknown" value.  They may then be computed when demanded, at which
   time they are cached.

   This operation is strictly one-way: "unknown" changes to a known
   value.  Moreover, each of these fields occupies exactly one machine
   word.  So, if we can assume word-sized memory reads and writes are 
   atomic on the running machine, the worst that could happen is more than
   one client thread calling Hash or Card in parallel could note the
   value is unknown and redundantly compute it.  The eventual multiple
   stores would all be of the same value.
*)

(* This module can be instantiated with any ordinal type as set elements.
   Internally, we almost always use the corresponding INTEGER values, doing
   type conversions close to where values are passed in and returned.
   The parts of variable names that denote element values as integers use 
   the prefix "I" to indicate that.
*)   

; TYPE IElemTyp = INTEGER 
; TYPE ValidIElemTyp 
    = [ ORD ( FIRST ( ValidElemT ) ) .. ORD ( LAST ( ValidElemT ) ) ] 
; CONST IElemNull = ORD ( NullElem ) 

; TYPE BitwordTyp = Word . T
; CONST BitsPerBitword = BITSIZE ( BitwordTyp )
; TYPE BitNoTyp = [ 0 .. BitsPerBitword - 1 ] 
; TYPE BitCtTyp = [ 0 .. BitsPerBitword ]
 
; TYPE BitwordArrayTyp = ARRAY OF BitwordTyp
; TYPE BitwordArrayRefTyp = REF BitwordArrayTyp 

; TYPE BitwordNoTyp = INTEGER  
; TYPE ArraySsTyp = CARDINAL 

; TYPE BiasTyp = INTEGER 

; CONST HashZero : HashTyp = 0 (* This would be a bad real hash code anyway. *) 

; TYPE BitsetInfoTyp 
    = RECORD 
        Bias : BiasTyp := 0 
        (* The Bitword number that is found in BitwordArrayRef ^ [ 0 ]. 
           This is because open arrays must have lower bound of zero.
           Also, it allows some reuse of Bitword arrays. 
        *) 
      ; Card : CardTyp := CardUnknown   
        (* If # CardUnknown , the cached cardinality of the set. *) 
      ; Hash : HashTyp := HashZero 
        (* If # HashZero , the cached hash value for the set. *) 
      ; BitsetLo : ValidIElemTyp
        (* The minimum element present in the represented set. *)  
      ; BitsetHi : ValidIElemTyp 
        (* The maximum element present in the represented set. *)  
      END 

(* Corresponding to a value of BitsetInfoTyp is an open array of Bitwords.
   For a heap-allocated Bitset, its reference is stored in the object,
   along with the BitsetInfo.  They satisfy the following 
   INVARIANT: The presence of element E is represented by bit BitNo ( E ), 
              within Bitwords [ ArraySsOfIElem ( E , Bias ) ]
              The bits corresponding to BitsetLo and BitsetHi are ones.
              At least one 0-bit lies somewhere between. 
              Bitwords contains all necessary elements to hold
              bit numbers in [BitsetLo..BitsetHi], subject to Bias.
              Bitwords can have unnecessary words at either end. 
              Bits in these, as well as bits in necessary words but
              outside [BitsetLo..BitsetHi] are called "Garbage bits"
              and can have any value.  
              Bitwords could be shared by other Bitsets, with different
              bounds and Bias, and for such a set, the garbage bits of
              this set could be meaningful.   
*) 

; REVEAL T = BRANDED Brand OBJECT END (* Abstract. *) 
  (* INVARIANT: VAR S:T represents empty iff S = NIL. *) 

; TYPE BitsetTyp 
  = T OBJECT 
        BitsetInfo : BitsetInfoTyp 
      ; BitwordArrayRef : BitwordArrayRefTyp 
        (* INVARIANT: BitwordArrayRef # NIL *) 
      END (* OBJECT *)

; TYPE RangesetTyp 
  = T OBJECT 
        RangeLo , RangeHi : ValidIElemTyp 
      (* INVARIANT: RangeLo <= RangeHi
         Represents { RangeLo .. RangeHi }  
      *) 
      END 

; CONST CardUnknown = 0 
  (* We can get away with using this as an "unknown" internally for 
     Bitsets, because they are required by invariants to have
     real cardinality at least 2.
  *) 

; CONST AllOnes = Word . Not ( 0 )  

(* ========================== Low level utility ============================ *)

(* We consistently use little-endian bit numbering, for easier computations 
   in this module.  This need not match the endianness of the machine.
*) 

; <* INLINE *> 
  PROCEDURE BitNo ( IElem : IElemTyp ) : BitNoTyp 
  (* The bit number within whatever word IElem is in. *) 

  = BEGIN 
      RETURN IElem MOD BitsPerBitword 
    END BitNo  

; <* INLINE *> 
  PROCEDURE BitMaskOfIElem ( IElem : IElemTyp ) : BitwordTyp  
  (* A mask with a 1-bit for BitNo(IElem). *)  

  = BEGIN 
      RETURN Word . LeftShift ( 1 , BitNo ( IElem ) )  
    END BitMaskOfIElem  

; <* INLINE *> 
  PROCEDURE LEMaskOfIElem ( IElem : IElemTyp ) : BitwordTyp  
  (* A mask with 1-bits for BitNo(IElem) and all lower-numbered bits. *)  

  = BEGIN 
      RETURN 
        Word . RightShift 
          ( - 1 , BitsPerBitword - 1 - BitNo ( IElem ) )  
    END LEMaskOfIElem  

; <* INLINE *> 
  PROCEDURE GEMaskOfIElem ( IElem : IElemTyp ) : BitwordTyp  
  (* A mask with 1-bits for BitNo(IElem) and all higher-numbered bits. *)  

  = BEGIN 
      RETURN Word . LeftShift ( - 1 , BitNo ( IElem ) )  
    END GEMaskOfIElem  

; <* INLINE *> 
  PROCEDURE LTMaskOfIElem ( IElem : IElemTyp ) : BitwordTyp  
  (* A mask with 1-bits for all lower-numbered bits than BitNo(IElem). *)  

  = BEGIN 
      RETURN 
        Word . Not 
          ( Word . LeftShift ( - 1 , BitNo ( IElem ) ) )
    END LTMaskOfIElem  

; <* INLINE *> 
  PROCEDURE GTMaskOfIElem ( IElem : IElemTyp ) : BitwordTyp  
  (* A mask with 1-bits for all higher-numbered bits than BitNo(IElem). *)  

  = BEGIN 
      RETURN 
        Word . Not 
         (  Word . RightShift 
              ( - 1 , BitsPerBitword - 1 - BitNo ( IElem ) )  
         ) 
    END GTMaskOfIElem  

; <* INLINE *> 
  PROCEDURE GTMaskOfBitNo ( BitNo : BitNoTyp ) : BitwordTyp  
  (* A mask with 1-bits for all higher-numbered bits than BitNo. *)  

  = BEGIN 
      RETURN 
        Word . Not 
         (  Word . RightShift 
              ( - 1 , BitsPerBitword - 1 - BitNo )  
         ) 
    END GTMaskOfBitNo  

; <* INLINE *> 
  PROCEDURE BitwordNoOfIElem ( IElem : IElemTyp ) : BitwordNoTyp 
  (* The number of the Bitword IElem lies in. *) 

  = BEGIN 
      RETURN IElem DIV BitsPerBitword 
    END BitwordNoOfIElem  

; <* INLINE *> 
  PROCEDURE ArraySsOfBitwordNo  
     ( BitwordNo : BitwordNoTyp ; Bias : BiasTyp ) 
  : INTEGER
    (* ^We sometimes need to be able to return invalid hypothetical subscript
       values, including negative values, to a caller that is only checking 
       whether such an element exists.
    *)  
  (* The subscript in a Bitword array of BitwordNo, taking Bias into account. *)

  = BEGIN 
      RETURN BitwordNo - Bias 
    END ArraySsOfBitwordNo  

; <* INLINE *> 
  PROCEDURE ArraySsOfIElem 
     ( IElem : IElemTyp ; Bias : BiasTyp ) 
  : INTEGER 
    (* ^We sometimes need to be able to return invalid hypothetical subscript
       values, including negative values, to a caller that is only checking 
       whether such an element exists.
    *)  
  (* The subscript in a Bitword array of the Bitword containing IElem, 
     taking Bias into account. 
  *)  

  = BEGIN 
      RETURN ArraySsOfBitwordNo ( BitwordNoOfIElem ( IElem ) , Bias ) 
    END ArraySsOfIElem   

; <* INLINE *> 
  PROCEDURE BitZeroIElemOfBitwordNo ( BitwordNo : BitwordNoTyp ) 
  : IElemTyp  
  (* The IElem value of bit number zero, within the Bitword at BitwordNo. *)  

  = BEGIN 
      RETURN BitwordNo * BitsPerBitword 
    END BitZeroIElemOfBitwordNo

(* ============================ Heap allocation. =========================== *) 

(* Accumulated statistics. *) 

; VAR GRangesetCt : LONGINT := 0L 
; VAR GBitsetCt : LONGINT := 0L 
; VAR GArrayCt : LONGINT := 0L 
; VAR GArrayElemCt : LONGINT := 0L 
; VAR GPseudoBitsetCt : LONGINT := 0L
; VAR GPseudoRangesetCt : LONGINT := 0L
; <*UNUSED*> VAR GBitsetInfoSize := BYTESIZE ( BitsetInfoTyp ) 
                 (* ^Check this in a debugger *) 

; PROCEDURE NewRangeset ( RangeLo , RangeHi : ValidIElemTyp ) : RangesetTyp  

  = BEGIN 
      INC ( GRangesetCt ) 
    ; RETURN NEW ( RangesetTyp , RangeLo := RangeLo , RangeHi := RangeHi ) 
    END NewRangeset 

; <* INLINE *> 
  PROCEDURE NewBitwordArray ( N : CARDINAL ) : BitwordArrayRefTyp

  = BEGIN 
      INC ( GArrayCt ) 
    ; INC ( GArrayElemCt , VAL ( N , LONGINT ) ) 
    ; RETURN NEW ( BitwordArrayRefTyp , N ) 
    END NewBitwordArray 

; <* INLINE *>
  PROCEDURE NewBitset ( ): BitsetTyp  

  = BEGIN 
      INC ( GBitsetCt ) 
    ; RETURN NEW ( BitsetTyp )  
    END NewBitset

; CONST ReuseBitwordCtMin = 8 (* Always reuse Bitword arrays <= this length. *)
; CONST ReuseFractionNum = 5  (* Reuse if fraction Num/Den of the elements *)  
; CONST ReuseFractionDen = 10 (* are needed. *) 
  (* If the old array is not garbage collected, it will always be better to
     reuse it in another set.  If it ever could have been collected,
     after that point, we are wasting space for the retained, unused
     elements.  Remember that there are 3 extra words actually
     allocated from the heap, in addition to the open array elements
     themselves, plus heap fragmentation losses, time to allocate and
     collect, and increased working set size.
  *) 

; PROCEDURE DoReuseBitwordArray 
    ( OldBitwordCt , NewBitwordCt : CARDINAL ) 
  : BOOLEAN 

  = BEGIN 
      RETURN OldBitwordCt <= ReuseBitwordCtMin 
             OR NewBitwordCt 
                >= OldBitwordCt * ReuseFractionNum DIV ReuseFractionDen  
    END DoReuseBitwordArray

(* ====================== Handling of pseudo-pointers. ===================== *) 

(* Psuedo-pointers:

   This module can optionally use pseudo-pointers for some set values.
   These are values of what is statically a heap pointer type, but actually
   has its LSB set, making it misaligned for any heap object on any machine.
   The rest of the bits are then used to represent limited cases of actual
   set values right in the variable, avoiding the very high space overhead
   of heap allocation.

   The next least significant bit is used to distinguish a pseudo-pointer
   Rangeset from a pseudo-pointer Bitset.  

   For a RangeSet, the remaining bits are divided equally and contain the
   bounds of the range, as unsigned values.  If either bound won't fit in 
   this field, a heap object is necessarily used.  

   For a Bitset, the remaining bits constitute a slightly shortened Bitword,
   with bit 2 of the pseudo-pointer being bit 0 of the Bitword.  The fields of
   a BitsetInfo for the set can be computed from this abbreviated Bitword or set
   to default values.  If the entire set won't fit in this field, a heap object 
   is necessarily used.    
*) 

; CONST BitsPerPseudoBitword = BitsPerBitword - 2 
; CONST PseudoBitsetMax = BitsPerPseudoBitword - 1 
; CONST BitsPerPseudoBound = BitsPerPseudoBitword DIV 2
; CONST PseudoBoundMax = Word . LeftShift ( 1 , BitsPerPseudoBound - 1 ) - 1 
; CONST PseudoBoundMin = - PseudoBoundMax - 1 
; CONST PseudoBoundMask = Word . LeftShift ( 1 , BitsPerPseudoBound ) - 1  

; CONST TrueReferenceFlagBits = 2_00 
; CONST PseudoBitsetFlagBits = 2_01 
; CONST PseudoRangeFlagBits = 2_11 

(* To avoid a combinatorial explosion of different set representations,
   lots of support routines accept parameters that can come from either
   heap allocated objects or the field(s) within pseudo-pointers.

   For Bitsets, these are the BitsetInfo subfield of the heap object and the
   Bitword array, passed as an open array.  These can be constructed
   from a pseudo-pointer or taken directly from a heap-allocated Bitset. 

   Additionally, some procedures have cases where they can reuse the 
   heap object and/or the original set (possibly a pseudo-pointer).  Such
   procedures accept these as additional parameters, with the precondition
   that they correspond to the BitsetInfo and Bitword array parameters.  For the
   heap object, this has type BitsetTyp.  It can be NIL if no heap object
   is available.  It will not be a pseudo-pointer.  For the original set,
   this just has type T.  

   For a range, the bounds are passed in separate parameters.  Similarly,
   some procedures have cases that could reuse an original heap-allocated
   Rangeset, in which case it is passed as an extra parameter of type
   RangesetTyp.  It can be NIL if unavailable.
*)    

; PROCEDURE ExtractWSignExt ( x : Word . T ; i , n : CARDINAL ) : Word . T 
  (* Like Word . Extract, but sign-extends the extracted field in the result. *)

  = VAR LResult : Word . T 

  ; BEGIN
    (* Oh, how I wish we just had an arithmetic (sign-extending) right
       shift in Word. 
    *) 
      LResult := Word . Extract ( x , i , n ) 
    ; IF Word . And ( LResult , Word . LeftShift ( 1 , n - 1 ) ) # 0 
      THEN (* Negative *) 
        LResult := Word . Or ( LResult , Word . LeftShift ( - 1 , n - 1 ) )
      END (* IF *) 
    ; RETURN LResult  
    END ExtractWSignExt 

; PROCEDURE DissectPseudoPointer 
    ( Set : T ; VAR Bitword : BitwordTyp ; VAR Lo , Hi : IElemTyp )
  (* - If Set is a pseudo pointer containing a Bitword, Bitword will be that 
       Bitword, and by invariant, nonzero, and Lo & Hi will be its Min and Max. 
     - If Set is a pseudo pointer containing a range, Lo and Hi will be
       its bounds and Bitword will be zero.
     - Otherwise, Bitword will be zero and Lo and Hi will be IElemNull. 
  *) 

  = VAR LWord , LFlag : Word . T 

  ; BEGIN 
      IF DoPseudoPointers (* Hopefully, a compiler will fold this. *) 
      THEN 
        LWord := UnsafeUtils . IntOfRefany ( Set ) 
      ; LFlag := Word . And ( LWord , 2_11 ) 
      ; CASE LFlag 
        OF TrueReferenceFlagBits  
        => (* Fall through. *) 
        | PseudoBitsetFlagBits  
        => Bitword := Word . RightShift ( LWord , 2 )  
        ; Lo := Least1BitNoInBitword ( Bitword ) 
        ; Hi := Greatest1BitNoInBitword ( Bitword ) 
        ; RETURN 
        | PseudoRangeFlagBits 
        => Bitword := 0 
        ; Hi := ExtractWSignExt ( LWord , 2 , BitsPerPseudoBound )   
        ; Lo := ExtractWSignExt 
                  ( LWord , 2 + BitsPerPseudoBound , BitsPerPseudoBound )   
        ; RETURN 
        ELSE <* ASSERT FALSE *> 
        END (* CASE *)
   (* ELSE Fall through. *)   
      END (* IF *) 
    (* Either we aren't checking for a pseudo-pointer, or we don't have one. *)
    ; Bitword := 0 
    ; Lo := IElemNull
    ; Hi := IElemNull 
    END DissectPseudoPointer 

; TYPE DissectInfo 
  = RECORD 
      Set : T := NIL 
    ; RangeLo , RangeHi : IElemTyp := IElemNull 
    ; RSet : RangesetTyp := NIL  
    ; BitsetInfo : BitsetInfoTyp
    ; BSet : BitsetTyp := NIL 
    END 

; TYPE OneSetProcTyp 
  = PROCEDURE 
      ( READONLY DInfo : DissectInfo 
      ; READONLY Bitwords : ARRAY OF BitwordTyp 
      ) 
    RAISES ANY  
 
; TYPE TwoSetsProcTyp 
  = PROCEDURE 
      ( READONLY DInfo1 : DissectInfo 
      ; READONLY Bitwords1 : ARRAY OF BitwordTyp 
      ; READONLY DInfo2 : DissectInfo 
      ; READONLY Bitwords2 : ARRAY OF BitwordTyp 
      ) 
    RAISES ANY

; PROCEDURE ConstructBitsetInfo  
    ( Bitword : BitwordTyp ; VAR BitsetInfo : BitsetInfoTyp ) 
  (* Bitword is as for a single-bitword Bitset with Bias of zero.
     Set the fields of BitsetInfo as they would be for this Bitword.
  *) 

  = BEGIN  
      BitsetInfo . BitsetLo := Least1BitNoInBitword ( Bitword ) 
    ; BitsetInfo . BitsetHi := Greatest1BitNoInBitword ( Bitword )
    ; BitsetInfo . Bias := 0  
    ; BitsetInfo . Card := CardUnknown  
    ; BitsetInfo . Hash := HashZero 
    END ConstructBitsetInfo 

; CONST BitwordArrayEmpty = ARRAY OF BitwordTyp { } 

; PROCEDURE CallWithOneSet ( Set : T ; Proc : OneSetProcTyp ) RAISES ANY 
  (* Call Proc with parameters taken from Set. *)  

  = VAR LWord , LFlag : Word . T 
  ; VAR LBitword : BitwordTyp 
  ; VAR LDInfo : DissectInfo 

  ; BEGIN 
      LDInfo . Set := Set 
    ; IF DoPseudoPointers (* Hopefully, a compiler will fold this. *) 
      THEN 
        LWord := UnsafeUtils . IntOfRefany ( Set ) 
      ; LFlag := Word . And ( LWord , 2_11 ) 
      ; CASE LFlag <* NOWARN *>  

        OF PseudoBitsetFlagBits  
        => LBitword := Word . RightShift ( LWord , 2 )  
        ; ConstructBitsetInfo ( LBitword , (*VAR*) LDInfo . BitsetInfo ) 
        ; Proc ( LDInfo , ARRAY OF BitwordTyp { LBitword } ) 
        ; RETURN 

        | PseudoRangeFlagBits 
        => LDInfo . RangeHi 
            := ExtractWSignExt ( LWord , 2 , BitsPerPseudoBound )   
        ; LDInfo . RangeLo 
            := ExtractWSignExt 
                 ( LWord , 2 + BitsPerPseudoBound , BitsPerPseudoBound )   
        ; Proc ( LDInfo , BitwordArrayEmpty ) 
        ; RETURN 

        | TrueReferenceFlagBits  
        => (* Fall through. *) 

        END (* CASE *)  
   (* ELSE Fall through. *)   
      END (* IF *) 

    ; TYPECASE Set <* NOWARN *>
      OF NULL 
      => Proc ( LDInfo , BitwordArrayEmpty )

      | BitsetTyp ( TBSet ) 
      => LDInfo . BitsetInfo := TBSet . BitsetInfo 
      ; LDInfo . BSet := TBSet 
      ; Proc ( LDInfo , TBSet . BitwordArrayRef ^ ) 

      | RangesetTyp ( TRSet ) 
      => LDInfo . RangeLo := TRSet . RangeLo 
      ; LDInfo . RangeHi := TRSet . RangeHi 
      ; LDInfo . RSet := TRSet 
      ; Proc ( LDInfo , BitwordArrayEmpty )  

      END (* CASE *) 
    END CallWithOneSet 

; PROCEDURE CallWithTwoSets ( Set1 , Set2 : T ; Proc : TwoSetsProcTyp ) 
  RAISES ANY  
  (* Call Proc with parameters taken from Set1 and Set2. *)  

  = PROCEDURE Intermediate 
      ( READONLY DInfo1 : DissectInfo 
      ; READONLY Bitwords1 : ARRAY OF BitwordTyp 
      ) 
    RAISES ANY 

    = PROCEDURE Final 
        ( READONLY DInfo2 : DissectInfo 
        ; READONLY Bitwords2 : ARRAY OF BitwordTyp 
        ) 
      RAISES ANY 

      = BEGIN (* Final *) 
          Proc ( DInfo1 , Bitwords1 , DInfo2 , Bitwords2 ) 
        END Final 

    ; BEGIN (* Intermediate *) 
        CallWithOneSet ( Set2 , Final ) 
      END Intermediate 
    
  ; BEGIN (* CallWithTwoSets *) 
      CallWithOneSet ( Set1 , Intermediate ) 
    END CallWithTwoSets 

; PROCEDURE ConstructBitset 
    ( READONLY BitsetInfo : BitsetInfoTyp 
    ; BitwordArrayRef : BitwordArrayRefTyp 
    ; WasNewlyAllocated : BOOLEAN := TRUE 
      (* ^BitwordArrayRef was allocated during the current set operation. *)
    ) 
  : T
  (* Construct a pseudo-pointer Bitset if possible, else heap-allocate. *)

  = VAR LResultBitset : BitsetTyp 
  ; VAR LResult : T 
  ; VAR LResultWord : Word . T  

  ; BEGIN 
      IF DoPseudoPointers (* Hopefully, a compiler will fold this. *) 
      THEN 
        IF 0 <= BitsetInfo . BitsetLo 
           AND BitsetInfo . BitsetHi <= PseudoBitsetMax 
           AND BitsetInfo . Bias = 0 
        THEN (* Put BitwordArrayRef ^ [ 0 ] into a Bitset pseudo pointer. *)
          LResultWord := BitwordArrayRef ^ [ 0 ] 
        ; LResultWord 
            := Word . And 
                 ( LResultWord , GEMaskOfIElem ( BitsetInfo . BitsetLo ) )
                 (* ^Zero low garbage bits. *) 
        ; LResultWord 
            := Word . And 
                 ( LResultWord , LEMaskOfIElem ( BitsetInfo . BitsetHi ) )
                 (* ^Zero high garbage bits. *) 
        ; LResultWord := Word . LeftShift ( LResultWord , 2 ) 
        ; LResultWord := Word . Or ( LResultWord , PseudoBitsetFlagBits ) 
        ; LResult := UnsafeUtils . NULLOfInt ( LResultWord )    
        ; INC ( GPseudoBitsetCt ) 
        ; IF WasNewlyAllocated AND BitwordArrayRef # NIL 
          THEN (* This bitword array was newly allocated during the current set
                  operation.  We are abandoning it as garbage, so uncount its
                  statistics.
               *) 
            DEC ( GArrayCt ) 
          ; DEC 
              ( GArrayElemCt , VAL ( NUMBER ( BitwordArrayRef ^ ) , LONGINT ) )
          END (* IF *) 
        ; RETURN LResult 
        END (* IF *) 
      END (* IF *) 
    ; LResultBitset := NewBitset ( ) 
    ; LResultBitset . BitsetInfo := BitsetInfo 
    ; LResultBitset . BitwordArrayRef := BitwordArrayRef 
    ; RETURN LResultBitset 
    END ConstructBitset 

; PROCEDURE ConstructRangeset 
    ( Lo , Hi : ValidIElemTyp ) : T 
  (* Construct a pseudo-pointer Rangeset if possible, else heap-allocate. *)  

  = VAR LResultWord : Word . T 
  ; VAR LResult : T 

  ; BEGIN 
      IF DoPseudoPointers (* Hopefully, a compiler will fold this. *) 
      THEN 
        IF PseudoBoundMin <= Lo AND Lo <= PseudoBoundMax 
           AND PseudoBoundMin <= Hi AND Hi <= PseudoBoundMax 
        THEN (* Put Lo and Hi into a Rangeset pseudo pointer. *)     
          LResultWord := Word . LeftShift ( Lo , BitsPerPseudoBound ) 
        ; LResultWord 
            := Word . Or ( LResultWord , Word . And ( Hi , PseudoBoundMask ) ) 
        ; LResultWord := Word . LeftShift ( LResultWord , 2 ) 
        ; LResultWord := Word . Or ( LResultWord , PseudoRangeFlagBits ) 
        ; LResult := UnsafeUtils . NULLOfInt ( LResultWord )    
        ; INC ( GPseudoRangesetCt ) 
        ; RETURN LResult  
        END (* IF *)  
      END (* IF *) 
    ; LResult := NewRangeset ( Lo , Hi ) 
    ; RETURN LResult 
    END ConstructRangeset 

(* ==================== Middle level utility procedures =================== *) 

; PROCEDURE NoOf1BitsInBitword ( Bitword : BitwordTyp ) : BitCtTyp  

  = VAR LResult : CARDINAL 
  ; VAR LBitword : BitwordTyp 

  ; BEGIN 
      LResult := 0 
    ; LBitword := Bitword 
    ; INC ( LResult 
          , BitNoTable . NoOf1BitsInByte [ Word . And ( LBitword , 16_FF ) ] 
          ) 
    ; LBitword := Word . RightShift ( LBitword , 8 ) 
    ; INC ( LResult 
          , BitNoTable . NoOf1BitsInByte [ Word . And ( LBitword , 16_FF ) ] 
          ) 
    ; LBitword := Word . RightShift ( LBitword , 8 ) 
    ; INC ( LResult 
          , BitNoTable . NoOf1BitsInByte [ Word . And ( LBitword , 16_FF ) ] 
          ) 
    ; LBitword := Word . RightShift ( LBitword , 8 ) 
    ; INC ( LResult 
          , BitNoTable . NoOf1BitsInByte [ Word . And ( LBitword , 16_FF ) ] 
          ) 
    ; IF BitsPerBitword > 32 
      THEN (* Hopefully, a good compiler will eliminate this test and code
              on a 32-bit machine. 
           *) 
        LBitword := Word . RightShift ( LBitword , 8 ) 
      ; INC ( LResult 
            , BitNoTable . NoOf1BitsInByte [ Word . And ( LBitword , 16_FF ) ] 
            ) 
      ; LBitword := Word . RightShift ( LBitword , 8 ) 
      ; INC ( LResult 
            , BitNoTable . NoOf1BitsInByte [ Word . And ( LBitword , 16_FF ) ] 
            ) 
      ; LBitword := Word . RightShift ( LBitword , 8 ) 
      ; INC ( LResult 
            , BitNoTable . NoOf1BitsInByte [ Word . And ( LBitword , 16_FF ) ] 
            ) 
      ; LBitword := Word . RightShift ( LBitword , 8 ) 
      ; INC ( LResult 
            , BitNoTable . NoOf1BitsInByte [ Word . And ( LBitword , 16_FF ) ] 
            ) 
      END (* IF *) 
    ; RETURN LResult 
    END NoOf1BitsInBitword

; PROCEDURE Least1BitNoInBitword ( Bitword : BitwordTyp ) : BitCtTyp

  = VAR LBitword : BitwordTyp
  ; VAR LResult : CARDINAL := 0  

  ; BEGIN 
      LBitword := Bitword 
    ; IF LBitword = 0 
      THEN RETURN BitsPerBitword 
      ELSE (* We now know there is a 1-bit here somewhere. *) 
        IF BitsPerBitword <= 32 
           (* ^Hopefully, a smart compiler will fold this test, giving no 
              performance penalty for this code's self-adapting to 32- or
              64-bit words.
           *) 
           OR Word . And ( LBitword , 16_FFFFFFFF ) # 0 
        THEN (* It's in the right 32 bits. *) 
          LResult := 0 
        ELSE (* It must be in the left 32 bits of 64. *) 
          LResult := 32  
        ; LBitword := Word . RightShift ( LBitword , 32 ) 
        (* ^Expect "warning: value out of range" here when compiling on a  
            32-bit machine, where it's harmless because this code will not
            be executed. 
        *) 
        END (* IF *) 

      ; IF Word . And ( LBitword , 16_FFFF ) # 0 
        THEN (* It's in the right 16 bits. *) 
        ELSE (* It must be in the left 16 bits of 32. *) 
          INC ( LResult , 16 ) 
        ; LBitword := Word . RightShift ( LBitword , 16 ) 
        END (* IF *) 
      ; IF Word . And ( LBitword , 16_FF ) # 0 
        THEN (* It's in the right 8 bits. *) 
        ELSE (* It must be in the left 8 bits of 16. *) 
          INC ( LResult , 8 ) 
        ; LBitword := Word . RightShift ( LBitword , 8 ) 
        END (* IF *) 
      ; INC ( LResult 
            , BitNoTable . Least1BitNoInByte 
                [ Word . And ( LBitword , 16_FF ) ] 
            ) 
      ; RETURN LResult 
      END (* IF *) 
    END Least1BitNoInBitword 

; <* INLINE *> 
  PROCEDURE Least0BitNoInBitword ( Bitword : BitwordTyp ) : BitCtTyp

  = BEGIN 
      RETURN Least1BitNoInBitword ( Word . Not ( Bitword ) ) 
    END Least0BitNoInBitword 

; PROCEDURE Greatest1BitNoInBitword ( Bitword : BitwordTyp ) : BitCtTyp

  = VAR LBitword : BitwordTyp
  ; VAR LResult : CARDINAL := 0  

  ; BEGIN 
      LBitword := Bitword 
    ; IF LBitword = 0 
      THEN RETURN BitsPerBitword 
      ELSE (* We now know there is a 1-bit here somewhere. *) 
        IF BitsPerBitword > 32 
           (* ^Hopefully, a smart compiler will fold this test, giving no 
              performance penalty for this code's self-adapting to 32- or
              64-bit words.
           *) 
           AND Word . And ( LBitword , Word . Not ( 16_FFFFFFFF ) ) # 0 
        THEN (* It's in the left (high) 32 bits. *) 
          LResult := 32  
        ; LBitword := Word . RightShift ( LBitword , 32 ) 
        (* ^Expect "warning: value out of range" here when compiling on a  
            32-bit machine, where it's harmless because this code will not
            be executed. 
        *) 
        ELSE (* It must be in the right 32 bits of 64. *) 
          LResult := 0 
        END (* IF *) 

      ; IF Word . And ( LBitword , 16_FFFF0000 ) # 0 
        THEN (* It's in the left 16 bits. *) 
          INC ( LResult , 16 ) 
        ; LBitword := Word . RightShift ( LBitword , 16 ) 
    (*  ELSE It must be in the right 16 bits of 32. *) 
        END (* IF *) 
      ; IF Word . And ( LBitword , 16_FF00 ) # 0 
        THEN (* It's in the left 8 bits. *) 
          INC ( LResult , 8 ) 
        ; LBitword := Word . RightShift ( LBitword , 8 ) 
     (* ELSE It must be in the right 8 bits of 16. *) 
        END (* IF *) 
      ; INC ( LResult 
            , BitNoTable . Greatest1BitNoInByte 
                [ Word . And ( LBitword , 16_FF ) ] 
            ) 
      ; RETURN LResult 
      END (* IF *) 
    END Greatest1BitNoInBitword 

; <* INLINE *> 
  PROCEDURE Greatest0BitNoInBitword ( Bitword : BitwordTyp ): BitCtTyp  

  = BEGIN 
      RETURN Greatest1BitNoInBitword ( Word . Not ( Bitword ) ) 
    END Greatest0BitNoInBitword 

; PROCEDURE FillBitwordArray 
    ( NewArrayRef : BitwordArrayRefTyp 
    ; OldArrayRef : BitwordArrayRefTyp 
    ; Value : BitwordTyp 
    ) 
  (* Fill all elements of NewArrayRef ^ with Value.  But if OldArrayRef # NIL,
     assume all its elements contain Value, and use it to do the job faster. 
  *) 

  = VAR LNewLength , LOldLength , LSegmentLength : CARDINAL 
  ; VAR LSs : CARDINAL 

  ; BEGIN 
      LNewLength := NUMBER ( NewArrayRef ^ ) 
    ; IF OldArrayRef = NIL 
      THEN
        FOR RI := 0 TO LNewLength - 1 
        DO
          NewArrayRef ^ [ RI ] := Value 
        END (* FOR *)  
      ELSE 
        LOldLength := NUMBER ( OldArrayRef ^ )
      ; LSs := 0   
      ; WHILE LSs < LNewLength 
        DO 
          LSegmentLength := MIN ( LNewLength - LSs , LOldLength ) 
        ; SUBARRAY ( NewArrayRef ^ , LSs , LSegmentLength ) 
            := SUBARRAY ( OldArrayRef ^ , 0 , LSegmentLength ) 
        ; INC ( LSs , LSegmentLength ) 
        END (* WHILE *)  
      END (* IF *) 
    END FillBitwordArray 

(* Length bounds on globally-retained constant-like arrays. *) 
; CONST MinRefLength = 10 (* Bitwords *) 
; CONST MaxRefLength = 200 (* Bitwords *) 

; VAR AllZerosArrayRef : BitwordArrayRefTyp := NIL 
; VAR AllOnesArrayRef : BitwordArrayRefTyp := NIL 

; PROCEDURE ExpandBitwordArray 
    ( VAR ArrayRef : BitwordArrayRefTyp 
    ; DesiredLength : CARDINAL 
    ; Value : BitwordTyp 
    ) 
  (* Ensure that ArrayRef points to an array whose elements all contain Value,
     of length in [ MinRefLength .. MIN ( DesiredLength , MaxRefLength ) ]
     PRE: ArrayRef # NIL IMPLIES ArrayRef ^ = BitwordArrayTyp { Value , .. }  
  *) 

  = VAR LOldLength , LNewLength : CARDINAL 
  ; VAR LNewArrayRef : BitwordArrayRefTyp 

  ; BEGIN 
      IF ArrayRef = NIL 
      THEN LOldLength := 0 
      ELSE LOldLength := NUMBER ( ArrayRef ^ ) 
      END (* IF *) 
    ; IF LOldLength = 0 
         OR ( LOldLength < MaxRefLength AND LOldLength < DesiredLength ) 
      THEN (* Lazy allocate/expand an array of all Value. *) 
        LNewLength := MAX ( DesiredLength , 2 * LOldLength ) 
      ; LNewLength := MAX ( LNewLength , MinRefLength ) 
      ; LNewLength := MIN ( LNewLength , MaxRefLength ) 
      ; LNewArrayRef := NewBitwordArray ( LNewLength ) 
      ; FillBitwordArray ( LNewArrayRef , ArrayRef , Value ) 
      ; ArrayRef := LNewArrayRef 
      END (* IF *)  
    END ExpandBitwordArray 

; PROCEDURE BitwordArrayIsAllZeros ( READONLY BWArray : BitwordArrayTyp ) 
  : BOOLEAN (* Vacuously TRUE for an empty array. *) 
  (* This is hopefully faster than a coded loop through single Bitwords.  
     Even if the target machine lacks a single-instruction long memory 
     compare, we presume the compiler-generated machine-code loop for 
     whole array comparison will be tighter than can be translated from 
     a source language loop.
  *) 

  = VAR LBWLength , LSegmentLength : CARDINAL 
  ; VAR LSs : CARDINAL 

  ; BEGIN 
      LBWLength := NUMBER ( BWArray ) 
    ; ExpandBitwordArray ( AllZerosArrayRef , LBWLength , 0 ) 
    ; LSegmentLength := NUMBER ( AllZerosArrayRef ^ ) 
    ; LSs := 0 
    ; WHILE LSs < LBWLength 
      DO 
        LSegmentLength := MIN ( LBWLength , LSegmentLength ) 
      ; IF SUBARRAY ( BWArray , LSs , LSegmentLength ) 
           # SUBARRAY ( AllZerosArrayRef ^ , 0 , LSegmentLength ) 
        THEN
          RETURN FALSE 
        ELSE 
          INC ( LSs , LSegmentLength ) 
        ; DEC ( LBWLength , LSegmentLength ) 
        END (* IF *) 
      END (* WHILE *)  
    ; RETURN TRUE 
    END BitwordArrayIsAllZeros 

; PROCEDURE BitwordArrayIsAllOnes ( READONLY BWArray : BitwordArrayTyp ) 
  : BOOLEAN (* Vacuously TRUE for an empty array. *) 

  = VAR LBWLength , LSegmentLength : CARDINAL 
  ; VAR LSs : CARDINAL 

  ; BEGIN 
      LBWLength := NUMBER ( BWArray ) 
    ; ExpandBitwordArray ( AllOnesArrayRef , LBWLength , AllOnes ) 
    ; LSegmentLength := NUMBER ( AllOnesArrayRef ^ ) 
    ; LSs := 0 
    ; WHILE LSs < LBWLength 
      DO 
        LSegmentLength := MIN ( LBWLength , LSegmentLength ) 
      ; IF SUBARRAY ( BWArray , LSs , LSegmentLength ) 
           # SUBARRAY ( AllOnesArrayRef ^ , 0 , LSegmentLength ) 
        THEN
          RETURN FALSE 
        ELSE 
          INC ( LSs , LSegmentLength ) 
        ; DEC ( LBWLength , LSegmentLength ) 
        END (* IF *) 
      END (* WHILE *)  
    ; RETURN TRUE 
    END BitwordArrayIsAllOnes 

; PROCEDURE BitsetRangeIsEmpty  
    ( READONLY BSetInfo : BitsetInfoTyp 
    ; READONLY BSetBitwords : ARRAY OF BitwordTyp 
    ; RSetLo , RSetHi : ValidIElemTyp 
    ) 
  : BOOLEAN 
  (* BSet projected onto {RSetLo..RSetHi} = {}.  
     May be vacuously TRUE. 
  *) 

  = VAR LIElemLo , LIElemHi : ValidIElemTyp 
  ; VAR LLoBitwordNo , LHiBitwordNo : BitwordNoTyp 
  ; VAR LBitwordCt : CARDINAL 
  ; VAR LBitword : BitwordTyp 
  ; VAR LSs : CARDINAL 

  ; BEGIN
      RSetLo := MIN ( RSetLo , BSetInfo . BitsetHi ) 
    ; RSetHi := MAX ( RSetHi , BSetInfo . BitsetLo ) 
    ; LIElemLo := MAX ( RSetLo , BSetInfo . BitsetLo ) 
    ; LIElemHi := MIN ( RSetHi , BSetInfo . BitsetHi ) 
    ; LLoBitwordNo := BitwordNoOfIElem ( LIElemLo ) 
    ; LHiBitwordNo := BitwordNoOfIElem ( LIElemHi ) 
    ; LSs := ArraySsOfBitwordNo ( LLoBitwordNo , BSetInfo . Bias ) 
    ; LBitword := BSetBitwords [ LSs ] 
    ; LBitword := Word . And ( LBitword , GEMaskOfIElem ( LIElemLo ) ) 
    ; IF LLoBitwordNo = LHiBitwordNo 
      THEN (* Only one Bitword is involved. *) 
        LBitword := Word . And ( LBitword , LEMaskOfIElem ( LIElemHi ) ) 
      ; RETURN LBitword = 0 
      ELSIF LBitword # 0  
      THEN 
        RETURN FALSE 
      ELSE 
        INC ( LSs )
      ; INC ( LLoBitwordNo ) 
      ; LBitwordCt := LHiBitwordNo - LLoBitwordNo 
      ; IF NOT BitwordArrayIsAllZeros 
                 ( SUBARRAY ( BSetBitwords , LSs , LBitwordCt ) )  
        THEN 
          RETURN FALSE 
        ELSE 
          INC ( LSs , LBitwordCt ) 
        ; LBitword := BSetBitwords [ LSs ] 
        ; LBitword := Word . And ( LBitword , LEMaskOfIElem ( LIElemHi ) )
        ; RETURN LBitword = AllOnes 
        END (* IF *) 
      END (* IF *) 
    END BitsetRangeIsEmpty  

; PROCEDURE BitsetRangeIsFull  
    ( READONLY BSetInfo : BitsetInfoTyp 
    ; READONLY BSetBitwords : ARRAY OF BitwordTyp 
    ; RSetLo , RSetHi : ValidIElemTyp 
    ) 
  : BOOLEAN 
  (* BSet projected onto {RSetLo..RSetHi} = {RSetLo..RSetHi}.  
     May be vacuously TRUE. 
  *) 

  = VAR LIElemLo , LIElemHi : ValidIElemTyp 
  ; VAR LLoBitwordNo , LHiBitwordNo : BitwordNoTyp 
  ; VAR LBitwordCt : CARDINAL 
  ; VAR LBitword : BitwordTyp 
  ; VAR LSs : CARDINAL 

  ; BEGIN 
      RSetLo := MIN ( RSetLo , BSetInfo . BitsetHi ) 
    ; RSetHi := MAX ( RSetHi , BSetInfo . BitsetLo ) 
    ; LIElemLo := MAX ( RSetLo , BSetInfo . BitsetLo ) 
    ; LIElemHi := MIN ( RSetHi , BSetInfo . BitsetHi ) 
    ; LLoBitwordNo := BitwordNoOfIElem ( LIElemLo ) 
    ; LHiBitwordNo := BitwordNoOfIElem ( LIElemHi ) 
    ; LSs := ArraySsOfBitwordNo ( LLoBitwordNo , BSetInfo . Bias ) 
    ; LBitword := BSetBitwords [ LSs ] 
    ; LBitword := Word . Or ( LBitword , LTMaskOfIElem ( LIElemLo ) ) 
    ; IF LLoBitwordNo = LHiBitwordNo 
      THEN (* Only one Bitword is involved. *) 
        LBitword := Word . Or ( LBitword , GTMaskOfIElem ( LIElemHi ) ) 
      ; RETURN LBitword = AllOnes   
      ELSIF LBitword # AllOnes 
      THEN 
        RETURN FALSE 
      ELSE 
        INC ( LSs )
      ; INC ( LLoBitwordNo ) 
      ; LBitwordCt := LHiBitwordNo - LLoBitwordNo 
      ; IF NOT BitwordArrayIsAllOnes 
                 ( SUBARRAY ( BSetBitwords , LSs , LBitwordCt ) )  
        THEN 
          RETURN FALSE 
        ELSE 
          INC ( LSs , LBitwordCt ) 
        ; LBitword := BSetBitwords [ LSs ] 
        ; LBitword := Word . Or ( LBitword , GTMaskOfIElem ( LIElemHi ) )
        ; RETURN LBitword = AllOnes 
        END (* IF *) 
      END (* IF *) 
    END BitsetRangeIsFull  

; PROCEDURE AssignAllZerosToBitwordArray 
    ( VAR BWArray : BitwordArrayTyp ) 

  = VAR LBWLength , LSegmentLength : CARDINAL 
  ; VAR LSs : CARDINAL 

  ; BEGIN 
      LBWLength := NUMBER ( BWArray ) 
    ; ExpandBitwordArray ( AllZerosArrayRef , LBWLength , 0 ) 
    ; LSegmentLength := NUMBER ( AllZerosArrayRef ^ ) 
    ; LSs := 0 
    ; WHILE LSs < LBWLength 
      DO 
        LSegmentLength := MIN ( LBWLength , LSegmentLength ) 
      ; SUBARRAY ( BWArray , LSs , LSegmentLength ) 
          := SUBARRAY ( AllZerosArrayRef ^ , 0 , LSegmentLength ) 
      ; INC ( LSs , LSegmentLength ) 
      ; DEC ( LBWLength , LSegmentLength ) 
      END (* WHILE *)  
    END AssignAllZerosToBitwordArray 

; PROCEDURE AssignAllOnesToBitwordArray 
    ( VAR BWArray : BitwordArrayTyp ) 

  = VAR LBWLength , LSegmentLength : CARDINAL 
  ; VAR LSs : CARDINAL 

  ; BEGIN 
      LBWLength := NUMBER ( BWArray ) 
    ; ExpandBitwordArray ( AllOnesArrayRef , LBWLength , AllOnes ) 
    ; LSegmentLength := NUMBER ( AllOnesArrayRef ^ ) 
    ; LSs := 0 
    ; WHILE LSs < LBWLength 
      DO 
        LSegmentLength := MIN ( LBWLength , LSegmentLength ) 
      ; SUBARRAY ( BWArray , LSs , LSegmentLength ) 
          := SUBARRAY ( AllOnesArrayRef ^ , 0 , LSegmentLength ) 
      ; INC ( LSs , LSegmentLength ) 
      ; DEC ( LBWLength , LSegmentLength ) 
      END (* WHILE *)  
    END AssignAllOnesToBitwordArray 

; PROCEDURE LeastPresentIElemOfBSetInRange  
    ( READONLY BSetInfo : BitsetInfoTyp 
    ; READONLY BSetBitwords : ARRAY OF BitwordTyp 
    ; RSetLo , RSetHi : ValidIElemTyp 
    )
  : IElemTyp 
  (* Return the least element of BSet Intersection [RSetLo..RSetHi], 
     or IElemNull, if no such element exists. 
  
     PRE: If nonempy, [ RSetLo .. RSetHi ] is a subrange of 
          [ BSetInfo . BitsetLo .. BSetInfo . BitsetHi ].
  *) 

  = VAR LLoBitwordNo , LHiBitwordNo , LBitwordNo : BitwordNoTyp 
  ; VAR LSetSs : ArraySsTyp 
  ; VAR LBit0IElem : IElemTyp 
  ; VAR LBitword : BitwordTyp 

  ; BEGIN 
      IF RSetLo > RSetHi 
      THEN 
        RETURN IElemNull 
      ELSE 
        LLoBitwordNo := BitwordNoOfIElem ( RSetLo ) 
      ; LHiBitwordNo := BitwordNoOfIElem ( RSetHi ) 
      ; LSetSs := ArraySsOfBitwordNo ( LLoBitwordNo , BSetInfo . Bias ) 
      ; LBit0IElem := BitZeroIElemOfBitwordNo ( LLoBitwordNo ) 
      ; LBitword := BSetBitwords [ LSetSs ]  
      ; LBitword := Word . And ( LBitword , GEMaskOfIElem ( RSetLo ) ) 
      ; LBitwordNo := LLoBitwordNo 
      ; LOOP 
          IF LBitwordNo = LHiBitwordNo 
          THEN (* Last word, where we need to mask high bits. *)
            LBitword := Word . And ( LBitword , LEMaskOfIElem ( RSetHi ) ) 
          ; IF LBitword # 0 
            THEN (* The sought bit is in this Bitword. *) 
              LBit0IElem 
                := Word . Plus 
                     ( LBit0IElem 
                     , Word . Times 
                         ( LBitwordNo - LLoBitwordNo , BitsPerBitword )
                     ) 
            ; RETURN LBit0IElem + Least1BitNoInBitword ( LBitword ) 
            ELSE RETURN IElemNull 
            END (* IF *) 
          ELSIF LBitword # 0 
          THEN (* The sought bit is in this Bitword. *) 
            LBit0IElem 
              := Word . Plus 
                   ( LBit0IElem 
                   , Word . Times ( LBitwordNo - LLoBitwordNo , BitsPerBitword )
                   ) 
          ; RETURN LBit0IElem + Least1BitNoInBitword ( LBitword ) 
          ELSE (* Advance to the next word. *) 
            INC ( LSetSs )  
          ; INC ( LBitwordNo ) 
          ; LBitword := BSetBitwords [ LSetSs ]  
          END (* LOOP *) 
        END (* IF *) 
      END (* IF *) 
    END LeastPresentIElemOfBSetInRange  

; PROCEDURE LeastAbsentIElemOfBSetInRange  
    ( READONLY BSetInfo : BitsetInfoTyp 
    ; READONLY BSetBitwords : ARRAY OF BitwordTyp 
    ; RSetLo , RSetHi : ValidIElemTyp 
    )
  : IElemTyp 
  (* Return the least element in [RSetLo..RSetHi] but absent from BSet, 
     or IElemNull, if no such element exists. 
  
     PRE: If nonempy, [ RSetLo .. RSetHi ] is a subrange of 
          [ BSetInfo . BitsetLo .. BSetInfo . BitsetHi ].
  *) 

  = VAR LLoBitwordNo , LHiBitwordNo , LBitwordNo : BitwordNoTyp 
  ; VAR LSetSs : ArraySsTyp 
  ; VAR LBit0IElem : IElemTyp 
  ; VAR LBitword : BitwordTyp 

  ; BEGIN 
      IF RSetLo > RSetHi 
      THEN 
        RETURN IElemNull 
      ELSE 
        LLoBitwordNo := BitwordNoOfIElem ( RSetLo ) 
      ; LHiBitwordNo := BitwordNoOfIElem ( RSetHi ) 
      ; LSetSs := ArraySsOfBitwordNo ( LLoBitwordNo , BSetInfo . Bias ) 
      ; LBit0IElem := BitZeroIElemOfBitwordNo ( LLoBitwordNo ) 
      ; LBitword := BSetBitwords [ LSetSs ]  
      ; LBitword := Word . Or ( LBitword , LTMaskOfIElem ( RSetLo ) ) 
      ; LBitwordNo := LLoBitwordNo 
      ; LOOP 
          IF LBitwordNo = LHiBitwordNo 
          THEN (* Last word, where we need to mask high bits. *)
            LBitword := Word . Or ( LBitword , GTMaskOfIElem ( RSetHi ) ) 
          ; IF LBitword # AllOnes 
            THEN (* The sought bit is in this Bitword. *) 
              LBit0IElem 
                := Word . Plus 
                     ( LBit0IElem 
                     , Word . Times 
                         ( LBitwordNo - LLoBitwordNo , BitsPerBitword )
                     ) 
            ; RETURN LBit0IElem + Least0BitNoInBitword ( LBitword ) 
            ELSE RETURN IElemNull 
            END (* IF *) 
          ELSIF LBitword # AllOnes  
          THEN (* The sought bit is in this Bitword. *) 
            LBit0IElem 
              := Word . Plus 
                   ( LBit0IElem 
                   , Word . Times ( LBitwordNo - LLoBitwordNo , BitsPerBitword )
                   ) 
          ; RETURN LBit0IElem + Least0BitNoInBitword ( LBitword ) 
          ELSE (* Advance to the next word. *) 
            INC ( LSetSs )  
          ; INC ( LBitwordNo ) 
          ; LBitword := BSetBitwords [ LSetSs ]  
          END (* LOOP *) 
        END (* IF *) 
      END (* IF *) 
    END LeastAbsentIElemOfBSetInRange  

; PROCEDURE GreatestPresentIElemOfBSetInRange  
    ( READONLY BSetInfo : BitsetInfoTyp 
    ; READONLY BSetBitwords : ARRAY OF BitwordTyp 
    ; RSetLo , RSetHi : ValidIElemTyp 
    )
  : IElemTyp 
  (* Return the greatest element of BSet Intersection [RSetLo..RSetHi], 
     or IElemNull, if no such  element exists. 

     PRE: If nonempy, [ RSetLo .. RSetHi ] is a subrange of 
          [ BSetInfo . BitsetLo .. BSetInfo . BitsetHi ].
  *) 

  = VAR LLoBitwordNo , LHiBitwordNo , LBitwordNo : BitwordNoTyp 
  ; VAR LSetSs : ArraySsTyp 
  ; VAR LBit0IElem : IElemTyp 
  ; VAR LBitword : BitwordTyp 

  ; BEGIN 
      IF RSetLo > RSetHi 
      THEN 
        RETURN IElemNull 
      ELSE 
        LLoBitwordNo := BitwordNoOfIElem ( RSetLo ) 
      ; LHiBitwordNo := BitwordNoOfIElem ( RSetHi ) 
      ; LSetSs := ArraySsOfBitwordNo ( LHiBitwordNo , BSetInfo . Bias ) 
      ; LBit0IElem := BitZeroIElemOfBitwordNo ( LHiBitwordNo ) 
      ; LBitword := BSetBitwords [ LSetSs ]  
      ; LBitword := Word . And ( LBitword , LEMaskOfIElem ( RSetHi ) ) 
      ; LBitwordNo := LHiBitwordNo 
      ; LOOP 
          IF LBitwordNo = LLoBitwordNo 
          THEN (* Lowest Bitword, where we need to mask low bits. *)
            LBitword := Word . And ( LBitword , GEMaskOfIElem ( RSetLo ) ) 
          ; IF LBitword # 0 
            THEN (* The sought bit is in this Bitword. *) 
              LBit0IElem 
                := Word . Minus 
                     ( LBit0IElem 
                     , Word . Times 
                         ( LHiBitwordNo - LBitwordNo , BitsPerBitword )
                     ) 
            ; RETURN LBit0IElem + Greatest1BitNoInBitword ( LBitword ) 
            ELSE RETURN IElemNull 
            END (* IF *) 
          ELSIF LBitword # 0 
          THEN (* The sought bit is in this Bitword. *) 
            LBit0IElem 
              := Word . Minus 
                   ( LBit0IElem 
                   , Word . Times ( LHiBitwordNo - LBitwordNo , BitsPerBitword )
                   ) 
          ; RETURN LBit0IElem + Greatest1BitNoInBitword ( LBitword ) 
          ELSE (* Advance to the next word. *) 
            DEC ( LSetSs )  
          ; DEC ( LBitwordNo ) 
          ; LBitword := BSetBitwords [ LSetSs ]  
          END (* LOOP *) 
        END (* IF *) 
      END (* IF *) 
    END GreatestPresentIElemOfBSetInRange  

; PROCEDURE GreatestAbsentIElemOfBSetInRange  
    ( READONLY BSetInfo : BitsetInfoTyp 
    ; READONLY BSetBitwords : ARRAY OF BitwordTyp 
    ; RSetLo , RSetHi : ValidIElemTyp 
    )
  : IElemTyp 
  (* Return the greatest element in [RSetLo..RSetHi] but absent from BSet, 
     or IElemNull, if no such  element exists. 

     PRE: If nonempy, [ RSetLo .. RSetHi ] is a subrange of 
          [ BSetInfo . BitsetLo .. BSetInfo . BitsetHi ].
  *) 

  = VAR LLoBitwordNo , LHiBitwordNo , LBitwordNo : BitwordNoTyp 
  ; VAR LSetSs : ArraySsTyp 
  ; VAR LBit0IElem : IElemTyp 
  ; VAR LBitword : BitwordTyp 

  ; BEGIN 
      IF RSetLo > RSetHi 
      THEN 
        RETURN IElemNull 
      ELSE 
        LLoBitwordNo := BitwordNoOfIElem ( RSetLo ) 
      ; LHiBitwordNo := BitwordNoOfIElem ( RSetHi ) 
      ; LSetSs := ArraySsOfBitwordNo ( LHiBitwordNo , BSetInfo . Bias ) 
      ; LBit0IElem := BitZeroIElemOfBitwordNo ( LHiBitwordNo ) 
      ; LBitword := BSetBitwords [ LSetSs ]  
      ; LBitword := Word . Or ( LBitword , GTMaskOfIElem ( RSetHi ) ) 
      ; LBitwordNo := LHiBitwordNo 
      ; LOOP 
          IF LBitwordNo = LLoBitwordNo 
          THEN (* Lowest Bitword, where we need to mask low bits. *)
            LBitword := Word . Or ( LBitword , LTMaskOfIElem ( RSetLo ) ) 
          ; IF LBitword # AllOnes 
            THEN (* The sought bit is in this Bitword. *) 
              LBit0IElem 
                := Word . Minus 
                     ( LBit0IElem 
                     , Word . Times 
                         ( LHiBitwordNo - LBitwordNo , BitsPerBitword ) 
                     ) 
            ; RETURN LBit0IElem + Greatest0BitNoInBitword ( LBitword ) 
            ELSE RETURN IElemNull 
            END (* IF *) 
          ELSIF LBitword # AllOnes  
          THEN (* The sought bit is in this Bitword. *) 
            LBit0IElem 
              := Word . Minus 
                   ( LBit0IElem 
                   , Word . Times ( LHiBitwordNo - LBitwordNo , BitsPerBitword )
                   ) 
          ; RETURN LBit0IElem + Greatest0BitNoInBitword ( LBitword ) 
          ELSE (* Advance to the next word. *) 
            DEC ( LSetSs )  
          ; DEC ( LBitwordNo ) 
          ; LBitword := BSetBitwords [ LSetSs ]  
          END (* LOOP *) 
        END (* IF *) 
      END (* IF *) 
    END GreatestAbsentIElemOfBSetInRange  

; PROCEDURE TryToReuseRangeset 
    ( RSet : RangesetTyp := NIL 
    ; RSetLo , RSetHi : ValidIElemTyp  
    ) 
  : T 
  (* Return a Rangeset for [RSetLo..RSetHi].  Use RSet if possible.  
     PRE: RSet is not a pseudo-pointer.
  *)  

  = BEGIN 
      IF RSet = NIL 
         OR RSet . RangeLo # RSetLo
         OR RSet . RangeHi # RSetHi 
      THEN 
        RETURN ConstructRangeset ( Lo := RSetLo , Hi := RSetHi )
      ELSE 
        RETURN RSet
      END (* IF *) 
    END TryToReuseRangeset 

; PROCEDURE TryToReuseOneRangeset 
    ( RSet1 , RSet2 : RangesetTyp := NIL   
    ; RSetLo , RSetHi : ValidIElemTyp 
    ) 
  : T 
  (* Return a Rangeset for [RSetLo..RSetHi].  Use RSet1 or RSet2 if possible.  
     PRE: Neither RSet1 nor RSet2 is a pseudo-pointer.
  *)  

  = BEGIN 
      IF RSet1 # NIL 
         AND RSet1 . RangeLo = RSetLo
         AND RSet1 . RangeHi = RSetHi
      THEN RETURN RSet1 
      ELSIF RSet2 # NIL 
            AND RSet2 . RangeLo = RSetLo
            AND RSet2 . RangeHi = RSetHi
      THEN RETURN RSet2
      ELSE  
        RETURN ConstructRangeset ( Lo := RSetLo , Hi := RSetHi ) 
      END (* IF *) 
     END TryToReuseOneRangeset

(* =============== Construction of sets from their elements. =============== *) 

(* VISIBLE: *) 
; PROCEDURE Empty ( ) : T 
  (* Empty set. *) 

  = BEGIN (* Empty *) 
      RETURN NIL 
    END Empty

(* VISIBLE: *) 
; PROCEDURE Singleton ( Elem : ElemT ) : T 
(* Singleton set containing just Elem.  Empty set if 
   Elem not in ValidElemT 
*) 

  = VAR LResult : T 

  ; BEGIN (* Singleton *) 
      IF Elem < FIRST ( ValidElemT ) OR LAST ( ValidElemT ) < Elem  
      THEN 
        RETURN NIL 
      ELSE 
        LResult := ConstructRangeset ( Lo := ORD ( Elem ) , Hi := ORD ( Elem ) )
      ; RETURN LResult 
      END (* IF *) 
    END Singleton

(* VISIBLE: *) 
; PROCEDURE Range ( Lo , Hi : ElemT ) : T 
  (* Set containing all elements in the range [ Lo .. Hi ].  Empty set if
     either Lo or Hi is not in ValidElemT, or Lo > Hi  
  *) 

  = VAR LResult : T 

  ; BEGIN (* Range *)
      IF Lo < FIRST ( ValidElemT ) OR LAST ( ValidElemT ) < Lo 
         OR Hi < FIRST ( ValidElemT ) OR LAST ( ValidElemT ) < Hi 
         OR Lo > Hi 
      THEN
        RETURN NIL 
      ELSE 
        LResult := ConstructRangeset ( Lo := ORD ( Lo ) , Hi := ORD ( Hi ) ) 
      ; RETURN LResult 
      END (* IF *) 
    END Range 

; PROCEDURE FromArray ( READONLY Elements : ARRAY OF ElemT ) : T 
  (* Construct a set containing the valid members of Elements. 
     More efficient than repeated Includes or Unions of Singletons. 
  *) 

  = VAR LLo , LHi : IElemTyp 
  ; VAR LCard : CardTyp 
  ; VAR LResultLoBitwordNo , LResultHiBitwordNo : BitwordNoTyp 
  ; VAR LResultInfo : BitsetInfoTyp 
  ; VAR LResultBitwordArrayRef : BitwordArrayRefTyp 
  ; VAR LResult : T 
  ; VAR LBitword : BitwordTyp 
  ; VAR LResultSs : CARDINAL 
  ; VAR LBitsHash : HashTyp 

  ; BEGIN 
    (* First pass: Ascertain bounds and cardinality. *) 
      LLo := IElemNull 
    ; LHi := IElemNull 
    ; LCard := 0 
    ; FOR RI := FIRST ( Elements ) TO LAST ( Elements ) 
      DO 
        WITH WIElem = ORD ( Elements [ RI ] )  
        DO 
          IF WIElem # IElemNull 
          THEN
            LCard := Word . Plus ( LCard , 1 ) 
          ; IF LLo = IElemNull OR WIElem < LLo THEN LLo := WIElem END 
          ; IF LHi = IElemNull OR WIElem > LHi THEN LHi := WIElem END 
          END (* IF *) 
        END (* WITH *) 
      END (* FOR *) 
    (* Second pass: construct the set. *) 
    ; IF LCard = 0 
      THEN RETURN NIL 
      ELSIF Word . Plus ( 1 , Word . Minus ( LHi , LLo ) ) = LCard 
      THEN (* It's a range. *) 
        LResult := ConstructRangeset ( LLo , LHi ) 
      ; RETURN LResult 
      ELSE (* It has to be a Bitset. *) 
        LResultInfo . Bias := BitwordNoOfIElem ( LLo ) 
      ; LResultInfo . BitsetLo := LLo 
      ; LResultInfo . BitsetHi := LHi 
      ; LResultInfo . Card := LCard 
      ; LResultLoBitwordNo := BitwordNoOfIElem ( LLo )  
      ; LResultHiBitwordNo := BitwordNoOfIElem ( LHi )
      ; LResultBitwordArrayRef 
          := NewBitwordArray ( LResultHiBitwordNo - LResultLoBitwordNo + 1 ) 
      ; LResultSs := 0 
      ; LBitword := LTMaskOfIElem ( LLo ) (* Set low garbage bits. *)    
      ; LBitsHash := LBitword 
      ; LResultBitwordArrayRef ^ [ 0 ] := LBitword 
      ; LResultSs := ArraySsOfIElem ( LHi , LResultInfo . Bias ) 
      ; LBitword := GTMaskOfIElem ( LHi ) (* Set high garbage bits. *) 
      ; LBitsHash := Word . Xor ( LBitsHash , LBitword ) 
      ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword 
      ; FOR RI := FIRST ( Elements ) TO LAST ( Elements ) 
        DO 
          WITH WIElem = ORD ( Elements [ RI ] )  
          DO 
            IF WIElem # IElemNull 
            THEN 
              LResultSs := ArraySsOfIElem ( WIElem , LResultInfo . Bias ) 
            ; WITH WBitword = LResultBitwordArrayRef ^ [ LResultSs ] 
              DO 
                LBitword := BitMaskOfIElem ( WIElem ) 
              ; LBitsHash := Word . Xor ( LBitsHash , LBitword ) 
              ; WBitword := Word . Or ( WBitword , LBitword ) 
              END (* WITH *) 
            END (* IF *) 
          END (* WITH *) 
        END (* FOR *) 
      ; StoreBitsetHashAbs ( LResultInfo , LBitsHash ) 
      ; LResult := ConstructBitset ( LResultInfo , LResultBitwordArrayRef ) 
      ; RETURN LResult 
      END (* IF *) 
    END FromArray 

(* ================================= Union ================================  *)

; PROCEDURE UnionUntouchingOrderedRanges 
    ( RSet1Lo , RSet1Hi , RSet2Lo , RSet2Hi : ValidIElemTyp ) 
  : T 
  (* Union of two ranges. 
     RSet1 is lower. 
     The ranges have at least one element between them that is not a member
     of either.
  *) 

  (* PRE: RSet1Lo <= RSet1Hi.
     PRE: RSet2Lo <= RSet2Hi.
     PRE: RSet1Hi + 1 < RSet2Lo.
  *) 

  = VAR LResultInfo : BitsetInfoTyp 
  ; VAR LResultBitwordArrayRef : BitwordArrayRefTyp 
  ; VAR LResult : T
  ; VAR LSet1HiArraySs : CARDINAL  
  ; VAR LSet2LoArraySs , LSet2HiArraySs : CARDINAL 
  ; VAR LBitwordCt : CARDINAL 
  ; VAR LBitsHash : HashTyp  
  ; VAR LBitword : BitwordTyp  

  ; BEGIN 
      LResultInfo . Bias := BitwordNoOfIElem ( RSet1Lo ) 
    ; LSet1HiArraySs := ArraySsOfIElem ( RSet1Hi , LResultInfo . Bias ) 
    ; LSet2LoArraySs := ArraySsOfIElem ( RSet2Lo , LResultInfo . Bias ) 
    ; LSet2HiArraySs := ArraySsOfIElem ( RSet2Hi , LResultInfo . Bias ) 
    ; LResultInfo . BitsetLo := RSet1Lo 
    ; LResultInfo . BitsetHi := RSet2Hi 
    ; LResultInfo . Card 
        := Word . Plus 
             ( Word . Plus 
                 ( Word . Minus ( RSet1Hi , RSet1Lo )  
                 , Word . Minus ( RSet2Hi , RSet2Lo )
                 ) 
             , 2
             )  
    ; LResultBitwordArrayRef := NewBitwordArray ( LSet2HiArraySs + 1 ) 
    ; AssignAllOnesToBitwordArray 
        ( SUBARRAY ( LResultBitwordArrayRef ^ , 0 , LSet1HiArraySs  ) )  
    ; WITH WWord = LResultBitwordArrayRef ^ [ LSet1HiArraySs ] 
      DO
        WWord := LEMaskOfIElem ( RSet1Hi )  
      ; IF LSet1HiArraySs = LSet2LoArraySs
        THEN 
          WWord := Word . Or ( WWord , GEMaskOfIElem ( RSet2Lo ) )
        ; LBitsHash := WWord 
        ELSE 
          AssignAllZerosToBitwordArray
            ( SUBARRAY 
                ( LResultBitwordArrayRef ^ 
                , LSet1HiArraySs + 1 
                , LSet2LoArraySs - LSet1HiArraySs - 1  
                ) 
            ) 
        ; LBitword := GEMaskOfIElem ( RSet2Lo )  
        ; LBitsHash := Word . Xor ( WWord , LBitword ) 
        ; LResultBitwordArrayRef^ [ LSet2LoArraySs ] := LBitword 
        END (* IF *)  
      END (* WITH *) 
    ; LBitwordCt := LSet2HiArraySs - LSet2LoArraySs 
    ; AssignAllOnesToBitwordArray 
        ( SUBARRAY 
            ( LResultBitwordArrayRef ^ 
            , LSet2LoArraySs + 1 
            , LBitwordCt 
            ) 
        ) 
    ; IF ( LSet1HiArraySs + LBitwordCt ) MOD 2 # 0 
      THEN 
        LBitsHash := Word . Not ( LBitsHash ) 
      END (* IF *) 
    ; StoreBitsetHashAbs ( LResultInfo , LBitsHash ) 
    ; LResult := ConstructBitset ( LResultInfo , LResultBitwordArrayRef ) 
    ; RETURN LResult 
    END UnionUntouchingOrderedRanges 

; PROCEDURE UnionOrderedDisjointBitwordsBitsetRange
    ( READONLY BSet1Info : BitsetInfoTyp 
    ; READONLY BSet1Bitwords : ARRAY OF BitwordTyp 
    ; RSet2Lo , RSet2Hi : ValidIElemTyp 
    ) 
  : T 
  (* Union of a Bitset and a range.  
     The two do not share a Bitword with the same BitwordNo.
     The range is higher. 
  *) 

  (* PRE: No shared Bitwords.
     PRE: Bitset is low. 
  *) 

  = VAR LSetLoBitwordNo , LSetHiBitwordNo : BitwordNoTyp 
  ; VAR LRSetLoBitwordNo , LRSetHiBitwordNo : BitwordNoTyp 
  ; VAR LSetSs , LResultSs : ArraySsTyp 
  ; VAR LResultInfo : BitsetInfoTyp 
  ; VAR LResultBitwordArrayRef : BitwordArrayRefTyp 
  ; VAR LResult : T
  ; VAR LBitwordCt : CARDINAL 
  ; VAR LBitword : BitwordTyp 
  ; VAR LHashIncr : HashTyp 
  ; VAR LHashIsComputable : BOOLEAN 

  ; BEGIN 
    (* Result can be neither empty nor all ones and is guaranteed to have
       a 1-bit at each end. *) 
      LSetLoBitwordNo := BitwordNoOfIElem ( BSet1Info . BitsetLo ) 
    ; LSetHiBitwordNo := BitwordNoOfIElem ( BSet1Info . BitsetHi ) 
    ; LRSetLoBitwordNo := BitwordNoOfIElem ( RSet2Lo ) 
    ; LRSetHiBitwordNo := BitwordNoOfIElem ( RSet2Hi ) 
    ; LResultInfo . BitsetLo := BSet1Info . BitsetLo 
    ; LResultInfo . BitsetHi := RSet2Hi 
    ; LResultInfo . Bias := LSetLoBitwordNo 
    ; LBitwordCt := LRSetHiBitwordNo - LSetLoBitwordNo + 1 
    ; LResultBitwordArrayRef := NewBitwordArray ( LBitwordCt ) 
    ; LSetSs := ArraySsOfBitwordNo ( LSetLoBitwordNo , BSet1Info . Bias ) 
    ; RecoverBitsHash 
        ( BSet1Info , (*VAR*) LHashIsComputable , (*VAR*) LHashIncr ) 

    (* Low Bitword of BSet1: *) 
    ; LBitword := BSet1Bitwords [ LSetSs ] 
    ; LBitword 
        := Word . Or ( LBitword , LTMaskOfIElem ( BSet1Info . BitsetLo ) ) 
      (* Set low garbage bits of BSet1. *)
    ; IF LSetLoBitwordNo = LSetHiBitwordNo 
      THEN (* This is the only Bitword of BSet1. *) 
        LBitword 
          := Word . Or ( LBitword , GTMaskOfIElem ( BSet1Info . BitsetHi ) )
        (* Temporarily set BSet1 high garbage bits. *) 
      ; LHashIncr := Word . Xor ( LHashIncr , LBitword ) (* Remove old. *)  
      ; LBitword 
          := Word . And ( LBitword , LEMaskOfIElem ( BSet1Info . BitsetHi ) )
        (* Zero BSet1 high garbage bits -- nonGarbage in result. *) 
      ; LHashIncr := Word . Xor ( LHashIncr , LBitword ) (* Include new. *)  
      ; LResultBitwordArrayRef ^ [ 0 ] := LBitword 
      ; LResultSs := 1 
      ELSE (* Do the rest of the Bitwords of BSet1. *) 
        LResultBitwordArrayRef ^ [ 0 ] := LBitword 
      ; INC ( LSetSs ) 
      ; LBitwordCt := LSetHiBitwordNo - LSetLoBitwordNo - 1 
      ; SUBARRAY ( LResultBitwordArrayRef ^ , 1 , LBitwordCt ) 
          := SUBARRAY ( BSet1Bitwords , LSetSs , LBitwordCt ) 
      ; INC ( LSetSs , LBitwordCt ) 
      ; LResultSs := 1 + LBitwordCt  
      ; LBitword := BSet1Bitwords [ LSetSs ] 
      ; LBitword 
          := Word . Or ( LBitword , GTMaskOfIElem ( BSet1Info . BitsetHi ) )
        (* Temporarily set BSet1 high garbage bits. *) 
      ; LHashIncr := Word . Xor ( LHashIncr , LBitword ) (* Remove old. *)  
      ; LBitword 
          := Word . And ( LBitword , LEMaskOfIElem ( BSet1Info . BitsetHi ) )
        (* Zero BSet1 high garbage bits -- nonGarbage in result. *) 
      ; LHashIncr := Word . Xor ( LHashIncr , LBitword ) (* Include new. *)  
      ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword 
      ; INC ( LResultSs ) 
      END (* IF *) 

    (* Zero Bitwords between BSet1 and the range. *) 
    ; LBitwordCt := LRSetLoBitwordNo - LSetHiBitwordNo - 1 
    ; AssignAllZerosToBitwordArray 
        ( SUBARRAY 
            ( LResultBitwordArrayRef ^ , LResultSs , LBitwordCt )
        ) 
    ; INC ( LResultSs , LBitwordCt ) 

    (* Low Bitword of range, containing RSet2Lo *) 
    ; LBitword:= GEMaskOfIElem ( RSet2Lo ) 
      (* ^If this is also LRSetHiBitwordNo, this will give ones in the 
         result garbage bits. 
      *) 
    ; LHashIncr := Word . Xor ( LHashIncr , LBitword ) (* Include new. *)  
    ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword 
    ; INC ( LResultSs )

    (* Middle and high Bitwords containing the range. *)  
    ; LBitwordCt := LRSetHiBitwordNo - LRSetLoBitwordNo 
    ; IF LBitwordCt MOD 2 # 0 
      THEN 
        LHashIncr := Word . Not ( LHashIncr ) 
      END (* IF *) 
    ; AssignAllOnesToBitwordArray 
        ( SUBARRAY ( LResultBitwordArrayRef ^ , LResultSs , LBitwordCt ) )
    ; INC ( LResultSs , LBitwordCt ) 
    ; <* ASSERT LResultSs = LAST ( LResultBitwordArrayRef ^ ) + 1 *> 

      IF BSet1Info . Card = CardUnknown 
      THEN 
        LResultInfo . Card := CardUnknown 
      ELSE 
        LResultInfo . Card 
          := Word . Plus 
               ( BSet1Info . Card 
               , Word . Plus ( Word . Minus ( RSet2Hi , RSet2Lo ) , 1 ) 
               )
      END (* IF *)  
    ; StoreBitsetHashIncr ( LResultInfo , LHashIncr , LHashIsComputable )  
    ; LResult := ConstructBitset ( LResultInfo , LResultBitwordArrayRef ) 
    ; RETURN LResult 
    END UnionOrderedDisjointBitwordsBitsetRange

; PROCEDURE UnionOrderedDisjointBitwordsRangeBitset
    ( RSet1Lo , RSet1Hi : ValidIElemTyp 
    ; READONLY BSet2Info : BitsetInfoTyp 
    ; READONLY BSet2Bitwords : ARRAY OF BitwordTyp 
    ) 
  : T 
  (* Union of a range, given by its bounds, and a Bitset.  The two do
     not share a BitwordNo, and the range is lower. 
  *) 

  (* PRE: No shared Bitwords. 
     PRE: Bounds are low. 
  *) 

  = VAR LSetLoBitwordNo , LSetHiBitwordNo : BitwordNoTyp 
  ; VAR LRSetLoBitwordNo , LRSetHiBitwordNo : BitwordNoTyp 
  ; VAR LSetSs , LResultSs : ArraySsTyp 
  ; VAR LResultInfo : BitsetInfoTyp 
  ; VAR LResultBitwordArrayRef : BitwordArrayRefTyp 
  ; VAR LResult : T 
  ; VAR LBitwordCt : CARDINAL 
  ; VAR LBitword : BitwordTyp 
  ; VAR LHashIncr : HashTyp 
  ; VAR LHashIsComputable : BOOLEAN 

  ; BEGIN 
    (* Result can be neither empty nor all ones and is guaranteed to have a
       1-bit at each end. 
    *) 
      LRSetLoBitwordNo := BitwordNoOfIElem ( RSet1Lo ) 
    ; LRSetHiBitwordNo := BitwordNoOfIElem ( RSet1Hi ) 
    ; LSetLoBitwordNo := BitwordNoOfIElem ( BSet2Info . BitsetLo ) 
    ; LSetHiBitwordNo := BitwordNoOfIElem ( BSet2Info . BitsetHi ) 
    ; LResultInfo . BitsetLo := RSet1Lo 
    ; LResultInfo . BitsetHi := BSet2Info . BitsetHi 
    ; LResultInfo . Bias := LRSetLoBitwordNo 
    ; LBitwordCt := LSetHiBitwordNo - LRSetLoBitwordNo + 1   
    ; LResultBitwordArrayRef := NewBitwordArray ( LBitwordCt ) 
    ; RecoverBitsHash 
        ( BSet2Info , (*VAR*) LHashIsComputable , (*VAR*) LHashIncr ) 

    (* Low and middle Bitwords containing the bounds. *) 
    ; LBitwordCt := LRSetHiBitwordNo - LRSetLoBitwordNo  
    ; IF LBitwordCt MOD 2 # 0 
      THEN 
        LHashIncr := Word . Not ( LHashIncr ) 
      END (* IF *) 
    ; AssignAllOnesToBitwordArray 
        ( SUBARRAY ( LResultBitwordArrayRef ^ , 0 , LBitwordCt ) ) 
    ; LResultSs := LBitwordCt  

    (* High Bitword containing RSet1Hi. *) 
    ; LBitword := LEMaskOfIElem ( RSet1Hi )
      (* ^If this is also the low and only Bitword of the range, this
         will give ones for the low result garbage bits.
      *)  
    ; LHashIncr := Word . Xor ( LHashIncr , LBitword ) (* Include new. *)  
    ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword 
    ; INC ( LResultSs ) 

    (* Zero Bitwords between the range and BSet2. *) 
    ; LBitwordCt := LSetLoBitwordNo - LRSetHiBitwordNo - 1 
    ; AssignAllZerosToBitwordArray 
        ( SUBARRAY ( LResultBitwordArrayRef ^ , LResultSs , LBitwordCt ) ) 
    ; INC ( LResultSs , LBitwordCt ) 

    (* Low Bitword from BSet2. *) 
    ; LSetSs := ArraySsOfBitwordNo ( LSetLoBitwordNo , BSet2Info . Bias ) 
    ; LBitword := BSet2Bitwords [ LSetSs ] 
    ; LBitword 
        := Word . Or ( LBitword , LTMaskOfIElem ( BSet2Info . BitsetLo ) )
      (* Set BSet2 low garbage bits. *) 
    ; IF LSetLoBitwordNo = LSetHiBitwordNo 
      THEN (* And this is also the high and only Bitword from BSet2. *) 
        LBitword 
          := Word . Or ( LBitword , GTMaskOfIElem ( BSet2Info . BitsetHi ) )
        (* Set result high garbage bits. *) 
      ; LHashIncr := Word . Xor ( LHashIncr , LBitword ) (* Remove old. *)  
      ; LBitword 
          := Word . And ( LBitword , GEMaskOfIElem ( BSet2Info . BitsetLo ) )
        (* Zero BSet2 low garbage bits--nonGarbage in result. *) 
      ; LHashIncr := Word . Xor ( LHashIncr , LBitword ) (* Include new. *)  
      ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword 
      ; INC ( LResultSs ) 
      ELSE (* Multiple words from BSet2. *) 
        LHashIncr := Word . Xor ( LHashIncr , LBitword ) (* Remove old. *)  
      ; LBitword 
          := Word . And ( LBitword , GEMaskOfIElem ( BSet2Info . BitsetLo ) )
        (* Zero BSet2 low garbage bits--nonGarbage in result. *) 
      ; LHashIncr := Word . Xor ( LHashIncr , LBitword ) (* Include new. *)  
      ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword 
      ; INC ( LSetSs ) 
      ; INC ( LResultSs ) 
      (* Do the rest of the Bitwords from BSet2. *) 
      ; LBitwordCt := LSetHiBitwordNo - LSetLoBitwordNo - 1 
      ; SUBARRAY ( LResultBitwordArrayRef ^ , LResultSs , LBitwordCt ) 
          := SUBARRAY ( BSet2Bitwords , LSetSs , LBitwordCt ) 
      ; INC ( LSetSs , LBitwordCt ) 
      ; INC ( LResultSs , LBitwordCt ) 
      ; LBitword := BSet2Bitwords [ LSetSs ] 
      ; LBitword 
          := Word . Or ( LBitword , GTMaskOfIElem ( BSet2Info . BitsetHi ) )
        (* Set result high  garbage bits. *) 
      (* No change in hash contribution. *) 
      ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword 
      ; INC ( LResultSs ) 
      END (* IF *) 
    ; <* ASSERT LResultSs = LAST ( LResultBitwordArrayRef ^ ) + 1 *> 

      IF BSet2Info . Card = CardUnknown 
      THEN 
        LResultInfo . Card := CardUnknown 
      ELSE 
        LResultInfo . Card 
          := Word . Plus 
               ( BSet2Info . Card 
               , Word . Plus ( Word . Minus ( RSet1Hi , RSet1Lo ) , 1 )
               ) 
      END (* IF *)  
    ; StoreBitsetHashIncr ( LResultInfo , LHashIncr , LHashIsComputable )  
    ; LResult := ConstructBitset ( LResultInfo , LResultBitwordArrayRef ) 
    ; RETURN LResult 
    END UnionOrderedDisjointBitwordsRangeBitset

; PROCEDURE UnionBitsetRange 
    ( READONLY BSetInfo : BitsetInfoTyp 
    ; READONLY BSetBitwords : ARRAY OF BitwordTyp 
    ; BSet : BitsetTyp 
    ; RSetLo , RSetHi : ValidIElemTyp ; RSet : RangesetTyp := NIL 
    ) 
  : T 
  (* Union of a Bitset and a Range. *) 

  (* PRE: BSet # NIL IMPLIES BSetInfo and BSetBitwords come from BSet. 
     PRE: RSet # NIL IMPLIES RSetLo and RSetHi come from RSet. 
     PRE: RSet is nonempty. 
  *) 

  = VAR LBSetLoBitwordNo , LBSetHiBitwordNo : BitwordNoTyp 
  ; VAR LRSetLoBitwordNo , LRSetHiBitwordNo : BitwordNoTyp 
  ; VAR LResultLoBitwordNo , LResultHiBitwordNo : BitwordNoTyp 
  ; VAR LOverlapLoBitwordNo , LOverlapHiBitwordNo : BitwordNoTyp 
  ; VAR LResultLoIElem , LResultHiIElem : ValidIElemTyp 
  ; VAR LResultBias : BiasTyp 
  ; VAR LSetSs , LResultSs : INTEGER (* Could be negative. *)  
  ; VAR LResultCard : CardTyp 
  ; VAR LResultInfo : BitsetInfoTyp 
  ; VAR LResultBitwordArrayRef : BitwordArrayRefTyp 
  ; VAR LResult : T 
  ; VAR LBitwordCt : CARDINAL 
  ; VAR LBitword , LSetBitword , LRangeBitword , LReuseBitword : BitwordTyp 
  ; VAR LMask : BitwordTyp 
  ; VAR LHashIncr , LHashRemove , LHashAbs : HashTyp := 0  
  ; VAR LAllResultBitsAreOnes : BOOLEAN 
  ; VAR LResultCardIsComputable : BOOLEAN 
  ; VAR LCanReuseBitwordArray : BOOLEAN 
  ; VAR LHashIncrIsComputable , LHashAbsIsComputable : BOOLEAN 

  ; BEGIN 
      IF RSetLo <= BSetInfo . BitsetLo 
         AND BSetInfo . BitsetHi <= RSetHi 
      THEN (* Range completely overlaps Bitset. *)  
        RETURN TryToReuseRangeset ( RSet , RSetLo , RSetHi ) 
      ELSE
        INC ( GUnionCt ) 
      ; LRSetLoBitwordNo := BitwordNoOfIElem ( RSetLo ) 
      ; LRSetHiBitwordNo := BitwordNoOfIElem ( RSetHi ) 
      ; LBSetLoBitwordNo := BitwordNoOfIElem ( BSetInfo . BitsetLo ) 
      ; LBSetHiBitwordNo := BitwordNoOfIElem ( BSetInfo . BitsetHi ) 
      ; IF LBSetHiBitwordNo < LRSetLoBitwordNo 
        THEN (* Disjoint Bitwords, BSet is low. *)  
          RETURN UnionOrderedDisjointBitwordsBitsetRange
                   ( BSetInfo , BSetBitwords , RSetLo , RSetHi ) 
        ELSIF LRSetHiBitwordNo < LBSetLoBitwordNo 
        THEN (* Disjoint Bitwords, range is low. *) 
          RETURN UnionOrderedDisjointBitwordsRangeBitset
                   ( RSetLo , RSetHi , BSetInfo , BSetBitwords ) 
        ELSE (* There is an overlapping Bitword. This is our main case. *) 
          LResultLoBitwordNo := MIN ( LRSetLoBitwordNo , LBSetLoBitwordNo ) 
        ; LResultHiBitwordNo := MAX ( LRSetHiBitwordNo , LBSetHiBitwordNo ) 
        ; LOverlapHiBitwordNo := MIN ( LRSetHiBitwordNo , LBSetHiBitwordNo ) 
        ; LResultLoIElem := MIN ( RSetLo , BSetInfo . BitsetLo ) 
        ; LResultHiIElem := MAX ( RSetHi , BSetInfo . BitsetHi ) 
        ; LResultBitwordArrayRef 
            := NewBitwordArray (LResultHiBitwordNo - LResultLoBitwordNo + 1 )
        ; LResultBias := LResultLoBitwordNo 
        ; LAllResultBitsAreOnes := TRUE (* May be refuted later. *) 
        ; LCanReuseBitwordArray 
            := ArraySsOfBitwordNo ( LResultLoBitwordNo , BSetInfo . Bias ) >= 0 
          (* ^May be refuted later. *) 
        ; LCanReuseBitwordArray 
            := LCanReuseBitwordArray
               AND ArraySsOfBitwordNo ( LResultHiBitwordNo , BSetInfo . Bias ) 
                   <= LAST ( BSetBitwords ) 
          (* ^May still be refuted later. *) 
        ; LResultCard 
            := Word . Minus ( 0 , BitNo ( LResultLoIElem ) ) 
               (* ^Discount result lo garbage bits. *)
        ; LResultCard 
            := Word . Minus  
                 ( LResultCard , BitsPerBitword - 1 - BitNo ( LResultHiIElem ) )
                 (* ^Discount result high garbage bits. *) 
        ; LResultCardIsComputable := TRUE (* May be refuted later. *) 
        ; RecoverBitsHash 
            ( BSetInfo  
            , (*VAR*) LHashIncrIsComputable (* Refutable. *)  
            , (*VAR*) LHashIncr 
            )
        ; LHashAbsIsComputable := TRUE (* Refutable. *)  

        (* Handle the low end of the result, through the overlap word: *) 

        ; IF LRSetLoBitwordNo < LBSetLoBitwordNo  
          THEN (* There are low words from the range. *) 
            LBitwordCt := LBSetLoBitwordNo - LRSetLoBitwordNo 
          ; IF LBitwordCt MOD 2 = 0 
            THEN 
              LHashAbs := 0 
            ELSE 
              LHashIncr := Word . Not ( LHashIncr ) 
            ; LHashAbs := AllOnes 
            END 
          ; AssignAllOnesToBitwordArray 
              ( SUBARRAY 
                  ( LResultBitwordArrayRef ^ , 0 , LBitwordCt )
              ) 
          ; LResultCard 
              := Word . Plus 
                   ( LResultCard 
                   , Word . Times ( LBitwordCt , BitsPerBitword ) 
                   )
          ; IF LCanReuseBitwordArray 
            THEN
              LSetSs 
                := ArraySsOfBitwordNo ( LResultLoBitwordNo , BSetInfo . Bias )  
            ; LBitword := BSetBitwords [ LSetSs ] 
            ; LBitword 
                := Word . Or ( LBitword , LTMaskOfIElem ( LResultLoIElem ) )
            ; LCanReuseBitwordArray := LBitword = AllOnes 
            ; INC ( LSetSs ) 
            ; LCanReuseBitwordArray 
                := LCanReuseBitwordArray 
                   AND BitwordArrayIsAllOnes 
                         ( SUBARRAY 
                             ( BSetBitwords 
                             , LSetSs 
                             , LBitwordCt - 1 
                             )
                         ) 
            END (* IF *)  
          ; LResultSs := LBitwordCt  
          ; LSetSs := ArraySsOfBitwordNo ( LBSetLoBitwordNo , BSetInfo . Bias )
          ; LSetBitword := BSetBitwords [ LSetSs ] 
          ; LReuseBitword := LSetBitword 
          ; LHashRemove 
              := Word . Or 
                   ( LSetBitword , LTMaskOfIElem ( BSetInfo . BitsetLo ) ) 
            (* ^Set BSet low garbage bits for removal from LHashIncr. *) 
          ; LSetBitword 
              := Word . And 
                   ( LSetBitword , GEMaskOfIElem ( BSetInfo . BitsetLo ) ) 
            (* ^Zero BSet low garbage bits for result. *) 
          ; LRangeBitword := AllOnes 
          ; LOverlapLoBitwordNo := LBSetLoBitwordNo 
          ELSIF LBSetLoBitwordNo < LRSetLoBitwordNo 
          THEN (* There are low words from BSet only.  *) 
          (* The lowest result word, from BSet: *) 
            LSetSs := ArraySsOfBitwordNo ( LBSetLoBitwordNo , BSetInfo . Bias )
          ; LBitword := BSetBitwords [ LSetSs ]
          ; LBitword 
             := Word . Or ( LBitword , LTMaskOfIElem ( LResultLoIElem ) )
            (* ^Set result low garbage bits, which are also g-bits of BSet. *)
          (* No change to LHashIncr. *) 
          ; LHashAbs := LBitword    
          ; LResultBitwordArrayRef ^ [ 0 ] := LBitword 
          ; LAllResultBitsAreOnes 
              := LAllResultBitsAreOnes AND LBitword = AllOnes 
          ; LResultCard 
              := Word . Plus ( LResultCard , NoOf1BitsInBitword ( LBitword ) ) 
          ; INC ( LSetSs ) 
          (* Copy any intermediate BSet words below the overlap. *)  
          ; LBitwordCt := LRSetLoBitwordNo - LBSetLoBitwordNo - 1  
          ; IF LBitwordCt > 0 
            THEN 
              WITH WResultSub 
                   = SUBARRAY ( LResultBitwordArrayRef ^ , 1 , LBitwordCt )
              DO WResultSub  
                   := SUBARRAY 
                        ( BSetBitwords , LSetSs , LBitwordCt )
              ; LAllResultBitsAreOnes 
                  := LAllResultBitsAreOnes 
                     AND BitwordArrayIsAllOnes ( WResultSub )
              ; INC ( LSetSs , LBitwordCt ) 
              END (* WITH *) 
            ; LResultCardIsComputable := FALSE 
            ; LHashAbsIsComputable := FALSE 
            END (* IF *) 
          ; LResultSs := 1 + LBitwordCt 
          (* The low overlap Bitword. *) 
          ; LSetBitword := BSetBitwords [ LSetSs ] 
          ; LReuseBitword := LSetBitword 
          ; LHashRemove := LSetBitword 
          (* Postpone handling possible hi garbage bits. *)
          ; LRangeBitword := GEMaskOfIElem ( RSetLo ) 
          ; LOverlapLoBitwordNo := LRSetLoBitwordNo 
          ELSE (* Same low Bitword from both BSet and bounds. *)  
            LSetSs := ArraySsOfBitwordNo ( LBSetLoBitwordNo , BSetInfo . Bias )
          ; LSetBitword := BSetBitwords [ LSetSs ] 
          ; LReuseBitword := LSetBitword 
          ; LHashRemove 
              := Word . Or 
                   ( LSetBitword , LTMaskOfIElem ( BSetInfo . BitsetLo ) ) 
            (* ^Set BSet low garbage bits. *) 
          ; LHashAbs:= 0 
          ; LSetBitword 
              := Word . And 
                   ( LSetBitword , GEMaskOfIElem ( BSetInfo . BitsetLo ) ) 
            (* ^Remove BSet low garbage bits. *) 
          (* Postpone handling possible hi garbage bits. *)
          ; LRangeBitword := GEMaskOfIElem ( RSetLo ) 
            (* ^Omit bounds low garbage bits. *) 
          ; LOverlapLoBitwordNo := LRSetLoBitwordNo 
          ; LResultSs := 0 
          END (* IF *) 

        (* Finish the low overlapping word: *) 
        ; IF LOverlapLoBitwordNo = LBSetHiBitwordNo 
          THEN (* This is also the hi Bitword.  Handle hi garbage bits now. *)
            LHashRemove 
              := Word . Or 
                   ( LHashRemove , GTMaskOfIElem ( BSetInfo . BitsetHi ) )
            (* ^Set BSet high garbage bits. *) 
          ; LSetBitword 
              := Word . And 
                   ( LSetBitword , LEMaskOfIElem ( BSetInfo . BitsetHi ) ) 
            (* ^Remove BSet high garbage bits. *) 
          END (* IF *) 
        ; IF LOverlapLoBitwordNo = LRSetHiBitwordNo 
          THEN
            LRangeBitword 
              := Word . And ( LRangeBitword , LEMaskOfIElem ( RSetHi ) ) 
            (* ^Remove bounds high garbage bits. *) 
          END (* IF *) 
        ; LBitword := Word . Or ( LSetBitword , LRangeBitword ) 
        ; IF LOverlapLoBitwordNo = LResultLoBitwordNo 
          THEN 
            LMask := LTMaskOfIElem ( LResultLoIElem ) 
          ; LBitword := Word . Or ( LBitword , LMask )
            (* ^Set low garbage bits of result. *) 
          ; LReuseBitword := Word . Or ( LReuseBitword , LMask )
            (* ^Set low garbage bits of LReuseBitword, for comparison. *) 
          END (* IF *) 
        ; IF LOverlapLoBitwordNo = LResultHiBitwordNo 
          THEN 
            LMask := GTMaskOfIElem ( LResultHiIElem ) 
          ; LBitword := Word . Or ( LBitword , LMask )
            (* ^Set high garbage bits of result. *) 
          ; LReuseBitword := Word . Or ( LReuseBitword , LMask )
            (* ^Set high garbage bits of LReuseBitword, for comparison. *) 
          END (* IF *) 
        ; LAllResultBitsAreOnes 
            := LAllResultBitsAreOnes AND LBitword = AllOnes 
        ; LHashIncr := Word . Xor ( LHashIncr , LHashRemove ) (* Remove. *) 
        ; LHashIncr := Word . Xor ( LHashIncr , LBitword ) (* Add. *) 
        ; LHashAbs := Word . Xor ( LHashAbs , LBitword ) (* Add. *) 
        ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword 
        ; IF LResultCardIsComputable 
          THEN 
            LResultCard 
              := Word . Plus ( LResultCard , NoOf1BitsInBitword ( LBitword ) ) 
          END (* IF *) 
        ; LCanReuseBitwordArray 
            := LCanReuseBitwordArray AND LReuseBitword = LBitword 
        ; INC ( LSetSs ) 
        ; INC ( LResultSs ) 

        (* Intermediate and high overlapping Bitwords: *) 

        ; IF LOverlapHiBitwordNo > LOverlapLoBitwordNo 
          THEN (* There are intermediate overlappping Bitwords. *) 
            LBitwordCt := LOverlapHiBitwordNo - LOverlapLoBitwordNo - 1 
          ; IF LBitwordCt > 0 
            THEN 
              LHashIncrIsComputable := FALSE 
            ; IF LBitwordCt MOD 2 # 0 
              THEN 
                LHashAbs := Word . Not ( LHashAbs ) 
              END (* IF *) 
            ; AssignAllOnesToBitwordArray 
                ( SUBARRAY 
                    ( LResultBitwordArrayRef ^ , LResultSs , LBitwordCt ) 
                )
            ; LCanReuseBitwordArray 
                := LCanReuseBitwordArray 
                   AND BitwordArrayIsAllOnes 
                         ( SUBARRAY 
                             ( BSetBitwords , LSetSs , LBitwordCt )
                         )
            ; LResultCard 
                := Word . Plus 
                     ( LResultCard 
                     , Word . Times ( LBitwordCt , BitsPerBitword ) 
                     ) 
            ; INC ( LSetSs , LBitwordCt ) 
            ; INC ( LResultSs , LBitwordCt ) 
            END (* IF *) 
          (* Do the high overlapping Bitword. It is not the low overlapping. *) 
          ; LSetBitword := BSetBitwords [ LSetSs ] 
          ; LReuseBitword := LSetBitword 
          ; LHashRemove := LSetBitword 

          ; IF LOverlapHiBitwordNo = LBSetHiBitwordNo 
            THEN (* This is also the high bitword of BSet. *) 
              LHashRemove 
                := Word . Or 
                     ( LHashRemove , GTMaskOfIElem ( BSetInfo . BitsetHi ) ) 
              (* ^Set BSet high garbage bits. *) 
            ; LSetBitword 
                := Word . And 
                     ( LSetBitword , LEMaskOfIElem ( BSetInfo . BitsetHi ) ) 
              (* ^Zero BSet high garbage bits. *) 
            END (* IF *) 
          ; IF LOverlapHiBitwordNo = LRSetHiBitwordNo 
            THEN (* This is also the high bitword of the range. *) 
              LRangeBitword := LEMaskOfIElem ( RSetHi ) 
              (* ^Omit bounds high garbage bits. *) 
            ELSE 
              LRangeBitword := AllOnes 
            END (* IF *) 
          ; LBitword := Word . Or ( LSetBitword , LRangeBitword ) 
          ; IF LOverlapHiBitwordNo = LResultHiBitwordNo 
            THEN
              LMask := GTMaskOfIElem ( LResultHiIElem ) 
            ; LBitword := Word . Or ( LBitword , LMask )
              (* ^Set high garbage bits of result. *) 
            ; LReuseBitword := Word . Or ( LReuseBitword , LMask )
              (* ^Set high garbage bits of LReuseBitword, for comparison. *) 
            END (* IF *) 
          ; LAllResultBitsAreOnes 
              := LAllResultBitsAreOnes AND LBitword = AllOnes 
          ; LHashIncr := Word . Xor ( LHashIncr , LHashRemove ) (* Remove. *) 
          ; LHashIncr := Word . Xor ( LHashIncr , LBitword ) (* Add. *) 
          ; LHashAbs := Word . Xor ( LHashAbs , LBitword ) (* Add. *) 
          ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword 
          ; IF LResultCardIsComputable 
            THEN 
              LResultCard 
                := Word . Plus ( LResultCard , NoOf1BitsInBitword ( LBitword ) )
            END (* IF *) 
          ; LCanReuseBitwordArray 
              := LCanReuseBitwordArray AND LReuseBitword = LBitword 
          ; INC ( LSetSs ) 
          ; INC ( LResultSs ) 
          END (* IF *) 

        (* Handle the high end of the result: *) 
        ; IF LRSetHiBitwordNo > LBSetHiBitwordNo 
          THEN (* There are high Bitwords from the range. *) 
            LBitwordCt := LRSetHiBitwordNo - LBSetHiBitwordNo 
          ; LHashIncrIsComputable := FALSE 
          ; IF LBitwordCt MOD 2 # 0 
            THEN LHashAbs := Word . Not ( LHashAbs ) 
            END (* IF *) 
          ; AssignAllOnesToBitwordArray 
              ( SUBARRAY ( LResultBitwordArrayRef ^ , LResultSs , LBitwordCt ) )
          ; IF LCanReuseBitwordArray 
            THEN
              LBitword 
                := BSetBitwords 
                     [ ArraySsOfBitwordNo 
                         ( LResultHiBitwordNo , BSetInfo . Bias ) 
                     ]
            ; LBitword 
                := Word . Or ( LBitword , GTMaskOfIElem ( LResultHiIElem ) )
            ; LCanReuseBitwordArray := LBitword = AllOnes 
            ; LCanReuseBitwordArray 
                := LCanReuseBitwordArray 
                   AND BitwordArrayIsAllOnes 
                         ( SUBARRAY 
                             ( BSetBitwords 
                             , LSetSs 
                             , LBitwordCt - 1 
                             )
                         ) 
            END (* IF *)  
          ; LResultCard 
              := Word . Plus 
                   ( LResultCard 
                   , Word . Times ( LBitwordCt , BitsPerBitword ) 
                   ) 
          ; INC ( LResultSs , LBitwordCt ) 
          ELSIF LBSetHiBitwordNo > LRSetHiBitwordNo 
          THEN (* There are high Bitwords from the BSet. *) 
          (* Intermediate Bitwords from the high end of BSet. *) 
            LBitwordCt := LBSetHiBitwordNo - LRSetHiBitwordNo - 1  
          ; IF LBitwordCt > 0 
            THEN 
              WITH WResultSub 
                = SUBARRAY 
                    ( LResultBitwordArrayRef ^ , LResultSs , LBitwordCt )
              DO WResultSub  
                   := SUBARRAY 
                        ( BSetBitwords , LSetSs , LBitwordCt )
              ; LAllResultBitsAreOnes 
                  := LAllResultBitsAreOnes 
                     AND BitwordArrayIsAllOnes ( WResultSub )
              ; LHashAbsIsComputable := FALSE 
              ; LResultCardIsComputable := FALSE 
              ; INC ( LSetSs , LBitwordCt ) 
              ; INC ( LResultSs , LBitwordCt ) 
              END (* WITH *) 
            END (* IF *) 
          (* The high Bitword from the BSet. *) 
          ; LBitword := BSetBitwords [ LSetSs ] 
          ; LBitword 
              := Word . Or ( LBitword , GTMaskOfIElem ( LResultHiIElem ) )
            (* ^Set high garbage bits of result. *) 
          ; LAllResultBitsAreOnes 
              := LAllResultBitsAreOnes AND LBitword = AllOnes 
          (* No change to LHashIncr. *) 
          ; LHashAbs := Word . Xor ( LHashAbs , LBitword ) 
          ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword 
          ; IF LResultCardIsComputable 
            THEN 
              LResultCard 
                := Word . Plus ( LResultCard , NoOf1BitsInBitword ( LBitword ) )
            END (* IF *) 
          ; INC ( LSetSs ) 
          ; INC ( LResultSs ) 
          END (* IF *) 
        ; <* ASSERT LResultSs = LAST ( LResultBitwordArrayRef ^ ) + 1 *>
          IF LAllResultBitsAreOnes 
          THEN 
            INC ( GUnionRangeCt ) 
          ; RETURN 
              TryToReuseRangeset 
                ( RSet , LResultLoIElem , LResultHiIElem ) 
          END (* IF *) 
        ; IF LCanReuseBitwordArray AND BSet # NIL  
          THEN 
            LResultBitwordArrayRef := BSet . BitwordArrayRef  
          ; LResultBias := BSetInfo . Bias 
          ; INC ( GUnionReuseCt ) 
          END 
        ; LResultInfo . BitsetLo := LResultLoIElem 
        ; LResultInfo . BitsetHi := LResultHiIElem 
        ; LResultInfo . Bias := LResultBias 
        ; IF LResultCardIsComputable 
          THEN
            LResultInfo . Card := LResultCard  
          ELSE 
            LResultInfo . Card := CardUnknown 
          END (* IF *) 
        ; StoreBitsetHashAbsOrIncr 
            ( LResultInfo 
            , LHashAbs , LHashAbsIsComputable 
            , LHashIncr , LHashIncrIsComputable
            ) 

        ; LResult 
            := ConstructBitset 
                 ( LResultInfo 
                 , LResultBitwordArrayRef 
                 , WasNewlyAllocated := NOT LCanReuseBitwordArray 
                 ) 
        ; RETURN LResult 
        END (* IF *) 
      END (* IF *) 
    END UnionBitsetRange 

; PROCEDURE UnionDisjointOrderedBitsets 
    ( READONLY BSet1Info : BitsetInfoTyp 
    ; READONLY BSet1Bitwords : ARRAY OF BitwordTyp 
    ; READONLY BSet2Info : BitsetInfoTyp 
    ; READONLY BSet2Bitwords : ARRAY OF BitwordTyp 
    ) 
  : T 

  (* PRE: The ranges are disjoint (but may touch). 
     PRE: The range of BSet1 lies below that of BSet2. 
  *) 

  = VAR LBSet1LoBitwordNo , LBSet1HiBitwordNo : BitwordNoTyp 
  ; VAR LBSet2LoBitwordNo , LBSet2HiBitwordNo : BitwordNoTyp 
  ; VAR LBitword , LBitword2 : BitwordTyp 
  ; VAR LBSet1Ss , LBSet2Ss , LResultSs : CARDINAL  
  ; VAR LResultInfo : BitsetInfoTyp 
  ; VAR LResultBitwordArrayRef : BitwordArrayRefTyp 
  ; VAR LResult : T 
  ; VAR LBitwordCt : CARDINAL 
  ; VAR LHash1 , LHash2 , LHashRemove , LHashIncr : HashTyp := 0  
  ; VAR LHashIsComputable : BOOLEAN 

  ; BEGIN 
      LBSet1LoBitwordNo := BitwordNoOfIElem ( BSet1Info . BitsetLo ) 
    ; LBSet1HiBitwordNo := BitwordNoOfIElem ( BSet1Info . BitsetHi ) 
    ; LBSet2LoBitwordNo := BitwordNoOfIElem ( BSet2Info . BitsetLo ) 
    ; LBSet2HiBitwordNo := BitwordNoOfIElem ( BSet2Info . BitsetHi ) 
    ; LResultInfo . BitsetLo := BSet1Info . BitsetLo  
    ; LResultInfo . BitsetHi := BSet2Info . BitsetHi  
    ; LResultInfo . Bias := LBSet1LoBitwordNo 
    ; LResultBitwordArrayRef 
        := NewBitwordArray ( LBSet2HiBitwordNo - LBSet1LoBitwordNo + 1 ) 
    ; IF BSet1Info . Card = CardUnknown OR BSet2Info . Card = CardUnknown   
      THEN LResultInfo . Card := CardUnknown 
      ELSE 
        LResultInfo . Card 
          := Word . Plus ( BSet1Info . Card , BSet2Info . Card ) 
      END (* IF *)
    ; RecoverBitsHash ( BSet1Info , (*VAR*) LHashIsComputable , (*VAR*) LHash1 )
    ; IF LHashIsComputable 
      THEN
        RecoverBitsHash 
          ( BSet2Info , (*VAR*) LHashIsComputable , (*VAR*) LHash2 )
      END (* IF *)  
    ; LHashIncr := Word . Xor ( LHash1 , LHash2 ) 
    ; LBSet1Ss := ArraySsOfBitwordNo ( LBSet1LoBitwordNo , BSet1Info . Bias ) 
    ; LBSet2Ss := ArraySsOfBitwordNo ( LBSet2LoBitwordNo , BSet2Info . Bias ) 
    ; LResultSs := 0 

    (* Start low (and maybe high too) Bitword of BSet1: *)
    ; LBitword 
        := Word . Or 
             ( BSet1Bitwords [ LBSet1Ss ]  
             , LTMaskOfIElem ( BSet1Info . BitsetLo ) 
               (* ^Set low garbage bits of result. *) 
             )

    (* Intermediate Bitwords of BSet1: *) 
    ; IF LBSet1LoBitwordNo < LBSet1HiBitwordNo 
      THEN (* Not the last Bitword of BSet1. *) 
        LResultBitwordArrayRef ^ [ LResultSs ] := LBitword 
      ; INC ( LBSet1Ss ) 
      ; INC ( LResultSs )
      ; LBitwordCt := LBSet1HiBitwordNo - LBSet1LoBitwordNo - 1  
      ; SUBARRAY ( LResultBitwordArrayRef ^ , LResultSs , LBitwordCt ) 
          := SUBARRAY ( BSet1Bitwords , LBSet1Ss , LBitwordCt ) 
      ; INC ( LBSet1Ss , LBitwordCt ) 
      ; INC ( LResultSs , LBitwordCt ) 
      (* Fetch high Bitword of BSet1, not the same as low Bitword. *) 
      ; LBitword := BSet1Bitwords [ LBSet1Ss ] 
      END (* IF *) 

    (* Finish high (and maybe low too) Bitword of BSet1: *)
    ; LBitword 
        := Word . Or ( LBitword , GTMaskOfIElem ( BSet1Info . BitsetHi ) )  
           (* ^Temporarily set BSet1 hi garbage bits for removal from hash. *) 
    ; LHashIncr := Word . Xor ( LHashIncr , LBitword ) (* Remove. *)  
    ; LBitword 
        := Word . And ( LBitword , LEMaskOfIElem ( BSet1Info . BitsetHi ) ) 
           (* ^Remove high garbage bits of BSet1 for result. *) 

    ; IF LBSet1HiBitwordNo = LBSet2LoBitwordNo 
      THEN (* BSet1 and BSet2 have one overlapping Bitword. *) 
      (* Partially handle low (and maybe high too) Bitword of BSet2: *)
        LBitword2 
          := Word . And 
               ( BSet2Bitwords [ LBSet2Ss ]   
               , GEMaskOfIElem ( BSet2Info . BitsetLo ) 
               )
             (* ^Remove low garbage bits of BSet2 for result. *) 
      ; LHashRemove  
          := Word . Or ( LBitword2 , LTMaskOfIElem ( BSet2Info . BitsetLo ) ) 
             (* ^Set low garbage bits of BSet2 for hash removal. *) 
      ; LBitword := Word . Or ( LBitword , LBitword2 ) 
      ELSE (* BSet1 and BSet2 do not have an overlapping Bitword. *) 
        LHashIncr := Word . Xor ( LHashIncr , LBitword ) (* Add. *) 
      ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword 
      ; INC ( LResultSs ) 
      (* Zero Bitwords in the gap. *) 
      ; LBitwordCt := LBSet2LoBitwordNo - LBSet1HiBitwordNo - 1
      (* Zeros contribute nothing to hash. *) 
      ; AssignAllZerosToBitwordArray 
          ( SUBARRAY ( LResultBitwordArrayRef ^ , LResultSs , LBitwordCt ) )
      ; INC ( LResultSs , LBitwordCt ) 

      (* Partially handle low (and maybe high too) Bitword of BSet2: *)
      ; LBitword 
          := Word . And 
               ( BSet2Bitwords [ LBSet2Ss ]  
               , GEMaskOfIElem ( BSet2Info . BitsetLo ) 
               )
             (* ^Remove low garbage bits of BSet2 for result. *) 
      ; LHashRemove  
          := Word . Or ( LBitword , LTMaskOfIElem ( BSet2Info . BitsetLo ) ) 
             (* ^Set low garbage bits of BSet2 for hash removal. *) 
      END (* IF *) 

    (* Intermediate Bitwords of BSet2: *) 
    ; IF LBSet2LoBitwordNo = LBSet2HiBitwordNo 
      THEN (* This is also the high Bitword of BSet2. *)  
        LHashRemove 
          := Word . Or ( LHashRemove , GTMaskOfIElem ( BSet2Info . BitsetHi ) ) 
             (* ^Set high garbage bits of BSet2 for hash removal. *) 
      ; LHashIncr := Word . Xor ( LHashIncr , LHashRemove ) (* Remove. *) 
      ; LBitword 
          := Word . Or ( LBitword , GTMaskOfIElem ( BSet2Info . BitsetHi ) ) 
             (* ^Set high garbage bits of result Bitword. *) 
      ; LHashIncr := Word . Xor ( LHashIncr , LBitword ) (* Add. *)  
      ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword 
      ELSE (* Not the last Bitword of BSet2. *) 
        LHashIncr := Word . Xor ( LHashIncr , LHashRemove ) (* Remove. *) 
      ; LHashIncr := Word . Xor ( LHashIncr , LBitword ) (* Add. *)
      ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword 
      ; INC ( LBSet2Ss ) 
      ; INC ( LResultSs )
      ; LBitwordCt := LBSet2HiBitwordNo - LBSet2LoBitwordNo - 1  
      ; SUBARRAY ( LResultBitwordArrayRef ^ , LResultSs , LBitwordCt ) 
          := SUBARRAY ( BSet2Bitwords , LBSet2Ss , LBitwordCt ) 
      ; INC ( LBSet2Ss , LBitwordCt ) 
      ; INC ( LResultSs , LBitwordCt ) 
      ; LBitword := BSet2Bitwords [ LBSet2Ss ] 
      ; LBitword 
          := Word . Or ( LBitword , GTMaskOfIElem ( BSet2Info . BitsetHi ) ) 
             (* ^Set high garbage bits of result Bitword. *) 
      ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword 
      END (* IF *) 
    ; StoreBitsetHashIncr ( LResultInfo , LHashIncr , LHashIsComputable ) 

    ; LResult := ConstructBitset ( LResultInfo , LResultBitwordArrayRef ) 
    ; RETURN LResult  
    END UnionDisjointOrderedBitsets 

; PROCEDURE UnionOverlappingOrderedHiBitsets 
    ( READONLY BSet1Info : BitsetInfoTyp 
    ; READONLY BSet1Bitwords : ARRAY OF BitwordTyp 
    ; READONLY BSet2Info : BitsetInfoTyp 
    ; READONLY BSet2Bitwords : ARRAY OF BitwordTyp 
    ) 
  : T 

  (* PRE: The ranges overlap (not just touch), but are not identical. 
     PRE: BSet1Info . BitsetHi <= BSet2Info . BitsetHi. 
  *) 

  = VAR LSet1LoBitwordNo , LSet1HiBitwordNo : BitwordNoTyp 
  ; VAR LSet2LoBitwordNo , LSet2HiBitwordNo : BitwordNoTyp 
  ; VAR LResultLoIElem , LResultHiIElem : ValidIElemTyp 
  ; VAR LResultBias : BitwordNoTyp 
  ; VAR LOverlapLoBitwordNo : BitwordNoTyp 
  ; VAR LLoBitwordCt , LHiBitwordCt : CARDINAL   
  ; VAR LSet1Ss , LSet2Ss , LResultSs : CARDINAL 
  ; VAR LBitword1 , LBitword2 , LResultBitword : BitwordTyp 
  ; VAR LResultCard : CardTyp 
  ; VAR LResultInfo : BitsetInfoTyp 
  ; VAR LResultBitwordArrayRef : BitwordArrayRefTyp 
  ; VAR LResult : T
  ; VAR LResultHasA0Bit : BOOLEAN 
  ; VAR LResultCardIsComputable : BOOLEAN 
  ; VAR LHashIncr , LHashAbs , LHash1 : HashTyp := 0  
  ; VAR LHashIncrIsComputable , LHashAbsIsComputable : BOOLEAN 

  ; BEGIN 
      LSet1LoBitwordNo := BitwordNoOfIElem ( BSet1Info . BitsetLo ) 
    ; LSet1HiBitwordNo := BitwordNoOfIElem ( BSet1Info . BitsetHi ) 
    ; LSet2LoBitwordNo := BitwordNoOfIElem ( BSet2Info . BitsetLo ) 
    ; LSet2HiBitwordNo := BitwordNoOfIElem ( BSet2Info . BitsetHi ) 
    ; LResultLoIElem 
        := MIN ( BSet1Info . BitsetLo , BSet2Info . BitsetLo ) 
    ; LResultHiIElem 
        := MAX ( BSet2Info . BitsetHi , BSet2Info . BitsetHi ) 
    ; LResultSs := 0 
    ; LResultHasA0Bit := FALSE (* Could be refuted later. *) 
    ; LResultCard 
        := Word . Minus ( 0 , BitNo ( LResultLoIElem ) ) 
           (* ^Discount result lo garbage bits. *)
    ; LResultCard 
        := Word . Minus  
             ( LResultCard , BitsPerBitword - 1 - BitNo ( LResultHiIElem ) ) 
             (* ^Discount result high garbage bits. *) 
    ; LResultCardIsComputable 
        := LSet2HiBitwordNo - LSet1HiBitwordNo < 2 (* Could be refuted later. *)
    ; RecoverBitsHash 
        ( BSet1Info , (*VAR*) LHashIncrIsComputable , (*VAR*) LHash1 )
    ; IF LHashIncrIsComputable 
      THEN
        RecoverBitsHash 
          ( BSet2Info , (*VAR*) LHashIncrIsComputable , (*VAR*) LHashIncr )
      END (* IF *) 
    ; LHashIncr := Word . Xor ( LHash1 , LHashIncr ) 
    ; LHashAbs := 0 
    ; LHashAbsIsComputable := TRUE (* Refutable. *) 

    (* Copy any words below the low end overlapping region. *) 
    ; IF BSet1Info . BitsetLo < BSet2Info . BitsetLo 
      THEN (* Low words, if any, come from Set1. *) 
        LResultBias := LSet1LoBitwordNo  
      ; LOverlapLoBitwordNo := LSet2LoBitwordNo 
      ; LLoBitwordCt := LOverlapLoBitwordNo - LSet1LoBitwordNo 
      ; LResultCardIsComputable 
          := LResultCardIsComputable AND LLoBitwordCt < 2 
        (* ^Doing this now can help avoid some fruitless bit counting later. *)
      ; LSet1Ss := ArraySsOfBitwordNo ( LSet1LoBitwordNo , BSet1Info . Bias )
      ; LResultBitwordArrayRef 
          := NewBitwordArray ( LSet2HiBitwordNo - LSet1LoBitwordNo + 1 )
      ; LBitword1 := BSet1Bitwords [ LSet1Ss ] 
      ; LBitword1 
          := Word . Or ( LBitword1 , LTMaskOfIElem ( BSet1Info . BitsetLo ) ) 
             (* ^Set low garbage bits of BSet1 and result. *) 
      ; IF LLoBitwordCt > 0 
        THEN 
          LHashAbs := Word . Xor ( LHashAbs , LBitword1 ) 
        ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword1 
        ; LResultHasA0Bit := LResultHasA0Bit OR LBitword1 # AllOnes 
        ; IF LResultCardIsComputable 
          THEN 
            LResultCard 
              := Word . Plus ( LResultCard , NoOf1BitsInBitword ( LBitword1 ) ) 
          END (* IF *) 
        ; INC ( LSet1Ss ) 
        ; INC ( LResultSs ) 
        ; DEC ( LLoBitwordCt )  
        ; IF LLoBitwordCt > 0 
          THEN 
            LHashAbsIsComputable := FALSE 
          ; WITH WSub 
              = SUBARRAY 
                  ( LResultBitwordArrayRef ^ , LResultSs , LLoBitwordCt ) 
            DO 
              WSub 
                := SUBARRAY 
                     ( BSet1Bitwords , LSet1Ss , LLoBitwordCt ) 
            ; LResultHasA0Bit 
                := LResultHasA0Bit OR NOT BitwordArrayIsAllOnes ( WSub ) 
            ; INC ( LSet1Ss , LLoBitwordCt ) 
            ; INC ( LResultSs , LLoBitwordCt ) 
            END (* WITH *) 
          END (* IF *) 
        ; LBitword1 := BSet1Bitwords [ LSet1Ss ] 
        END (* IF *) 

      (* Take care of the bits in the low overlapping word. *) 
      ; IF LOverlapLoBitwordNo = LSet1HiBitwordNo 
        THEN (* This is also the high Bitword of BSet1. *)  
          LBitword1 
            := Word . Or 
                 ( LBitword1 , GTMaskOfIElem ( BSet1Info . BitsetHi ) ) 
               (* Temporarily set BSet1 high garbage bits for hash removal. *)
        ; LHashIncr := Word . Xor ( LHashIncr , LBitword1 ) (* Remove. *) 
        ; LBitword1 
            := Word . And 
                 ( LBitword1 , LEMaskOfIElem ( BSet1Info . BitsetHi ) ) 
               (* Now zero BSet1 high garbage bits for result. *)
        ELSE 
          LHashIncr := Word . Xor ( LHashIncr , LBitword1 ) (* Remove. *) 
        END (* IF *) 
      ; LSet2Ss := ArraySsOfBitwordNo ( LOverlapLoBitwordNo , BSet2Info . Bias )
      ; LBitword2 := BSet2Bitwords [ LSet2Ss ] 
      ; LBitword2 
          := Word . Or ( LBitword2 , LTMaskOfIElem ( BSet2Info . BitsetLo ) ) 
        (* Temporarily set low garbage bits of BSet2 for hash removal. *)
      ; IF LOverlapLoBitwordNo = LSet2HiBitwordNo 
        THEN (* This is also the high Bitword of BSet2 *) 
          LBitword2 
            := Word . Or 
                 ( LBitword2 , GTMaskOfIElem ( BSet2Info . BitsetHi ) ) 
               (* Set BSet2 high garbage bits for hash removal and result. *)
        END (* IF *)   
      ; LHashIncr := Word . Xor ( LHashIncr , LBitword2 ) (* Remove. *) 
      ; LBitword2 
          := Word . And ( LBitword2 , GEMaskOfIElem ( BSet2Info . BitsetLo ) ) 
        (* Now zero low garbage bits of BSet2. *)
      ELSE (* Low words, if any, come from BSet2. *) 
        LResultBias := LSet2LoBitwordNo  
      ; LOverlapLoBitwordNo := LSet1LoBitwordNo 
      ; LLoBitwordCt := LOverlapLoBitwordNo - LSet2LoBitwordNo 
      ; LResultCardIsComputable 
          := LResultCardIsComputable AND LLoBitwordCt < 2 
        (* ^Doing this now helps avoid some fruitless bit counting later. *)  
      ; LSet2Ss := ArraySsOfBitwordNo ( LSet2LoBitwordNo , BSet2Info . Bias )
      ; LResultBitwordArrayRef 
          := NewBitwordArray ( LSet2HiBitwordNo - LSet2LoBitwordNo + 1 )
      ; LBitword2 := BSet2Bitwords [ LSet2Ss ] 
      ; LBitword2 
          := Word . Or ( LBitword2 , LTMaskOfIElem ( BSet2Info . BitsetLo ) ) 
             (* ^Set low garbage bits of BSet1 and result. *) 
      ; IF LLoBitwordCt > 0 
        THEN 
          LHashAbs := Word . Xor ( LHashAbs , LBitword2 ) 
        ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword2 
        ; LResultHasA0Bit := LResultHasA0Bit OR LBitword2 # AllOnes 
        ; IF LResultCardIsComputable 
          THEN 
            LResultCard 
              := Word . Plus ( LResultCard , NoOf1BitsInBitword ( LBitword2 ) )
          END (* IF *) 
        ; INC ( LSet2Ss ) 
        ; INC ( LResultSs ) 
        ; DEC ( LLoBitwordCt )  
        ; IF LLoBitwordCt > 0 
          THEN 
            LHashAbsIsComputable := FALSE 
          ; WITH 
              WSub 
                = SUBARRAY 
                    ( LResultBitwordArrayRef ^ , LResultSs , LLoBitwordCt ) 
             DO 
               WSub 
                 := SUBARRAY 
                      ( BSet2Bitwords , LSet2Ss , LLoBitwordCt ) 
            ; LResultHasA0Bit 
                := LResultHasA0Bit OR NOT BitwordArrayIsAllOnes ( WSub ) 
            ; INC ( LSet2Ss , LLoBitwordCt ) 
            ; INC ( LResultSs , LLoBitwordCt ) 
            END (* WTIH *) 
          END (* IF *) 
        ; LBitword2 := BSet2Bitwords [ LSet2Ss ] 
        END (* IF *) 

      (* Take care of the bits in the low overlapping word.  *) 
      ; IF LOverlapLoBitwordNo = LSet2HiBitwordNo 
        THEN (* This is also the high Bitword of BSet2. *)  
          LBitword2 
            := Word . Or 
                 ( LBitword2 , GTMaskOfIElem ( BSet2Info . BitsetHi ) ) 
               (* Set BSet2 high garbage bits for hash removal and result. *)
        END (* IF *) 
      ; LHashIncr := Word . Xor ( LHashIncr , LBitword2 ) (* Remove. *) 
      ; LSet1Ss := ArraySsOfBitwordNo ( LOverlapLoBitwordNo , BSet1Info . Bias )
      ; LBitword1 := BSet1Bitwords [ LSet1Ss ] 
      ; LBitword1 
          := Word . Or ( LBitword1 , LTMaskOfIElem ( BSet1Info . BitsetLo ) ) 
        (* Temporarily set low garbage bits of BSet1 for hash removal. *)
      ; IF LOverlapLoBitwordNo = LSet1HiBitwordNo 
        THEN (* This is also the high Bitword of BSet1 *) 
          LBitword1 
            := Word . Or 
                 ( LBitword1 , GTMaskOfIElem ( BSet1Info . BitsetHi ) ) 
               (* Temporarily set BSet1 high garbage bits for hash removal. *)
        ; LHashIncr := Word . Xor ( LHashIncr , LBitword1 ) (* Remove. *) 
        ; LBitword1 
            := Word . And 
                 ( LBitword1 , LEMaskOfIElem ( BSet1Info . BitsetHi ) ) 
               (* Zero BSet1 high garbage bits for result. *)
        ELSE 
          LHashIncr := Word . Xor ( LHashIncr , LBitword1 ) (* Remove. *) 
        END (* IF *)   
      ; LBitword1 
          := Word . And ( LBitword1 , GEMaskOfIElem ( BSet1Info . BitsetLo ) ) 
        (* Now zero low garbage bits of BSet1. *)
      END (* IF *) 

    (* Finish the low overlap Bitword. *) 
    ; LResultBitword := Word . Or ( LBitword1 , LBitword2 ) 
    ; LHashIncr := Word . Xor ( LHashIncr , LResultBitword ) (* Add. *) 
    ; LHashAbs := Word . Xor ( LHashAbs , LResultBitword ) (* Add. *) 
    ; LResultBitwordArrayRef ^ [ LResultSs ] := LResultBitword 
    ; LResultHasA0Bit := LResultHasA0Bit OR LResultBitword # AllOnes 
    ; IF LResultCardIsComputable 
      THEN 
        LResultCard 
          := Word . Plus ( LResultCard , NoOf1BitsInBitword ( LResultBitword ) )
      END (* IF *) 
    ; INC ( LSet1Ss ) 
    ; INC ( LSet2Ss ) 
    ; INC ( LResultSs ) 

    ; IF LOverlapLoBitwordNo < LSet1HiBitwordNo 
      THEN (* High overlap Bitword is separate from the low overlap Bitword. *)
      (* Take care of any full words between the overlap points. *) 
        FOR RI := LOverlapLoBitwordNo + 1 TO LSet1HiBitwordNo - 1  
        DO 
          LBitword1 := BSet1Bitwords [ LSet1Ss ]
        ; LBitword2 := BSet2Bitwords [ LSet2Ss ]
        ; LHashIncr := Word . Xor ( LHashIncr , LBitword1 ) (* Remove. *) 
        ; LHashIncr := Word . Xor ( LHashIncr , LBitword2 ) (* Remove. *) 
        ; LResultBitword := Word . Or ( LBitword1 , LBitword2 ) 
        ; LHashIncr := Word . Xor ( LHashIncr , LResultBitword ) (* Add. *) 
        ; LHashAbs := Word . Xor ( LHashAbs , LResultBitword ) (* Add. *) 
        ; LResultBitwordArrayRef ^ [ LResultSs ] := LResultBitword 
        ; LResultHasA0Bit := LResultHasA0Bit OR LResultBitword # AllOnes 
        ; IF LResultCardIsComputable 
          THEN 
            LResultCard 
              := Word . Plus 
                   ( LResultCard , NoOf1BitsInBitword ( LResultBitword ) )
          END (* IF *) 
        ; INC ( LSet1Ss ) 
        ; INC ( LSet2Ss ) 
        ; INC ( LResultSs ) 
        END (* FOR *)

      (* Take care of the high overlap word. *) 
      (* This is not the low Bitword of BSet1 and is its high Bitword. *) 
      ; LBitword1 := BSet1Bitwords [ LSet1Ss ] 
      ; LBitword1 
          := Word . Or ( LBitword1 , GTMaskOfIElem ( BSet1Info . BitsetHi ) ) 
        (* ^Temporarily set high garbage bits of BSet1 for hash removal. *)  
      ; LHashIncr := Word . Xor ( LHashIncr , LBitword1 ) (* Remove. *) 
      ; LBitword1 
          := Word . And ( LBitword1 , LEMaskOfIElem ( BSet1Info . BitsetHi ) ) 
        (* ^Now zero high garbage bits of Set1 for result. *)  
      (* This is not the low Bitword of BSet2. *) 
      ; LBitword2 := BSet2Bitwords [ LSet2Ss ] 
      ; IF LSet1HiBitwordNo = LSet2HiBitwordNo 
        THEN (* And it is the high Bitword of BSet2. *) 
          LBitword2 
            := Word . Or ( LBitword2 , GTMaskOfIElem ( BSet2Info . BitsetHi ) ) 
          (* ^Set high garbage bits of BSet2 for hash removal and result. *)  
        END (* IF *)   
      ; LHashIncr := Word . Xor ( LHashIncr , LBitword2 ) (* Remove. *) 
      ; LResultBitword := Word . Or ( LBitword1 , LBitword2 ) 
      ; LHashIncr := Word . Xor ( LHashIncr , LResultBitword ) (* Add. *) 
      ; LHashAbs := Word . Xor ( LHashAbs , LResultBitword ) (* Add. *) 
      ; LResultBitwordArrayRef ^ [ LResultSs ] := LResultBitword 
      ; LResultHasA0Bit := LResultHasA0Bit OR LResultBitword # AllOnes 
      ; IF LResultCardIsComputable 
        THEN 
          LResultCard 
              := Word . Plus 
                   ( LResultCard , NoOf1BitsInBitword ( LResultBitword ) )
        END (* IF *) 
      ; INC ( LSet2Ss ) 
      ; INC ( LResultSs ) 
      END (* IF *) 

    (* Copy any words above the high overlap.  By precondition, these can
       only come from BSet2: 
    *) 
    ; LHiBitwordCt := LSet2HiBitwordNo - LSet1HiBitwordNo   
    ; IF LHiBitwordCt > 0 
      THEN 
        DEC ( LHiBitwordCt ) 
      ; IF LHiBitwordCt > 0 
        THEN
          LHashAbsIsComputable := FALSE  
        ; WITH WSub 
            = SUBARRAY ( LResultBitwordArrayRef ^ , LResultSs , LHiBitwordCt ) 
          DO 
            WSub 
              := SUBARRAY 
                   ( BSet2Bitwords , LSet2Ss , LHiBitwordCt ) 
          ; LResultHasA0Bit 
              := LResultHasA0Bit OR NOT BitwordArrayIsAllOnes ( WSub ) 
          ; INC ( LSet2Ss , LHiBitwordCt ) 
          ; INC ( LResultSs , LHiBitwordCt ) 
          END (* WITH *) 
        END (* IF *) 
      ; LBitword2 := BSet2Bitwords [ LSet2Ss ] 
      ; LBitword2 
          := Word . Or ( LBitword2 , GTMaskOfIElem ( BSet2Info . BitsetHi ) ) 
        (* Set hi garbage bits for hash computation and result. *) 
      ; LHashAbs := Word . Xor ( LHashAbs , LBitword2 ) (* Add. *) 
      ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword2 
      ; LResultHasA0Bit := LResultHasA0Bit OR LBitword2 # AllOnes 
      ; IF LResultCardIsComputable 
        THEN 
          LResultCard 
            := Word . Plus ( LResultCard , NoOf1BitsInBitword ( LBitword2 ) )
        END (* IF *) 
      ; INC ( LResultSs ) 
      END (* IF *) 
    ; <* ASSERT LResultSs = LAST ( LResultBitwordArrayRef ^ ) + 1 *> 
      IF NOT LResultHasA0Bit 
      THEN
        RETURN 
          ConstructRangeset ( Lo := LResultLoIElem , Hi := LResultHiIElem ) 
      ELSE 
        LResultInfo . BitsetLo := LResultLoIElem 
      ; LResultInfo . BitsetHi := LResultHiIElem 
      ; LResultInfo . Bias := LResultBias 
      ; IF LResultCardIsComputable 
        THEN 
          LResultInfo . Card := LResultCard  
        ELSE 
          LResultInfo . Card := CardUnknown 
        END (* IF *) 
      ; StoreBitsetHashAbsOrIncr 
          ( LResultInfo 
          , LHashAbs , LHashAbsIsComputable 
          , LHashIncr , LHashIncrIsComputable
          ) 

      ; LResult := ConstructBitset ( LResultInfo , LResultBitwordArrayRef ) 
      ; RETURN LResult 
      END (* IF *) 
    END UnionOverlappingOrderedHiBitsets 

; PROCEDURE UnionBitsets 
    ( READONLY BSet1Info : BitsetInfoTyp 
    ; READONLY BSet1Bitwords : ARRAY OF BitwordTyp 
    ; READONLY BSet2Info : BitsetInfoTyp 
    ; READONLY BSet2Bitwords : ARRAY OF BitwordTyp 
    ) 
  : T 

  = BEGIN 
      IF BSet1Info . BitsetHi < BSet2Info . BitsetLo 
      THEN (* Disjoint ranges, BSet1 is low. *) 
        RETURN UnionDisjointOrderedBitsets 
                 ( BSet1Info , BSet1Bitwords 
                 , BSet2Info , BSet2Bitwords  
                 ) 
      ELSIF BSet2Info . BitsetHi < BSet1Info . BitsetLo 
      THEN (* Disjoint ranges, BSet2 is low. *) 
        RETURN UnionDisjointOrderedBitsets 
                 ( BSet2Info , BSet2Bitwords  
                 , BSet1Info , BSet1Bitwords 
                 ) 
      ELSE (* Overlapping, non-identical ranges. *) 
        IF BSet1Info . BitsetHi <= BSet2Info . BitsetHi
        THEN 
          RETURN 
            UnionOverlappingOrderedHiBitsets 
              ( BSet1Info , BSet1Bitwords
              , BSet2Info , BSet2Bitwords
              ) 
        ELSE
          RETURN 
            UnionOverlappingOrderedHiBitsets 
              ( BSet2Info , BSet2Bitwords
              , BSet1Info , BSet1Bitwords
              ) 
        END (* IF *) 
      END (* IF *) 
    END UnionBitsets 

(* Statistics to measure storage reuse. *) 
; VAR GUnionCt : INTEGER := 0 
; VAR GUnionReuseCt : INTEGER := 0 
; VAR GUnionRangeCt : INTEGER := 0 

(* VISIBLE: *) 
; PROCEDURE Union ( Set1 : T ; Set2 : T ) : T
(* Union of Set1 and Set2. *) 

  = VAR UnionResult : T

  ; PROCEDURE InnerUnion 
      ( READONLY DInfo1 : DissectInfo 
      ; READONLY Bitwords1 : ARRAY OF BitwordTyp 
      ; READONLY DInfo2 : DissectInfo 
      ; READONLY Bitwords2 : ARRAY OF BitwordTyp 
      ) 

    = VAR LResultLoIElem , LResultHiIElem : ValidIElemTyp 

    ; BEGIN (* InnerUnion *)
        IF DInfo1 . RangeLo # IElemNull 
        THEN (* Set1 is a Rangeset. *)
          IF DInfo2 . RangeLo # IElemNull 
          THEN (* Set1 is a Rangeset and Set2 is a Rangeset. *) 
            IF DInfo1 . RangeLo <= DInfo2 . RangeLo 
               AND DInfo1 . RangeHi >= DInfo2 . RangeHi 
            THEN (* Set1 completely covers Set2. *) 
              UnionResult := DInfo1 . Set 
            ELSIF DInfo2 . RangeLo <= DInfo1 . RangeLo 
                  AND DInfo2 . RangeHi >= DInfo1 . RangeHi 
            THEN (* Set2 completely covers Set1. *) 
              UnionResult := DInfo2 . Set 
            ELSIF DInfo1 . RangeHi + 1 < DInfo2 . RangeLo 
            THEN (* There is a gap between the ranges and Set1 is low. *) 
              UnionResult := 
                UnionUntouchingOrderedRanges 
                  ( DInfo1 . RangeLo , DInfo1 . RangeHi 
                  , DInfo2 . RangeLo , DInfo2 . RangeHi 
                  ) 
            ELSIF DInfo2 . RangeHi + 1 < DInfo1 . RangeLo 
            THEN (* There is a gap between the ranges and Set2 is low. *) 
              UnionResult 
                := UnionUntouchingOrderedRanges 
                     ( DInfo2 . RangeLo , DInfo2 . RangeHi 
                     , DInfo1 . RangeLo , DInfo1 . RangeHi 
                     ) 
            ELSE (* Two touching  ranges. *) 
              LResultLoIElem := MIN ( DInfo1 . RangeLo , DInfo2 . RangeLo ) 
            ; LResultHiIElem := MAX ( DInfo1 . RangeHi , DInfo2 . RangeHi ) 
            ; UnionResult 
                := TryToReuseOneRangeset 
                     ( DInfo1 . RSet , DInfo2 . RSet 
                     , LResultLoIElem , LResultHiIElem 
                     ) 
            END (* IF *) 
          ELSIF NUMBER ( Bitwords2 ) = 0
          THEN (* Set1 is a Rangeset and Set2 is empty. *)
            UnionResult := DInfo1 . Set 
          ELSE (* Set1 is a Rangeset and Set2 is a Bitset. *)
            UnionResult := 
               UnionBitsetRange 
                 ( DInfo2 . BitsetInfo , Bitwords2 , DInfo2 . BSet   
                 , DInfo1 . RangeLo , DInfo1 . RangeHi , DInfo1 . RSet 
                 ) 
          END (* IF *) 

        ELSIF NUMBER ( Bitwords1 ) = 0 
        THEN (* Set1 is empty. *)
          UnionResult := DInfo2 . Set 

        ELSE (* Set1 is a Bitset. *)
          IF DInfo2 . RangeLo # IElemNull 
          THEN (* Set1 is a Bitset and Set2 is a Rangeset. *)  
            UnionResult 
              := UnionBitsetRange 
                   ( DInfo1 . BitsetInfo , Bitwords1 , DInfo1 . BSet  
                   , DInfo2 . RangeLo , DInfo2 . RangeHi , DInfo2 . RSet  
                   ) 
          ELSIF NUMBER ( Bitwords2 ) = 0 
          THEN (* Set1 is a Bitset and Set2 is empty. *)
            UnionResult := DInfo1 . Set 
          ELSE (* Set1 is a Bitset and Set2 is a Bitset. *)
            UnionResult := UnionBitsets 
                ( DInfo1 . BitsetInfo , Bitwords1 
                , DInfo2 . BitsetInfo , Bitwords2  
                ) 
          END (* IF *) 
        END (* IF *) 
      END InnerUnion

  ; BEGIN (* Union *) 
      <* FATAL ANY *> BEGIN 
        CallWithTwoSets ( Set1 , Set2 , InnerUnion ) 
      END (* Block *) 
    ; RETURN UnionResult 
    END Union

(* ============================ Intersection =============================== *)

; PROCEDURE IntersectionNonemptyRanges
   ( RSet1Lo , RSet1Hi : ValidIElemTyp ; RSet1 : RangesetTyp 
   ; RSet2Lo , RSet2Hi : ValidIElemTyp ; RSet2 : RangesetTyp 
   ) 
  : T 

  (* PRE: RSet1 # NIL IMPLIES RSet1Lo and RSet1Hi come from RSet1. 
     PRE: RSet2 # NIL IMPLIES RSet2Lo and RSet2Hi come from RSet2. 
     PRE: RSet1Lo <= RSet1Hi
     PRE: RSet2Lo <= RSet2Hi
  *) 

  = BEGIN 
      IF RSet1Lo <= RSet2Lo 
      THEN (* 1Lo <= 2Lo *) 
        IF RSet2Hi <= RSet1Hi 
        THEN (* 1Lo <= 2Lo <= 2Hi <= 1Hi *) 
          RETURN 
            TryToReuseRangeset 
              ( RSet2 
              , RSetLo := RSet2Lo 
              , RSetHi := RSet2Hi 
              ) 
        ELSIF RSet1Hi < RSet2Lo 
        THEN (* 1Lo <= 1Hi < 2Lo <= 2Hi *) 
          RETURN NIL 
        ELSE (* 1Lo <= 2Lo <= 1Hi < 2Hi *) 
          RETURN 
            TryToReuseRangeset 
              ( RSet1 
              , RSetLo := RSet2Lo 
              , RSetHi := RSet1Hi 
              ) 
        END (* IF *) 
      ELSE (* 2Lo < 1Lo *) 
        IF RSet1Hi <= RSet2Hi 
        THEN (* 2Lo < 1Lo <= 1Hi <= 2Hi *)  
          RETURN 
            TryToReuseRangeset 
              ( RSet1 
              , RSetLo := RSet1Lo 
              , RSetHi := RSet1Hi 
              ) 
        ELSIF RSet2Hi < RSet1Lo 
        THEN (* 2Lo <= 2Hi < 1Lo <= 1Hi *) 
          RETURN NIL 
        ELSE (* 2Lo < 1Lo <= 2Hi < 1Hi *) 
          RETURN 
            ConstructRangeset ( Lo := RSet1Lo , Hi := RSet2Hi ) 
        END (* IF *) 
      END (* IF *)
    END IntersectionNonemptyRanges

; PROCEDURE LeastIntersectionIElemInRange  
    ( READONLY BSet1Info : BitsetInfoTyp 
    ; READONLY BSet1Bitwords : ARRAY OF BitwordTyp 
    ; READONLY BSet2Info : BitsetInfoTyp 
    ; READONLY BSet2Bitwords : ARRAY OF BitwordTyp 
    ; RSetLo , RSetHi : ValidIElemTyp 
    )
  : IElemTyp 
  (* Return the least element of the intersection of BSet1 and BSet2
     projected onto [ RSetLo .. RSetHi ], or IElemNull, if no such 
     element exists.
  *)  

  (* PRE: RSetLo <= RSetHi. 
     PRE: [ RSetLo .. RSetHi ] is a subrange of 
          [ BSet1Info . BitsetLo .. BSet1Info . BitsetHi ].
     PRE: [ RSetLo .. RSetHi ] is a subrange of 
          [ BSet2Info . BitsetLo .. BSet2Info . BitsetHi ].
  *) 

  = VAR LLoBitwordNo , LHiBitwordNo , LBitwordNo : BitwordNoTyp 
  ; VAR LSet1Ss , LSet2Ss : ArraySsTyp 
  ; VAR LBit0IElem : IElemTyp 
  ; VAR LBitword : BitwordTyp 

  ; BEGIN 
      LLoBitwordNo := BitwordNoOfIElem ( RSetLo ) 
    ; LHiBitwordNo := BitwordNoOfIElem ( RSetHi ) 
    ; LSet1Ss := ArraySsOfBitwordNo ( LLoBitwordNo , BSet1Info . Bias ) 
    ; LSet2Ss := ArraySsOfBitwordNo ( LLoBitwordNo , BSet2Info . Bias ) 
    ; LBit0IElem := BitZeroIElemOfBitwordNo ( LLoBitwordNo ) 
    ; LBitword 
        := Word . And 
             ( BSet1Bitwords [ LSet1Ss ]  
             , BSet2Bitwords [ LSet2Ss ]  
             ) 
    ; LBitword := Word . And ( LBitword , GEMaskOfIElem ( RSetLo ) ) 
      (* Remove lo garbage bits. *) 
    ; LBitwordNo := LLoBitwordNo 
    ; LOOP 
        IF LBitwordNo = LHiBitwordNo 
        THEN (* Last word, where we need to zero high bits. *)
          LBitword := Word . And ( LBitword , LEMaskOfIElem ( RSetHi ) ) 
        ; IF LBitword # 0 
          THEN (* The sought bit is in this Bitword. *) 
            LBit0IElem 
              := Word . Plus 
                   ( LBit0IElem 
                   , Word . Times ( LBitwordNo - LLoBitwordNo , BitsPerBitword )
                   ) 
          ; RETURN LBit0IElem + Least1BitNoInBitword ( LBitword ) 
          ELSE RETURN IElemNull 
          END (* IF *) 
        ELSIF LBitword # 0 
        THEN (* The sought bit is in this Bitword. *) 
          LBit0IElem 
            := Word . Plus 
                 ( LBit0IElem 
                 , Word . Times ( LBitwordNo - LLoBitwordNo , BitsPerBitword ) 
                 ) 
        ; RETURN LBit0IElem + Least1BitNoInBitword ( LBitword ) 
        ELSE (* Advance to the next word. *) 
          INC ( LSet1Ss )  
        ; INC ( LSet2Ss )  
        ; INC ( LBitwordNo ) 
        ; LBitword 
            := Word . And 
                 ( BSet1Bitwords [ LSet1Ss ]  
                 , BSet2Bitwords [ LSet2Ss ]  
                 ) 
        END (* LOOP *) 
      END (* IF *) 
    END LeastIntersectionIElemInRange  

; PROCEDURE GreatestIntersectionIElemInRange
    ( READONLY BSet1Info : BitsetInfoTyp 
    ; READONLY BSet1Bitwords : ARRAY OF BitwordTyp 
    ; READONLY BSet2Info : BitsetInfoTyp 
    ; READONLY BSet2Bitwords : ARRAY OF BitwordTyp 
    ; RSetLo , RSetHi : ValidIElemTyp 
    )
  : IElemTyp 
  (* Return the greatest element of the intersection of BSet1 and BSet2
     projected onto [ RSetLo .. RSetHi ], or IElemNull, if no such 
     element exists. 
  *) 

  (* PRE: RSetLo <= RSetHi. 
     PRE: [ RSetLo .. RSetHi ] is a subrange of 
          [ BSet1Info . BitsetLo .. BSet1Info . BitsetHi ].
     PRE: [ RSetLo .. RSetHi ] is a subrange of 
          [ BSet2Info . BitsetLo .. BSet2Info . BitsetHi ].
  *) 

  = VAR LLoBitwordNo , LHiBitwordNo , LBitwordNo : BitwordNoTyp 
  ; VAR LSet1Ss , LSet2Ss : ArraySsTyp 
  ; VAR LBit0IElem : IElemTyp 
  ; VAR LBitword : BitwordTyp 

  ; BEGIN 
      LLoBitwordNo := BitwordNoOfIElem ( RSetLo ) 
    ; LHiBitwordNo := BitwordNoOfIElem ( RSetHi ) 
    ; LSet1Ss := ArraySsOfBitwordNo ( LHiBitwordNo , BSet1Info . Bias ) 
    ; LSet2Ss := ArraySsOfBitwordNo ( LHiBitwordNo , BSet2Info . Bias ) 
    ; LBit0IElem := BitZeroIElemOfBitwordNo ( LHiBitwordNo ) 
    ; LBitword 
        := Word . And 
             ( BSet1Bitwords [ LSet1Ss ]  
             , BSet2Bitwords [ LSet2Ss ]  
             ) 
    ; LBitword := Word . And ( LBitword , LEMaskOfIElem ( RSetHi ) ) 
      (* Zero hi garbage bits. *) 
    ; LBitwordNo := LHiBitwordNo 
    ; LOOP 
        IF LBitwordNo = LLoBitwordNo 
        THEN (* Lowest Bitword, where we need to zero low garbage bits. *)
          LBitword := Word . And ( LBitword , GEMaskOfIElem ( RSetLo ) ) 
        ; IF LBitword # 0 
          THEN (* The sought bit is in this Bitword. *) 
            LBit0IElem 
              := Word . Minus 
                   ( LBit0IElem 
                   , Word . Times ( LHiBitwordNo - LBitwordNo , BitsPerBitword )
                   ) 
          ; RETURN LBit0IElem + Greatest1BitNoInBitword ( LBitword ) 
          ELSE RETURN IElemNull 
          END (* IF *) 
        ELSIF LBitword # 0 
        THEN (* The sought bit is in this Bitword. *) 
          LBit0IElem
            := Word . Minus 
                 ( LBit0IElem 
                 , Word . Times ( LHiBitwordNo - LBitwordNo , BitsPerBitword ) 
                 ) 
        ; RETURN LBit0IElem + Greatest1BitNoInBitword ( LBitword ) 
        ELSE (* Advance to the next word. *) 
          DEC ( LSet1Ss )  
        ; DEC ( LSet2Ss )  
        ; DEC ( LBitwordNo ) 
        ; LBitword 
            := Word . And 
                 ( BSet1Bitwords [ LSet1Ss ]  
                 , BSet2Bitwords [ LSet2Ss ]  
                 ) 
        END (* LOOP *) 
      END (* IF *) 
    END GreatestIntersectionIElemInRange  

; PROCEDURE IntersectionRangeBitset 
    ( RSetLo , RSetHi : ValidIElemTyp 
    ; READONLY BSetInfo : BitsetInfoTyp 
    ; READONLY BSetBitwords : ARRAY OF BitwordTyp 
    ; Set : T 
    ; BSet : BitsetTyp 
    ) 
  : T 

  (* PRE: BSetInfo and BSetBitwords come from Set. 
     PRE: BSet # NIL IMPLIES BSetInfo and BSetBitwords come from BSet.
  *)  

  = VAR LSetSs : ArraySsTyp 
  ; VAR LResultLoIElem , LResultHiIElem : IElemTyp 
  ; VAR LResultBitwordCt : CARDINAL 
  ; VAR LResultLoBitwordNo , LResultHiBitwordNo : BitwordNoTyp 
  ; VAR LLoBitword , LHiBitword : BitwordTyp 
  ; VAR LResultInfo : BitsetInfoTyp 
  ; VAR LResultBitwordArrayRef : BitwordArrayRefTyp 
  ; VAR LResult : T
  ; VAR LResultCard : CardTyp 
  ; VAR LResultBias : INTEGER  
  ; VAR LHashAbs : HashTyp 
  ; VAR LResultHasA0Bit , LMiddleHasA0Bit : BOOLEAN 
  ; VAR LMiddleHasA1Bit : BOOLEAN 
  ; VAR LHashAbsIsComputable : BOOLEAN := TRUE  
  ; VAR LBitwordsNewlyAllocated : BOOLEAN 

  ; BEGIN 
      IF RSetHi < BSetInfo . BitsetLo 
         OR BSetInfo . BitsetHi < RSetLo 
         OR RSetLo > RSetHi 
      THEN (* Disjoint or empty ranges.  Result is empty. *) 
        RETURN NIL 
      ELSIF RSetLo <= BSetInfo . BitsetLo 
            AND BSetInfo . BitsetHi <= RSetHi 
      THEN (* BSet is entirely covered by the bounds, and is the result. *) 
        RETURN Set
      ELSE (* The ranges overlap. *) 
        LResultLoIElem := MAX ( RSetLo , BSetInfo . BitsetLo ) 
      ; LResultHiIElem := MIN ( RSetHi , BSetInfo . BitsetHi ) 
      ; LResultLoIElem 
          := LeastPresentIElemOfBSetInRange 
               ( BSetInfo , BSetBitwords , LResultLoIElem , LResultHiIElem ) 
      ; IF LResultLoIElem = IElemNull 
        THEN (* Result is empty. *)
          RETURN NIL 
        ELSE 
          LResultHiIElem 
            := GreatestPresentIElemOfBSetInRange 
                 ( BSetInfo , BSetBitwords , LResultLoIElem , LResultHiIElem )
        ; <* ASSERT LResultHiIElem # IElemNull *> 

          LResultLoBitwordNo := BitwordNoOfIElem ( LResultLoIElem ) 
        ; LResultHiBitwordNo := BitwordNoOfIElem ( LResultHiIElem ) 
        ; LResultBitwordCt := LResultHiBitwordNo - LResultLoBitwordNo + 1 
        ; LHashAbsIsComputable := LResultBitwordCt <= 2 
        ; LSetSs := ArraySsOfBitwordNo ( LResultLoBitwordNo , BSetInfo . Bias )
        ; LLoBitword := BSetBitwords [ LSetSs ] 
        ; LLoBitword 
            := Word . And ( LLoBitword , GEMaskOfIElem ( LResultLoIElem ) ) 
          (* ^Remove result low garbage bits. *) 
        ; IF LResultBitwordCt = 1 
          THEN (* There is exactly one result Bitword. *)  
            LLoBitword 
              := Word . And ( LLoBitword , LEMaskOfIElem ( LResultHiIElem ) )
                 (* ^Remove result high garbage bits from LLoBitword. *) 
          ; LResultCard := NoOf1BitsInBitword ( LLoBitword ) 
          ; LLoBitword 
              := Word . Or 
                   ( LLoBitword , LTMaskOfIElem ( LResultLoIElem ) )
            (* ^Set result low garbage bits of LLoBitword. *) 
          ; LLoBitword 
              := Word . Or 
                   ( LLoBitword , GTMaskOfIElem ( LResultHiIElem ) )
            (* ^Set result high garbage bits of LLoBitword. *) 
          ; LResultHasA0Bit := LLoBitword # AllOnes 
          ; IF NOT LResultHasA0Bit 
            THEN
              RETURN 
                ConstructRangeset 
                  ( Lo := LResultLoIElem , Hi := LResultHiIElem ) 
            ELSIF BSet # NIL 
                  AND DoReuseBitwordArray 
                        ( NUMBER ( BSetBitwords ) 
                        , LResultBitwordCt 
                        ) 
            THEN 
              LResultBitwordArrayRef := BSet . BitwordArrayRef   
            ; LBitwordsNewlyAllocated := FALSE 
            ; LResultBias := BSetInfo . Bias 
            ELSE 
              LResultBitwordArrayRef := NewBitwordArray ( 1 ) 
            ; LBitwordsNewlyAllocated := TRUE  
            ; LResultBitwordArrayRef ^ [ 0 ] := LLoBitword 
            ; LResultBias := LResultLoBitwordNo 
            END (* IF *) 
          ; LHashAbs := LLoBitword 
          ELSE (* There is a distinct high Bitword in the result. *)  
            WITH 
              WMiddle 
              = SUBARRAY 
                  ( BSetBitwords 
                  , LSetSs + 1 
                  , LResultBitwordCt - 2 
                  )
            DO 
              LHiBitword 
                := BSetBitwords 
                     [ LSetSs + LResultBitwordCt - 1  ] 
            ; LHiBitword 
                := Word . And ( LHiBitword , LEMaskOfIElem ( LResultHiIElem ) )
                   (* ^Temporarily zero result hi garbage bits. *) 
            ; LMiddleHasA1Bit := NOT BitwordArrayIsAllZeros ( WMiddle ) 
            ; LResultCard 
                := Word . Plus 
                     ( NoOf1BitsInBitword ( LLoBitword ) 
                     , NoOf1BitsInBitword ( LHiBitword ) 
                     ) 
            ; LLoBitword 
                := Word . Or 
                     ( LLoBitword , LTMaskOfIElem ( LResultLoIElem ) )
              (* ^Set result low garbage bits of LLoBitword. *) 
            ; LHiBitword 
                := Word . Or 
                     ( LHiBitword , GTMaskOfIElem ( LResultHiIElem ) )
              (* ^Set result high garbage bits of LHiBitword. *) 
            ; LMiddleHasA0Bit := NOT BitwordArrayIsAllOnes ( WMiddle ) 
            ; LResultHasA0Bit 
                := LLoBitword # AllOnes 
                   OR LHiBitword # AllOnes 
                   OR LMiddleHasA0Bit 
            ; IF NOT LResultHasA0Bit 
              THEN
                RETURN 
                  ConstructRangeset 
                    ( Lo := LResultLoIElem , Hi := LResultHiIElem ) 
              ELSE
                IF NOT LMiddleHasA0Bit 
                THEN 
                  LResultCard 
                    := Word . Plus 
                         ( LResultCard 
                         , Word . Times 
                             ( LResultBitwordCt - 2 , BitsPerBitword )
                         )
                ELSIF LMiddleHasA1Bit 
                THEN 
                  LResultCard := CardUnknown 
                END (* IF *) 
              ; LHashAbs := Word . Xor ( LLoBitword , LHiBitword ) 
              ; IF BSet # NIL 
                   AND DoReuseBitwordArray 
                         ( NUMBER ( BSetBitwords ) 
                         , LResultBitwordCt 
                         ) 
                THEN 
                  LResultBitwordArrayRef := BSet . BitwordArrayRef 
                ; LBitwordsNewlyAllocated := FALSE 
                ; LResultBias := BSetInfo . Bias 
                ELSE
                  LResultBitwordArrayRef := NewBitwordArray ( LResultBitwordCt )
                ; LBitwordsNewlyAllocated := TRUE  
                ; LResultBias := LResultLoBitwordNo 
                ; LResultBitwordArrayRef ^ [ 0 ] := LLoBitword 
                ; SUBARRAY 
                    ( LResultBitwordArrayRef ^ , 1 , LResultBitwordCt - 2 ) 
                  := WMiddle 
                ; LResultBitwordArrayRef ^ [ LResultBitwordCt - 1 ] 
                    := LHiBitword 
                END (* IF *) 
              END (* IF *) 
            END (* WITH *) 
          END (* IF *) 
        ; LResultInfo . BitsetLo := LResultLoIElem 
        ; LResultInfo . BitsetHi := LResultHiIElem 
        ; LResultInfo . Bias := LResultBias  
        ; LResultInfo . Card := LResultCard 
        ; StoreBitsetHashAbs 
            ( LResultInfo , LHashAbs , LHashAbsIsComputable ) 
        ; LResult 
            := ConstructBitset 
                 ( LResultInfo 
                 , LResultBitwordArrayRef 
                 , WasNewlyAllocated := LBitwordsNewlyAllocated 
                 ) 
        ; RETURN LResult 
        END (* IF *) 
      END (* IF *) 
    END IntersectionRangeBitset 

; PROCEDURE IntersectionBitsets 
    ( READONLY BSet1Info : BitsetInfoTyp 
    ; READONLY BSet1Bitwords : ARRAY OF BitwordTyp 
    ; READONLY BSet2Info : BitsetInfoTyp 
    ; READONLY BSet2Bitwords : ARRAY OF BitwordTyp 
    ) 
  : T 

  = VAR LResultLoIElem : IElemTyp 
  ; VAR LResultHiIElem : ValidIElemTyp 
  ; VAR LResultLoBitwordNo , LResultHiBitwordNo : BitwordNoTyp 
  ; VAR LLoBitword , LHiBitword , LBitword : BitwordTyp 
  ; VAR LResultBitwordCt : CARDINAL 
  ; VAR LResultInfo : BitsetInfoTyp 
  ; VAR LResultBitwordArrayRef : BitwordArrayRefTyp 
  ; VAR LResult : T
  ; VAR LResultCard : CardTyp 
  ; VAR LSet1Ss : ArraySsTyp
  ; VAR LSet2Ss : ArraySsTyp
  ; VAR LResultSs : ArraySsTyp
  ; VAR LHashAbs : HashTyp 
  ; VAR LResultHasA0Bit , LResultHasA1Bit : BOOLEAN 

  ; BEGIN 
      IF BSet1Info . BitsetHi < BSet2Info . BitsetLo 
         OR BSet2Info . BitsetHi < BSet1Info . BitsetLo 
      THEN (* No possible overlap, result is empty. *) 
        RETURN NIL 
      ELSE 
        LResultLoIElem 
          := MAX ( BSet1Info . BitsetLo , BSet2Info . BitsetLo ) 
      ; LResultHiIElem 
          := MIN ( BSet1Info . BitsetHi , BSet2Info . BitsetHi ) 
      ; LResultLoIElem 
          := LeastIntersectionIElemInRange 
               ( BSet1Info , BSet1Bitwords 
               , BSet2Info , BSet2Bitwords 
               , LResultLoIElem , LResultHiIElem 
               ) 
      ; IF LResultLoIElem = IElemNull 
        THEN (* No bit in intersection, result is empty. *) 
          RETURN NIL 
        ELSE 
          LResultHiIElem (* Can't fail. *) 
            := GreatestIntersectionIElemInRange 
                 ( BSet1Info , BSet1Bitwords 
                 , BSet2Info , BSet2Bitwords 
                 , LResultLoIElem , LResultHiIElem 
                 ) 
        ; LResultLoBitwordNo := BitwordNoOfIElem ( LResultLoIElem ) 
        ; LResultHiBitwordNo := BitwordNoOfIElem ( LResultHiIElem ) 
        ; LResultBitwordCt := LResultHiBitwordNo - LResultLoBitwordNo + 1 
        ; LSet1Ss 
            := ArraySsOfBitwordNo ( LResultLoBitwordNo , BSet1Info . Bias )
        ; LSet2Ss 
            := ArraySsOfBitwordNo ( LResultLoBitwordNo , BSet2Info . Bias )
        ; LLoBitword 
            := Word . And 
                 ( BSet1Bitwords [ LSet1Ss ] , BSet2Bitwords [ LSet2Ss ] ) 
        ; LLoBitword 
            := Word . And ( LLoBitword , GEMaskOfIElem ( LResultLoIElem ) ) 
          (* ^Remove result low garbage bits. *) 

        ; IF LResultBitwordCt = 1 
          THEN (* There is exactly one result Bitword. *)  
            LLoBitword 
              := Word . And ( LLoBitword , LEMaskOfIElem ( LResultHiIElem ) )
                 (* ^Zero result high garbage bits from LLoBitword. *) 
          ; LResultHasA1Bit := LLoBitword # 0 
          ; LResultCard := NoOf1BitsInBitword ( LLoBitword ) 
          ; LLoBitword 
              := Word . Or 
                   ( LLoBitword , LTMaskOfIElem ( LResultLoIElem ) )
            (* ^Set result low garbage bits of LLoBitword. *) 
          ; LLoBitword 
              := Word . Or 
                   ( LLoBitword , GTMaskOfIElem ( LResultHiIElem ) )
            (* ^Set result high garbage bits of LLoBitword. *) 
          ; LResultHasA0Bit := LLoBitword # AllOnes 
          ; IF NOT LResultHasA0Bit 
            THEN
              RETURN 
                ConstructRangeset 
                  ( Lo := LResultLoIElem , Hi := LResultHiIElem ) 
            ELSE 
              LResultBitwordArrayRef := NewBitwordArray ( 1 ) 
            ; LHashAbs := LLoBitword 
            ; LResultBitwordArrayRef ^ [ 0 ] := LLoBitword 
            END (* IF *) 
          ELSE (* There is a distinct high Bitword in the result. *)  
            LResultCard := NoOf1BitsInBitword ( LLoBitword )  
          ; LResultHasA0Bit := FALSE 
          ; LLoBitword 
              := Word . Or 
                   ( LLoBitword , LTMaskOfIElem ( LResultLoIElem ) )
            (* ^Set result low garbage bits of LLoBitword. *) 
          ; LResultHasA0Bit := LLoBitword # AllOnes 
          ; LResultBitwordArrayRef := NewBitwordArray ( LResultBitwordCt ) 
          ; LHashAbs := LLoBitword 
          ; LResultBitwordArrayRef ^ [ 0 ] := LLoBitword 
          ; LResultSs := 1 
          ; INC ( LSet1Ss ) 
          ; INC ( LSet2Ss )
          ; FOR RI := 3 TO LResultBitwordCt 
            DO 
              LBitword 
                := Word . And 
                     ( BSet1Bitwords [ LSet1Ss ] , BSet2Bitwords [ LSet2Ss ] ) 
            ; LResultCard 
              := Word . Plus ( LResultCard , NoOf1BitsInBitword ( LBitword ) )  
            ; LResultHasA0Bit := LResultHasA0Bit OR LBitword # AllOnes 
            ; LHashAbs := Word . Xor ( LHashAbs , LBitword ) 
            ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword  
            ; INC ( LSet1Ss ) 
            ; INC ( LSet2Ss )
            ; INC ( LResultSs )
            END (* FOR *) 
          ; LHiBitword 
              := Word . And 
                   ( BSet1Bitwords [ LSet1Ss ] , BSet2Bitwords [ LSet2Ss ] ) 
          ; LHiBitword 
              := Word . And ( LHiBitword , LEMaskOfIElem ( LResultHiIElem ) )
                 (* ^Remove result high garbage bits from LHiBitword. *) 
          ; LResultCard 
              := Word . Plus 
                   ( LResultCard , NoOf1BitsInBitword ( LHiBitword ) ) 
          ; LHiBitword 
              := Word . Or 
                   ( LHiBitword , GTMaskOfIElem ( LResultHiIElem ) )
            (* ^Set result high garbage bits of LHiBitword. *) 
          ; LHashAbs := Word . Xor ( LHashAbs , LHiBitword ) 
          ; LResultBitwordArrayRef ^ [ LResultSs ] := LHiBitword  
          ; LResultHasA0Bit := LResultHasA0Bit OR LHiBitword # AllOnes 
          ; IF NOT LResultHasA0Bit 
            THEN (* Abandon LResultBitwordArrayRef as garbage. *) 
              RETURN 
                ConstructRangeset 
                  ( Lo := LResultLoIElem , Hi := LResultHiIElem ) 
            END (* IF *) 
          END (* IF Number of Bitwords in result. *) 
        ; LResultInfo . BitsetLo := LResultLoIElem 
        ; LResultInfo . BitsetHi := LResultHiIElem 
        ; LResultInfo . Bias := LResultLoBitwordNo 
        ; LResultInfo . Card := LResultCard 
        ; StoreBitsetHashAbs ( LResultInfo , LHashAbs ) 
        ; LResult := ConstructBitset ( LResultInfo , LResultBitwordArrayRef ) 
        ; RETURN LResult 
        END (* IF *) 
      END (* IF *) 
    END IntersectionBitsets 

(* VISIBLE: *) 
; PROCEDURE Intersection ( Set1 : T ; Set2 : T ) : T 
(* Intersection of Set1 and Set2. *) 

  = VAR IntersectionResult : T

  ; PROCEDURE InnerIntersection 
      ( READONLY DInfo1 : DissectInfo
      ; READONLY Bitwords1 : ARRAY OF BitwordTyp
      ; READONLY DInfo2 : DissectInfo
      ; READONLY Bitwords2 : ARRAY OF BitwordTyp
      )

    = BEGIN (* InnerIntersection *)
        IF DInfo1 . RangeLo # IElemNull
        THEN (* Set1 is a Rangeset. *)
          IF DInfo2 . RangeLo # IElemNull
          THEN (* Set1 is a Rangeset and Set2 is a Rangeset. *)
            IntersectionResult 
              := IntersectionNonemptyRanges
                   ( DInfo1 . RangeLo , DInfo1 . RangeHi , DInfo1 . RSet 
                   , DInfo2 . RangeLo , DInfo2 . RangeHi , DInfo2 . RSet 
                   ) 
          ELSIF NUMBER ( Bitwords2 ) = 0
          THEN (* Set1 is a Rangeset and Set2 is empty. *)
            IntersectionResult := NIL 
          ELSE (* Set1 is a Rangeset and Set2 is a Bitset. *)
            IntersectionResult 
              := IntersectionRangeBitset 
                   ( DInfo1 . RangeLo , DInfo1 . RangeHi 
                   , DInfo2 . BitsetInfo , Bitwords2 , Set2 , DInfo2 . BSet  
                   ) 
          END (* IF *)

        ELSIF NUMBER ( Bitwords1 ) = 0
        THEN (* Set1 is empty. *)
          IntersectionResult := NIL 

        ELSE (* Set1 is a Bitset. *)
          IF DInfo2 . RangeLo # IElemNull
          THEN (* Set1 is a Bitset and Set2 is a Rangeset. *)
            IntersectionResult 
              := IntersectionRangeBitset 
                   ( DInfo2 . RangeLo , DInfo2 . RangeHi 
                   , DInfo1 . BitsetInfo , Bitwords1 , Set1 , DInfo1 . BSet  
                   ) 
          ELSIF NUMBER ( Bitwords2 ) = 0
          THEN (* Set1 is a Bitset and Set2 is empty. *)
            IntersectionResult := NIL 
          ELSE (* Set1 is a Bitset and Set2 is a Bitset. *)
            IntersectionResult 
              := IntersectionBitsets 
                   ( DInfo1 . BitsetInfo , Bitwords1 
                   , DInfo2 . BitsetInfo , Bitwords2 
                   ) 
          END (* IF *)
        END (* IF *)
      END InnerIntersection

  ; BEGIN (* Intersection *)
      <* FATAL ANY *> BEGIN 
        CallWithTwoSets ( Set1 , Set2 , InnerIntersection )
      END (* Block *) 
    ; RETURN IntersectionResult
    END Intersection

(* ================================ Project ================================ *)

(* VISIBLE: *) 
; PROCEDURE Project ( Set : T ; Min , Max : ElemT ) : T 
  (* Remove elements outside the range Min .. Max *)

  = VAR ProjectResult : T

  ; PROCEDURE InnerProject
      ( READONLY DInfo : DissectInfo ; READONLY Bitwords : ARRAY OF BitwordTyp )

    = VAR LIMin , LIMax : ValidIElemTyp 

    ; BEGIN (* InnerProject *)
        IF DInfo . RangeLo # IElemNull
        THEN (* Set is a Rangeset. *)
          LIMin := MAX ( ORD ( Min ) , FIRST ( ValidIElemTyp ) ) 
        ; LIMax := MIN ( ORD ( Max ) , LAST ( ValidIElemTyp ) ) 
        ; ProjectResult 
            := IntersectionNonemptyRanges
                 ( DInfo . RangeLo , DInfo . RangeHi , DInfo . RSet 
                 , RSet2Lo := LIMin , RSet2Hi := LIMax , RSet2 := NIL  
                 ) 

        ELSIF NUMBER ( Bitwords ) = 0
        THEN (* Set is empty. *)
          ProjectResult := NIL 

        ELSE (* Set is a Bitset. *)
          LIMin := MAX ( ORD ( Min ) , FIRST ( ValidIElemTyp ) ) 
        ; LIMax := MIN ( ORD ( Max ) , LAST ( ValidIElemTyp ) ) 
        ; ProjectResult 
            := IntersectionRangeBitset 
                 ( LIMin , LIMax 
                 , DInfo . BitsetInfo , Bitwords , Set , DInfo . BSet 
                 ) 
        END (* IF *)
      END InnerProject

  ; BEGIN (*  *) 
      IF Min < FIRST ( ValidElemT ) OR LAST ( ValidElemT ) < Min   
      THEN (* Min is not valid.  Set it to minimum valid value. *) 
        Min := FIRST ( ValidElemT )   
      END (* IF *) 
    ; IF Max < FIRST ( ValidElemT ) OR LAST ( ValidElemT ) < Max   
      THEN (* Max is not valid.  Set it to maximum valid value. *) 
        Max := LAST ( ValidElemT )   
      END (* IF *) 
    ; IF Min > Max
      THEN (* Project into empty range. *) 
        RETURN NIL 
      ELSE 
        <* FATAL ANY *> BEGIN 
          CallWithOneSet ( Set , InnerProject )
        END (* Block *) 
      ; RETURN ProjectResult
      END(* IF *) 
    END Project

(* ==============================  Difference ============================== *)

; PROCEDURE DifferenceNonemptyUnequalRanges 
    ( RSetLLo , RSetLHi : ValidIElemTyp ; RSetL : RangesetTyp := NIL 
    ; RSetRLo , RSetRHi : ValidIElemTyp  
    ) 
  : T 

  (* PRE: RSetL # NIL IMPLIES RSetLLo and RSetLHi come from RSetL. 
     PRE: The sets' ranges are non-empty. 
     PRE: The sets' ranges are not identical.
  *) 

  = BEGIN 
      IF RSetLHi < RSetRLo 
         OR RSetRHi < RSetLLo 
      THEN (* Disjoint *)
        RETURN TryToReuseRangeset ( RSetL , RSetLLo , RSetLHi )  
      ELSIF RSetLLo < RSetRLo 
      THEN (* LLo < RLo. *) 
        IF RSetRHi < RSetLHi  
        THEN (* LLo < RLo <= RHi < LHi *) 
          RETURN 
            UnionUntouchingOrderedRanges 
              ( RSetLLo 
              , RSetRLo - 1 
              , RSetRHi + 1 
              , RSetLHi
              ) 
        ELSE (* LLo < RLo <= LHi <= RHi *) 
          RETURN 
            ConstructRangeset ( Lo := RSetLLo , Hi := RSetRLo - 1 ) 
        END (* IF *) 
      ELSE (* RLo <= LLo *) 
        IF RSetRHi < RSetLHi
        THEN (* RLo <= LLo <= RHi < LHi *) 
          RETURN ConstructRangeset ( Lo := RSetRHi + 1 , Hi := RSetLHi ) 
        ELSE (* RLo <= LLo <= LHi <= RHi *) 
          RETURN NIL 
        END (* IF *) 
      END (* IF *) 
    END DifferenceNonemptyUnequalRanges  

; PROCEDURE DifferenceOverlappingRangeBitset  
    ( RSetLLo , RSetLHi : ValidIElemTyp ; RSetL : RangesetTyp := NIL 
    ; READONLY BSetRInfo : BitsetInfoTyp 
    ; READONLY BSetRBitwords : ARRAY OF BitwordTyp 
    ) 
  : T 

  (* PRE: RSetL # NIL IMPLIES RSetLLo and RSetLHi come from RSetL. 
     PRE: The two sets have a bit number in common. 
  *) 

  = VAR LBSetRLoBitwordNo , LBSetRHiBitwordNo : BitwordNoTyp 
  ; VAR LResultLoIElem , LResultHiIElem : ValidIElemTyp 
  ; VAR LIElem : IElemTyp 
  ; VAR LResultLoBitwordNo , LResultHiBitwordNo : BitwordNoTyp 
  ; VAR LOverlapLoBitwordNo , LOverlapHiBitwordNo : BitwordNoTyp 
  ; VAR LSetRLoSs , LSetRHiSs , LSetRSs , LResultSs : ArraySsTyp
  ; VAR LLoBitword , LHiBitword , LResultBitword : BitwordTyp 
  ; VAR L1BitCt : BitCtTyp  
  ; VAR LResultBitwordCt , LBitwordCtL , LBitwordCtR : CARDINAL 
  ; VAR LResultCard : CardTyp (* Can be temporarily negative. *) 
  ; VAR LHashAbs : HashTyp 
  ; VAR LResultInfo : BitsetInfoTyp 
  ; VAR LResultBitwordArrayRef : BitwordArrayRefTyp 
  ; VAR LResult : T
  ; VAR LResultIsRange : BOOLEAN 

  ; BEGIN 
      LBSetRLoBitwordNo := BitwordNoOfIElem ( BSetRInfo . BitsetLo ) 
    ; LBSetRHiBitwordNo := BitwordNoOfIElem ( BSetRInfo . BitsetHi ) 

    (* Figure out what we have at the low end of the result. *) 
    ; IF RSetLLo < BSetRInfo . BitsetLo 
      THEN (* The range has a lower numbered element than BSetR has. *) 
        LResultLoIElem := RSetLLo 
      ; LResultLoBitwordNo := BitwordNoOfIElem ( RSetLLo ) 
      ; LOverlapLoBitwordNo := LBSetRLoBitwordNo 
      ; LSetRLoSs 
          := ArraySsOfBitwordNo ( LOverlapLoBitwordNo , BSetRInfo . Bias ) 
      ; LLoBitword := Word . Not ( BSetRBitwords [ LSetRLoSs ] ) 
        (* ^Negate to get result bits. *) 
      ; LLoBitword 
          := Word . Or ( LLoBitword , LTMaskOfIElem ( BSetRInfo . BitsetLo ) ) 
        (* ^These bits correspond to low garbage bits in BSetR.  In the result,
           each is either a meaningful 1-bit from the range or a garbage bit. 
        *) 
      ELSE (* The range does not extend lower than BSetR. *) 
        IF BSetRInfo . BitsetHi < RSetLHi   
        THEN (* The range extends higher than BSetR. *)  
          LIElem 
            := LeastAbsentIElemOfBSetInRange
                 ( BSetRInfo , BSetRBitwords  , RSetLLo , BSetRInfo . BitsetHi )
        ; IF LIElem = IElemNull 
          THEN (* No such 0-bit.  Result is the upper end of SetL. *) 
            RETURN 
              ConstructRangeset 
                ( Lo := BSetRInfo . BitsetHi + 1 , Hi := RSetLHi ) 
          END (* IF *) 
        ELSE (* The range does not extend higher than BSetR. *)  
          LIElem 
            := LeastAbsentIElemOfBSetInRange 
                 ( BSetRInfo , BSetRBitwords  , RSetLLo , RSetLHi )
        ; IF LIElem = IElemNull 
          THEN (* No such 0-bit.  Result is empty. *) 
            RETURN NIL  
          END (* IF *) 
        END (* IF *) 
      ; LResultLoIElem := LIElem 
      ; LResultLoBitwordNo := BitwordNoOfIElem ( LResultLoIElem )
      ; LOverlapLoBitwordNo := LResultLoBitwordNo  
      ; LSetRLoSs 
          := ArraySsOfBitwordNo ( LOverlapLoBitwordNo , BSetRInfo . Bias ) 
      ; LLoBitword := Word . Not ( BSetRBitwords [ LSetRLoSs ] ) 
      ; LLoBitword 
          := Word . Or ( LLoBitword , LTMaskOfIElem ( LResultLoIElem ) ) 
        (* ^These bits will become low garbage bits in the result. *) 
      END (* IF *) 

    (* Figure out what we have at the high end of the result. *) 
    ; IF RSetLHi > BSetRInfo . BitsetHi 
      THEN (* The range has a higher numbered element than BSetR. *) 
        LResultHiIElem := RSetLHi 
      ; LResultHiBitwordNo := BitwordNoOfIElem ( RSetLHi ) 
      ; LOverlapHiBitwordNo := LBSetRHiBitwordNo 
      ; IF LOverlapLoBitwordNo = LOverlapHiBitwordNo 
        THEN (* High Bitword is the already partially computed low Bitword. *)
          LSetRHiSs := LSetRLoSs 
        ; LLoBitword 
            := Word . Or ( LLoBitword , GTMaskOfIElem ( BSetRInfo . BitsetHi ) )
          (* ^These bits correspond to high garbage bits in BSetR.  In the 
             result, they are either meaningful 1-bits from the range or 
             garbage bits. 
          *) 
        ELSE 
          LSetRHiSs 
            := ArraySsOfBitwordNo ( LOverlapHiBitwordNo , BSetRInfo . Bias ) 
        ; LHiBitword := Word . Not ( BSetRBitwords [ LSetRHiSs ] )
        (* ^Negate to get result bits. *) 
        ; LHiBitword 
            := Word . Or ( LHiBitword , GTMaskOfIElem ( BSetRInfo . BitsetHi ) )
          (* ^These bits correspond to high garbage bits in BSetR.  In the 
             result, they are either meaningful 1-bits from the range or 
             garbage bits. 
          *) 
        END (* IF *)       
      ELSE (* The range does not extend higher than BSetR. *) 
        IF BSetRInfo . BitsetLo > RSetLLo 
        THEN (* The range extends lower than BSetR. *) 
          LIElem 
            := GreatestAbsentIElemOfBSetInRange
                 ( BSetRInfo , BSetRBitwords  , BSetRInfo . BitsetLo , RSetLHi )
        ; IF LIElem = IElemNull 
          THEN (* No such 0-bit.  REsult is the lower ens of SetL. *) 
            RETURN 
              ConstructRangeset 
                ( Lo := RSetLLo , Hi := BSetRInfo . BitsetLo - 1 ) 
          END (* IF *) 
        ELSE (* The range does not extend lower than BSetR. *) 
          LIElem 
            := GreatestAbsentIElemOfBSetInRange 
                 ( BSetRInfo , BSetRBitwords  , LResultLoIElem , RSetLHi )
               (* ^Can't fail, or we would have found it above. *) 
        ; <* ASSERT LIElem # IElemNull *> 
          <* ASSERT LIElem > BSetRInfo . BitsetLo *>
        END (* IF *) 
      ; LResultHiIElem := LIElem 
      ; LResultHiBitwordNo := BitwordNoOfIElem ( LResultHiIElem )
      ; LOverlapHiBitwordNo := LResultHiBitwordNo
      ; IF LOverlapLoBitwordNo = LOverlapHiBitwordNo 
        THEN (* High Bitword is the already partially computed low Bitword. *)
          LSetRHiSs := LSetRLoSs 
        ; LLoBitword 
            := Word . Or ( LLoBitword , GTMaskOfIElem ( LResultHiIElem ) )
          (* ^These bits will become high garbage bits in the result. *) 
        ELSE 
          LSetRHiSs 
            := ArraySsOfBitwordNo ( LOverlapHiBitwordNo , BSetRInfo . Bias ) 
        ; LHiBitword := Word . Not ( BSetRBitwords [ LSetRHiSs ] )
        (* ^Negate to get result bits. *) 
        ; LHiBitword 
            := Word . Or ( LHiBitword , GTMaskOfIElem ( LResultHiIElem ) )
          (* ^These bits will become high garbage bits in the result. *) 
        END (* IF *)       
      END (* IF *) 

    (* Construct the result. *) 

    (* Check easy cases of result is a Rangeset, no allocation required: *) 
    ; IF LResultLoIElem + 1 >= LResultHiIElem 
      THEN (* Singleton or doubleton. *) 
        RETURN TryToReuseRangeset ( RSetL , LResultLoIElem , LResultHiIElem ) 
      ELSIF LOverlapLoBitwordNo = LOverlapHiBitwordNo 
      THEN (* One Bitword of overlap in result. *) 
        IF LLoBitword = AllOnes  
        THEN (* And non-overlapping Bitwords are all ones too. *)  
          RETURN 
            TryToReuseRangeset ( RSetL , LResultLoIElem , LResultHiIElem ) 
        ELSE 
          LResultIsRange := FALSE 
        END (* IF *) 
      ELSIF LOverlapLoBitwordNo + 1 = LOverlapHiBitwordNo 
      THEN (* Two Bitwords of overlap in result. *) 
        IF LLoBitword = AllOnes AND LHiBitword = AllOnes   
        THEN (* And non-overlapping Bitwords are all ones too. *)  
          RETURN 
            TryToReuseRangeset ( RSetL , LResultLoIElem , LResultHiIElem ) 
        ELSE 
          LResultIsRange := FALSE 
        END (* IF *) 
      ELSE (* Three or more overlapping Bitwords. *) 
        LResultIsRange := LLoBitword = AllOnes AND LHiBitword = AllOnes   
        (* ^Refutable. *) 
      END (* IF *) 

    (* Here, if there are one or two overlapping Bitwords, then the entire
       result is not a range.  If three or more, knowing whether the result
       is a range requires looking at the intermediate overlapping Bitwords.
       Either way, we now need to allocate a result BitwordArray, to store
       the possibly-needed intermediate result Bitwords. 
    *)  

    ; LResultBitwordCt := LResultHiBitwordNo - LResultLoBitwordNo + 1 
    ; LResultBitwordArrayRef := NewBitwordArray ( LResultBitwordCt ) 
    ; LResultCard 
        := Word . Minus ( 0 , BitNo ( LResultLoIElem ) ) 
           (* ^Discount result lo garbage bits. *)
    ; LResultCard 
        := Word . Minus  
             ( LResultCard , BitsPerBitword - 1 - BitNo ( LResultHiIElem ) ) 
             (* ^Discount result high garbage bits. *) 

    ; LSetRSs := LSetRLoSs + 1   
    ; LResultSs 
        := ArraySsOfBitwordNo 
             ( LOverlapLoBitwordNo + 1 , Bias := LResultLoBitwordNo ) 
    (* Handle interior overlapped words. *) 
    ; LHashAbs := 0 
    ; FOR RI := 2 TO LOverlapHiBitwordNo - LOverlapLoBitwordNo 
      DO 
        LResultBitword 
          := Word . Not ( BSetRBitwords [ LSetRSs ] )  
      ; L1BitCt := NoOf1BitsInBitword ( LResultBitword ) 
      ; LResultIsRange := LResultIsRange AND  L1BitCt = BitsPerBitword 
      ; LHashAbs := Word . Xor ( LHashAbs , LResultBitword ) (* Add *) 
      ; LResultBitwordArrayRef ^ [ LResultSs ] := LResultBitword 
      ; LResultCard := Word . Plus ( LResultCard , L1BitCt )
      ; INC ( LSetRSs ) 
      ; INC ( LResultSs )
      END (* FOR *) 
    ; IF LResultIsRange 
      THEN 
        RETURN TryToReuseRangeset ( RSetL , LResultLoIElem , LResultHiIElem ) 
      ELSE (* Result is a Bitset. *) 
        LResultInfo . BitsetLo := LResultLoIElem 
      ; LResultInfo . BitsetHi := LResultHiIElem 
      ; LResultInfo . Bias := LResultLoBitwordNo 

      (* Set any low outlying whole words to AllOnes. *) 
      ; LResultSs := 0 
      ; LBitwordCtL := LOverlapLoBitwordNo - LResultLoBitwordNo 
      ; AssignAllOnesToBitwordArray 
          ( SUBARRAY ( LResultBitwordArrayRef ^ , LResultSs , LBitwordCtL ) ) 
      ; INC ( LResultSs , LBitwordCtL ) 
      ; LResultCard 
          := Word . Plus 
               ( LResultCard , Word . Times ( LBitwordCtL , BitsPerBitword ) ) 
      (* Store the low (and maybe high) overlapping Bitword. *) 
      ; LResultSs 
          := ArraySsOfBitwordNo ( LOverlapLoBitwordNo , LResultInfo . Bias )
      ; LHashAbs := Word . Xor ( LHashAbs , LLoBitword ) (* Add *) 
      ; LResultBitwordArrayRef ^ [ LResultSs ] := LLoBitword  
      ; LResultCard 
          := Word . Plus ( LResultCard , NoOf1BitsInBitword ( LLoBitword ) ) 
      ; IF LOverlapLoBitwordNo < LOverlapHiBitwordNo 
        THEN (* Set the high overlapped word. *) 
          LResultSs 
            := ArraySsOfBitwordNo ( LOverlapHiBitwordNo , LResultInfo . Bias )
        ; LHashAbs := Word . Xor ( LHashAbs , LHiBitword ) (* Add *) 
        ; LResultBitwordArrayRef ^ [ LResultSs ] := LHiBitword  
        ; LResultCard 
            := Word . Plus ( LResultCard , NoOf1BitsInBitword ( LHiBitword ) ) 
        END (* IF *) 
      ; INC ( LResultSs )

      (* Set any high outlying whole words to AllOnes. *) 
      ; LBitwordCtR := LResultHiBitwordNo - LOverlapHiBitwordNo  
      ; AssignAllOnesToBitwordArray 
          ( SUBARRAY ( LResultBitwordArrayRef ^ , LResultSs , LBitwordCtR ) ) 
      ; LResultCard 
          := Word . Plus 
               ( LResultCard , Word . Times ( LBitwordCtR , BitsPerBitword ) ) 
      ; <* ASSERT Word . GE ( LResultCard , 2 ) *> 
        LResultInfo . Card := LResultCard  
      ; IF ( LBitwordCtL + LBitwordCtR ) MOD 2 # 0 
        THEN 
          LHashAbs := Word . Not ( LHashAbs ) 
        END (* IF *) 
      ; StoreBitsetHashAbs ( LResultInfo , LHashAbs ) 
      ; LResult := ConstructBitset ( LResultInfo , LResultBitwordArrayRef ) 
      ; RETURN LResult  
      END (* IF *) 
    END DifferenceOverlappingRangeBitset  

; PROCEDURE DifferenceOverlappingBitsetRange
    ( READONLY BSetLInfo : BitsetInfoTyp 
    ; READONLY BSetLBitwords : ARRAY OF BitwordTyp 
    ; SetL : T 
    ; BSetL : BitsetTyp 
    ; RSetRLo , RSetRHi : ValidIElemTyp 
    ) 
  : T 

  (* PRE: BSetLInfo and BSetLBitwords come from SetL. 
     PRE: BSetL # NIL IMPLIES BSetLInfo and BSetLBitwords come from BSetL.
     PRE: RSetRLo <= RSetRHi. 
     PRE: SetL has a bit in RSetRLo .. RSetRHi.
  *) 

  = VAR LSetLLoBitwordNo , LSetLHiBitwordNo : BitwordNoTyp 
  ; VAR LRangeLoBitwordNo , LRangeHiBitwordNo : BitwordNoTyp 
  ; VAR LResultSs : ArraySsTyp
  ; VAR LLoMask , LHiMask : BitwordTyp 
  ; VAR LLoBitword , LHiBitword : BitwordTyp 
  ; VAR LResultBitwordCt , LBitwordCt : CARDINAL 
  ; VAR LResultInfo : BitsetInfoTyp 
  ; VAR LResultBitwordArrayRef : BitwordArrayRefTyp 
  ; VAR LResult : T
  ; VAR LResultCard : CardTyp (* Can be temporarily negative. *) 
  ; VAR LHashIncr , LHashAbs : HashTyp 
  ; VAR LResultCardIsComputable : BOOLEAN 
  ; VAR LHashIncrIsComputable , LHashAbsIsComputable : BOOLEAN 
   
  ; BEGIN 
      LSetLLoBitwordNo := BitwordNoOfIElem ( BSetLInfo . BitsetLo ) 
    ; LSetLHiBitwordNo := BitwordNoOfIElem ( BSetLInfo . BitsetHi ) 
    ; IF BSetLInfo . BitsetHi > RSetRHi
      THEN (* 2Hi < 1Hi.  The high bit of BSetL will not be removed and is 
              the high bit of the result. 
           *) 
        IF BSetLInfo . BitsetLo < RSetRLo 
        THEN (* 1Lo < 2Lo <= 2Hi < 1Hi *) 
        (* Additionally, the low bit of BSetL will not be removed either
           and is the low bit of the result.  The bounds define a doubly 
           proper subrange of the range BSetL. 
        *) 
        (* Check whether the hole of 0-bits that we need to put into BSetL
           is already there. 
        *) 
          IF BitsetRangeIsEmpty 
               ( BSetLInfo , BSetLBitwords , RSetRLo , RSetRHi ) 
          THEN (* The result = SetL. *)  
            RETURN SetL 
          END (* IF *)  
        (* Can't reuse BSetL.  Construct the result. *) 
        ; LResultInfo . BitsetLo := BSetLInfo . BitsetLo 
        ; LResultInfo . BitsetHi := BSetLInfo . BitsetHi 
        ; LResultBitwordCt := LSetLHiBitwordNo - LSetLLoBitwordNo + 1 
        ; LResultInfo . Bias := LSetLLoBitwordNo 
        ; LResultBitwordArrayRef := NewBitwordArray ( LResultBitwordCt ) 
        ; LRangeLoBitwordNo := BitwordNoOfIElem ( RSetRLo )
        ; LRangeHiBitwordNo := BitwordNoOfIElem ( RSetRHi )
        ; LResultCard 
            := Word . Minus ( 0 , BitNo ( LResultInfo . BitsetLo ) ) 
               (* ^Discount result lo garbage bits. *)
        ; LResultCard 
            := Word . Minus  
                 ( LResultCard 
                 , BitsPerBitword - 1 - BitNo ( LResultInfo . BitsetHi ) 
                 )
               (* ^Discount result high garbage bits. *) 
        ; LResultCardIsComputable := TRUE (* Could be refuted later. *) 
        ; RecoverBitsHash 
            ( BSetLInfo , (*VAR*) LHashIncrIsComputable , (*VAR*) LHashIncr )
        ; LHashAbs := 0 
        (* Copy the low full Bitwords of BSetL. *) 
        ; LBitwordCt := LRangeLoBitwordNo - LSetLLoBitwordNo  
        ; IF LBitwordCt > 0 
          THEN 
            SUBARRAY ( LResultBitwordArrayRef ^ , LResultSs , LBitwordCt ) 
            := SUBARRAY 
                 ( BSetLBitwords 
                 , ArraySsOfBitwordNo ( LSetLLoBitwordNo , BSetLInfo . Bias ) 
                 , LBitwordCt 
                 ) 
          ; LResultCardIsComputable := FALSE 
          ; LHashAbsIsComputable := FALSE 
          ; INC ( LResultSs , LBitwordCt ) 

          END 
        (* Compute the midsection. *) 
        ; LLoBitword 
            := BSetLBitwords 
                 [ ArraySsOfBitwordNo ( LRangeLoBitwordNo , BSetLInfo . Bias ) ]
        ; LLoMask := GEMaskOfIElem ( RSetRLo ) 
        ; IF LRangeLoBitwordNo = LRangeHiBitwordNo 
          THEN (* Only one Bitword covered by the bounds. *) 
            IF LRangeLoBitwordNo = LSetLLoBitwordNo 
            THEN 
              LLoBitword 
                := Word . Or 
                     ( LLoBitword , LTMaskOfIElem ( BSetLInfo . BitsetLo ) )
              (* ^Set result low garbage bits. *) 
            END (* IF *) 
          ; IF LRangeLoBitwordNo = LSetLHiBitwordNo 
            THEN 
              LLoBitword 
                := Word . Or 
                     ( LLoBitword , GTMaskOfIElem ( BSetLInfo . BitsetHi ) )
              (* ^Set result high garbage bits. *) 
            END (* IF *) 
          ; LHashIncr := Word . Xor ( LHashIncr , LLoBitword ) (* Remove. *) 
          ; LLoMask := Word . And ( LLoMask , LEMaskOfIElem ( RSetRHi ) )
          ; LLoBitword := Word . And ( LLoBitword , Word . Not ( LLoMask ) ) 
          ; LHashIncr := Word . Xor ( LHashIncr , LLoBitword ) (* Add. *) 
          ; LHashAbs := Word . Xor ( LHashAbs , LLoBitword ) (* Add. *) 
          ; LResultBitwordArrayRef ^ [ LResultSs ] := LLoBitword 
          ; LResultCard 
              := Word . Plus 
                   ( LResultCard , NoOf1BitsInBitword ( LLoBitword ) ) 
          ; INC ( LResultSs )  
          ELSE (* Multiple Bitwords covered by the bounds. *) 
            IF LRangeLoBitwordNo = LSetLLoBitwordNo 
            THEN 
              LLoBitword 
                := Word . Or 
                     ( LLoBitword , LTMaskOfIElem ( BSetLInfo . BitsetLo ) )
              (* ^Set result low garbage bits. *) 
            END (* IF *) 
          ; LHashIncr := Word . Xor ( LHashIncr , LLoBitword ) (* Remove. *) 
          ; LLoBitword := Word . And ( LLoBitword , Word . Not ( LLoMask ) ) 
          ; LHashIncr := Word . Xor ( LHashIncr , LLoBitword ) (* Add. *) 
          ; LHashAbs := Word . Xor ( LHashAbs , LLoBitword ) (* Add. *) 
          ; LResultBitwordArrayRef ^ [ LResultSs ] := LLoBitword 
          ; LResultCard 
              := Word . Plus 
                   ( LResultCard , NoOf1BitsInBitword ( LLoBitword ) ) 
          ; INC ( LResultSs )  

          (* Middle Bitwords of the bounds' range: *) 
          ; LBitwordCt := LRangeHiBitwordNo - LRangeLoBitwordNo - 1 
          ; IF LBitwordCt > 0 
            THEN
              LHashIncrIsComputable := FALSE  
            ; AssignAllZerosToBitwordArray 
                ( SUBARRAY 
                    ( LResultBitwordArrayRef ^ , LResultSs , LBitwordCt ) 
                ) 
            ; INC ( LResultSs , LBitwordCt ) 
            END (* IF *) 

          (* High Bitword of result: *) 
          ; LHiBitword 
              := BSetLBitwords 
                   [ ArraySsOfBitwordNo 
                       ( LRangeHiBitwordNo , BSetLInfo . Bias ) 
                   ] 
          ; IF LRangeHiBitwordNo = LSetLHiBitwordNo 
            THEN 
              LHiBitword 
                := Word . Or 
                     ( LHiBitword , GTMaskOfIElem ( BSetLInfo . BitsetHi ) )
              (* ^Set result high garbage bits. *) 
            END (* IF *) 
          ; LHashIncr := Word . Xor ( LHashIncr , LHiBitword ) (* Remove. *) 
          ; LHiMask := LEMaskOfIElem ( RSetRHi )
          ; LHiBitword := Word . And ( LHiBitword , Word . Not ( LHiMask ) ) 
          ; LHashIncr := Word . Xor ( LHashIncr , LHiBitword ) (* Add. *) 
          ; LHashAbs := Word . Xor ( LHashAbs , LHiBitword ) (* Add. *) 
          ; LResultBitwordArrayRef ^ [ LResultSs ] := LHiBitword 
          ; LResultCard 
              := Word . Plus 
                   ( LResultCard , NoOf1BitsInBitword ( LHiBitword ) ) 
          ; INC ( LResultSs ) 
          END (* IF *)  
        (* Copy the high full Bitwords of BSetL. *) 
        ; LBitwordCt := LSetLHiBitwordNo - LRangeHiBitwordNo  
        ; IF LBitwordCt > 0 
          THEN
            LHashAbsIsComputable := FALSE  
          ; SUBARRAY 
              ( LResultBitwordArrayRef ^ , LResultSs , LBitwordCt ) 
            := SUBARRAY 
                 ( BSetLBitwords 
                 , ArraySsOfBitwordNo 
                     ( LRangeHiBitwordNo + 1 , BSetLInfo . Bias ) 
                 , LBitwordCt 
                 ) 
          ; LResultCardIsComputable := FALSE 
          ; INC ( LResultSs , LBitwordCt - 1 ) 
          ; WITH WHiBitword = LResultBitwordArrayRef ^ [ LResultSs ] 
            DO
              WHiBitword 
                := Word . Or 
                     ( WHiBitword , GTMaskOfIElem ( BSetLInfo . BitsetHi ) )
              (* Set result hi garbage bits. *) 
            END (* WITH *) 
          END (* IF *) 
        ; IF LResultCardIsComputable 
          THEN
            <* ASSERT Word . GE ( LResultCard , 2 ) *> 
            LResultInfo . Card := LResultCard  
          ELSE 
            LResultInfo . Card := CardUnknown 
          END (* IF *) 
        ; StoreBitsetHashAbsOrIncr 
            ( LResultInfo 
            , LHashAbs , LHashAbsIsComputable 
            , LHashIncr , LHashIncrIsComputable
            ) 

        ; LResult := ConstructBitset ( LResultInfo , LResultBitwordArrayRef ) 
        ; RETURN LResult       
        ELSE (* 2Lo <= 1Lo <= 2Hi < 1Hi *) 
        (* Result comes entirely from within (2Hi,1Hi]. *) 
          RETURN 
            IntersectionRangeBitset 
              ( RSetRHi + 1 , BSetLInfo . BitsetHi 
              , BSetLInfo , BSetLBitwords , SetL , BSetL    
              ) 
        END (* IF *) 
      ELSE (* 1Hi <= 2Hi *) 
        IF BSetLInfo . BitsetLo < RSetRLo 
        THEN (* 1Lo < 2Lo <= 1Hi <= 2Hi *)
        (* Result comes entirely from within [ 1Lo , 2Lo ). *)  
          RETURN 
            IntersectionRangeBitset 
              ( BSetLInfo . BitsetLo , RSetRLo - 1 
              , BSetLInfo , BSetLBitwords , SetL , BSetL    
              ) 
        ELSE (* 2Lo <= 1Lo <= 1Hi <= 2Hi *) 
        (* The range of BSetL is a subrange of the bounds.  
           All bits of BSetL are removed.  Result is empty. 
        *) 
          RETURN NIL 
        END (* IF *) 
      END (* IF *) 
    END DifferenceOverlappingBitsetRange 

; PROCEDURE DifferenceOverlappingBitsets 
    ( READONLY BSetLInfo : BitsetInfoTyp 
    ; READONLY BSetLBitwords : ARRAY OF BitwordTyp 
    ; SetL : T   
    ; BSetL : BitsetTyp 
    ; READONLY BSetRInfo : BitsetInfoTyp 
    ; READONLY BSetRBitwords : ARRAY OF BitwordTyp 
    ) 
  : T 

  (* PRE: BSetLInfo and BSetLBitwords come from SetL. 
     PRE: BSetL # NIL IMPLIES BSetLInfo and BSetLBitwords come from BSetL.
     PRE: The ranges of BSetL and BSetR overlap. 
  *) 

  = VAR LSetLLoBitwordNo , LSetLHiBitwordNo : BitwordNoTyp 
  ; VAR LSetRLoBitwordNo , LSetRHiBitwordNo : BitwordNoTyp 
  ; VAR LOverlapLoBitwordNo , LOverlapHiBitwordNo : BitwordNoTyp 
  ; VAR LBitwordL , LBitwordR , LResultBitword : BitwordTyp 
  ; VAR LOverlapLoBitword , LOverlapHiBitword : BitwordTyp 
  ; VAR LResultLoIElem , LResultHiIElem : ValidIElemTyp 
  ; VAR LBit0IElem : IElemTyp 
  ; VAR LResultLoBitwordNo , LResultHiBitwordNo : BitwordNoTyp 
  ; VAR LBitwordCt , LResultBitwordCt : INTEGER (* Can be negative. *)  
  ; VAR LResultInfo : BitsetInfoTyp 
  ; VAR LResultBitwordArrayRef : BitwordArrayRefTyp 
  ; VAR LResult : T
  ; VAR LSetLSs , LSetRSs , LResultSs : ArraySsTyp
  ; VAR LResultCard : CardTyp (* Can be temporarily negative. *) 
  ; VAR LHashIncr , LHashRemove : HashTyp 
  ; VAR LResultCardIsComputable : BOOLEAN 
  ; VAR LHashIncrIsComputable : BOOLEAN 
  ; VAR LAllResultBitsAreOnes : BOOLEAN 

  ; BEGIN 
      LSetLLoBitwordNo := BitwordNoOfIElem ( BSetLInfo . BitsetLo ) 
    ; LSetLHiBitwordNo := BitwordNoOfIElem ( BSetLInfo . BitsetHi ) 
    ; LSetRLoBitwordNo := BitwordNoOfIElem ( BSetRInfo . BitsetLo ) 
    ; LSetRHiBitwordNo := BitwordNoOfIElem ( BSetRInfo . BitsetHi ) 
    ; LAllResultBitsAreOnes := TRUE (* Could be refuted later. *) 
    ; RecoverBitsHash 
        ( BSetLInfo , (*VAR*) LHashIncrIsComputable , (*VAR*) LHashIncr ) 

    (* Compute LResultLoIElem as the least nonzero bit of the result 
       and LResultLoBitwordNo as the containing Bitword.
       Also compute LOverlapLoBitwordNo and LOverlapLoBitword for the lowest 
       Bitword that gets its contents from both BSetL and BSetR.  
    *) 
    ; IF BSetLInfo . BitsetLo < BSetRInfo . BitsetLo 
      THEN (* The low result bit is the low bit of BSetL. *)  
        LResultLoBitwordNo := LSetLLoBitwordNo 
      ; LResultLoIElem := BSetLInfo . BitsetLo 
      ; LOverlapLoBitwordNo := LSetRLoBitwordNo 
      ; <* ASSERT LSetRLoBitwordNo <= LSetLHiBitwordNo *> 
        LBitwordL 
          := BSetLBitwords 
               [ ArraySsOfBitwordNo ( LOverlapLoBitwordNo , BSetLInfo . Bias ) ]
      ; LHashRemove := LBitwordL 
      ; IF LOverlapLoBitwordNo = LSetLLoBitwordNo 
        THEN 
          LHashRemove  
            := Word . Or 
                 ( LBitwordL , LTMaskOfIElem ( BSetLInfo . BitsetLo ) )
              (* ^Set low garbage bits for removal from hash. *) 
        END (* IF *) 
      ; IF LOverlapLoBitwordNo = LSetLHiBitwordNo 
        THEN
          LBitwordL 
            := Word . And 
                 ( LBitwordL , LEMaskOfIElem ( BSetLInfo . BitsetHi ) )
               (* ^Zero hi garbage bits so search won't find them. *) 
        ; LHashRemove  
            := Word . Or 
                 ( LHashRemove , GTMaskOfIElem ( BSetLInfo . BitsetHi ) )
               (* ^Set hi garbage bits for removal from hash. *) 
        END (* IF *) 
      ; LHashIncr := Word . Xor ( LHashIncr , LHashRemove ) (* Remove. *) 
      ; LBitwordR 
          := BSetRBitwords 
               [ ArraySsOfBitwordNo ( LOverlapLoBitwordNo , BSetRInfo . Bias ) ]
      ; LBitwordR 
          := Word . And ( LBitwordR , GEMaskOfIElem ( BSetRInfo . BitsetLo ) ) 
             (* ^Zero low garbage bits from BSetR. *) 
      ; IF LOverlapLoBitwordNo = LSetRHiBitwordNo 
        THEN 
          LBitwordR 
            := Word . And ( LBitwordR , LEMaskOfIElem ( BSetRInfo . BitsetHi ) )
            (* ^Zero high garbage bits from BSetR. *) 
        END (* IF *) 
      ; LOverlapLoBitword 
          := Word . And ( LBitwordL , Word . Not ( LBitwordR ) ) 
      ; IF LOverlapLoBitwordNo = LResultLoBitwordNo  
        THEN 
          LOverlapLoBitword 
            := Word . Or 
                 ( LOverlapLoBitword , LTMaskOfIElem ( LResultLoIElem ) )
          (* ^Set low garbage bits. *) 
        END (* IF *) 
      ELSE (* BSetL does not have a lower bit than BSetR. 
              Search for the lowest result 1-bit. 
           *)  
        LOverlapLoBitwordNo := LSetLLoBitwordNo 
      ; LSetLSs := ArraySsOfBitwordNo ( LOverlapLoBitwordNo , BSetLInfo . Bias )
      ; LSetRSs := ArraySsOfBitwordNo ( LOverlapLoBitwordNo , BSetRInfo . Bias )
      ; LBit0IElem := BitZeroIElemOfBitwordNo ( LSetLLoBitwordNo ) 
      ; LBitwordL := BSetLBitwords [ LSetLSs ]
      ; LHashRemove 
          := Word . Or ( LBitwordL , LTMaskOfIElem ( BSetLInfo . BitsetLo ) )   
             (* ^Set lo garbage bits of BSetL. *) 
      ; LBitwordL 
          := Word . And ( LBitwordL , GEMaskOfIElem ( BSetLInfo . BitsetLo ) ) 
        (* ^Zero lo garbage bits so  search won't find one of them. *) 
      ; LOOP 
          IF LOverlapLoBitwordNo = LSetLHiBitwordNo 
          THEN 
            LHashRemove 
              := Word . Or 
                   ( LHashRemove , GTMaskOfIElem ( BSetLInfo . BitsetHi ) )
                 (* ^Set hi garbage bits for removal from hash code. *) 
          ; LBitwordL 
              := Word . And 
                   ( LBitwordL , LEMaskOfIElem ( BSetLInfo . BitsetHi ) )
                 (* ^Zero hi garbage bits of BSetL, so search won't find one. *)
          END (* IF *) 
        ; LHashIncr := Word . Xor ( LHashIncr , LHashRemove ) (* Remove *) 
        ; LBitwordR := BSetRBitwords [ LSetRSs ] 
          (* ^Any low garbage bits of BSetR correspond to (already zeroed) 
             garbage bits of BSetL, so are harmless. Leave them alone.  
          *)
        ; IF LOverlapLoBitwordNo = LSetRHiBitwordNo 
          THEN 
            LBitwordR 
              := Word . And 
                   ( LBitwordR , LEMaskOfIElem ( BSetRInfo . BitsetHi ) )
            (* ^Zero high garbage bits of BSetR. *)
          END (* IF *) 
        ; LOverlapLoBitword 
            := Word . And ( LBitwordL , Word . Not ( LBitwordR ) ) 
        ; IF LOverlapLoBitword # 0 
          THEN (* The sought bit is in LOverlapLoBitword *)
            LBit0IElem 
              := Word . Plus 
                   ( LBit0IElem 
                   , Word . Times 
                       ( LOverlapLoBitwordNo - LSetLLoBitwordNo 
                       , BitsPerBitword 
                       ) 
                   )  
          ; LResultLoIElem 
              := LBit0IElem 
                 + Least1BitNoInBitword ( LOverlapLoBitword ) (* Can't fail. *) 
          ; LOverlapLoBitword 
              := Word . Or 
                   ( LOverlapLoBitword , LTMaskOfIElem ( LResultLoIElem ) ) 
            (* ^Set result low garbage bits. *) 
          ; EXIT 
          ELSIF LOverlapLoBitwordNo = LSetLHiBitwordNo 
          THEN (* Reached high end of BSetL. The whole result is empty. *) 
            RETURN NIL 
          ELSIF LOverlapLoBitwordNo = LSetRHiBitwordNo 
          THEN (* Reached high end of BSetR.  We are out of words in BSetR, 
                  but haven't found a result 1-bit, so still need to scan any
                  more words  BSetL.  The result will come entirely from 
                  higher words of BSetL than exist in BSetR.  BSetL must have
                  another Bitword and a 1-bit. 
               *) 
            LResultLoIElem 
              := LeastPresentIElemOfBSetInRange 
                   ( BSetLInfo , BSetLBitwords  
                   , BitZeroIElemOfBitwordNo ( LOverlapLoBitwordNo + 1 ) 
                   , BSetLInfo . BitsetHi 
                   ) 
          ; RETURN IntersectionRangeBitset (* Can't fail. *)
                     ( LResultLoIElem , BSetLInfo . BitsetHi 
                     , BSetLInfo , BSetLBitwords , SetL , BSetL  
                     ) 
          ELSE (* Advance to the next word. *) 
            INC ( LSetLSs )  
          ; LBitwordL := BSetLBitwords [ LSetLSs ]  
          ; LHashRemove := LBitwordL 
          ; INC ( LSetRSs )  
          ; INC ( LOverlapLoBitwordNo ) 
          END (* IF *) 
        END (* LOOP *) 
      ; LResultLoBitwordNo := LOverlapLoBitwordNo  
      END (* IF *) 

    (* Here, LResultLoBitwordNo and LResultLoIElem have been computed. 
       LOverlapLoBitword is partially done.  It has its low garbage bits 
       set and whatever bits turn out to belong to the result computed.
       If there are any high bits that correspond to high garbage bits of
       BSetL, they are zeroed.  
    *) 

    (* Compute LResultHiIElem as the greatest nonzero bit of the result
       and LResultHiBitwordNo as the containing Bitword. 
       Also compute LOverlapHiBitword as the highest Bitword that gets
       its contents from both BSetL and BSetR, unless this is the same
       word as LOverlapLoBitword.   
    *) 
    ; IF BSetLInfo . BitsetHi > BSetRInfo . BitsetHi 
      THEN (* The result high bit is the high bit of BSetL. *) 
        LResultHiBitwordNo := LSetLHiBitwordNo 
      ; LResultHiIElem := BSetLInfo . BitsetHi 
      ; LOverlapHiBitwordNo := LSetRHiBitwordNo 
      ; IF LOverlapLoBitwordNo = LOverlapHiBitwordNo 
        THEN (* Only one overlapping Bitword, partially computed above. *) 
          IF LOverlapLoBitwordNo = LResultHiBitwordNo 
          THEN 
            LOverlapLoBitword 
              := Word . Or 
                   ( LOverlapLoBitword , GTMaskOfIElem ( LResultHiIElem ) )
            (* ^Set high garbage bits of LOverlapLoBitword. *) 
          END (* IF *) 
        ELSE 
          <* ASSERT LSetRHiBitwordNo >= LSetLLoBitwordNo *> 
          LBitwordL 
            := BSetLBitwords 
                 [ ArraySsOfBitwordNo 
                     ( LOverlapHiBitwordNo , BSetLInfo . Bias ) 
                 ]
        ; IF LOverlapHiBitwordNo = LSetLHiBitwordNo 
          THEN
            LBitwordL 
              := Word . Or 
                   ( LBitwordL , GTMaskOfIElem ( BSetLInfo . BitsetHi ) ) 
                 (* ^Set hi garbage bits of BSetL for removal from hash code. *)
          END (* IF *) 
        ; LHashIncr := Word . Xor ( LHashIncr , LBitwordL ) (* Remove *) 
        ; LBitwordR 
            := BSetRBitwords 
                 [ ArraySsOfBitwordNo 
                     ( LOverlapHiBitwordNo , BSetRInfo . Bias ) 
                 ]
        ; LBitwordR 
            := Word . And ( LBitwordR , LEMaskOfIElem ( BSetRInfo . BitsetHi ) )
          (* ^Remove high garbage bits from BSetR. *) 
        ; LOverlapHiBitword 
            := Word . And ( LBitwordL , Word . Not ( LBitwordR ) ) 
          (* ^This will preserve unchanged, any high garbage bits of BSetL. *)
        END (* IF *) 
      ELSE (* Search for the highest result 1-bit. *) 
        LOverlapHiBitwordNo := LSetLHiBitwordNo 
      ; LSetLSs := ArraySsOfBitwordNo ( LOverlapHiBitwordNo , BSetLInfo . Bias )
      ; LSetRSs := ArraySsOfBitwordNo ( LOverlapHiBitwordNo , BSetRInfo . Bias )
      ; LBit0IElem := BitZeroIElemOfBitwordNo ( LSetLHiBitwordNo ) 
      ; LBitwordL := BSetLBitwords [ LSetLSs ]  
      ; LHashRemove 
          := Word . Or ( LBitwordL , GTMaskOfIElem ( BSetLInfo . BitsetHi ) ) 
             (* ^Set hi garbage bits of BSetL for removal from hash code. *)  
      ; LBitwordL 
          := Word . And ( LBitwordL , LEMaskOfIElem ( BSetLInfo . BitsetHi ) ) 
        (* ^Zero hi garbage bits of BSetL so search won't find them. *) 
      ; LOOP 
          IF LOverlapHiBitwordNo = LOverlapLoBitwordNo 
          THEN (* We already did most of the work on this Bitword, and the
                  results are in LResultLoBitword.  
               *) 
            IF LOverlapLoBitword # 0 
            THEN 
              LBit0IElem 
                := Word . Minus 
                     ( LBit0IElem 
                     , Word . Times 
                         ( LSetLHiBitwordNo - LOverlapHiBitwordNo 
                         , BitsPerBitword 
                         ) 
                     ) 
            ; LResultHiIElem 
                := LBit0IElem + Greatest1BitNoInBitword ( LOverlapLoBitword )  
            ; LOverlapLoBitword 
                := Word . Or 
                     ( LOverlapLoBitword , GTMaskOfIElem ( LResultHiIElem ) ) 
                   (* ^Set result high garbage bits. *) 
            ; EXIT 
            ELSE 
              (* If there were no result 1-bit at all , we would have 
                 discovered that in the upward search from the low end and 
                 returned without getting here.  Since we found none searching 
                 down from the top, the result will come entirely from 
                 lower words of BSetL than exist in BSetR.  BSetL must have
                 a lower Bitword and a 1-bit.   
              *) 
              LResultHiIElem 
                := GreatestPresentIElemOfBSetInRange (* Can't fail. *)
                     ( BSetLInfo , BSetLBitwords  
                     , BSetLInfo . BitsetLo 
                     , BitZeroIElemOfBitwordNo ( LOverlapLoBitwordNo ) - 1  
                     ) 
            ; RETURN IntersectionRangeBitset 
                       ( BSetLInfo . BitsetLo , LResultHiIElem 
                       , BSetLInfo , BSetLBitwords , SetL , BSetL 
                       ) 
            END (* IF *) 
          ELSE (* Not down to LOverlapLoBitwordNo yet. *) 
            LHashIncr := Word . Xor ( LHashIncr , LHashRemove ) (* Remove. *) 
          ; LBitwordR := BSetRBitwords [ LSetRSs ] 
            (* ^Any high garbage bits of BSetR correspond to (already zeroed) 
               garbage bits of BSetL, so are harmless to the Diff operation. 
            *)
          ; IF LOverlapHiBitwordNo = LSetRLoBitwordNo 
            THEN 
              LBitwordR 
                := Word . And 
                     ( LBitwordR , GEMaskOfIElem ( BSetRInfo . BitsetLo ) )
              (* ^Zero low garbage bits of BSetR. *)
            END (* IF *) 
          ; LOverlapHiBitword 
               := Word . And ( LBitwordL , Word . Not ( LBitwordR ) ) 
          ; IF LOverlapHiBitword # 0 
            THEN (* The sought bit is in LOverlapHiBitword *) 
              LBit0IElem 
                := Word . Minus 
                     ( LBit0IElem 
                     , Word . Times 
                         ( LSetLHiBitwordNo - LOverlapHiBitwordNo 
                         , BitsPerBitword 
                         ) 
                     ) 
            ; LResultHiIElem 
                := LBit0IElem + Greatest1BitNoInBitword ( LOverlapHiBitword )  
            ; LOverlapHiBitword 
                := Word . Or 
                     ( LOverlapHiBitword , GTMaskOfIElem ( LResultHiIElem ) ) 
                   (* ^Set result high garbage bits. *) 
            ; EXIT 
            ELSE (* Back down to the previous word. *) 
              DEC ( LSetLSs )  
            ; LBitwordL := BSetLBitwords [ LSetLSs ]  
            ; LHashRemove := LBitwordL 
            ; DEC ( LSetRSs )  
            ; DEC ( LOverlapHiBitwordNo ) 
            ; <* ASSERT LOverlapHiBitwordNo >= LResultLoBitwordNo *> 
            (* And loop. *) 
            END (* IF *) 
          END (* IF *) 
        END (* LOOP *) 
      ; LResultHiBitwordNo := LOverlapHiBitwordNo 
      END (* IF *) 

    (* Construct the result Bitset. *) 
    ; IF LResultLoIElem + 1 >= LResultHiIElem 
      THEN (* Singleton or doubleton. *)
        RETURN 
          ConstructRangeset ( Lo := LResultLoIElem , Hi := LResultHiIElem )
      ELSE 
        LResultInfo . BitsetLo := LResultLoIElem 
      ; LResultInfo . BitsetHi := LResultHiIElem 
      ; LResultInfo . Bias := LResultLoBitwordNo 
      ; LResultBitwordCt := LResultHiBitwordNo - LResultLoBitwordNo + 1 
      ; LResultBitwordArrayRef := NewBitwordArray ( LResultBitwordCt ) 
      ; LResultCardIsComputable := TRUE (* May be refuted later. *) 
      ; LAllResultBitsAreOnes := TRUE (* May be refuted later. *) 
      ; LResultCard 
          := Word . Minus ( 0 , BitNo ( LResultInfo . BitsetLo ) ) 
             (* ^Discount result lo garbage bits. *)
      ; LResultCard 
          := Word . Minus  
               ( LResultCard 
               , BitsPerBitword - 1 - BitNo ( LResultInfo . BitsetHi ) 
               )
             (* ^Discount result high garbage bits. *) 

      (* Copy any words from BSetL, at low end, to result. *) 
      ; LBitwordCt := LSetRLoBitwordNo - LSetLLoBitwordNo 
      ; IF LBitwordCt > 0   
        THEN 
          LResultSs := LBitwordCt (* For later use with overlapping words. *) 
        (* Lowest BSetL low word. *) 
        ; LSetLSs := ArraySsOfBitwordNo ( LSetLLoBitwordNo , BSetLInfo . Bias ) 
        ; LResultBitword := BSetLBitwords [ LSetLSs ] 
        ; LResultBitword
            := Word . Or ( LResultBitword , LTMaskOfIElem ( LResultLoIElem ) ) 
          (* ^Set result high garbage bits. *) 
        ; LResultBitwordArrayRef ^ [ 0 ] := LResultBitword 
        ; LAllResultBitsAreOnes 
            := LAllResultBitsAreOnes AND LResultBitword = AllOnes 
        ; LResultCard 
            := Word . Plus  
                 ( LResultCard , NoOf1BitsInBitword ( LResultBitword ) ) 
        ; INC ( LSetLSs ) 
        ; DEC ( LBitwordCt )  

        (* Additional BSetL low words. *) 
        ; IF LBitwordCt > 0 
          THEN 
            WITH 
              WSub 
                = SUBARRAY ( LResultBitwordArrayRef ^ , 1 , LBitwordCt ) 
            DO 
              WSub 
                := SUBARRAY 
                     ( BSetLBitwords , LSetLSs , LBitwordCt ) 
            ; LAllResultBitsAreOnes 
                := LAllResultBitsAreOnes 
                   AND BitwordArrayIsAllOnes ( WSub ) 
            ; LResultCardIsComputable := FALSE 
            ; INC ( LSetLSs , LBitwordCt ) 
            END (* WITH *) 
          END (* IF *) 

        ; LSetRSs := ArraySsOfBitwordNo ( LSetRLoBitwordNo , BSetRInfo . Bias )
        ELSE (* No low words.  Low overlapping word could be well up into
                the two sets. 
             *)  
          LSetLSs 
            := ArraySsOfBitwordNo ( LResultLoBitwordNo , BSetLInfo . Bias )
        ; LSetRSs 
            := ArraySsOfBitwordNo ( LResultLoBitwordNo , BSetRInfo . Bias )
        ; LResultSs := 0 
        END (* IF *) 

      (* Store the lowest overlapping word, computed earlier.  There will
         always be such a word in the result. 
      *) 
      ; LHashIncr := Word . Xor ( LHashIncr , LOverlapLoBitword ) (* Add. *) 
      ; LResultBitwordArrayRef ^ [ LResultSs ] := LOverlapLoBitword 
      ; LAllResultBitsAreOnes 
          := LAllResultBitsAreOnes AND LOverlapLoBitword = AllOnes 
      ; LResultCard 
          := Word . Plus 
               ( LResultCard , NoOf1BitsInBitword ( LOverlapLoBitword ) ) 
      ; INC ( LSetLSs ) 
      ; INC ( LSetRSs ) 
      ; INC ( LResultSs )

      ; IF LOverlapHiBitwordNo > LOverlapLoBitwordNo 
        THEN    
      (* Compute the difference bits in the intermediate overlapping words. *) 
          FOR RI := 2 TO LOverlapHiBitwordNo - LOverlapLoBitwordNo  
          DO 
            LBitwordL := BSetLBitwords [ LSetLSs ]
          ; LBitwordR := BSetRBitwords [ LSetRSs ] 
          ; LHashIncr := Word . Xor ( LHashIncr , LBitwordL ) (* Remove. *) 
          ; LResultBitword 
              := Word . And ( LBitwordL , Word . Not ( LBitwordR ) ) 
          ; LHashIncr := Word . Xor ( LHashIncr , LResultBitword ) (* Add. *) 
          ; LResultBitwordArrayRef ^ [ LResultSs ] := LResultBitword 
          ; LAllResultBitsAreOnes 
              := LAllResultBitsAreOnes AND LResultBitword = AllOnes 
          ; LResultCard 
              := Word . Plus 
                   ( LResultCard , NoOf1BitsInBitword ( LResultBitword ) ) 
          ; INC ( LSetLSs ) 
          ; INC ( LSetRSs ) 
          ; INC ( LResultSs ) 
          END (* FOR *) 

        (* Store the high overlapping Bitword, which is distinct from the low
           overlapping Bitword.  It was computed earlier. 
        *) 
        ; LHashIncr := Word . Xor ( LHashIncr , LOverlapHiBitword ) (* Add. *) 
        ; LResultBitwordArrayRef ^ [ LResultSs ] := LOverlapHiBitword 
        ; LAllResultBitsAreOnes 
            := LAllResultBitsAreOnes AND LOverlapHiBitword = AllOnes 
        ; LResultCard 
            := Word . Plus 
                 ( LResultCard , NoOf1BitsInBitword ( LOverlapHiBitword ) ) 
        ; INC ( LSetLSs ) 
        ; INC ( LResultSs )
        END (* IF *) 

      (* Copy any words from BSetL, at the high end, to result. *) 
      ; LBitwordCt := LSetLHiBitwordNo - LSetRHiBitwordNo 
      ; IF LBitwordCt > 0 
        THEN 
          DEC ( LBitwordCt ) 
        ; IF LBitwordCt > 0 
          THEN 
            WITH 
              WSub 
                 = SUBARRAY 
                     ( LResultBitwordArrayRef ^ , LResultSs , LBitwordCt )
            DO 
              WSub := SUBARRAY 
                        ( BSetLBitwords , LSetLSs , LBitwordCt ) 
            ; LAllResultBitsAreOnes 
                := LAllResultBitsAreOnes 
                   AND BitwordArrayIsAllOnes ( WSub ) 
            ; LResultCardIsComputable := FALSE 
            ; INC ( LSetLSs , LBitwordCt ) 
            ; INC ( LResultSs , LBitwordCt ) 
            END (* WITH *) 
          END (* IF *) 
        ; LResultBitword
            := BSetLBitwords [ LSetLSs ]
        ; LResultBitword
            := Word . Or ( LResultBitword , GTMaskOfIElem ( LResultHiIElem ) ) 
          (* ^Set result high garbage bits. *) 
        ; LResultBitwordArrayRef ^ [ LResultSs ] := LResultBitword 
        ; LAllResultBitsAreOnes 
            := LAllResultBitsAreOnes AND LResultBitword = AllOnes 
        ; LResultCard 
            := Word . Plus 
                 ( LResultCard , NoOf1BitsInBitword ( LResultBitword ) ) 
        END (* IF *) 

      (* Finish up the result. *) 
      ; IF LAllResultBitsAreOnes 
        THEN 
          RETURN
            ConstructRangeset ( Lo := LResultLoIElem , Hi := LResultHiIElem ) 
        ELSE 
          IF LResultCardIsComputable 
          THEN
            <* ASSERT Word . GE ( LResultCard , 2 ) *> 
            LResultInfo . Card := LResultCard  
          ELSE 
            LResultInfo . Card := CardUnknown 
          END (* IF *) 
        ; StoreBitsetHashIncr 
            ( LResultInfo , LHashIncr , LHashIncrIsComputable ) 
        ; LResult := ConstructBitset ( LResultInfo , LResultBitwordArrayRef ) 
        ; RETURN LResult 
        END (* IF *) 
      END (* IF *) 
    END DifferenceOverlappingBitsets 

(* VISIBLE: *) 
; PROCEDURE Difference ( SetL : T ; SetR : T ) : T 
  (* Set difference of SetL minus SetR. *) 

 = VAR DifferenceResult : T

  ; PROCEDURE InnerDifference
      ( READONLY DInfoL : DissectInfo
      ; READONLY BitwordsL : ARRAY OF BitwordTyp
      ; READONLY DInfoR : DissectInfo
      ; READONLY BitwordsR : ARRAY OF BitwordTyp
      )

    = BEGIN (* InnerDifference *)
        IF DInfoL . RangeLo # IElemNull
        THEN (* SetL is a Rangeset. *)
          IF DInfoR . RangeLo # IElemNull
          THEN (* SetL is a Rangeset and SetR is a Rangeset. *)
            IF DInfoL . RangeLo = DInfoR . RangeLo 
               AND DInfoL . RangeHi = DInfoR . RangeHi
            THEN (* The ranges are identical. *)  
              DifferenceResult := NIL 
            ELSE  
              DifferenceResult 
                := DifferenceNonemptyUnequalRanges 
                     ( RSetLLo := DInfoL . RangeLo 
                     , RSetLHi := DInfoL . RangeHi 
                     , RSetL := DInfoL . RSet 
                     , RSetRLo := DInfoR . RangeLo 
                     , RSetRHi := DInfoR . RangeHi 
                     ) 
            END (* IF *) 
          ELSIF NUMBER ( BitwordsR ) = 0
          THEN (* SetL is a Rangeset and SetR is empty. *)
            DifferenceResult := DInfoL . Set    
          ELSE (* SetL is a Rangeset and SetR is a Bitset. *)
            IF DInfoL . RangeHi < DInfoR . BitsetInfo . BitsetLo 
               OR DInfoL . RangeLo > DInfoR . BitsetInfo . BitsetHi   
            THEN (* No overlap. *) 
              DifferenceResult := DInfoL . Set 
            ELSE 
              DifferenceResult 
                :=  DifferenceOverlappingRangeBitset 
                      ( DInfoL . RangeLo , DInfoL . RangeHi , DInfoL . RSet   
                      , DInfoR . BitsetInfo , BitwordsR 
                      ) 
            END (* IF *) 
          END (* IF *)

        ELSIF NUMBER ( BitwordsL ) = 0
        THEN (* SetL is empty. *)
          DifferenceResult := NIL   

        ELSE (* SetL is a Bitset. *)
          IF DInfoR . RangeLo # IElemNull
          THEN (* SetL is a Bitset and SetR is a Rangeset. *)
            IF DInfoL . BitsetInfo . BitsetHi < DInfoR . RangeLo 
               OR DInfoL . BitsetInfo . BitsetLo > DInfoR . RangeHi 
            THEN (* No overlap. *) 
              DifferenceResult := DInfoL . Set 
            ELSE 
              DifferenceResult 
                := DifferenceOverlappingBitsetRange 
                     ( DInfoL . BitsetInfo , BitwordsL , SetL , DInfoL . BSet  
                     , DInfoR . RangeLo , DInfoR . RangeHi 
                     ) 
            END (* IF *) 
          ELSIF NUMBER ( BitwordsR ) = 0
          THEN (* SetL is a Bitset and SetR is empty. *)
            DifferenceResult :=  DInfoL . Set 
          ELSE (* SetL is a Bitset and SetR is a Bitset. *)
            IF DInfoL . BitsetInfo . BitsetHi < DInfoR . BitsetInfo . BitsetLo  
               OR DInfoL . BitsetInfo . BitsetLo 
                  > DInfoR . BitsetInfo . BitsetHi  
            THEN (* No overlap. *) 
              DifferenceResult :=  DInfoL . Set 
            ELSE 
              DifferenceResult 
                := DifferenceOverlappingBitsets 
                     ( DInfoL . BitsetInfo , BitwordsL , SetL , DInfoL . BSet 
                     , DInfoR . BitsetInfo , BitwordsR 
                     ) 
            END (* IF *) 
          END (* IF *)
        END (* IF *)
      END InnerDifference

  ; BEGIN (* Difference *)
      <* FATAL ANY *> BEGIN 
        CallWithTwoSets ( SetL , SetR , InnerDifference )
      END (* Block *) 
    ; RETURN DifferenceResult
    END Difference

(* ========================= Symmetric difference ========================== *)

; VAR GSymDiffCt : INTEGER := 0 
; VAR GSymDiffRangeCt : INTEGER := 0 

; PROCEDURE SymDiffOverlappingBitsetRange 
    ( READONLY BSet1Info : BitsetInfoTyp 
    ; READONLY BSet1Bitwords : ARRAY OF BitwordTyp 
    ; Set1 : T
    ; BSet1 : BitsetTyp 
    ; RSet2Lo , RSet2Hi : ValidIElemTyp 
    ) 
  : T 

  (* PRE: BSet1Info and BSet1Bitwords come from Set1. 
     PRE: BSet1 # NIL IMPLIES BSet1Info and BSet1Bitwords come from BSet1.
     PRE: RSet2Lo <= RSet2Hi. 
     PRE: BSet1 has a BitNo in RSet2Lo .. RSet2Hi.
  *) 

  = VAR LRSetLoBitwordNo , LRSetHiBitwordNo : BitwordNoTyp 
  ; VAR LSet1LoBitwordNo , LSet1HiBitwordNo : BitwordNoTyp 
  ; VAR LResultLoBitwordNo , LResultHiBitwordNo : BitwordNoTyp 
  ; VAR LOverlapLoBitwordNo , LOverlapHiBitwordNo : BitwordNoTyp 
  ; VAR LResultLoIElem , LResultHiIElem , LIElem : IElemTyp 
  ; VAR LResultBias : BiasTyp 
  ; VAR LSet1Ss , LResultSs : ArraySsTyp 
  ; VAR LResultCard : CardTyp 
  ; VAR LResultInfo : BitsetInfoTyp 
  ; VAR LResultBitwordArrayRef : BitwordArrayRefTyp 
  ; VAR LResult : T
  ; VAR LBitwordCt : CARDINAL 
  ; VAR LBitword , LSetBitword , LRangeBitword : BitwordTyp 
  ; VAR LHashIncr , LHashRemove , LHashAbs : HashTyp 
  ; VAR LAllResultBitsAreOnes : BOOLEAN 
  ; VAR LResultCardIsComputable : BOOLEAN 
  ; VAR LHashIncrIsComputable , LHashAbsIsComputable : BOOLEAN 

  ; BEGIN 
    (* We can't possibly reuse either the Bitword array or a range set. *) 

      INC ( GSymDiffCt ) 
    ; LRSetLoBitwordNo := BitwordNoOfIElem ( RSet2Lo ) 
    ; LRSetHiBitwordNo := BitwordNoOfIElem ( RSet2Hi ) 
    ; LSet1LoBitwordNo := BitwordNoOfIElem ( BSet1Info . BitsetLo ) 
    ; LSet1HiBitwordNo := BitwordNoOfIElem ( BSet1Info . BitsetHi ) 
    ; RecoverBitsHash 
        ( BSet1Info , (*VAR*) LHashIncrIsComputable , (*VAR*) LHashIncr ) 
    ; LHashAbs := 0 
    ; LHashAbsIsComputable := TRUE 

    (* Check the low end for trimming due to result 0-bits. *) 
    ; IF BSet1Info . BitsetLo = RSet2Lo 
      THEN (* This is the only way we can get a zero result bit at the
              low end.  Find the lowest 0-bit position in BSet1, which will
              give the lowest result 1-bit.
           *) 
        LIElem := MIN ( RSet2Hi , BSet1Info . BitsetHi ) 
      ; LResultLoIElem 
          := LeastAbsentIElemOfBSetInRange 
               ( BSet1Info , BSet1Bitwords , BSet1Info . BitsetLo + 1 , LIElem )
      ; IF LResultLoIElem = IElemNull 
        THEN (* There is no result 1-bit in the overlapping range.  Because 
                BSet1 has a 0-bit by invariant, this can only happen if 
                RSet2Hi < BSet1Info . BitsetHi - 1.  The higher remaining 
                part of BSet1 is the result. 
             *) 
          RETURN IntersectionRangeBitset 
                   ( RSet2Hi + 1 , BSet1Info . BitsetHi 
                   , BSet1Info , BSet1Bitwords , Set1 , BSet1 
                   )
        ELSE 
          LResultLoBitwordNo := BitwordNoOfIElem ( LResultLoIElem )  
        ; LOverlapLoBitwordNo := LResultLoBitwordNo  
        ; LSet1Ss 
            := ArraySsOfBitwordNo ( LResultLoBitwordNo , BSet1Info . Bias )  
        END (* IF *)
      ELSE 
        LResultLoIElem := MIN ( RSet2Lo , BSet1Info . BitsetLo ) 
      ; LResultLoBitwordNo := BitwordNoOfIElem ( LResultLoIElem )  
      ; LOverlapLoBitwordNo := MAX ( LRSetLoBitwordNo , LSet1LoBitwordNo )  
      ; LSet1Ss := ArraySsOfBitwordNo ( LSet1LoBitwordNo , BSet1Info . Bias )  
      END (* IF *) 

    (* Check the high end for trimming due to result 0-bits. *) 
    ; IF BSet1Info . BitsetHi = RSet2Hi 
      THEN (* This is the only way we can get a zero result bit at the
              high end.  Find the highest 0-bit position in BSet1, which will
              give the highest result 1-bit.
           *) 
        LIElem := MAX ( RSet2Lo , BSet1Info . BitsetLo ) 
      ; LResultHiIElem 
          := GreatestAbsentIElemOfBSetInRange 
               ( BSet1Info , BSet1Bitwords , LIElem , BSet1Info . BitsetHi - 1 )
      ; IF LResultHiIElem = IElemNull 
        THEN (* There is no result 1-bit in the overlapping range.  Because 
                BSet1 has a 0-bit by invariant, this can only happen if 
                RSet2Lo > BSet1Info . BitsetLo + 1.  The lower remaining 
                part of BSet1 is the result. 
             *) 
          RETURN IntersectionRangeBitset 
                   ( BSet1Info . BitsetLo , RSet2Lo - 1 
                   , BSet1Info , BSet1Bitwords , Set1 , BSet1 
                   )
        ELSE 
          LResultHiBitwordNo := BitwordNoOfIElem ( LResultHiIElem )  
        ; LOverlapHiBitwordNo := LResultHiBitwordNo   
        END (* IF *) 
      ELSE 
        LResultHiIElem := MAX ( RSet2Hi , BSet1Info . BitsetHi ) 
      ; LResultHiBitwordNo := BitwordNoOfIElem ( LResultHiIElem )  
      ; LOverlapHiBitwordNo := MIN ( LRSetHiBitwordNo , LSet1HiBitwordNo )   
      END (* IF *) 
    ; <* ASSERT LResultLoIElem <= LResultHiIElem *> 

    (* Setup for constructing the result Bitword array. *) 
      LResultBitwordArrayRef 
        := NewBitwordArray ( LResultHiBitwordNo - LResultLoBitwordNo + 1 )  
    ; LResultBias := LResultLoBitwordNo 
    ; LAllResultBitsAreOnes := TRUE (* May be refuted later. *) 
    ; LResultCard 
        := Word . Minus ( 0 , BitNo ( LResultLoIElem ) ) 
           (* ^Discount result lo garbage bits. *)
    ; LResultCard 
        := Word . Minus  
             ( LResultCard , BitsPerBitword - 1 - BitNo ( LResultHiIElem ) ) 
           (* ^Discount result high garbage bits. *) 
    ; LResultCardIsComputable := TRUE (* May be refuted later. *) 

    (* Handle the low end of the result, through the overlap word: *) 

    ; IF LSet1LoBitwordNo > LResultLoBitwordNo   
      THEN (* There are low words from the range.  Give result all ones. *) 
        LBitwordCt := LSet1LoBitwordNo - LRSetLoBitwordNo 
      ; AssignAllOnesToBitwordArray 
          ( SUBARRAY 
              ( LResultBitwordArrayRef ^ , 0 , LBitwordCt )
          ) 
      ; LResultCard 
          := Word . Plus 
               ( LResultCard , Word . Times ( LBitwordCt , BitsPerBitword ) )
      ; IF LBitwordCt MOD 2 # 0 
        THEN 
          LHashIncr := Word . Not ( LHashIncr ) 
        ; LHashAbs := Word . Not ( LHashAbs ) 
        END (* IF *) 
      ; LResultSs := LBitwordCt  
      ; LSetBitword := BSet1Bitwords [ LSet1Ss ] 
      ; LHashRemove := LSetBitword 
      ; LHashRemove 
          := Word . Or 
               ( LHashRemove , LTMaskOfIElem ( BSet1Info . BitsetLo ) ) 
        (* ^Set BSet1 low garbage bits for hash removal. *) 
      ; LSetBitword 
          := Word . And 
               ( LSetBitword , GEMaskOfIElem ( BSet1Info . BitsetLo ) ) 
        (* ^Remove BSet1 low garbage bits for result computation. *) 
      ; LRangeBitword := AllOnes 
      ELSIF LRSetLoBitwordNo > LResultLoBitwordNo  
      THEN (* There are low words from BSet1.  Copy them. *) 
      (* The lowest result word, from BSet: *) 
        LBitword := BSet1Bitwords [ LSet1Ss ]
      ; LBitword 
         := Word . Or ( LBitword , LTMaskOfIElem ( LResultLoIElem ) )
        (* ^Set result low garbage bits, which are also g-bits of BSet1. *)
      ; LHashAbs := Word . Xor ( LHashAbs , LBitword ) (* Add. *) 
      ; LResultBitwordArrayRef ^ [ 0 ] := LBitword 
      ; LAllResultBitsAreOnes 
          := LAllResultBitsAreOnes AND LBitword = AllOnes 
      ; LResultCard 
          := Word . Plus ( LResultCard , NoOf1BitsInBitword ( LBitword ) ) 
      ; INC ( LSet1Ss ) 
      (* Copy any intermediate BSet1 words below both the overlap. *)  
      ; LBitwordCt := LRSetLoBitwordNo - LSet1LoBitwordNo - 1  
      ; IF LBitwordCt > 0 
        THEN 
          WITH WResultSub 
               = SUBARRAY ( LResultBitwordArrayRef ^ , 1 , LBitwordCt )
          DO WResultSub  
               := SUBARRAY 
                    ( BSet1Bitwords , LSet1Ss , LBitwordCt )
          ; LAllResultBitsAreOnes 
              := LAllResultBitsAreOnes 
                 AND BitwordArrayIsAllOnes ( WResultSub )
          ; LHashAbsIsComputable := FALSE 
          ; INC ( LSet1Ss , LBitwordCt ) 
          END (* WITH *) 
        ; LResultCardIsComputable := FALSE 
        END (* IF *) 
      ; LResultSs := 1 + LBitwordCt 
      ; LSetBitword := BSet1Bitwords [ LSet1Ss ] 
      ; LHashRemove := LSetBitword 
      ; LRangeBitword := GEMaskOfIElem ( RSet2Lo ) 
      ELSIF LSet1LoBitwordNo = LResultLoBitwordNo 
      THEN (* Result low bitword comes from the low Bitwords of both Sets. *)  
        LSetBitword := BSet1Bitwords [ LSet1Ss ] 
      ; LHashRemove := LSetBitword 
      ; LHashRemove  
          := Word . Or 
               ( LHashRemove , LTMaskOfIElem ( BSet1Info . BitsetLo ) ) 
        (* ^Set BSet1 low garbage bits for hash code removal. *) 
      ; LSetBitword 
          := Word . And 
               ( LSetBitword , GEMaskOfIElem ( BSet1Info . BitsetLo ) ) 
        (* ^Zero BSet1 low garbage bits for result computation. *) 
      ; LRangeBitword := GEMaskOfIElem ( RSet2Lo ) 
      ELSE (* ResultLoBitwordNo > LSet1LoBitwordNo = LRSetLoBitwordNo *) 
        LBitwordCt := LResultLoBitwordNo -  LSet1LoBitwordNo 
      (* LBitwordCt Bitwords of BSet1 have to be all 1-bits. *) 
      ; IF LBitwordCt MOD 2 # 0 
        THEN 
          LHashIncr := Word . Not ( LHashIncr ) 
        END (* IF *) 
      ; LSetBitword := BSet1Bitwords [ LSet1Ss ] 
      ; LHashRemove := LSetBitword 
      ; LRangeBitword := AllOnes 
      END (* IF *) 

    (* Finish the low overlapping word: *) 
    ; IF LOverlapLoBitwordNo = LSet1HiBitwordNo 
      THEN
        LHashRemove  
          := Word . Or 
               ( LHashRemove , GTMaskOfIElem ( BSet1Info . BitsetHi ) ) 
        (* ^Set BSet1 high garbage bits for hash code removal. *) 
      ; LSetBitword 
          := Word . And 
               ( LSetBitword , LEMaskOfIElem ( BSet1Info . BitsetHi ) ) 
        (* ^Zero BSet1 high garbage bits for result computation. *) 
      END (* IF *) 
    ; IF LOverlapLoBitwordNo = LRSetHiBitwordNo 
      THEN
        LRangeBitword 
          := Word . And 
               ( LRangeBitword , LEMaskOfIElem ( RSet2Hi ) ) 
        (* ^Remove bounds high garbage bits. *) 
      END (* IF *) 
    ; LHashIncr := Word . Xor ( LHashIncr , LHashRemove ) (* Remove. *) 
    ; LBitword := Word . Xor ( LSetBitword , LRangeBitword ) 
    ; IF LOverlapLoBitwordNo = LResultLoBitwordNo 
      THEN 
        LBitword 
          := Word . Or ( LBitword , LTMaskOfIElem ( LResultLoIElem ) )
        (* ^Set low garbage bits of result. *) 
      END (* IF *) 
    ; IF LOverlapLoBitwordNo = LResultHiBitwordNo 
      THEN 
        LBitword 
          := Word . Or ( LBitword , GTMaskOfIElem ( LResultHiIElem ) )
        (* ^Set high garbage bits of result. *) 
      END (* IF *) 
    ; LAllResultBitsAreOnes 
        := LAllResultBitsAreOnes AND LBitword = AllOnes 
    ; LHashIncr := Word . Xor ( LHashIncr , LBitword ) (* Add. *) 
    ; LHashAbs := Word . Xor ( LHashAbs , LBitword ) (* Add. *) 
    ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword 
    ; IF LResultCardIsComputable 
      THEN 
        LResultCard 
          := Word . Plus ( LResultCard , NoOf1BitsInBitword ( LBitword ) ) 
      END (* IF *) 
    ; INC ( LSet1Ss ) 
    ; INC ( LResultSs ) 

    (* Any remaining overlapping Bitwords. *) 
    ; IF LOverlapHiBitwordNo > LOverlapLoBitwordNo 
      THEN 
      (* Intermediate overlapping Bitwords: *) 
        FOR RI := LOverlapLoBitwordNo + 1 TO LOverlapHiBitwordNo - 1 
        DO 
          LBitword := Word . Not ( BSet1Bitwords [ LSet1Ss ] ) 
        ; LAllResultBitsAreOnes 
            := LAllResultBitsAreOnes AND LBitword = AllOnes 
        ; LHashIncr := Word . Not ( LHashIncr ) (* Change in place. *) 
        ; LHashAbs := Word . Xor ( LHashAbs , LBitword ) (* Add. *) 
        ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword 
        ; IF LResultCardIsComputable 
          THEN 
            LResultCard 
              := Word . Plus ( LResultCard , NoOf1BitsInBitword ( LBitword ) ) 
          END (* IF *) 
        ; INC ( LSet1Ss ) 
        ; INC ( LResultSs ) 
        END (* FOR *) 

      (* Distinct, high overlapping Bitword. *) 
      ; LSetBitword := BSet1Bitwords [ LSet1Ss ] 
      ; LHashRemove := LSetBitword 
      ; IF LOverlapHiBitwordNo = LSet1HiBitwordNo 
        THEN
          LHashRemove 
            := Word . Or 
                 ( LHashRemove , GTMaskOfIElem ( BSet1Info . BitsetHi ) ) 
          (* ^Set BSet1 high garbage bits for hash code removal. *) 
        ; LSetBitword 
            := Word . And 
                 ( LSetBitword , LEMaskOfIElem ( BSet1Info . BitsetHi ) ) 
          (* ^Zero BSet1 high garbage bits for result computation. *) 
        END (* IF *) 
      ; IF LOverlapHiBitwordNo = LRSetHiBitwordNo 
        THEN
          LRangeBitword := LEMaskOfIElem ( RSet2Hi ) 
          (* ^Zero bounds high garbage bits. *) 
        ELSE 
          LRangeBitword := AllOnes 
        END (* IF *) 
      ; LHashIncr := Word . Xor ( LHashIncr , LHashRemove ) (* Remove. *) 
      ; LBitword := Word . Xor ( LSetBitword , LRangeBitword ) 
      ; IF LOverlapHiBitwordNo = LResultHiBitwordNo 
        THEN 
          LBitword 
              := Word . Or ( LBitword , GTMaskOfIElem ( LResultHiIElem ) )
          (* ^Set high garbage bits of result. *) 
        END (* IF *) 
      ; LAllResultBitsAreOnes 
          := LAllResultBitsAreOnes AND LBitword = AllOnes 
      ; LHashIncr := Word . Xor ( LHashIncr , LBitword ) (* Add. *) 
      ; LHashAbs := Word . Xor ( LHashAbs , LBitword ) (* Add. *) 
      ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword 
      ; IF LResultCardIsComputable 
        THEN 
          LResultCard 
            := Word . Plus ( LResultCard , NoOf1BitsInBitword ( LBitword ) ) 
        END (* IF *) 
      ; INC ( LSet1Ss ) 
      ; INC ( LResultSs ) 
      END (* IF *) 

    (* High non-overlapping end of the result. *) 
    ; IF LSet1HiBitwordNo < LResultHiBitwordNo  
      THEN (* There are high Bitwords from the range. *) 
        LBitwordCt := LRSetHiBitwordNo - LSet1HiBitwordNo 
      ; IF LBitwordCt MOD 2 # 0 
        THEN 
          LHashIncr := Word . Not ( LHashIncr ) 
        ; LHashAbs := Word . Not ( LHashAbs ) 
        END (* IF *) 
      ; AssignAllOnesToBitwordArray 
          ( SUBARRAY ( LResultBitwordArrayRef ^ , LResultSs , LBitwordCt ) )
      ; LResultCard 
          := Word . Plus 
               ( LResultCard , Word . Times ( LBitwordCt , BitsPerBitword ) ) 
      ; INC ( LResultSs , LBitwordCt ) 
      ELSIF LRSetHiBitwordNo < LResultHiBitwordNo 
      THEN (* There are high Bitwords from the BSet. *) 
      (* Intermediate Bitwords from the BSet. *) 
        LBitwordCt := LSet1HiBitwordNo - LRSetHiBitwordNo - 1  
      ; IF LBitwordCt > 0 
        THEN 
          WITH WResultSub 
            = SUBARRAY 
                ( LResultBitwordArrayRef ^ , LResultSs , LBitwordCt )
          DO WResultSub  
               := SUBARRAY 
                    ( BSet1Bitwords , LSet1Ss , LBitwordCt )
          ; LAllResultBitsAreOnes 
              := LAllResultBitsAreOnes 
                 AND BitwordArrayIsAllOnes ( WResultSub )
          ; LResultCardIsComputable := FALSE 
          ; LHashAbsIsComputable := FALSE 
          ; INC ( LSet1Ss , LBitwordCt ) 
          ; INC ( LResultSs , LBitwordCt ) 
          END (* WITH *) 
        END (* IF *) 
      (* The high Bitword from the BSet. *) 
      ; LBitword := BSet1Bitwords [ LSet1Ss ] 
      ; LBitword 
          := Word . Or ( LBitword , GTMaskOfIElem ( LResultHiIElem ) )
        (* ^Set high garbage bits of result. *) 
      ; LAllResultBitsAreOnes 
          := LAllResultBitsAreOnes AND LBitword = AllOnes 
      ; LHashAbs := Word . Xor ( LHashAbs , LBitword ) (* Add. *) 
      ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword 
      ; IF LResultCardIsComputable 
        THEN 
          LResultCard 
            := Word . Plus  ( LResultCard , NoOf1BitsInBitword ( LBitword ) )
        END (* IF *) 
      ; INC ( LSet1Ss ) 
      ; INC ( LResultSs ) 
      ELSE (* The high end could have bitwords from both sets. *) 
        LBitwordCt := LSet1HiBitwordNo - LResultHiBitwordNo 
      (* LBitwordCt Bitwords of BSet1 have to be all 1-bits. *) 
      ; IF LBitwordCt MOD 2 # 0 
        THEN 
          LHashIncr := Word . Not ( LHashIncr ) 
        END (* IF *) 
      END (* IF *) 
    ; <* ASSERT LResultSs = LAST ( LResultBitwordArrayRef ^ ) + 1 *>
      IF LAllResultBitsAreOnes 
      THEN 
        INC ( GSymDiffRangeCt ) 
      ; RETURN 
          ConstructRangeset ( Lo := LResultLoIElem , Hi := LResultHiIElem )
      END (* IF *) 
    ; LResultInfo . BitsetLo := LResultLoIElem 
    ; LResultInfo . BitsetHi := LResultHiIElem 
    ; LResultInfo . Bias := LResultBias 
    ; IF LResultCardIsComputable 
      THEN
        LResultInfo . Card := LResultCard  
      ELSE 
        LResultInfo . Card := CardUnknown 
      END (* IF *) 
    ; StoreBitsetHashAbsOrIncr 
        ( LResultInfo 
        , LHashAbs , LHashAbsIsComputable 
        , LHashIncr , LHashIncrIsComputable
        ) 

    ; LResult := ConstructBitset ( LResultInfo , LResultBitwordArrayRef ) 
    ; RETURN LResult 
    END SymDiffOverlappingBitsetRange

; PROCEDURE SymDiffOverlappingBitsets 
    ( READONLY BSet1Info : BitsetInfoTyp 
    ; READONLY BSet1Bitwords : ARRAY OF BitwordTyp  
    ; Set1 : T 
    ; BSet1 : BitsetTyp 
    ; READONLY BSet2Info : BitsetInfoTyp 
    ; READONLY BSet2Bitwords : ARRAY OF BitwordTyp 
    ; Set2 : T  
    ; BSet2 : BitsetTyp 
    ) 
  : T

  (* PRE: BSet1Info and BSet1Bitwords come from Set1. 
     PRE: BSet1 # NIL IMPLIES BSet1Info and BSet1Bitwords come from BSet1.
     PRE: BSet2Info and BSet2Bitwords come from Set2. 
     PRE: BSet2 # NIL IMPLIES BSet2Info and BSet2Bitwords come from BSet2.
     PRE: The ranges of BSet1 and BSet2 have a BitNo in common.
  *)  
 
  = VAR LSet1LoBitwordNo , LSet1HiBitwordNo : BitwordNoTyp 
  ; VAR LSet2LoBitwordNo , LSet2HiBitwordNo : BitwordNoTyp 
  ; VAR LResultLoBitwordNo , LResultHiBitwordNo : BitwordNoTyp 
  ; VAR LResultLoIElem , LResultHiIElem : IElemTyp 
  ; VAR LOverlapLoBitwordNo , LOverlapHiBitwordNo : BitwordNoTyp 
  ; VAR LLoBitwordCt , LHiBitwordCt : CARDINAL   
  ; VAR LSet1Ss , LSet2Ss , LSet1HiSs , LSet2HiSs : CARDINAL 
  ; VAR LResultSs : CARDINAL 
  ; VAR LBitword1 , LBitword2 : BitwordTyp 
  ; VAR LOverlapLoBitword , LOverlapHiBitword : BitwordTyp 
  ; VAR LResultBitword : BitwordTyp 
  ; VAR LResultCard : CardTyp 
  ; VAR LResultInfo : BitsetInfoTyp 
  ; VAR LResultBitwordArrayRef : BitwordArrayRefTyp 
  ; VAR LResult : T
  ; VAR LHashIncr , LHashRemove1 , LHashRemove2 , LHashAbs : HashTyp 
  ; VAR LResultHasA0Bit : BOOLEAN 
  ; VAR LResultCardIsComputable : BOOLEAN 
  ; VAR LHashIncrIsComputable , LHashAbsIsComputable : BOOLEAN 

  ; BEGIN 
(* TODO: Maybe. Perhaps try to reuse one or the other of the operand Bitword 
         arrays.  This is a fairly unlikely case.  To reuse the array for
         operand 1, operand 2 would have to have all zeros corresponding to
         real bits of operand 1  (which implies 2 is longer than 1 on both 
         ends), while outlying bits of 2 would have to match corresponding 
         garbage bits of 1.   
*) 
      LSet1LoBitwordNo := BitwordNoOfIElem ( BSet1Info . BitsetLo ) 
    ; LSet1HiBitwordNo := BitwordNoOfIElem ( BSet1Info . BitsetHi ) 
    ; LSet2LoBitwordNo := BitwordNoOfIElem ( BSet2Info . BitsetLo ) 
    ; LSet2HiBitwordNo := BitwordNoOfIElem ( BSet2Info . BitsetHi ) 
    ; LOverlapLoBitwordNo := MAX ( LSet1LoBitwordNo , LSet2LoBitwordNo )
    ; LOverlapHiBitwordNo := MIN ( LSet1HiBitwordNo , LSet2HiBitwordNo ) 
      (* ^These could change later, if we trim either end of the result. *) 
    ; RecoverBitsHash 
        ( BSet1Info , (*VAR*) LHashIncrIsComputable , (*VAR*) LHashAbs ) 
    ; IF LHashIncrIsComputable 
      THEN 
        RecoverBitsHash 
          ( BSet2Info , (*VAR*) LHashIncrIsComputable , (*VAR*) LHashIncr ) 
      END (* IF *) 
    ; LHashIncr := Word . Xor ( LHashIncr , LHashAbs ) 
      (* LHashIncrIsComputable is still refutable. *) 
    ; LHashAbs := 0 
    ; LHashAbsIsComputable := TRUE (* Refutable. *)  

    (* Compute LResultLoIElem, LResultLoBitwordNo, LOverlapLoBitwordNo,
       and LOverlapLoBitword.  
    *)    
    ; LSet1Ss := ArraySsOfBitwordNo ( LOverlapLoBitwordNo , BSet1Info . Bias ) 
    ; LBitword1 := BSet1Bitwords [ LSet1Ss ] 
    ; LHashRemove1 := LBitword1 
    ; IF LOverlapLoBitwordNo = LSet1LoBitwordNo 
      THEN 
        LHashRemove1 
          := Word . Or ( LHashRemove1 , LTMaskOfIElem ( BSet1Info . BitsetLo ) )
          (* ^Set BSet1 low garbage bits for hash removal. *) 
      ; LBitword1 
          := Word . And ( LBitword1 , GEMaskOfIElem ( BSet1Info . BitsetLo ) )
          (* ^Zero BSet1 low garbage bits for result computation. *) 
      END (* IF *) 
    ; LSet2Ss := ArraySsOfBitwordNo ( LOverlapLoBitwordNo , BSet2Info . Bias ) 
    ; LBitword2 := BSet2Bitwords [ LSet2Ss ] 
    ; LHashRemove2 := LBitword2 
    ; IF LOverlapLoBitwordNo = LSet2LoBitwordNo 
      THEN 
        LHashRemove2  
          := Word . Or ( LHashRemove2 , LTMaskOfIElem ( BSet2Info . BitsetLo ) )
          (* ^Set BSet2 low garbage bits for hash removal. *) 
      ; LBitword2 
          := Word . And ( LBitword2 , GEMaskOfIElem ( BSet2Info . BitsetLo ) ) 
          (* ^Zero BSet2 low garbage bits for result computation. *) 
      END (* IF *) 
    ; LOOP 
        IF LOverlapLoBitwordNo = LSet1HiBitwordNo 
        THEN 
          LHashRemove1  
            := Word . Or 
                 ( LHashRemove1 , GTMaskOfIElem ( BSet1Info . BitsetHi ) )
            (* ^Set BSet1 high garbage bits for hash removal. *) 
        ; LBitword1 
            := Word . And 
                 ( LBitword1 , LEMaskOfIElem ( BSet1Info . BitsetHi ) )
            (* ^Zero BSet1 high garbage bits for result computation. *) 
        END (* IF *) 
      ; IF LOverlapLoBitwordNo = LSet2HiBitwordNo 
        THEN 
          LHashRemove2  
            := Word . Or 
                 ( LHashRemove2 , GTMaskOfIElem ( BSet2Info . BitsetHi ) )
            (* ^Set BSet1 high garbage bits for hash removal. *) 
        ; LBitword2 
            := Word . And 
                 ( LBitword2 , LEMaskOfIElem ( BSet2Info . BitsetHi ) )
            (* ^Zero BSet2 high garbage bits for result computation. *) 
        END (* IF *) 
      ; LHashIncr := Word . Xor ( LHashIncr , LHashRemove1 ) (* Remove. *) 
      ; LHashIncr := Word . Xor ( LHashIncr , LHashRemove2 ) (* Remove. *) 
      ; LOverlapLoBitword := Word . Xor ( LBitword1 , LBitword2 ) 
      ; IF BSet1Info . BitsetLo # BSet2Info . BitsetLo 
        THEN 
          LResultLoIElem 
            := MIN ( BSet1Info . BitsetLo , BSet2Info . BitsetLo ) 
        ; LResultLoBitwordNo := BitwordNoOfIElem ( LResultLoIElem ) 
        ; EXIT (* having never iterated nor incremented LSet1Ss nor LSet2Ss. *) 
        ELSIF LOverlapLoBitword # 0 
        THEN (* The sought result low 1-bit is in this Bitword. *) 
          LResultLoIElem 
            := Least1BitNoInBitword ( LOverlapLoBitword ) 
               + LOverlapLoBitwordNo * BitsPerBitword 
        ; LResultLoBitwordNo := LOverlapLoBitwordNo 
        ; EXIT 
        ELSIF LOverlapLoBitwordNo >= LOverlapHiBitwordNo 
        THEN (* There is no result 1-bit in or below the overlapping range. *) 
          IF BSet1Info . BitsetHi > BSet2Info . BitsetHi 
          THEN (* Result is the upper non-overlapped part of BSet1. *) 
            RETURN IntersectionRangeBitset 
                     ( BSet2Info . BitsetHi + 1 , BSet1Info . BitsetHi 
                     , BSet1Info , BSet1Bitwords , Set1 , BSet1 
                     )
          ELSE (* Result is the upper non-overlapped part of BSet2, or maybe
                  empty, if BSet1Info . BitsetHi = BSet2Info . BitsetHi. 
               *) 
            RETURN IntersectionRangeBitset 
                     ( BSet1Info . BitsetHi + 1 , BSet2Info . BitsetHi 
                     , BSet2Info , BSet2Bitwords , Set2 , BSet2 
                     )
          END (* IF *) 
        ELSE (* There is another Bitword in both BSet1 and BSet2. *) 
          INC ( LOverlapLoBitwordNo ) 
        ; INC ( LSet1Ss ) 
        ; LBitword1 := BSet1Bitwords [ LSet1Ss ] 
        ; LHashRemove1 := LBitword1 
        ; INC ( LSet2Ss ) 
        ; LBitword2 := BSet2Bitwords [ LSet2Ss ]  
        ; LHashRemove2 := LBitword2 
        END (* IF *) 
      END (* LOOP *) 

    (* Compute LResultHiIElem, LResultHiBitwordNo, LOverlapHiBitwordNo,
       and LOverlapHiBitword.  
    *)    

    ; IF LOverlapLoBitwordNo < LOverlapHiBitwordNo  
      THEN (* This is not the already-computed LOverlapLoBitword. *) 
        LSet1HiSs 
          := ArraySsOfBitwordNo ( LOverlapHiBitwordNo , BSet1Info . Bias ) 
      ; LBitword1 := BSet1Bitwords [ LSet1HiSs ] 
      ; LHashRemove1 := LBitword1 
      ; IF LOverlapHiBitwordNo = LSet1HiBitwordNo 
        THEN 
          LHashRemove1 
            := Word . Or 
                 ( LHashRemove1 , GTMaskOfIElem ( BSet1Info . BitsetHi ) )
            (* ^Set BSet1 High garbage bits for hash removal. *) 
        ; LBitword1 
            := Word . And ( LBitword1 , LEMaskOfIElem ( BSet1Info . BitsetHi ) )
          (* Zero BSet1 high garbage bits for result computation. *) 
        END (* IF *) 
      ; LSet2HiSs 
          := ArraySsOfBitwordNo ( LOverlapHiBitwordNo , BSet2Info . Bias ) 
      ; LBitword2 := BSet2Bitwords [ LSet2HiSs ] 
      ; LHashRemove2 := LBitword2 
      ; IF LOverlapHiBitwordNo = LSet2HiBitwordNo 
        THEN 
          LHashRemove2  
            := Word . Or 
                 ( LHashRemove2 , GTMaskOfIElem ( BSet2Info . BitsetHi ) )
            (* ^Set BSet2 high garbage bits for hash removal. *) 
        ; LBitword2 
            := Word . And ( LBitword2 , LEMaskOfIElem ( BSet2Info . BitsetHi ) )
          (* Zero BSet2 high garbage bits for result computation. *) 
        END (* IF *) 
      END (* IF *) 
    ; LOOP
        IF LOverlapLoBitwordNo >= LOverlapHiBitwordNo  
        THEN (* This is LOverlapLoBitword, which we already computed. *) 
          LOverlapHiBitword := LOverlapLoBitword  
        ELSE 
          IF LOverlapHiBitwordNo = LSet1LoBitwordNo 
          THEN 
            LHashRemove1 
              := Word . Or 
                   ( LHashRemove1 , LTMaskOfIElem ( BSet1Info . BitsetLo ) )
              (* ^Set BSet1 low garbage bits for hash removal. *) 
          ; LBitword1 
              := Word . And 
                   ( LBitword1 , GEMaskOfIElem ( BSet1Info . BitsetLo ) )
              (* ^Zero BSet1 low garbage bits for result computation. *) 
          END (* IF *) 
        ; IF LOverlapHiBitwordNo = LSet2LoBitwordNo 
          THEN 
            LHashRemove2  
              := Word . Or 
                   ( LHashRemove2 , LTMaskOfIElem ( BSet2Info . BitsetLo ) )
              (* ^Set BSet2 low garbage bits for hash removal. *) 
          ; LBitword2 
              := Word . And 
                   ( LBitword2 , GEMaskOfIElem ( BSet2Info . BitsetLo ) )
              (* ^Zero BSet2 low garbage bits for result computation. *) 
          END (* IF *) 
        ; LHashIncr := Word . Xor ( LHashIncr , LHashRemove1 ) (* Remove. *) 
        ; LHashIncr := Word . Xor ( LHashIncr , LHashRemove2 ) (* Remove. *) 
        ; LOverlapHiBitword := Word . Xor ( LBitword1 , LBitword2 ) 
        END (* IF *) 
      ; IF BSet1Info . BitsetHi # BSet2Info . BitsetHi 
        THEN 
          LResultHiIElem 
            := MAX ( BSet1Info . BitsetHi , BSet2Info . BitsetHi ) 
        ; LResultHiBitwordNo := BitwordNoOfIElem ( LResultHiIElem ) 
        ; EXIT 
          (* having never iterated nor decremented LSet1HiSs nor LSet2HiSs. *) 
        ELSIF LOverlapHiBitword # 0 
        THEN (* The sought result high 1-bit is in this Bitword. *) 
          LResultHiIElem 
             := Greatest1BitNoInBitword ( LOverlapHiBitword ) 
                + LOverlapHiBitwordNo * BitsPerBitword 
        ; LResultHiBitwordNo := LOverlapHiBitwordNo 
        ; EXIT 
        ELSIF LOverlapLoBitwordNo >= LOverlapHiBitwordNo 
        THEN (* No result 1-bit in the overlapping range. *) 
          IF BSet1Info . BitsetLo < BSet2Info . BitsetLo 
          THEN (* Result is the lower non-overlapped part of BSet1. *) 
            RETURN IntersectionRangeBitset 
                     ( BSet1Info . BitsetLo , BSet2Info . BitsetLo - 1 
                     , BSet1Info , BSet1Bitwords , Set1 , BSet1 
                     )
          ELSE (* Result is the lower non-overlapped part of BSet2. *)  
            RETURN IntersectionRangeBitset 
                     ( BSet2Info . BitsetLo , BSet1Info . BitsetLo - 1 
                     , BSet2Info , BSet2Bitwords , Set2 , BSet2 
                     ) 
          END (* IF *) 
        ELSE (* There is another Bitword in both BSet1 and BSet2. *) 
          DEC ( LOverlapHiBitwordNo ) 
        ; DEC ( LSet1HiSs ) 
        ; LBitword1 := BSet1Bitwords [ LSet1HiSs ] 
        ; LHashRemove1 := LBitword1 
        ; DEC ( LSet2HiSs ) 
        ; LBitword2 := BSet2Bitwords [ LSet2HiSs ]  
        ; LHashRemove2 := LBitword2 
        END (* IF *) 
      END (* LOOP *) 

    (* Build the result BitwordArray. *) 
    ; LResultBitwordArrayRef 
        := NewBitwordArray ( LResultHiBitwordNo - LResultLoBitwordNo + 1 )
    ; LResultSs := 0 
    ; LResultHasA0Bit := FALSE (* Could be refuted later. *) 
    ; LResultCard 
        := Word . Minus ( 0 , BitNo ( LResultLoIElem ) ) 
           (* ^Discount result lo garbage bits. *)
    ; LResultCard 
        := Word . Minus  
             ( LResultCard , BitsPerBitword - 1 - BitNo ( LResultHiIElem ) ) 
             (* ^Discount result high garbage bits. *) 
    ; LResultCardIsComputable := TRUE (* Could be refuted later. *) 

    (* Copy any Bitwords below the low overlapping region. *) 
    ; IF LResultLoBitwordNo < LSet2LoBitwordNo  
      THEN (* There are low Bitwords to copy from Set1. *) 
           (* This can happen only if no low end trimming was done. *)   
        LLoBitwordCt := LOverlapLoBitwordNo - LSet1LoBitwordNo 
      ; LSet1Ss := ArraySsOfBitwordNo ( LSet1LoBitwordNo , BSet1Info . Bias ) 
      ; LBitword1 := BSet1Bitwords [ LSet1Ss ] 
      (* No need to remove low garbage bits. *) 
      ; LBitword1 
          := Word . Or ( LBitword1 , LTMaskOfIElem ( BSet1Info . BitsetLo ) ) 
        (* ^Set result low garbage bits. *) 
      ; LHashAbs := Word . Xor ( LHashAbs , LBitword1 ) (* Add. *) 
      ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword1 
      ; LResultHasA0Bit := LResultHasA0Bit OR LBitword1 # AllOnes 
      ; LResultCard 
          := Word . Plus ( LResultCard , NoOf1BitsInBitword ( LBitword1 ) ) 
      ; INC ( LSet1Ss ) 
      ; INC ( LResultSs ) 
      ; DEC ( LLoBitwordCt )  
      ; IF LLoBitwordCt > 0 
        THEN 
          WITH WSub 
            = SUBARRAY ( LResultBitwordArrayRef ^ , LResultSs , LLoBitwordCt ) 
          DO 
            WSub 
              := SUBARRAY 
                   ( BSet1Bitwords , LSet1Ss , LLoBitwordCt ) 
          ; LResultHasA0Bit 
              := LResultHasA0Bit OR NOT BitwordArrayIsAllOnes ( WSub ) 
          ; LResultCardIsComputable := FALSE 
          ; LHashAbsIsComputable := FALSE 
          ; INC ( LSet1Ss , LLoBitwordCt ) 
          ; INC ( LResultSs , LLoBitwordCt ) 
          END (* WITH *) 
        END (* IF *) 
      ELSIF LResultLoBitwordNo < LSet1LoBitwordNo 
      THEN (* There are low Bitwords to copy from Set2. *) 
           (* This can happen only if no low end trimming was done. *)   
        LLoBitwordCt := LOverlapLoBitwordNo - LSet2LoBitwordNo 
      ; LSet2Ss := ArraySsOfBitwordNo ( LSet2LoBitwordNo , BSet2Info . Bias ) 
      ; LBitword2 := BSet2Bitwords [ LSet2Ss ] 
      (* No need to remove low garbage bits. *) 
      ; LBitword2 
          := Word . Or ( LBitword2 , LTMaskOfIElem ( BSet2Info . BitsetLo ) ) 
        (* ^Set result low garbage bits. *) 
      ; LHashAbs := Word . Xor ( LHashAbs , LBitword2 ) (* Add. *) 
      ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword2 
      ; LResultHasA0Bit := LResultHasA0Bit OR LBitword2 # AllOnes 
      ; LResultCard 
          := Word . Plus ( LResultCard , NoOf1BitsInBitword ( LBitword2 ) )
      ; INC ( LSet2Ss ) 
      ; INC ( LResultSs ) 
      ; DEC ( LLoBitwordCt )  
      ; IF LLoBitwordCt > 0 
        THEN 
          WITH WSub 
            = SUBARRAY ( LResultBitwordArrayRef ^ , LResultSs , LLoBitwordCt ) 
           DO 
             WSub 
               := SUBARRAY 
                    ( BSet2Bitwords , LSet2Ss , LLoBitwordCt ) 
          ; LResultHasA0Bit 
              := LResultHasA0Bit OR NOT BitwordArrayIsAllOnes ( WSub ) 
          ; LResultCardIsComputable := FALSE 
          ; LHashAbsIsComputable := FALSE 
          ; INC ( LSet2Ss , LLoBitwordCt ) 
          ; INC ( LResultSs , LLoBitwordCt ) 
          END (* WITH *) 
        END (* IF *) 
      END (* IF *) 

    (* Finish with the low overlapping Bitword. *) 
    ; IF LOverlapLoBitwordNo = LResultLoBitwordNo 
      THEN 
        LOverlapLoBitword 
          := Word . Or 
               ( LOverlapLoBitword , LTMaskOfIElem ( LResultLoIElem ) ) 
        (* ^Set result low garbage bits. *) 
      END (* IF *) 
    ; IF LOverlapLoBitwordNo = LResultHiBitwordNo 
      THEN (* Last Bitword of result. *) 
        LOverlapLoBitword 
          := Word . Or 
               ( LOverlapLoBitword , GTMaskOfIElem ( LResultHiIElem ) ) 
        (* ^Set result high garbage bits. *) 
      END (* IF *) 
    ; LHashIncr := Word . Xor ( LHashIncr , LOverlapLoBitword ) (* Add. *) 
    ; LHashAbs := Word . Xor ( LHashAbs , LOverlapLoBitword ) (* Add. *) 
    ; LResultBitwordArrayRef ^ [ LResultSs ] := LOverlapLoBitword 
    ; LResultHasA0Bit := LResultHasA0Bit OR LOverlapLoBitword # AllOnes 
    ; LResultCard 
        := Word . Plus 
             ( LResultCard , NoOf1BitsInBitword ( LOverlapLoBitword ) )
    ; INC ( LSet1Ss ) 
    ; INC ( LSet2Ss ) 
    ; INC ( LResultSs ) 
    ; IF LOverlapLoBitwordNo < LResultHiBitwordNo
      THEN (* There are more Bitwords in the result after the low overlap 
              Bitword, although they do not necessarily overlap. 
           *)  

      (* Handle any full result Bitwords between the overlap points. *) 
        FOR RI := LOverlapLoBitwordNo + 1 TO LOverlapHiBitwordNo - 1  
        DO 
          LResultBitword 
            := Word . Xor 
                 ( BSet1Bitwords [ LSet1Ss ] 
                 , BSet2Bitwords [ LSet2Ss ] 
                 ) 
        ; LHashAbs := Word . Xor ( LHashAbs , LResultBitword ) (* Add. *) 
        ; LResultBitwordArrayRef ^ [ LResultSs ] := LResultBitword 
        ; LResultHasA0Bit := LResultHasA0Bit OR LResultBitword # AllOnes 
        ; LResultCard 
            := Word . Plus 
                 ( LResultCard , NoOf1BitsInBitword ( LResultBitword ) )
        ; INC ( LSet1Ss ) 
        ; INC ( LSet2Ss ) 
        ; INC ( LResultSs ) 
        END (* FOR *)

      (* Handle the high overlapping Bitword, if any. *) 
      ; IF LOverlapLoBitwordNo < LOverlapHiBitwordNo  
        THEN (* There is a separate high overlapping Bitword. *) 
          IF LOverlapHiBitwordNo = LResultHiBitwordNo 
          THEN (* This is the high Bitword of the result. *) 
            LOverlapHiBitword 
              := Word . Or 
                   ( LOverlapHiBitword , GTMaskOfIElem ( LResultHiIElem ) ) 
            (* ^Set result high garbage bits. *) 
          END (* IF *) 
        ; LHashIncr := Word . Xor ( LHashIncr , LOverlapHiBitword ) (* Add. *) 
        ; LHashAbs := Word . Xor ( LHashAbs , LOverlapHiBitword ) (* Add. *) 
        ; LResultBitwordArrayRef ^ [ LResultSs ] := LOverlapHiBitword 
        ; LResultHasA0Bit := LResultHasA0Bit OR LOverlapHiBitword # AllOnes 
        ; LResultCard 
            := Word . Plus 
                 ( LResultCard , NoOf1BitsInBitword ( LOverlapHiBitword ) )
        ; INC ( LSet1Ss ) 
        ; INC ( LSet2Ss ) 
        ; INC ( LResultSs ) 
        END (* IF *) 

      (* Copy any words above the high overlap. *) 
      ; IF LResultHiBitwordNo > LSet2HiBitwordNo 
        THEN (* There are high Bitwords to copy from Set1. *)  
             (* This can happen only if no high end trimming was done. *)   
          LHiBitwordCt := LSet1HiBitwordNo - LOverlapHiBitwordNo - 1   
        ; IF LHiBitwordCt > 0 
          THEN 
            WITH WSub 
              = SUBARRAY 
                  ( LResultBitwordArrayRef ^ , LResultSs , LHiBitwordCt ) 
            DO 
              WSub 
                := SUBARRAY 
                     ( BSet1Bitwords , LSet1Ss , LHiBitwordCt ) 
            ; LResultHasA0Bit 
                := LResultHasA0Bit OR NOT BitwordArrayIsAllOnes ( WSub ) 
            ; LResultCardIsComputable := FALSE 
            ; LHashAbsIsComputable := FALSE 
            ; INC ( LSet1Ss , LHiBitwordCt ) 
            ; INC ( LResultSs , LHiBitwordCt ) 
            END (* WITH *) 
          END (* IF *) 
        ; LBitword1 := BSet1Bitwords [ LSet1Ss ] 
        ; LBitword1 
            := Word . Or 
                 ( LBitword1 , GTMaskOfIElem ( BSet1Info . BitsetHi ) ) 
          (* Set result high garbage bits. *) 
        ; LHashAbs := Word . Xor ( LHashAbs , LBitword1 ) (* Add. *) 
        ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword1 
        ; LResultHasA0Bit := LResultHasA0Bit OR LBitword1 # AllOnes 
        ; LResultCard 
            := Word . Plus ( LResultCard , NoOf1BitsInBitword ( LBitword1 ) )
        ; INC ( LResultSs ) 
        ELSIF LResultHiBitwordNo > LSet1HiBitwordNo 
        THEN (* There are high Bitwords to copy from Set2. *)
             (* This can happen only if no high end trimming was done. *)   
          LHiBitwordCt := LSet2HiBitwordNo - LOverlapHiBitwordNo - 1   
        ; IF LHiBitwordCt > 0 
          THEN 
            WITH WSub 
              = SUBARRAY 
                  ( LResultBitwordArrayRef ^ , LResultSs , LHiBitwordCt ) 
            DO 
              WSub 
                := SUBARRAY 
                     ( BSet2Bitwords , LSet2Ss , LHiBitwordCt ) 
            ; LResultHasA0Bit 
                := LResultHasA0Bit OR NOT BitwordArrayIsAllOnes ( WSub ) 
            ; LResultCardIsComputable := FALSE 
            ; LHashAbsIsComputable := FALSE 
            ; INC ( LSet2Ss , LHiBitwordCt ) 
            ; INC ( LResultSs , LHiBitwordCt ) 
            END (* WITH *) 
          END (* IF *) 
        ; LBitword2 := BSet2Bitwords [ LSet2Ss ] 
        ; LBitword2 
            := Word . Or ( LBitword2 , GTMaskOfIElem ( BSet2Info . BitsetHi ) )
          (* Set result high garbage bits. *) 
        ; LHashAbs := Word . Xor ( LHashAbs , LBitword2 ) (* Add. *) 
        ; LResultBitwordArrayRef ^ [ LResultSs ] := LBitword2 
        ; LResultHasA0Bit := LResultHasA0Bit OR LBitword2 # AllOnes 
        ; LResultCard 
            := Word . Plus ( LResultCard , NoOf1BitsInBitword ( LBitword2 ) )
        ; INC ( LResultSs ) 
        END (* IF *) 
      END (* IF *) 
    ; <* ASSERT LResultSs = LAST ( LResultBitwordArrayRef ^ ) + 1 *> 

    (* Complete the result. *) 
      IF NOT LResultHasA0Bit 
      THEN
        RETURN 
          ConstructRangeset ( Lo := LResultLoIElem , Hi := LResultHiIElem ) 
      ELSE 
        LResultInfo . BitsetLo := LResultLoIElem 
      ; LResultInfo . BitsetHi := LResultHiIElem 
      ; LResultInfo . Bias := LResultLoBitwordNo  
      ; IF LResultCardIsComputable 
        THEN 
          LResultInfo . Card := LResultCard  
        ELSE 
          LResultInfo . Card := CardUnknown 
        END (* IF *) 
      ; StoreBitsetHashAbsOrIncr 
          ( LResultInfo 
          , LHashAbs , LHashAbsIsComputable 
          , LHashIncr , LHashIncrIsComputable
          ) 

      ; LResult := ConstructBitset ( LResultInfo , LResultBitwordArrayRef ) 
      ; RETURN LResult 
      END (* IF *) 
    END SymDiffOverlappingBitsets

(* VISIBLE: *) 
; PROCEDURE SymDiff ( Set1 : T ; Set2 : T ) : T 
  (* Symmetric difference of Set1 and Set2. 
     IsElement(E,SymDiff(S1,S2)) iff IsElement(E,S1) # IsElement(E,S2) 
  *) 

  = VAR SymDiffResult : T

  ; PROCEDURE InnerSymDiff
      ( READONLY DInfo1 : DissectInfo
      ; READONLY Bitwords1 : ARRAY OF BitwordTyp
      ; READONLY DInfo2 : DissectInfo
      ; READONLY Bitwords2 : ARRAY OF BitwordTyp
      )

    = BEGIN (* InnerSymDiff *)
        IF DInfo1 . RangeLo # IElemNull
        THEN (* Set1 is a Rangeset. *)
          IF DInfo2 . RangeLo # IElemNull
          THEN (* Set1 is a Rangeset and Set2 is a Rangeset. *)
            IF DInfo1 . RangeHi + 1 < DInfo2 . RangeLo 
            THEN (* Set1 is properly below Set2. *)  
              SymDiffResult 
                := UnionUntouchingOrderedRanges
                     ( DInfo1 . RangeLo , DInfo1 . RangeHi 
                     , DInfo2 . RangeLo , DInfo2 . RangeHi
                     )  
            ELSIF DInfo1 . RangeHi + 1 = DInfo2 . RangeLo 
            THEN (* Set1 is below and just touches Set2. *)  
              SymDiffResult 
                := ConstructRangeset 
                     ( Lo := DInfo1 . RangeLo , Hi := DInfo2 . RangeHi )
            ELSIF DInfo2 . RangeHi + 1 < DInfo1 . RangeLo 
            THEN (* Set2 is properly below Set1. *)  
              SymDiffResult 
                := UnionUntouchingOrderedRanges
                     ( DInfo2 . RangeLo , DInfo2 . RangeHi 
                     , DInfo1 . RangeLo , DInfo1 . RangeHi
                     )  
            ELSIF DInfo2 . RangeHi + 1 = DInfo1 . RangeLo 
            THEN (* Set2 is below and just touches Set1 *)  
              SymDiffResult 
                := ConstructRangeset 
                     ( Lo := DInfo2 . RangeLo , Hi := DInfo1 . RangeHi )
            ELSIF DInfo1 . RangeLo = DInfo2 . RangeLo 
                  AND DInfo2 . RangeHi = DInfo1 . RangeHi 
            THEN (* Ranges are identical. *) 
              SymDiffResult := NIL 
            ELSIF DInfo1 . RangeLo <= DInfo2 . RangeLo 
                  AND DInfo2 . RangeHi <= DInfo1 . RangeHi 
            THEN (* Set2 is a proper subrange of Set1 *) 
              SymDiffResult 
                := DifferenceNonemptyUnequalRanges 
                     ( RSetLLo := DInfo1 . RangeLo 
                     , RSetLHi := DInfo1 . RangeHi 
                     , RSetL := DInfo1 . RSet 
                     , RSetRLo := DInfo2 . RangeLo 
                     , RSetRHi := DInfo2 . RangeHi 
                     ) 
            ELSIF DInfo2 . RangeLo <= DInfo1 . RangeLo 
                  AND DInfo1 . RangeHi <= DInfo2 . RangeHi 
            THEN (* Set1 is a proper subrange of Set2 *) 
              SymDiffResult 
                := DifferenceNonemptyUnequalRanges 
                     ( RSetLLo := DInfo2 . RangeLo 
                     , RSetLHi := DInfo2 . RangeHi 
                     , RSetL := DInfo2 . RSet 
                     , RSetRLo := DInfo1 . RangeLo 
                     , RSetRHi := DInfo1 . RangeHi 
                     ) 
            (* Here, the ranges have a nonempty intersection and neither is
               a subrange of the other. 
            *) 
            ELSIF DInfo1 . RangeLo < DInfo2 . RangeLo 
            THEN (* Overlapping, TRSet1 starts low. *) 
              SymDiffResult 
                := UnionUntouchingOrderedRanges 
                     ( DInfo1 . RangeLo , DInfo2 . RangeLo - 1 
                     , DInfo1 . RangeHi + 1 , DInfo2 . RangeHi 
                     ) 
            ELSE (* DInfo2 . RangeLo < DInfo1 . RangeLo *) 
                 (* Overlapping, Set2 starts low. *) 
              SymDiffResult 
                := UnionUntouchingOrderedRanges 
                     ( DInfo2 . RangeLo , DInfo1 . RangeLo - 1 
                     , DInfo2 . RangeHi + 1 , DInfo1 . RangeHi 
                     ) 
            END (* IF *) 
          ELSIF NUMBER ( Bitwords2 ) = 0
          THEN (* Set1 is a Rangeset and Set2 is empty. *)
            SymDiffResult := DInfo1 . Set  
          ELSE (* Set1 is a Rangeset and Set2 is a Bitset. *)
            IF DInfo2 . BitsetInfo . BitsetHi < DInfo1 . RangeLo
               OR DInfo1 . RangeHi < DInfo2 . BitsetInfo . BitsetLo 
            THEN (* The sets are disjoint. *) 
              SymDiffResult 
                := UnionBitsetRange 
                     ( DInfo2 . BitsetInfo , Bitwords2 , DInfo2 . BSet 
                     , DInfo1 . RangeLo , DInfo1 . RangeHi , DInfo1 . RSet 
                     ) 
            ELSE (* The two sets have a BitNo in common. *)
              SymDiffResult 
                := SymDiffOverlappingBitsetRange 
                     ( DInfo2 . BitsetInfo , Bitwords2 , Set2 , DInfo2 . BSet  
                     , DInfo1 . RangeLo , DInfo1 . RangeHi 
                     ) 
            END (* IF *) 
          END (* IF *)

        ELSIF NUMBER ( Bitwords1 ) = 0
        THEN (* Set1 is empty. *)
          SymDiffResult := DInfo2 . Set 

        ELSE (* Set1 is a Bitset. *)
          IF DInfo2 . RangeLo # IElemNull
          THEN (* Set1 is a Bitset and Set2 is a Rangeset. *)
            IF DInfo1 . BitsetInfo . BitsetHi < DInfo2 . RangeLo
               OR DInfo2 . RangeHi < DInfo1 . BitsetInfo . BitsetLo 
            THEN (* The sets are disjoint. *) 
              SymDiffResult 
                := UnionBitsetRange 
                     ( DInfo1 . BitsetInfo , Bitwords1 , DInfo1 . BSet  
                     , DInfo2 . RangeLo , DInfo2 . RangeHi , DInfo2 . RSet 
                     ) 
            ELSE (* The two sets have a BitNo in common. *)
              SymDiffResult 
                := SymDiffOverlappingBitsetRange 
                     ( DInfo1 . BitsetInfo , Bitwords1 , Set1 , DInfo1 . BSet   
                     , DInfo2 . RangeLo , DInfo2 . RangeHi 
                     ) 
            END (* IF *) 
          ELSIF NUMBER ( Bitwords2 ) = 0
          THEN (* Set1 is a Bitset and Set2 is empty. *)
            SymDiffResult := DInfo1 . Set 
          ELSE (* Set1 is a Bitset and Set2 is a Bitset. *)
            IF DInfo1 . BitsetInfo . BitsetHi < DInfo2 . BitsetInfo . BitsetLo 
            THEN (* Non-overlapping ranges, Set1 is low. *) 
              SymDiffResult 
                := UnionDisjointOrderedBitsets 
                     ( DInfo1 . BitsetInfo , Bitwords1 
                     , DInfo2 . BitsetInfo , Bitwords2  
                     ) 
            ELSIF DInfo2 . BitsetInfo . BitsetHi 
                  < DInfo1 . BitsetInfo . BitsetLo 
            THEN (* Non-overlapping ranges, Set2 is low. *) 
              SymDiffResult := UnionDisjointOrderedBitsets 
                       ( DInfo2 . BitsetInfo , Bitwords2  
                       , DInfo1 . BitsetInfo , Bitwords1 
                       ) 
            ELSE (* Two Bitsets with overlapping ranges. *) 
              SymDiffResult 
                := SymDiffOverlappingBitsets 
                     ( DInfo1 . BitsetInfo , Bitwords1 , Set1 , DInfo1 . BSet  
                     , DInfo2 . BitsetInfo , Bitwords2 , Set2 , DInfo2 . BSet 
                     ) 
            END (* IF *) 
          END (* IF *)
        END (* IF *)
      END InnerSymDiff

  ; BEGIN (* SymDiff *)
      <* FATAL ANY *> BEGIN 
        CallWithTwoSets ( Set1 , Set2 , InnerSymDiff )
      END (* Block *) 
    ; RETURN SymDiffResult
    END SymDiff

(* ============================ Other operations =========================== *)

(* VISIBLE: *) 
; PROCEDURE Include ( Set : T ; Elem : ElemT ) : T 
  (* Union of Set and Singleton(Elem), but often more efficient. *) 

  = VAR IncludeResult : T

  ; PROCEDURE InnerInclude
      ( READONLY DInfo : DissectInfo; READONLY Bitwords : ARRAY OF BitwordTyp )

    = VAR LIElem := ORD ( Elem ) 

    ; BEGIN (* InnerInclude *)
        IF DInfo . RangeLo # IElemNull
        THEN (* Set is a Rangeset. *)
          LIElem := ORD ( Elem ) 
        ; IF LIElem + 1 < DInfo . RangeLo 
          THEN 
            IncludeResult 
              := UnionUntouchingOrderedRanges 
                   ( LIElem , LIElem , DInfo . RangeLo , DInfo . RangeHi ) 
          ELSIF DInfo . RangeHi + 1 < LIElem 
          THEN 
            IncludeResult 
              := UnionUntouchingOrderedRanges 
                   ( DInfo . RangeLo , DInfo . RangeHi , LIElem , LIElem ) 
          ELSIF LIElem + 1 = DInfo . RangeLo 
          THEN 
            IncludeResult 
              := ConstructRangeset ( Lo := LIElem , Hi := DInfo . RangeHi ) 
          ELSIF DInfo . RangeHi + 1 = LIElem  
          THEN 
            IncludeResult 
              := ConstructRangeset ( Lo := DInfo . RangeLo , Hi := LIElem ) 
          ELSE 
            IncludeResult := Set
          END (* IF *) 

        ELSIF NUMBER ( Bitwords ) = 0
        THEN (* Set is empty. *) 
          IncludeResult := Singleton ( Elem ) 

        ELSE (* Set is a Bitset. *)
          LIElem := ORD ( Elem ) 
        ; IncludeResult 
             := UnionBitsetRange 
                  ( DInfo . BitsetInfo , Bitwords , DInfo . BSet  
                  , LIElem , LIElem 
                  ) 
        END (* IF *)
      END InnerInclude

  ; BEGIN (* Include *)
      IF Elem < FIRST ( ValidElemT ) OR LAST ( ValidElemT ) < Elem  
      THEN (* Elem is not valid. *) 
        RETURN Set 
      ELSE 
        <* FATAL ANY *> BEGIN 
          CallWithOneSet ( Set , InnerInclude )
        END (* Block *) 
      ; RETURN IncludeResult
      END (* IF *) 
    END Include 

(* VISIBLE: *) 
; PROCEDURE Exclude ( Set : T ; Elem : ElemT ) : T 
  (* Difference of Set minus Singleton(Elem), but often more efficient. *) 

  = VAR ExcludeResult : T

  ; PROCEDURE InnerExclude
      ( READONLY DInfo : DissectInfo
      ; READONLY Bitwords : ARRAY OF BitwordTyp
      )

    = VAR LIElem : ValidIElemTyp 

    ; BEGIN (* InnerExclude *)
        IF DInfo . RangeLo # IElemNull
        THEN (* Set is a Rangeset. *)
          LIElem := ORD ( Elem ) 
        ; IF LIElem < DInfo . RangeLo OR DInfo . RangeHi < LIElem 
          THEN (* LIElem lies outside Set. *) 
            ExcludeResult :=  Set   
          ELSIF DInfo . RangeLo = DInfo . RangeHi 
          THEN (* Set = the singleton { Elem } *)  
            ExcludeResult :=  NIL 
          ELSE 
            ExcludeResult 
              := DifferenceNonemptyUnequalRanges 
                   ( DInfo . RangeLo , DInfo . RangeHi , DInfo . RSet 
                   , LIElem , LIElem 
                   ) 
          END (* IF *) 

        ELSIF NUMBER ( Bitwords ) = 0
        THEN (* Set is empty. *)
          ExcludeResult := NIL 

        ELSE (* Set is a Bitset. *)
          LIElem := ORD ( Elem ) 
        ; IF LIElem < DInfo . BitsetInfo . BitsetLo 
             OR DInfo . BitsetInfo . BitsetHi < LIElem 
          THEN (* LIElem is outside the bit array range of Set. *) 
            ExcludeResult :=  Set  
          ELSIF IsIElemBitset 
                  ( LIElem , DInfo . BitsetInfo , Bitwords ) 
          THEN  
            ExcludeResult 
              :=  DifferenceOverlappingBitsetRange 
                    ( DInfo . BitsetInfo , Bitwords , Set , DInfo . BSet 
                    , LIElem , LIElem 
                    ) 
          ELSE ExcludeResult := Set 
          END (* IF *) 
        END (* IF *)
      END InnerExclude

  ; BEGIN (* Exclude *)
      IF Elem < FIRST ( ValidElemT ) OR LAST ( ValidElemT ) < Elem  
      THEN (* Elem is not valid. *) 
        RETURN Set 
      ELSE 
        <* FATAL ANY *> BEGIN 
          CallWithOneSet ( Set , InnerExclude )
        END (* Block *) 
      ; RETURN ExcludeResult
      END (* IF *) 
    END Exclude

(* VISIBLE: *) 
; PROCEDURE IsEmpty ( Set : T ) : BOOLEAN

  = BEGIN (* IsEmpty *)
      RETURN Set = NIL 
    END IsEmpty

(* VISIBLE: *) 
; PROCEDURE Minimum ( Set : T ) : ElemT 
  (* Minimum valued element of Set.  NullElem, if Set is empty. *) 

  = VAR MinimumResult : ElemT 

  ; PROCEDURE InnerMinimum
      ( READONLY DInfo : DissectInfo ; READONLY Bitwords : ARRAY OF BitwordTyp )

    = BEGIN (* InnerMinimum *)
        IF DInfo . RangeLo # IElemNull
        THEN (* Set is a Rangeset. *)
          MinimumResult := VAL ( DInfo . RangeLo , ElemT ) 

        ELSIF NUMBER ( Bitwords ) = 0
        THEN (* Set is empty. *)
          MinimumResult := NullElem  

        ELSE (* Set is a Bitset. *)
          MinimumResult := VAL ( DInfo . BitsetInfo . BitsetLo , ElemT ) 
        END (* IF *)
      END InnerMinimum

  ; BEGIN (* Minimum *)
      <* FATAL ANY *> BEGIN 
        CallWithOneSet ( Set , InnerMinimum )
      END (* Block *) 
    ; RETURN MinimumResult
    END Minimum

(* VISIBLE: *) 
; PROCEDURE Maximum ( Set : T ) : ElemT 
  (* Maximum valued element of Set.  NullElem, if Set is empty. *) 

  = VAR MaximumResult : ElemT 

  ; PROCEDURE InnerMaximum
      ( READONLY DInfo : DissectInfo ; READONLY Bitwords : ARRAY OF BitwordTyp )

    = BEGIN (* InnerMaximum *)
        IF DInfo . RangeLo # IElemNull
        THEN (* Set is a Rangeset. *)
          MaximumResult := VAL ( DInfo . RangeHi , ElemT ) 

        ELSIF NUMBER ( Bitwords ) = 0
        THEN (* Set is empty. *)
          MaximumResult := NullElem  

        ELSE (* Set is a Bitset. *)
          MaximumResult := VAL ( DInfo . BitsetInfo . BitsetHi , ElemT ) 
        END (* IF *)
      END InnerMaximum

  ; BEGIN (* Maximum *)
      <* FATAL ANY *> BEGIN 
        CallWithOneSet ( Set , InnerMaximum )
      END (* Block *) 
    ; RETURN MaximumResult
    END Maximum

(* VISIBLE: *) 
; <* INLINE *> 
  PROCEDURE ArbitraryMember ( Set : T ) : ElemT 
  (* An arbitrary member of Set.  NullElem, if Set is empty. *) 

  = BEGIN (* ArbitraryMember *)
      RETURN Minimum ( Set )
    END ArbitraryMember

(* VISIBLE: *) 
; PROCEDURE ExtractArbitraryMember ( VAR Set : T ) : ElemT 
  (* Equivalent to: 
       WITH W = ArbitraryMember ( Set ) 
       DO IF W # NullElem THEN Set := Exclude ( Set , W ) END 
       ; RETURN W 
       END
     but faster.  
  *) 

  = VAR ExtractIResult : IElemTyp

  ; PROCEDURE InnerExtract
      ( READONLY DInfo : DissectInfo ; READONLY Bitwords : ARRAY OF BitwordTyp )

    = BEGIN (* InnerExtract *)
        IF DInfo . RangeLo # IElemNull
        THEN (* Set is a Rangeset. *) 
          IF DInfo . RangeLo = DInfo . RangeHi 
          THEN (* Set is a singleton. *)  
            ExtractIResult := DInfo . RangeLo 
          ; Set := NIL 
          ELSE
            ExtractIResult := DInfo . RangeLo 
          ; Set := DifferenceNonemptyUnequalRanges 
                     ( RSetLLo := DInfo . RangeLo 
                     , RSetLHi := DInfo . RangeHi 
                     , RSetL := DInfo . RSet 
                     , RSetRLo := ExtractIResult 
                     , RSetRHi := ExtractIResult    
                     ) 
          END (* IF *) 

        ELSIF NUMBER ( Bitwords ) = 0
        THEN (* Set is empty. *)
          ExtractIResult := IElemNull 
    
        ELSE (* Set is a Bitset. *)
          ExtractIResult := DInfo . BitsetInfo . BitsetLo 
        ; Set := DifferenceOverlappingBitsetRange  
                   ( DInfo . BitsetInfo , Bitwords , Set , DInfo . BSet  
                   , ExtractIResult , ExtractIResult 
                   ) 
        END (* IF *)
      END InnerExtract

  ; BEGIN (* Extract *)
      <* FATAL ANY *> BEGIN 
        CallWithOneSet ( Set , InnerExtract )
      END (* Block *) 
    ; RETURN VAL ( ExtractIResult , ElemT )  
    END ExtractArbitraryMember

(* VISIBLE: *) 
; PROCEDURE Complement 
    ( Set : T ; UnivLo , UnivHi : ElemT := NullElem ) : T 
  (* Complement WRT a universe of [ UnivLo .. UnivHi ].
     The universe is first widened if necessary to cover Min(Set)..Max(Set). 
     If Set is empty and exactly one of UnivLo,UnivHi is valid, Set is 
     complemented WRT Singleton(TheOneValidUnivBound).  Otherwise Empty()
     is complemented WRT Empty().     

     WARNING: This can create a *very* large heap object if the
              universe is large.  You probably want a universe having
              dynamically computed bounds that are far less
              extravagant than ValidElemT.  And if ValidElemT weren't
              large, you would probably be using some other set
              representation, such as Modula-3's builtin SET types.
              There is no complement WRT ValidElemT, to make it harder
              to construct huge objects.

  *)   

  = VAR ComplementResult : T

  ; PROCEDURE InnerComplement
      ( READONLY DInfo : DissectInfo ; READONLY Bitwords : ARRAY OF BitwordTyp )

    = VAR LIUnivLo , LIUnivHi : IElemTyp 

    ;  BEGIN (* InnerComplement *)
        IF UnivLo < FIRST ( ValidElemT ) OR LAST ( ValidElemT ) < UnivLo 
        THEN LIUnivLo := IElemNull 
        ELSE LIUnivLo := ORD ( UnivLo ) 
        END (* IF *) 
      ; IF UnivHi < FIRST ( ValidElemT ) OR LAST ( ValidElemT ) < UnivHi 
        THEN LIUnivHi := IElemNull 
        ELSE LIUnivHi := ORD ( UnivHi ) 
        END (* IF *) 
      ; IF DInfo . RangeLo # IElemNull
        THEN (* Set is a Rangeset. *)
          IF LIUnivLo = IElemNull OR DInfo . RangeLo < LIUnivLo   
          THEN LIUnivLo := DInfo . RangeLo 
          END (* IF *)   
        ; IF LIUnivHi = IElemNull OR DInfo . RangeHi > LIUnivHi   
          THEN LIUnivHi := DInfo . RangeHi 
          END (* IF *)   
        ; IF DInfo . RangeLo = LIUnivLo AND DInfo . RangeHi = LIUnivHi 
          THEN 
            ComplementResult := NIL 
          ELSE 
            ComplementResult 
              := DifferenceNonemptyUnequalRanges 
                   ( RSetLLo := LIUnivLo 
                   , RSetLHi := LIUnivHi 
                   , RSetRLo := DInfo . RangeLo 
                   , RSetRHi := DInfo . RangeHi
                   ) 
          END (* IF *)  

        ELSIF NUMBER ( Bitwords ) = 0
        THEN (* Set is empty. *)
          IF LIUnivLo = IElemNull THEN LIUnivLo := LIUnivHi END (* IF *) 
        ; IF LIUnivHi = IElemNull THEN LIUnivHi := LIUnivLo END (* IF *) 
        ; IF LIUnivLo = IElemNull AND LIUnivHi = IElemNull
          THEN (* Complement empty Set WRT empty universe. *)  
            ComplementResult := NIL 
          ELSE 
            ComplementResult 
              :=  ConstructRangeset ( Lo := LIUnivLo , Hi := LIUnivHi ) 
          END (* IF *) 

        ELSE (* Set is a Bitset. *)
          IF LIUnivLo = IElemNull OR DInfo . RangeLo < LIUnivLo   
          THEN LIUnivLo := DInfo . RangeLo 
          END (* IF *)   
        ; IF LIUnivHi = IElemNull OR DInfo . RangeHi > LIUnivHi   
          THEN LIUnivHi := DInfo . RangeHi 
          END (* IF *)   
        ; ComplementResult 
            :=  DifferenceOverlappingRangeBitset 
                  ( RSetLLo := LIUnivLo 
                  , RSetLHi := LIUnivHi 
                  , BSetRInfo := DInfo . BitsetInfo 
                  , BSetRBitwords := Bitwords  
                  ) 
        END (* IF *)
      END InnerComplement

  ; BEGIN (* Complement *)
      <* FATAL ANY *> BEGIN 
        CallWithOneSet ( Set , InnerComplement )
      END (* Block *) 
    ; RETURN ComplementResult
    END Complement

(* =========================== Cardinalities =============================== *) 

(* Set cardinalities of Bitsets are computed eagerly, whenever this can be 
   done without visiting any Bitwords that would not otherwise be needed
   (i.e., to compute the set value.)  They are cached in field Card.  Value
   zero in this field means not known/not cached.  By invariant, a Bitset 
   can not have cardinality less than two, so this value does not clash with
   a real cardinality.  Uncached cardinalities are computed lazily on demand
   and cached before being returned.  Cardinalities are declared of type
   Word.T and are unsigned but full-range numbers.  This is enough value
   range, considering there is a NullElem that is not a set member value.   
*) 

; PROCEDURE BitsetCard 
    ( READONLY BSetInfo : BitsetInfoTyp 
    ; READONLY BSetBitwords : ARRAY OF BitwordTyp 
    ) 
  : CardTyp 
  (* Independently compute cardinality of BSet, regardless of the
     value of BSetInfo . Card. 
  *) 

  = VAR LLoBitwordNo , LHiBitwordNo : BitwordNoTyp 
  ; VAR LBitword : BitwordTyp 
  ; VAR LSs : CARDINAL  
  ; VAR LResult : CardTyp 

  ; BEGIN (* BitsetCard *)
      LLoBitwordNo := BitwordNoOfIElem ( BSetInfo . BitsetLo ) 
    ; LHiBitwordNo := BitwordNoOfIElem ( BSetInfo . BitsetHi ) 
    ; LSs := ArraySsOfBitwordNo ( LLoBitwordNo , BSetInfo . Bias ) 
    ; LBitword := BSetBitwords [ LSs ] 
    ; LBitword 
        := Word . And ( LBitword , GEMaskOfIElem ( BSetInfo . BitsetLo ) ) 
      (* Zero lo garbage bits. *) 
    ; IF LLoBitwordNo = LHiBitwordNo 
      THEN
        LBitword 
          := Word . And ( LBitword , LEMaskOfIElem ( BSetInfo . BitsetHi ) ) 
        (* Zero hi garbage bits. *) 
      ; RETURN NoOf1BitsInBitword ( LBitword ) 
      ELSE
        LResult := NoOf1BitsInBitword ( LBitword ) 
      ; INC ( LSs ) 
      ; FOR RI := 2 TO LHiBitwordNo - LLoBitwordNo 
        DO 
          LResult 
            := Word . Plus
                 ( LResult 
                 , NoOf1BitsInBitword ( BSetBitwords [ LSs ] )
                 )  
        ; INC ( LSs ) 
        END (* FOR *) 
      ; LBitword := BSetBitwords [ LSs ] 
      ; LBitword 
          := Word . And ( LBitword , LEMaskOfIElem ( BSetInfo . BitsetHi ) )
        (* Zero hi garbage bits. *) 
      ; LResult := Word . Plus ( LResult , NoOf1BitsInBitword ( LBitword ) ) 
      ; RETURN LResult  
      END (* IF *) 
    END BitsetCard

(* VISIBLE: *) 
; PROCEDURE Card ( Set : T ) : CardTyp 
  (* Cardinality of Set. *) 

  = VAR CardResult : CardTyp

  ; PROCEDURE InnerCard
      ( READONLY DInfo : DissectInfo ; READONLY Bitwords : ARRAY OF BitwordTyp )

    = BEGIN (* InnerCard *)
        IF DInfo . RangeLo # IElemNull
        THEN (* Set is a Rangeset. *)
          CardResult 
            := Word . Plus 
                 ( Word . Minus ( DInfo . RangeHi , DInfo . RangeLo ) , 1 )  

        ELSIF NUMBER ( Bitwords ) = 0
        THEN (* Set is empty. *)
          CardResult := 0 

        ELSE (* Set is a Bitset. *)
          CardResult := DInfo . BitsetInfo . Card 
        ; IF CardResult = CardUnknown 
          THEN
            CardResult := BitsetCard ( DInfo . BitsetInfo , Bitwords ) 
          ; IF DInfo . BSet # NIL 
            THEN (* Cache the computed result. *)  
              DInfo . BSet . BitsetInfo . Card := CardResult 
            END (* IF *) 
          END (* IF *) 
        END (* IF *)
      END InnerCard

  ; BEGIN (* Card *)
      <* FATAL ANY *> BEGIN 
        CallWithOneSet ( Set , InnerCard )
      END (* Block *) 
    ; RETURN CardResult
    END Card

(* ================================== Subsets ============================== *)

; PROCEDURE IsSubsetBitsets 
    ( READONLY BSetLInfo : BitsetInfoTyp 
    ; READONLY BSetLBitwords : ARRAY OF BitwordTyp 
    ; READONLY BSetRInfo : BitsetInfoTyp 
    ; READONLY BSetRBitwords : ARRAY OF BitwordTyp 
    ; IsProper : BOOLEAN (* TRUE to test for proper subset. *) 
    ) 
  : BOOLEAN 

  = VAR LLoBitwordNo , LHiBitwordNo : BitwordNoTyp 
  ; VAR LBitwordSub , LBitwordSuper : BitwordTyp 
  ; VAR LSetSubSs , LSetSuperSs , LSetSubFinalSs : CARDINAL 

  ; BEGIN 
      IF BSetLInfo . BitsetLo < BSetRInfo . BitsetLo  
         OR BSetRInfo . BitsetHi < BSetLInfo . BitsetHi  
      THEN (* Not a subset. *) 
        RETURN FALSE 
      ELSE (* The hard case.  BSetL's bounds are a subrange of BSetR's. *) 
        IF BSetRInfo . BitsetLo < BSetLInfo . BitsetLo 
           OR BSetLInfo . BitsetHi < BSetRInfo . BitsetHi 
        THEN  
          IsProper := FALSE 
          (* This could be confusing.  The meaning of 'IsProper' is "assuming
             BSetL turns out to be a subset of BSetR, we also need to
             check that BSetL is proper as well, before returning TRUE. 
             Here (and other places below), we have discovered a bit that
             BSetR has and BSetL lacks.  Thus, is-proper-if-subset
             is ensured, and we set IsProper to FALSE to indicate no further
             checking of propriety is required.
          *) 
        END (* IF *) 
      ; LLoBitwordNo := BitwordNoOfIElem ( BSetLInfo . BitsetLo ) 
      ; LHiBitwordNo := BitwordNoOfIElem ( BSetLInfo . BitsetHi ) 
      ; LSetSubSs := ArraySsOfBitwordNo ( LLoBitwordNo , BSetLInfo . Bias ) 
      ; LSetSuperSs := ArraySsOfBitwordNo ( LLoBitwordNo , BSetRInfo . Bias ) 
      ; LBitwordSub := BSetLBitwords [ LSetSubSs ] 
      ; LBitwordSuper := BSetRBitwords [ LSetSuperSs ] 
        (* Garbage bits of Super are harmless. *) 
      ; LBitwordSub 
          := Word . And 
               ( LBitwordSub , GEMaskOfIElem ( BSetLInfo . BitsetLo ) ) 
        (* Zero lo garbage bits of Sub. *) 
      ; IF LLoBitwordNo = LHiBitwordNo 
        THEN (* Only one Bitword is involved. *) 
          LBitwordSub 
            := Word . And 
                 ( LBitwordSub , LEMaskOfIElem ( BSetLInfo . BitsetHi ) ) 
          (* Zero hi garbage bits of Sub. *) 
        ; IF Word . And ( LBitwordSub , Word . Not ( LBitwordSuper ) ) # 0 
          THEN (* Not a subset. *) 
            RETURN FALSE 
          ELSIF IsProper
          THEN 
            LBitwordSub 
              := Word . Or 
                   ( LBitwordSub , LTMaskOfIElem ( BSetLInfo . BitsetLo ) ) 
          ; LBitwordSub 
              := Word . Or 
                   ( LBitwordSub , GTMaskOfIElem ( BSetLInfo . BitsetHi ) ) 
            (* Set hi garbage bits of Sub. *) 
          ; RETURN 
              Word . And ( LBitwordSuper , Word . Not ( LBitwordSub ) ) # 0  
          ELSE 
            RETURN TRUE 
          END (* IF *) 
        ELSE (* Multiple words to check. *) 

        (* First Bitword of multiple: *) 
          IF Word . And ( LBitwordSub , Word . Not ( LBitwordSuper ) ) # 0 
          THEN (* Not a subset. *) 
            RETURN FALSE 
          ELSE
            IF IsProper
            THEN 
              LBitwordSub 
                := Word . Or 
                     ( LBitwordSub , LTMaskOfIElem ( BSetLInfo . BitsetLo ) ) 
              (* Set low garbage bits of Sub. *) 
            ; IF Word . And ( LBitwordSuper , Word . Not ( LBitwordSub ) ) # 0 
              THEN (* BSetR has an element that BSetL lacks.  No need for
                      further propriety checking, be we still don't know
                      whether we have a subset.
                   *) 
                IsProper := FALSE 
              END (* IF *) 
            END (* IF *) 
          ; INC ( LSetSubSs ) 
          ; INC ( LSetSuperSs ) 
          END (* IF *)

        (* Middle Bitwords, if any: *) 
        ; LSetSubFinalSs := LSetSubSs + LHiBitwordNo - LLoBitwordNo - 1 
        ; WHILE IsProper AND LSetSubSs < LSetSubFinalSs 
          DO 
            LBitwordSub := BSetLBitwords [ LSetSubSs ] 
          ; LBitwordSuper := BSetRBitwords [ LSetSuperSs ] 
          ; IF Word . And ( LBitwordSub , Word . Not ( LBitwordSuper ) ) # 0  
            THEN (* Not a subset. *) 
              RETURN FALSE 
            ELSE 
              IF Word . And ( LBitwordSuper , Word . Not ( LBitwordSub ) ) # 0 
              THEN (* BSetR has an element that BSetL lacks.  No need for
                      further propriety checking, be we still don't know
                      for certain what if we have a subset.
                   *) 
                IsProper := FALSE 
              END (* IF *) 
            ; INC ( LSetSubSs ) 
            ; INC ( LSetSuperSs ) 
            END (* IF *) 
          END (* WHILE *) 
        ; WHILE LSetSubSs < LSetSubFinalSs 
          DO 
            LBitwordSub := BSetLBitwords [ LSetSubSs ] 
          ; LBitwordSuper := BSetRBitwords [ LSetSuperSs ] 
          ; IF Word . And ( LBitwordSub , Word . Not ( LBitwordSuper ) ) # 0 
            THEN (* Not a subset. *) 
              RETURN FALSE 
            ELSE 
              INC ( LSetSubSs ) 
            ; INC ( LSetSuperSs ) 
            END (* IF *) 
          END (* WHILE *) 

        (* Last Bitword: *) 
        ; LBitwordSub := BSetLBitwords [ LSetSubSs ] 
        ; LBitwordSuper := BSetRBitwords [ LSetSuperSs ] 
        ; LBitwordSub 
            := Word . And 
                 ( LBitwordSub , LEMaskOfIElem ( BSetLInfo . BitsetHi ) ) 
          (* Zero hi garbage bits of Sub. *) 
        ; IF Word . And ( LBitwordSub , Word . Not ( LBitwordSuper ) ) # 0 
          THEN (* Not a subset. *) 
            RETURN FALSE 
          ELSIF IsProper
          THEN 
            LBitwordSub 
              := Word . Or 
                   ( LBitwordSub , GTMaskOfIElem ( BSetLInfo . BitsetHi ) ) 
            (* Set hi garbage bits of Sub. *) 
          ; RETURN 
              Word . And ( LBitwordSuper , Word . Not ( LBitwordSub ) ) # 0  
          ELSE 
            RETURN TRUE 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END IsSubsetBitsets 

(* VISIBLE: *) 
; PROCEDURE IsSubset ( SetL : T ; SetR : T ) : BOOLEAN

  = VAR IsSubsetResult : BOOLEAN 

  ; PROCEDURE InnerIsSubset 
      ( READONLY DInfoL : DissectInfo
      ; READONLY BitwordsL : ARRAY OF BitwordTyp
      ; READONLY DInfoR : DissectInfo
      ; READONLY BitwordsR : ARRAY OF BitwordTyp
      )

    = BEGIN (* InnerIsSubset *)
        IF DInfoL . RangeLo # IElemNull
        THEN (* SetL is a Rangeset. *)
          IF DInfoR . RangeLo # IElemNull
          THEN (* SetL is a Rangeset and SetR is a Rangeset. *)
            IsSubsetResult 
              := DInfoR . RangeLo <= DInfoL . RangeLo 
                 AND DInfoL . RangeHi <= DInfoR . RangeHi 
          ELSIF NUMBER ( BitwordsR ) = 0
          THEN (* SetL is a Rangeset (which is nonempty) and SetR is empty. *)
            IsSubsetResult := FALSE
          ELSE (* SetL is a Rangeset and SetR is a Bitset. *)
            IF DInfoL . RangeLo < DInfoR . BitsetInfo . BitsetLo 
               OR DInfoR . BitsetInfo . BitsetHi < DInfoL . RangeHi 
            THEN 
              IsSubsetResult := FALSE 
            ELSE 
              IsSubsetResult 
                := BitsetRangeIsFull
                     ( DInfoR . BitsetInfo , BitwordsR 
                     , DInfoL . RangeLo , DInfoL . RangeHi 
                     ) 
            END (* IF *) 
           END (* IF *)

        ELSIF NUMBER ( BitwordsL ) = 0
        THEN (* SetL is empty. *)
          IsSubsetResult := TRUE 

        ELSE (* SetL is a Bitset. *)
          IF DInfoR . RangeLo # IElemNull
          THEN (* SetL is a Bitset and SetR is a Rangeset. *)
            IF DInfoL . BitsetInfo . BitsetLo < DInfoR . RangeLo    
               OR DInfoR . RangeHi < DInfoL . BitsetInfo . BitsetHi  
            THEN (* SetL has a bit that SetR doesn't. *) 
              IsSubsetResult := FALSE 
            ELSE (* SetL lies wholly within the range of SetR, which 
                    makes it a subset.  
                 *)  
              IsSubsetResult := TRUE 
            END (* IF *) 
          ELSIF NUMBER ( BitwordsR ) = 0
          THEN (* SetL is a Bitset and SetR is empty. *)
            IsSubsetResult := FALSE
          ELSE (* SetL is a Bitset and SetR is a Bitset. *)
            IsSubsetResult 
              := IsSubsetBitsets 
                   ( DInfoL . BitsetInfo , BitwordsL 
                   , DInfoR . BitsetInfo , BitwordsR 
                   , IsProper := FALSE 
                )   
          END (* IF *)
        END (* IF *)
      END InnerIsSubset 

  ; BEGIN (* IsSubset *)
      <* FATAL ANY *> BEGIN 
        CallWithTwoSets ( SetL , SetR , InnerIsSubset )
      END (* Block *) 
    ; RETURN IsSubsetResult
    END IsSubset 

(* VISIBLE: *) 
; PROCEDURE IsProperSubset ( SetL : T ; SetR : T ) : BOOLEAN

  = VAR IsProperSubsetResult : BOOLEAN

  ; PROCEDURE InnerIsProperSubset 
      ( READONLY DInfoL : DissectInfo
      ; READONLY BitwordsL : ARRAY OF BitwordTyp
      ; READONLY DInfoR : DissectInfo
      ; READONLY BitwordsR : ARRAY OF BitwordTyp
      )

    = BEGIN (* InnerIsProperSubset *)
        IF DInfoL . RangeLo # IElemNull
        THEN (* SetL is a Rangeset. *)
          IF DInfoR . RangeLo # IElemNull
          THEN (* SetL is a Rangeset and SetR is a Rangeset. *)
            IF DInfoR . RangeLo  > DInfoL . RangeLo  
               OR DInfoL . RangeHi > DInfoR . RangeHi 
            THEN (* Not subset at all. *) 
              IsProperSubsetResult := FALSE 
            ELSE
              IsProperSubsetResult 
                := DInfoL . RangeLo > DInfoR . RangeLo
                   OR DInfoL . RangeHi < DInfoR . RangeHi
            END (* IF *) 
          ELSIF NUMBER ( BitwordsR ) = 0
          THEN (* SetL is a Rangeset and SetR is empty. *)
            IsProperSubsetResult := FALSE 
          ELSE (* SetL is a Rangeset and SetR is a Bitset. *)
            IF DInfoL . RangeLo < DInfoR . BitsetInfo . BitsetLo 
               OR DInfoR . BitsetInfo . BitsetHi < DInfoL . RangeHi 
            THEN (* Not a subset. *) 
              IsProperSubsetResult := FALSE 
            ELSIF 
              NOT BitsetRangeIsFull
                    ( DInfoR . BitsetInfo , BitwordsR  
                    , DInfoL . RangeLo , DInfoL . RangeHi 
                    ) 
            THEN (* Not a subset. *) 
              IsProperSubsetResult := FALSE 
            ELSE 
              IsProperSubsetResult 
                := DInfoR . BitsetInfo . BitsetLo < DInfoL . RangeLo 
                   OR DInfoR . BitsetInfo . BitsetHi > DInfoL . RangeHi 
            END (* IF *) 
          END (* IF *)

        ELSIF NUMBER ( BitwordsL ) = 0
        THEN (* SetL is empty. *)
          IF DInfoR . RangeLo # IElemNull
          THEN (* SetL is empty and SetR is a Rangeset. *)
            IsProperSubsetResult := TRUE 
          ELSIF NUMBER ( BitwordsR ) = 0
          THEN (* SetL is empty and SetR is empty. *)
            IsProperSubsetResult := FALSE 
          ELSE (* SetL is empty and SetR is a Bitset. *)
            IsProperSubsetResult := TRUE 
          END (* IF *)

        ELSE (* SetL is a Bitset. *)
          IF DInfoR . RangeLo # IElemNull
          THEN (* SetL is a Bitset and SetR is a Rangeset. *)
            IF DInfoL . BitsetInfo . BitsetLo < DInfoR . RangeLo    
               OR DInfoR . RangeHi < DInfoL . BitsetInfo . BitsetHi  
            THEN (* SetL has a bit that SetR doesn't. *) 
              IsProperSubsetResult := FALSE 
            ELSE (* SetL lies wholly within the range of SetR, which 
                    makes it a subset.  Furthermore, it has a 0-bit, making it 
                    proper.
                 *) 
              IsProperSubsetResult := TRUE 
            END (* IF *) 
          ELSIF NUMBER ( BitwordsR ) = 0
          THEN (* SetL is a Bitset and SetR is empty. *)
            IsProperSubsetResult := FALSE 
          ELSE (* SetL is a Bitset and SetR is a Bitset. *)
            IsProperSubsetResult 
              := IsSubsetBitsets 
                   ( DInfoL . BitsetInfo , BitwordsL 
                   , DInfoR . BitsetInfo , BitwordsR 
                   , IsProper := TRUE 
                   )   
          END (* IF *)
        END (* IF *)
      END InnerIsProperSubset 

  ; BEGIN (* IsProperSubset *)
      <* FATAL ANY *> BEGIN 
        CallWithTwoSets ( SetL , SetR , InnerIsProperSubset )
      END (* Block *) 
    ; RETURN IsProperSubsetResult
    END IsProperSubset 

(* ============================= Set equality ============================== *)

; PROCEDURE EqualBitsets 
    ( READONLY BSet1Info : BitsetInfoTyp 
    ; READONLY BSet1Bitwords : ARRAY OF BitwordTyp  
    ; READONLY BSet2Info : BitsetInfoTyp 
    ; READONLY BSet2Bitwords : ARRAY OF BitwordTyp 
    ) 
  : BOOLEAN 

  = VAR LLoIElem , LHiIElem : ValidIElemTyp 
  ; VAR LLoBitwordNo , LHiBitwordNo : BitwordNoTyp 
  ; VAR LBitwordCt : CARDINAL 
  ; VAR LBitword1 , LBitword2, LMask : BitwordTyp 
  ; VAR LSet1Ss , LSet2Ss : CARDINAL 

  ; BEGIN 
      LLoIElem := BSet1Info . BitsetLo 
    ; LHiIElem := BSet1Info . BitsetHi 
    ; IF LLoIElem # BSet2Info . BitsetLo OR LHiIElem # BSet2Info . BitsetHi  
      THEN 
        RETURN FALSE 
      ELSE (* The hard case. bounds are equal. *) 
        LLoBitwordNo := BitwordNoOfIElem ( LLoIElem ) 
      ; LHiBitwordNo := BitwordNoOfIElem ( LHiIElem ) 
      ; LSet1Ss := ArraySsOfBitwordNo ( LLoBitwordNo , BSet1Info . Bias ) 
      ; LSet2Ss := ArraySsOfBitwordNo ( LLoBitwordNo , BSet2Info . Bias ) 
      ; LBitword1 := BSet1Bitwords [ LSet1Ss ] 
      ; LBitword2 := BSet2Bitwords [ LSet2Ss ] 
      ; LMask := GEMaskOfIElem ( LLoIElem ) 
      ; LBitword1 := Word . And ( LBitword1 , LMask ) 
      ; LBitword2 := Word . And ( LBitword2 , LMask ) 
      ; IF LLoBitwordNo = LHiBitwordNo 
        THEN (* Only one Bitword is involved. *) 
          LMask := LEMaskOfIElem ( LHiIElem ) 
        ; LBitword1 := Word . And ( LBitword1 , LMask ) 
        ; LBitword2 := Word . And ( LBitword2 , LMask ) 
        ; RETURN LBitword1 = LBitword2 
        ELSE (* Multiple words to check. *) 
          IF LBitword1 # LBitword2 
          THEN 
            RETURN FALSE 
          ELSE 
            INC ( LSet1Ss ) 
          ; INC ( LSet2Ss ) 
          ; LBitwordCt := LHiBitwordNo - LLoBitwordNo - 1 
          ; IF SUBARRAY ( BSet1Bitwords , LSet1Ss , LBitwordCt ) 
               # SUBARRAY ( BSet2Bitwords , LSet2Ss , LBitwordCt ) 
            THEN
              RETURN FALSE 
            ELSE 
              INC ( LSet1Ss , LBitwordCt ) 
            ; INC ( LSet2Ss , LBitwordCt ) 
            ; LBitword1 := BSet1Bitwords [ LSet1Ss ] 
            ; LBitword2 := BSet2Bitwords [ LSet2Ss ] 
            ; LMask := LEMaskOfIElem ( LHiIElem ) 
            ; LBitword1 := Word . And ( LBitword1 , LMask ) 
            ; LBitword2 := Word . And ( LBitword2 , LMask ) 
            ; RETURN LBitword1 = LBitword2  
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END EqualBitsets 

(* VISIBLE: *) 
; PROCEDURE Equal ( Set1 , Set2 : T ) : BOOLEAN

  = VAR EqualResult : BOOLEAN

  ; PROCEDURE InnerEqual
      ( READONLY DInfo1 : DissectInfo
      ; READONLY Bitwords1 : ARRAY OF BitwordTyp
      ; READONLY DInfo2 : DissectInfo
      ; READONLY Bitwords2 : ARRAY OF BitwordTyp
      )

    = BEGIN (* InnerEqual *)
        IF DInfo1 . RangeLo # IElemNull
        THEN (* Set1 is a Rangeset. *)
          IF DInfo2 . RangeLo # IElemNull
          THEN (* Set1 is a Rangeset and Set2 is a Rangeset. *)
            EqualResult 
              := DInfo1 . RangeLo = DInfo2 . RangeLo 
                 AND DInfo1 . RangeHi = DInfo2 . RangeHi 
          ELSE (* Set1 is a Rangeset and Set2 is empty or a Bitset. *)
            EqualResult := FALSE 
          END (* IF *)

        ELSIF NUMBER ( Bitwords1 ) = 0
        THEN (* Set1 is empty. *)
          IF DInfo2 . RangeLo # IElemNull
          THEN (* Set1 is empty and Set2 is a Rangeset. *)
            EqualResult := FALSE 
          ELSIF NUMBER ( Bitwords2 ) = 0
          THEN (* Set1 is empty and Set2 is empty. *)
            EqualResult := TRUE 
          ELSE (* Set1 is empty and Set2 is a Bitset. *)
            EqualResult := FALSE 
          END (* IF *)

        ELSE (* Set1 is a Bitset. *)
          IF DInfo2 . RangeLo # IElemNull
          THEN (* Set1 is a Bitset and Set2 is a Rangeset. *)
            EqualResult := FALSE 
          ELSIF NUMBER ( Bitwords2 ) = 0
          THEN (* Set1 is a Bitset and Set2 is empty. *)
            EqualResult := FALSE 
          ELSE (* Set1 is a Bitset and Set2 is a Bitset. *)
            EqualResult 
              := EqualBitsets 
                   ( DInfo1 . BitsetInfo , Bitwords1  
                   , DInfo2 . BitsetInfo , Bitwords2  
                   )   
          END (* IF *)
        END (* IF *)
      END InnerEqual 

  ; BEGIN (* Equal *)
      <* FATAL ANY *> BEGIN 
        CallWithTwoSets ( Set1 , Set2 , InnerEqual )
      END (* Block *) 
    ; RETURN EqualResult
    END Equal

(* VISIBLE: *) 
; <* INLINE *> PROCEDURE Unequal ( Set1 : T ; Set2 : T ) : BOOLEAN

  = BEGIN (* Unequal *)
      RETURN NOT Equal ( Set1 , Set2 )
    END Unequal

(* =========================== Disjointness . ============================== *) 

; PROCEDURE DisjointRangeBitset 
    ( RSetLo , RSetHi : ValidIElemTyp 
    ; READONLY BSetInfo : BitsetInfoTyp 
    ; READONLY BSetBitwords : ARRAY OF BitwordTyp 
    ) 
  : BOOLEAN

  = VAR LIntersectionIElem : IElemTyp 

  ; BEGIN (* DisjointRangeBitset *) 
      IF RSetHi < BSetInfo . BitsetLo 
         OR RSetLo > BSetInfo . BitsetHi 
      THEN (* The range and the BSet-bounded region are disjoint. *) 
        RETURN TRUE 
      ELSIF RSetLo > BSetInfo . BitsetLo 
            AND RSetHi < BSetInfo . BitsetHi 
      THEN (* The range lies doubly properly between the bounds of the Bset. *)
        LIntersectionIElem 
          := LeastPresentIElemOfBSetInRange
               ( BSetInfo , BSetBitwords , RSetLo , RSetHi ) 
      ; RETURN LIntersectionIElem = IElemNull 
      ELSE (* The range covers one of the bounds of the BSet. *) 
        RETURN FALSE  
      END (* IF *)  
    END DisjointRangeBitset 

; PROCEDURE DisjointBitsets  
    ( READONLY BSet1Info : BitsetInfoTyp 
    ; READONLY BSet1Bitwords : ARRAY OF BitwordTyp 
    ; READONLY BSet2Info : BitsetInfoTyp 
    ; READONLY BSet2Bitwords : ARRAY OF BitwordTyp 
    ) 
  : BOOLEAN 

  = VAR LIntersectionIElem : IElemTyp 
  ; VAR LOverlapLoIElem , LOverlapHiIElem : IElemTyp 

  ; BEGIN (* DisjointBitsets *)
      IF BSet1Info . BitsetHi < BSet2Info . BitsetLo 
         OR BSet1Info . BitsetLo > BSet2Info . BitsetHi 
      THEN (* The two bounded regions are disjoint. *) 
        RETURN TRUE 
      ELSIF BSet1Info . BitsetLo = BSet2Info . BitsetLo 
            OR BSet1Info . BitsetHi = BSet2Info . BitsetHi
            OR BSet1Info . BitsetLo = BSet2Info . BitsetHi
            OR BSet1Info . BitsetHi = BSet2Info . BitsetLo 
      THEN 
        RETURN FALSE 
      ELSE 
        LOverlapLoIElem := MAX ( BSet1Info . BitsetLo , BSet2Info . BitsetLo )
      ; LOverlapHiIElem := MIN ( BSet1Info . BitsetHi , BSet2Info . BitsetHi )
      ; LIntersectionIElem 
          := LeastIntersectionIElemInRange 
               ( BSet1Info , BSet1Bitwords 
               , BSet2Info , BSet2Bitwords 
               , LOverlapLoIElem , LOverlapHiIElem 
               ) 
      ; RETURN LIntersectionIElem = IElemNull 
      END (* IF *)  
    END DisjointBitsets 

(* VISIBLE: *) 
; PROCEDURE Disjoint ( Set1 , Set2 : T ) : BOOLEAN
  (* Set1 and Set2 are disjoint.  Usually faster than empty intersection, and
     never does any heap allocation. 
  *) 

  = VAR DisjointResult : BOOLEAN

  ; PROCEDURE InnerDisjoint
      ( READONLY DInfo1 : DissectInfo
      ; READONLY Bitwords1 : ARRAY OF BitwordTyp
      ; READONLY DInfo2 : DissectInfo
      ; READONLY Bitwords2 : ARRAY OF BitwordTyp
      )

    = BEGIN (* InnerDisjoint *)
        IF DInfo1 . RangeLo # IElemNull
        THEN (* Set1 is a Rangeset. *)
          IF DInfo2 . RangeLo # IElemNull
          THEN (* Set1 is a Rangeset and Set2 is a Rangeset. *)
            DisjointResult 
              := DInfo1 . RangeHi < DInfo2 . RangeLo 
                 OR DInfo1 . RangeLo > DInfo2 . RangeHi 
          ELSIF NUMBER( Bitwords2 ) = 0
          THEN (* Set2 is empty. *) 
            DisjointResult := TRUE  
          ELSE (* Set1 is a Rangeset and Set2 is a Bitset. *)
            DisjointResult 
              := DisjointRangeBitset 
                   ( DInfo1 . RangeLo , DInfo1 . RangeHi 
                   , DInfo2 . BitsetInfo , Bitwords2 
                   ) 
          END (* IF *)

        ELSIF NUMBER( Bitwords1 ) = 0 
        THEN (* Set1 is empty. *) 
          DisjointResult := TRUE 
        ELSE (* Set1 is a Bitset. *)
          IF DInfo2 . RangeLo # IElemNull
          THEN (* Set1 is a Bitset and Set2 is a Rangeset. *)
            DisjointResult 
              := DisjointRangeBitset 
                   ( DInfo2 . RangeLo , DInfo2 . RangeHi 
                   , DInfo1 . BitsetInfo , Bitwords1 
                   ) 

          ELSIF NUMBER( Bitwords2 ) = 0
          THEN (* Set2 is empty. *) 
            DisjointResult := TRUE  
          ELSE (* Set1 is a Bitset and Set2 is a Bitset. *)
            DisjointResult 
              := DisjointBitsets 
                   ( DInfo1 . BitsetInfo , Bitwords1  
                   , DInfo2 . BitsetInfo , Bitwords2  
                   )   
          END (* IF *)
        END (* IF *)
      END InnerDisjoint 

  ; BEGIN (* Disjoint *)
      <* FATAL ANY *> BEGIN 
        CallWithTwoSets ( Set1 , Set2 , InnerDisjoint )
      END (* Block *) 
    ; RETURN DisjointResult
    END Disjoint

(* ============================ Hash codes. ================================ *) 

(* All these statistics counters apply only to Bitsets. *) 
; VAR GHashAbsComputed : INTEGER := 0  (* Was computed absolutely. *) 
; VAR GHashIncrComputed : INTEGER := 0 (* Was computed incrementally. *) 
; VAR GHashUncomputed : INTEGER := 0   (* Could not be computed. *) 
; VAR GHashTrueZero: INTEGER := 0      (* Could not be cached because zero. *)

(* - The hash code and only the hash code of the empty set is zero.
   - The hash code of a Rangeset is the Xor of its bounds.
   - The hash code of a Bitset is the Xor of its bounds and of all the
     Bitwords in its range, with garbage bits set to ones.
   - Hash codes of Bitsets are computed eagerly whenever it can be
     done without visiting more Bitwords than otherwise necessary
     (i.e., to compute the set value) and cached in the Hash field.
     The Hash function computes them on demand when not cached.  The
     value HashZero in field Hash means not cached.  In case a real
     hash code turns out to be zero, it will not ever be cached, just
     recomputed on demand.
   - While this is no doubt not the greatest hash code possible, it
     allows hash codes to be computed incrementally from the
     operand(s)' cached hash codes in many cases where it could not
     otherwise be done. Some cases can be computed absolutely by the
     above rule, some incrementally, some neither, some both.  If
     both, the absolutely computed value is used, since this conveys
     self-healing.
   - In an experiment where nearly 1.6 million set operation results
     that were Bitsets were computed:
     . 38% had hash codes computed eagerly and absolutely.
     . 40% had hash codes computed eagerly and incrementally. 
           (but not absolutely)
     . 22% had no hash code computed eagerly.
     . .0088% of the eagerly computed hash codes were zero. 
*) 

; PROCEDURE BitwordsHash 
    ( READONLY BSetInfo : BitsetInfoTyp 
    ; READONLY BSetBitwords : ARRAY OF BitwordTyp 
    ) 
  : HashTyp  
  (* The contribution of the Bitset array to the hash code of a Bitset. *)

  = VAR LBitsHash : HashTyp 
  ; VAR LLoBitwordNo , LHiBitwordNo : BitwordNoTyp 
  ; VAR LBitword : BitwordTyp 
  ; VAR LSs : CARDINAL 

  ; BEGIN 
      LBitsHash := 0 
    ; LLoBitwordNo := BitwordNoOfIElem ( BSetInfo . BitsetLo ) 
    ; LHiBitwordNo := BitwordNoOfIElem ( BSetInfo . BitsetHi ) 
    ; LSs := ArraySsOfBitwordNo ( LLoBitwordNo , BSetInfo . Bias ) 
    ; LBitword := BSetBitwords [ LSs ] 
    ; LBitword 
        := Word . Or ( LBitword , LTMaskOfIElem ( BSetInfo . BitsetLo ) ) 
           (* ^Set low garbage bits. *) 
    ; IF LLoBitwordNo < LHiBitwordNo 
      THEN (* Multiple words to hash. *) 
        LBitsHash := Word . Xor ( LBitsHash , LBitword ) 
      ; INC ( LSs ) 
      ; FOR RI := 2 TO LHiBitwordNo - LLoBitwordNo 
        DO (* Do the middle Bitwords. *)  
          LBitsHash 
            := Word . Xor ( LBitsHash , BSetBitwords [ LSs ] ) 
        ; INC ( LSs ) 
        END (* FOR *) 
      ; LBitword := BSetBitwords [ LSs ] 
      END (* IF *) 
    ; LBitword 
        := Word . Or ( LBitword , GTMaskOfIElem ( BSetInfo . BitsetHi ) ) 
           (* ^Set high garbage bits. *) 
    ; LBitsHash := Word . Xor ( LBitsHash , LBitword ) 
    ; RETURN LBitsHash 
    END BitwordsHash  

; PROCEDURE BitsetHashCacheable 
    ( READONLY BSetInfo : BitsetInfoTyp ; BitsHash : HashTyp ) 
  : HashTyp 
  (* The value to cache and to return.  Includes the bounds' contribution. *) 

  = VAR LBoundsHash , LBitsetHash : HashTyp 

  ; BEGIN 
      LBoundsHash := Word . Xor ( BSetInfo . BitsetHi , BSetInfo . BitsetLo ) 
    ; LBitsetHash := Word . Xor ( BitsHash , LBoundsHash ) (* Add. *) 
    ; RETURN LBitsetHash 
    END BitsetHashCacheable 

; PROCEDURE StoreBitsetHash 
    ( VAR BSetInfo : BitsetInfoTyp 
    ; BitsHash : HashTyp 
    ; IsValid : BOOLEAN := TRUE 
    ) 

  = BEGIN 
      IF IsValid 
      THEN
        BSetInfo . Hash := BitsetHashCacheable ( BSetInfo , BitsHash ) 
      ; IF BSetInfo . Hash = HashZero 
        THEN 
        (* This is a valid zero.  We just lose it, since zero in 
           BSetInfo . Hash really means unknown. 
        *) 
          INC ( GHashTrueZero ) 
        END (* IF *) 
      ELSE 
        INC ( GHashUncomputed )
      ; BSetInfo . Hash := HashZero  
      END (* IF *) 
    END StoreBitsetHash  

; PROCEDURE StoreBitsetHashAbs
    ( VAR BSetInfo : BitsetInfoTyp 
    ; HashAbs : HashTyp 
    ; IsValidAbs : BOOLEAN := TRUE 
    ) 

  = BEGIN 
      IF IsValidAbs 
      THEN
        INC ( GHashAbsComputed ) 
      ; StoreBitsetHash ( BSetInfo , HashAbs , TRUE ) 
      ELSE 
        StoreBitsetHash ( BSetInfo , 0 , FALSE ) 
      END (* IF *) 
    END StoreBitsetHashAbs 

; PROCEDURE StoreBitsetHashIncr 
    ( VAR BSetInfo : BitsetInfoTyp 
    ; HashIncr : HashTyp 
    ; IsValidIncr : BOOLEAN := TRUE 
    ) 

  = BEGIN 
      IF IsValidIncr  
      THEN 
        INC ( GHashIncrComputed ) 
      ; StoreBitsetHash ( BSetInfo , HashIncr , TRUE ) 
      ELSE 
        StoreBitsetHash ( BSetInfo , 0 , FALSE ) 
      END (* IF *) 
    END StoreBitsetHashIncr 

; PROCEDURE StoreBitsetHashAbsOrIncr 
    ( VAR BSetInfo : BitsetInfoTyp 
    ; HashAbs : HashTyp ; IsValidAbs : BOOLEAN  
    ; HashIncr : HashTyp ; IsValidIncr : BOOLEAN 
    ) 

  = BEGIN 
      IF IsValidAbs 
      THEN
        INC ( GHashAbsComputed ) 
      ; StoreBitsetHash ( BSetInfo , HashAbs , TRUE ) 
      ELSIF IsValidIncr 
      THEN  
        INC ( GHashIncrComputed ) 
      ; StoreBitsetHash ( BSetInfo , HashIncr , TRUE ) 
      ELSE 
        StoreBitsetHash ( BSetInfo , 0 , FALSE ) 
      END (* IF *) 
    END StoreBitsetHashAbsOrIncr 

; PROCEDURE RecoverBitsHash 
    ( READONLY BSetInfo : BitsetInfoTyp 
    ; VAR IsKnown : BOOLEAN 
    ; VAR Result : HashTyp 
    )
  (* Recover the bits portion of the hash code of BSet, removing the bounds'  
     contribution.  TRUE if success, otherwise unrecoverable.  
  *) 

  = VAR LResult : HashTyp  

  ; BEGIN 
      LResult := BSetInfo . Hash 
    ; IsKnown := LResult # HashZero  
    ; LResult := Word . Xor ( LResult , BSetInfo . BitsetLo ) (* Remove. *)  
    ; LResult := Word . Xor ( LResult , BSetInfo . BitsetHi ) (* Remove. *)  
    ; Result := LResult 
    END RecoverBitsHash  

(* VISIBLE: *) 
; PROCEDURE Hash ( Set : T ) : HashTyp  
  (* Hash(S) = 0 iff Equal(S,Empty()) *)  

  = VAR HashResult : HashTyp 

  ; PROCEDURE InnerHash
      ( READONLY DInfo : DissectInfo ; READONLY Bitwords : ARRAY OF BitwordTyp )

    = VAR LBitwordsHash : HashTyp 

    ; BEGIN (* InnerHash *)
        IF DInfo . RangeLo # IElemNull
        THEN (* Set is a Rangeset. *)
          HashResult := DInfo . RangeLo 
        ; IF DInfo . RangeLo # DInfo . RangeHi 
          THEN HashResult := Word . Xor ( HashResult , DInfo . RangeHi )  
          END (* IF *) 

        ELSIF NUMBER ( Bitwords ) = 0
        THEN (* Set is empty. *)
          HashResult := HashZero 

        ELSE (* Set is a Bitset. *)
          HashResult := DInfo . BitsetInfo . Hash 
        ; IF HashResult = HashZero 
          THEN (* Compute and maybe cache the hash. *) 
            LBitwordsHash := BitwordsHash ( DInfo . BitsetInfo , Bitwords )    
          ; HashResult 
              := BitsetHashCacheable ( DInfo . BitsetInfo , LBitwordsHash )   
          ; IF HashResult = HashZero 
            THEN 
              (* If the complete hash code, including both the bounds and the
                 Bitwords, is zero, we cannot cache it because a zero in
                 BSetInfo . Hash denotes unknown.  But we still need to return a
                 hash code, and it must be nonzero.  Since the bounds can not
                 be equal, their contribution cannot be zero, and thus, if the
                 complete code is zero, the Bitwords portion is not.  We can
                 use it for the returned hash code in this special case.
              *) 
              <* ASSERT LBitwordsHash # HashZero *>
              INC ( GHashTrueZero ) 
            ; HashResult := LBitwordsHash 
            ELSIF DInfo . BSet # NIL 
            THEN (* Cache the computed result. *) 
              DInfo . BSet . BitsetInfo . Hash := HashResult 
            END (* IF *) 
          END (* IF *) 
        END (* IF *)
      END InnerHash

  ; BEGIN (* Hash *)
      <* FATAL ANY *> BEGIN 
        CallWithOneSet ( Set , InnerHash )
      END (* Block *) 
    ; RETURN HashResult
    END Hash

(* ============================= Set membership =========================== *) 

; PROCEDURE IsIElemBitset 
    ( IElem : IElemTyp 
    ; READONLY BSetInfo : BitsetInfoTyp 
    ; READONLY BSetBitwords : ARRAY OF BitwordTyp 
    )
  : BOOLEAN

  = VAR LBitwordNo : BitwordNoTyp 
  ; VAR LBitword : BitwordTyp 
  ; VAR LSs : ArraySsTyp 

  ; BEGIN 
      IF IElem < BSetInfo . BitsetLo OR BSetInfo . BitsetHi < IElem 
      THEN RETURN FALSE 
      ELSE 
        LBitwordNo := BitwordNoOfIElem ( IElem ) 
      ; LSs := ArraySsOfBitwordNo ( LBitwordNo , BSetInfo . Bias ) 
      ; LBitword := BSetBitwords [ LSs ] 
      ; RETURN Word . And ( BitMaskOfIElem ( IElem ) , LBitword ) # 0  
      END (* IF *)
    END IsIElemBitset  

(* VISIBLE: *) 
; PROCEDURE IsElement ( Elem : ElemT ; Set : T ) : BOOLEAN

  = VAR IsElementResult : BOOLEAN 

  ; PROCEDURE InnerIsElement
      ( READONLY DInfo : DissectInfo ; READONLY Bitwords : ARRAY OF BitwordTyp )

    = VAR LIElem : IElemTyp 

    ; BEGIN (* InnerIsElement *)
        IF DInfo . RangeLo # IElemNull
        THEN (* Set is a Rangeset. *)
          LIElem := ORD ( Elem )  
        ; IsElementResult 
            := DInfo . RangeLo <= LIElem AND LIElem <= DInfo . RangeHi 

        ELSIF NUMBER ( Bitwords ) = 0
        THEN (* Set is empty. *)
          IsElementResult := FALSE 

        ELSE (* Set is a Bitset. *)
          LIElem := ORD ( Elem )
        ; IsElementResult 
            := IsIElemBitset ( LIElem , DInfo . BitsetInfo , Bitwords ) 
        END (* IF *)
      END InnerIsElement 

  ; BEGIN (* IsElement *)
      <* FATAL ANY *> BEGIN 
        CallWithOneSet ( Set , InnerIsElement )
      END (* Block *) 
    ; RETURN IsElementResult
    END IsElement

(* ========================= Enumeration of members ======================== *)

; PROCEDURE ForAllInBitwordDo 
    ( Bitword : BitwordTyp ; Bit0IElem : IElemTyp ; Proc : ProcOfValidElem ) 
  RAISES ANY 

  = VAR LBitword := Bitword 
  ; VAR LBitNo : BitNoTyp

  ; BEGIN 
      WHILE LBitword # 0 
      DO
        LBitNo := Least1BitNoInBitword ( LBitword ) 
      ; Proc ( VAL ( Bit0IElem + LBitNo , ValidElemT ) )  
      ; LBitword := Word . And ( LBitword , GTMaskOfBitNo ( LBitNo ) ) 
      END (* WHILE *)  
    END ForAllInBitwordDo 

(* VISIBLE: *) 
; PROCEDURE ForAllDo ( Set : T ; Proc : ProcOfValidElem )
  RAISES ANY
  (* Callback Proc(m) for every member m of T, in ascending order of ORD(m) *) 

  = PROCEDURE InnerForAllDo
      ( READONLY DInfo : DissectInfo
      ; READONLY Bitwords : ARRAY OF BitwordTyp
      )
    RAISES ANY

    = VAR LLoBitwordNo , LHiBitwordNo : BitwordNoTyp 
    ; VAR LBitword : BitwordTyp 
    ; VAR LSs : ArraySsTyp 
    ; VAR LBit0IElem : ValidIElemTyp 
 
    ; BEGIN (* InnerForAllDo *)
        IF DInfo . RangeLo # IElemNull
        THEN (* Set is a Rangeset. *)
          FOR RIElem := DInfo . RangeLo TO DInfo . RangeHi 
          DO Proc ( VAL ( RIElem , ValidElemT ) ) 
          END (* FOR *) 

        ELSIF NUMBER ( Bitwords ) = 0
        THEN (* Set is empty. *) 
          (* Do nothing. *) 

        ELSE (* Set is a Bitset. *)
          LLoBitwordNo := BitwordNoOfIElem ( DInfo . BitsetInfo . BitsetLo ) 
        ; LHiBitwordNo := BitwordNoOfIElem ( DInfo . BitsetInfo . BitsetHi ) 
        ; LSs := ArraySsOfIElem 
                   ( DInfo . BitsetInfo . BitsetLo , DInfo . BitsetInfo . Bias )
        ; LBit0IElem := BitZeroIElemOfBitwordNo ( LLoBitwordNo ) 
        ; LBitword := Bitwords [ LSs ] 
        ; LBitword 
            := Word . And 
                 ( LBitword , GEMaskOfIElem ( DInfo . BitsetInfo . BitsetLo ) )
                 (* ^Zero low garbage bits of low Bitword. *) 
        ; IF LLoBitwordNo < LHiBitwordNo 
          THEN (* Multiple words to examine. *) 
            ForAllInBitwordDo ( LBitword , LBit0IElem , Proc )  
          ; INC ( LSs ) 
          ; INC ( LBit0IElem , BitsPerBitword ) 
          ; FOR RI := 2 TO LHiBitwordNo - LLoBitwordNo 
            DO 
              LBitword := Bitwords [ LSs ] 
            ; ForAllInBitwordDo ( LBitword , LBit0IElem , Proc )  
            ; INC ( LSs ) 
            ; INC ( LBit0IElem , BitsPerBitword ) 
            END (* FOR *) 
          ; LBitword := Bitwords [ LSs ] 
          END (* IF *) 
        ; LBitword 
            := Word . And 
                 ( LBitword , LEMaskOfIElem ( DInfo . BitsetInfo . BitsetHi ) )
               (* ^Zero high garbage bits of High Bitword. *) 
        ; ForAllInBitwordDo ( LBitword , LBit0IElem , Proc )  
        END (* IF *)
      END InnerForAllDo

  ; BEGIN (* ForAllDo *)
      CallWithOneSet ( Set , InnerForAllDo )
    END ForAllDo

(* ============================= Iterators ================================= *) 

; CONST IteratorBrand = Brand & "_Iterator" 

; REVEAL Iterator 
    = IteratorPublic BRANDED IteratorBrand OBJECT  
        CurrentIElem : IElemTyp := IElemNull  
      OVERRIDES 
        current := IteratorCurrent 
      ; advance := NoopIteratorAdvance 
      END (* Iterator *) 

; TYPE BSetHeapIterator 
    = Iterator OBJECT 
        BSet : BitsetTyp 
      OVERRIDES 
        advance := BSetHeapIteratorAdvance 
      END (* BSetHeapIterator *) 

; TYPE BSetPseudoIterator 
    = Iterator OBJECT 
        BPseudoHi : ValidIElemTyp 
      ; Bitword : BitwordTyp (* For when we got a pseudo pointer. *) 
      OVERRIDES 
        advance := BSetPseudoIteratorAdvance 
      END (* BSetPseudoIterator *) 
    
; TYPE RSetIterator 
    = Iterator OBJECT 
        RangeHi : ValidIElemTyp 
      OVERRIDES 
        advance := RSetIteratorAdvance 
      END (* RSetIterator *) 

; PROCEDURE NewIterator ( Set : T ) : Iterator 
  (* Return a new Iterator, initialized to iterate over members m of T.
     Set the current member to the m with least ORD(m), or NullElem,
     if T is empty. 
  *)

  = VAR LBitword : BitwordTyp 
  ; VAR LLo , LHi : IElemTyp 
  ; LResultIterator : Iterator 
  ; LResultBPseudo : BSetPseudoIterator 
  ; LResultBSetHeap : BSetHeapIterator 
  ; LResultRSet : RSetIterator 

  ; BEGIN 
      DissectPseudoPointer 
        ( Set , (*VAR*) LBitword , (*VAR*) LLo , (*VAR*) LHi ) 
    ; IF LBitword # 0 
      THEN (* It's a pseudo Bitset. *)  
        LResultBPseudo := NEW ( BSetPseudoIterator ) 
      ; LResultBPseudo . Bitword := LBitword 
      ; LResultBPseudo . CurrentIElem := LLo 
      ; LResultBPseudo . BPseudoHi := LHi 
      ; RETURN LResultBPseudo 
      ELSIF LLo # IElemNull 
      THEN (* It's a pseudo range set. *) 
        LResultRSet := NEW ( RSetIterator ) 
      ; LResultRSet . CurrentIElem := LLo 
      ; LResultRSet . RangeHi := LHi 
      ; RETURN LResultRSet  
      ELSE (* Not a pseudopointer. *) 
        TYPECASE Set <* NOWARN *>   
        OF NULL (* Empty Set. *) 
        => LResultIterator := NEW ( Iterator ) 
        ; LResultIterator . CurrentIElem := IElemNull
        ; RETURN LResultIterator 

        | BitsetTyp ( TBSet ) (* Heap allocated Bitset. *) 
        => LResultBSetHeap := NEW ( BSetHeapIterator )  
        ; LResultBSetHeap . BSet := TBSet 
        ; LResultBSetHeap . CurrentIElem := TBSet . BitsetInfo . BitsetLo 
        ; RETURN LResultBSetHeap 

        | RangesetTyp ( TRSet ) (* Heap allocated range set. *)  
        => LResultRSet := NEW ( RSetIterator )  
        ; LResultRSet . CurrentIElem := TRSet . RangeLo 
        ; LResultRSet . RangeHi := TRSet . RangeHi  
        ; RETURN LResultRSet  

        END (* TYPECASE *) 
      END (* IF *) 
    END NewIterator 

; PROCEDURE IteratorCurrent ( Self : Iterator ) : ElemT 
  (* Return the current member of T.   
     NullElem  when no more elements exist. 
  *) 

  = BEGIN 
      RETURN VAL ( Self . CurrentIElem , ElemT ) 
    END IteratorCurrent 

; PROCEDURE NoopIteratorAdvance ( <*UNUSED*> Self : Iterator ) 

  = BEGIN END NoopIteratorAdvance 

; PROCEDURE BSetHeapIteratorAdvance ( Self : BSetHeapIterator ) 
  (* Advance the current member by one, in ascending order of ORD(m) *) 

  = BEGIN
      IF Self . CurrentIElem # IElemNull 
      THEN
        IF Self . CurrentIElem = Self . BSet . BitsetInfo . BitsetHi 
        THEN 
          Self . CurrentIElem := IElemNull 
        ELSE 
          Self . CurrentIElem 
            := LeastPresentIElemOfBSetInRange  
                 ( Self . BSet . BitsetInfo 
                 , Self . BSet . BitwordArrayRef ^ 
                 , Self . CurrentIElem + 1 
                 , Self . BSet . BitsetInfo . BitsetHi 
                 )
        END (* IF *) 
      END (* IF *)  
    END BSetHeapIteratorAdvance  

; PROCEDURE BSetPseudoIteratorAdvance ( Self : BSetPseudoIterator ) 
  (* Advance the current member by one, in ascending order of ORD(m) *) 

  = BEGIN
      IF Self . CurrentIElem # IElemNull 
      THEN
        IF Self . CurrentIElem = Self . BPseudoHi 
        THEN 
          Self . CurrentIElem := IElemNull 
        ELSE 
          Self . Bitword 
            := Word . And 
                 ( Self . Bitword , GTMaskOfIElem ( Self . CurrentIElem ) )
        ; Self . CurrentIElem := Least1BitNoInBitword ( Self . Bitword ) 
        END (* IF *) 
      END (* IF *)  
    END BSetPseudoIteratorAdvance  

; PROCEDURE RSetIteratorAdvance ( Self : RSetIterator ) 
  (* Advance the current member by one, in ascending order of ORD(m) *) 

  = BEGIN
      IF Self . CurrentIElem # IElemNull 
      THEN
        IF Self . CurrentIElem = Self . RangeHi 
        THEN 
          Self . CurrentIElem := IElemNull 
        ELSE 
          INC ( Self . CurrentIElem ) 
        END (* IF *) 
      END (* IF *)  
    END RSetIteratorAdvance  

(* =========================== Image ======================================= *) 

(* VISIBLE: *) 
; PROCEDURE Image 
    ( Set : T 
    ; ElemImage : ElemImageFuncTyp 
    ; Prefix : TEXT := ""  
      (* ^If a new line is inserted, the next line will begin with Prefix. *)
    ; MaxLine : CARDINAL := 80
      (* Lines with more than one element or range will not exceed this. *) 
    ) 
  : TEXT  
  RAISES { Thread . Alerted , Wr . Failure } 

  = VAR LineLen : CARDINAL 
  ; VAR IsFirstElem : BOOLEAN := TRUE 
  ; VAR RangeLoIElem , RangeHiIElem : IElemTyp := IElemNull 
  ; VAR WrT : TextWr . T 
  ; VAR Result : TEXT 

  ; PROCEDURE FlushRange ( ) 
    RAISES { Thread . Alerted , Wr . Failure } 

    = VAR Lt : TEXT 
    ; VAR LLen : CARDINAL

    ; BEGIN 
        IF RangeLoIElem # IElemNull 
        THEN 
          IF RangeLoIElem < RangeHiIElem 
          THEN 
            Lt := ElemImage ( VAL ( RangeLoIElem , ElemT ) ) & ".." 
                  & ElemImage ( VAL ( RangeHiIElem , ElemT ) )
          ELSE 
            Lt := ElemImage ( VAL ( RangeHiIElem , ElemT ) )
          END (* IF *)  
        ; IF IsFirstElem 
          THEN
            IsFirstElem := FALSE  
          ELSE 
            Lt := "," & Lt 
          END (* IF *) 
        ; LLen := Text . Length ( Lt ) 
        ; IF LineLen + LLen > MaxLine 
          THEN
            Wr . PutText ( WrT , Wr . EOL )
          ; Wr . PutText ( WrT , Prefix )
          ; LineLen := Text . Length ( Prefix ) + LLen 
          ELSE INC ( LineLen , LLen )   
          END (* IF *) 
        ; Wr . PutText ( WrT , Lt ) 
        END (* IF *) 
      END FlushRange 

  ; PROCEDURE Visit ( Elem : ValidElemT ) 
    RAISES { Thread . Alerted , Wr . Failure } 

    = VAR LIElem : ValidIElemTyp := ORD ( Elem ) 

    ; BEGIN 
        IF RangeLoIElem = IElemNull 
        THEN (* Start a new range. *) 
          RangeLoIElem := LIElem 
        ; RangeHiIElem := LIElem 
        ELSIF LIElem = RangeHiIElem + 1 
        THEN (* Just extend the stored range with this element. *) 
          RangeHiIElem := LIElem 
        ELSE (* Print the stored range, then start a new range. *) 
          FlushRange ( ) 
        ; RangeLoIElem := LIElem 
        ; RangeHiIElem := LIElem 
        END (* IF *)  
      END Visit 

  ; BEGIN (* Image *)  
      WrT := TextWr . New ( ) 
    ; IF Set = NIL 
      THEN 
        Wr . PutText ( WrT , "{}" ) 
      ELSE 
        Wr . PutText ( WrT , "{" ) 
      ; LineLen := 1 
      ; <* FATAL ANY *> 
        BEGIN 
          ForAllDo ( Set , Visit ) 
        END (* Block *) 
      ; FlushRange ( ) 
      ; Wr . PutText ( WrT , "}" ) 
      END (* IF *) 
    ; Result := TextWr . ToText ( WrT ) 
    ; RETURN Result 
    END Image  

(* ================= Verification of internal invariants ================== *) 

(* VISIBLE: *) 
; PROCEDURE VerifySet ( Set : T ) RAISES { BadInvariant } 

  = PROCEDURE InnerVerifySet
      ( READONLY DInfo : DissectInfo ; READONLY Bitwords : ARRAY OF BitwordTyp )
    RAISES { BadInvariant } 

    = VAR LLoBitwordNo , LHiBitwordNo : BitwordNoTyp 
    ; VAR LLoBitword , LHiBitword : BitwordTyp 
    ; VAR LLoSs , LHiSs : ArraySsTyp 
    ; VAR LCard : CardTyp 
    ; VAR LHash , LBitwordsHash : HashTyp 

    ; BEGIN (* InnerVerifySet *)
        IF DInfo . RangeLo # IElemNull
        THEN (* Set is a Rangeset. *)
          IF DInfo . RangeLo > DInfo . RangeHi 
          THEN 
            RAISE BadInvariant 
                    ( "RangeLo: " & Fmt . Int ( DInfo . RangeLo ) 
                      & " exceeds RangeHi: " & Fmt . Int ( DInfo . RangeHi ) 
                    )  
          END (* IF *) 

        ELSIF NUMBER ( Bitwords ) = 0
        THEN (* Set is empty. *)
          IF DInfo . BSet # NIL 
          THEN (* It's not really a NIL set, it's a Bitset with BitwordArrayRef
                  either NIL or pointing to an empty array. *) 
            IF DInfo . BSet . BitwordArrayRef = NIL 
            THEN
              RAISE BadInvariant ( "NIL BitwordArrayRef" ) 
            ELSE 
              RAISE BadInvariant 
                      ( "BitwordArrayRef points to zero-length array" ) 
            END (* IF *) 
          END (* IF *) 

        ELSE (* Set is a Bitset. *)
          IF DInfo . BitsetInfo . BitsetLo + 2 > DInfo . BitsetInfo . BitsetHi 
          THEN 
            RAISE BadInvariant 
                    ( "BitsetLo: " & Fmt . Int ( DInfo . BitsetInfo . BitsetLo )
                      & " and BitsetHi: " 
                      & Fmt . Int ( DInfo . BitsetInfo . BitsetHi ) 
                      & " do not allow at least 3 bits" 
                    )  
          ELSIF DInfo . BSet # NIL 
                AND DInfo . BSet . BitwordArrayRef = NIL 
          THEN (* We probably would have gone down the empy set route,
                  if this were the case, but just for robustness... 
               *) 
            IF DInfo . BSet . BitwordArrayRef = NIL 
            THEN
              RAISE BadInvariant ( "NIL BitwordArrayRef" ) 
            END (* IF *) 
          ELSE 
            LLoBitwordNo := BitwordNoOfIElem ( DInfo . BitsetInfo . BitsetLo ) 
          ; LHiBitwordNo := BitwordNoOfIElem ( DInfo . BitsetInfo . BitsetHi )
          ; LLoSs := ArraySsOfBitwordNo 
                       ( LLoBitwordNo , DInfo . BitsetInfo . Bias )  
          ; LHiSs := ArraySsOfBitwordNo 
                       ( LHiBitwordNo , DInfo . BitsetInfo . Bias ) 
          ; IF LLoSs < 0 
            THEN 
              RAISE BadInvariant 
                      ( "Negative subscript for BitsetLo: " 
                        & Fmt . Int ( DInfo . BitsetInfo . BitsetLo ) 
                        & " and Bias: " 
                        & Fmt . Int ( DInfo . BitsetInfo . Bias )
                      ) 
            END (* IF *) 
          ; IF LHiSs > LAST ( Bitwords )  
            THEN 
              RAISE BadInvariant 
                      ( "Subscript for BitsetHi: " 
                        & Fmt . Int ( DInfo . BitsetInfo . BitsetHi ) 
                        & " and Bias: " 
                        & Fmt . Int ( DInfo . BitsetInfo . Bias )
                        & "Exceeds Bitword array upperbound: " 
                        & Fmt . Int ( LAST ( Bitwords ) )
                      ) 
            END (* IF *) 

          ; LLoBitword := Bitwords [ LLoSs ] 
          ; LHiBitword := Bitwords [ LHiSs ] 
          ; IF Word . And 
                 ( LLoBitword 
                 , BitMaskOfIElem ( DInfo . BitsetInfo . BitsetLo ) 
                 ) 
               = 0 
            THEN 
              RAISE BadInvariant
                      ( "Lowest bit: " 
                        & Fmt . Int ( DInfo . BitsetInfo . BitsetLo ) 
                        & " is not present"
                      ) 
            END (* IF *) 
          ; IF Word . And 
                 ( LHiBitword 
                 , BitMaskOfIElem ( DInfo . BitsetInfo . BitsetHi ) 
                 ) 
               = 0 
            THEN 
              RAISE BadInvariant
                      ( "Highest bit: " 
                        & Fmt . Int ( DInfo . BitsetInfo . BitsetHi ) 
                        &  " is not present"
                      ) 
            END (* IF *) 
          ; IF BitsetRangeIsFull 
                    ( DInfo . BitsetInfo , Bitwords 
                    , DInfo . BitsetInfo . BitsetLo + 1 
                    , DInfo . BitsetInfo . BitsetHi - 1 
                    ) 
            THEN 
              RAISE BadInvariant ( "Bitset has no zero-bit" ) 
            END (* IF *) 
          ; IF DInfo . BitsetInfo . Card # CardUnknown 
            THEN 
              LCard := BitsetCard ( DInfo . BitsetInfo , Bitwords ) 
            ; IF LCard # DInfo . BitsetInfo . Card 
              THEN 
                RAISE BadInvariant
                        ( "Cached cardinality: " 
                          & Fmt . Unsigned 
                              ( DInfo . BitsetInfo . Card , base := 10 ) 
                          &  " unequal to recomputed value: "
                          & Fmt . Unsigned ( LCard , base := 10 )
                        ) 
              END (* IF *) 
            END (* IF *) 
          ; IF DInfo . BitsetInfo . Hash # HashZero  
            THEN 
              LBitwordsHash 
                := BitwordsHash ( DInfo . BitsetInfo , Bitwords )    
            ; LHash 
                := BitsetHashCacheable ( DInfo . BitsetInfo , LBitwordsHash )   
            ; IF LHash # DInfo . BitsetInfo . Hash 
              THEN 
                RAISE BadInvariant
                        ( "Cached hash code: 16_" 
                          & Fmt . Unsigned 
                              ( DInfo . BitsetInfo . Hash , base := 16 ) 
                          &  " unequal to recomputed value: 16_"
                          & Fmt . Unsigned ( LHash , base := 16 )
                        ) 
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        END (* IF *)
      END InnerVerifySet

  ; <* FATAL ANY *> BEGIN (* VerifySet *)
      CallWithOneSet ( Set , InnerVerifySet )
    END VerifySet

; BEGIN (* OrdSets *)
  END OrdSets
.

