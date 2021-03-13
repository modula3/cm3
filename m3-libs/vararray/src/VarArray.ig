
(* -----------------------------------------------------------------------1- *)
(* File VarArray.ig                                                          *)
(* Modula-3 source code.                                                     *)
(* Copyright 2013, Rodney M. Bates.                                          *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *) 
(* -----------------------------------------------------------------------2- *)

GENERIC INTERFACE VarArray ( Subscript , Element , Ranges )

(* An instantiation of this interface provides a type T that, viewed
   abstractly, is an array having elements of type Element.T and bounds 
   covering the entire range of ordinal type Subscript.T, which can have 
   negative as well as positive values.

   Each value A in T has a distinguished initial element value InitElemValue(A), 
   provided by the client at the time A is created.  All elements of A contain 
   InitElemValue(A) until explicitly assigned otherwise by the client.  

   Each A in T also has a range of subscripts touched(A), called its _touched_ 
   range.  This module guarantees all elements outside of touched(A) are equal 
   to InitElemValue(A).  Usually, it does so by expanding touched(A) as 
   necessary.  The client can reduce touched(A), which implicitly assigns 
   InitElemValue to elements outside the reduced touched(A).    
    
   Concretely, the array is dynamically allocated and expanded/copied as
   needed to have elements that cover touched(A), plus often a controlled 
   amount of extra allocation to prevent overly frequent reallocation 
   and copying.  At any one time, there is only one allocated array in the 
   implementation.  A client has some control over allocation parameters.   

   All operations that accept a T as parameter require it to be a non-NIL
   value that was obtained as a result from a previous operation herein.
   
   All operations that mutate a T do so in place, i.e., if there are any
   pointer copies of the T value, they all see the mutation.
  
*) 

(* Instantiating interface Subscript must contain:
     TYPE T          An ordinal type, satisfying:
                       FIRST (INTEGER) <= ORD (FIRST (T)) 
                       AND ORD (LAST (T)) <= LAST (INTEGER)
                       AND NUMBER (T) >= 2 (So empty range is representable.)
                     The provided abstract array type T will be subscripted by 
                     this type.    

     CONST Brand     A TEXT, used to construct the brand for T.

   NOTE: There is little benefit to instantiating with a subrange for the 
         subscripts, since the arrays are dynamically allocated in response
         to actual use. 
*) 

(* Implementation restriction: 
     At any time, the number of allocated array elements cannot exceed
     LAST(CARDINAL).  This restriction is a consequence of Modula-3's 
     limitation of open arrays to this many elements.  The implementation 
     will not try to exceed this without explicit assignments being made 
     by the client.  Do you have this much RAM anyway? 
*) 

(* Instantiating interface Element must contain:
     TYPE T          A type other than an open array type.  
                     Elements of the provided array type T will have this 
                     type. 

     CONST Brand     A TEXT, used to construct the brand for T.
*) 

(* Instantiating interface Ranges must provide a few things that can be 
   conveniently provided by making it an instantiation of companion 
   generic Ranges, with its Base formal equal to Subscript here. 
*) 

(* ALLOCATION: Whenever the client performs any operation that does or
   could possibly assign to an element, the module, if needed, first expands 
   the allocated space to contain at least that element, in addition to any
   elements already allocated, which retain their values if touched.  
   Because a change to allocation entails O(N) copying of the prexisting 
   touched elements, whenever it must expand at all, it does so more 
   liberally than absolutely necessary, in order to reduce the frequency
   with which this happens.

   It multiplies the total allocated space by ExpansionFactor(A) (thus
   allocated size grows exponentially), but not more than an absolute
   maximum factor (thus eventually, allocated size reverts to growing
   linearly in large steps).  It also limits expansion to not exceed
   the limits of Subscript.T, nor the limits of CARDINAL subscripts to
   Modula-3 open arrays.  And of course, it expands not less that what
   is required to contain the about-to-be touched elements.  Subject
   to the above constraints, expansion is all upward, except when only 
   downward expansion is needed, in which case it is all downward.  

   A client can set ExpansionFactor(A) to 1.0 or less, with the result
   that the module will never allocate more than exactly needed to
   contain about-to-be-touched elements.  

   This module raises AllocationFailure whenever an operation cannot be
   correctly completed because of any space allocation problem, including 
   operations requiring the allocation of excessive-sized arrays, as well
   as actual NEW failures.  

*)

(* THREAD SAFETY: No synchronization is done internally.  Clients must ensure 
   that all operations within and changes to DefaultInitialSize and 
   ExpansionFactor(A) are mutually excluded. 
*)

; FROM Ranges IMPORT RangeTyp , EmptyRange , FullRange 

; TYPE SsTyp = Subscript . T 
; TYPE ElemTyp = Element . T 

; CONST Brand 
    = "VarArray1.0_of_" & Subscript . Brand & "_and_" & Element . Brand 

; TYPE Public 
    = OBJECT 
        ExpansionFactor : REAL 
        (* ^Clients may change this, between operations. *) 
      END 

; TYPE T <: Public (* The provided abstract variable-sized array type. *)

; VAR DefaultInitialSize : CARDINAL := 10
  (* ^Clients may change this.  The module uses it only the first time it 
     touches any part of an array that was created with empty allocation. *)   

; CONST DefaultExpansionFactor = 2.0 

; EXCEPTION AllocationFailure

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

; PROCEDURE TouchedRange ( Array : T ) : RangeTyp 
  (* PRE: Array # NIL *) 
  (* Abstract view: 
     If touched(Array) is empty, EmptyRange 
     Otherwise, touched(Array). 
     Performance: O(1). 
  *) 

; PROCEDURE Touch ( Array : T ; Range : RangeTyp ) 
  RAISES { AllocationFailure } 
  (* PRE: Array # NIL *) 
  (* Abstract view: Expand touched(Array) to include Range. 
     Concrete view: Expand allocation if needed to cover Range. 
     Performance: If expansion is necessary, O(NUMBER((new)touched(Array))). 
       Otherwise O(1). 
  *)     

; PROCEDURE InitElemValue ( Array : T ) : ElemTyp 
  (* PRE: Array # NIL *) 
  (* Abstract view: The value that was passed to the call on New that 
       created Array. 
     Performance: O(1). 
  *)

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

; PROCEDURE Fetch ( Array : T ; Ss : SsTyp ) : ElemTyp  
  (* PRE: Array # NIL *) 
  (* Abstract view: Return Array[Ss]. 
      Ss need not be in touched(A).
      Do not touch Array[Ss].
        (Untouched elements implicitly have InitElemValue(Array)) 
     Concrete view: (Make no change of allocation.)
     Performance: O(1). 
  *)  

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

; TYPE ProcOfSsTypVARElemTyp 
    = PROCEDURE ( Ss : SsTyp ; VAR Elem : ElemTyp ) RAISES ANY 

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

; TYPE ProcOfSsTypVARArrayTyp 
    = PROCEDURE ( Ss : SsTyp ; VAR Elems : ARRAY OF ElemTyp ) RAISES ANY 

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
     Let [F..T] be I, if By>=0, or [I.Hi..I.Lo], if By<0. 
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

; PROCEDURE ForAllInRange
    ( Array : T 
    ; From : SsTyp 
    ; To : SsTyp 
    ; By : INTEGER := 1 
    ; Do : ProcOfSsTypVARElemTyp 
    ) 
  RAISES ANY (* AllocationFailure from ForAllInRange, ANY from Do. *) 
  (* PRE: Array # NIL *) 
  (* Let R be the range [From..To], if By>=0, or [To..From], if By<0. 
     Abstract view: 
       1) Expand touched(Array) to cover R.  
       2) Call back Do, passing Ss and Array[Ss], for the sequence 
          Ss=From, From+By, From+2*By, ... so long as Ss<=To, if By>=0,
          or Ss>=To, if By<0. 
       Like a FOR statement, if By=0, this will loop forever.  
       Unlike a FOR statement, this will work right up against the lower 
       and upper bounds of SsTyp, both as starting and ending subscript.   
       The range is given in the style of a FOR-loop.   
     Concrete view: Expand allocation if needed to cover R. 
       More efficient than a client-written loop making repeated
       calls on other single-element-accessing procedures in this interface.
     Performance: O(NUMBER(R)). 
  *)  

; PROCEDURE Project ( Array : T ; Range : RangeTyp ) 
  (* PRE: Array # NIL *) 
  (* Let PRange be the intersection of Range with touched(Array). 
     Abstract view: 
       1) Set all elements of Array outside PRange to InitElemValue.  
       2) Make touched(Array) equal to PRange. 
     Concrete view: (Make no change of allocation.) 
     Performance: O(1).  
  *) 

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

(* Manipulation of space allocation only. *) 

; PROCEDURE AllocatedRange ( Array : T ) : RangeTyp 
  (* PRE: Array # NIL *) 
  (* Abstract view: A noop.
     Concrete view: The currently allocated space of Array. 
     Performance: O(1). 
  *) 

; PROCEDURE Compact ( Array : T ) 
  RAISES { AllocationFailure } 
  (* PRE: Array # NIL *) 
  (* Abstract view: A noop. 
     Concrete view: Deallocate space for everything outside touched(Array). 
     Performance: If any deallocation is done, O(NUMBER(touched(Array))), 
       otherwise, O(1).  
  *) 

; TYPE RawInfoTyp 
  = RECORD
      Touched : Ranges . RangeTyp   
    ; BiasInt : INTEGER  
    ; ArrayRef : REF ARRAY OF ElemTyp 
    END 

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

; END VarArray 
. 
