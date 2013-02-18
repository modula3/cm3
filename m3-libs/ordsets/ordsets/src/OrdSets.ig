  
(* -----------------------------------------------------------------------1- *)
(* File OrdSets.ig  Modula-3 source code.                                    *)
(* Copyright 2010 .. 2012, Rodney M. Bates.                                  *)
(* rbates@acm.org                                                            *)
(* Licensed under the Gnu Public License, version 2 or later.                *)
(* -----------------------------------------------------------------------2- *)

GENERIC INTERFACE OrdSets ( Element ) 

(* Instantiating interface Element must contain:
     TYPE T          An ordinal type, satisfying
                     FIRST (INTEGER) <= ORD (FIRST (T)) 
                     AND ORD (LAST (T)) <= LAST (INTEGER).   

     TYPE ValidElemT A nonempty, proper subrange of T. Elements of sets 
                     provided by instantiations of OrdSets will have this type. 

     CONST NullElem  A constant of type T, that is not a member of ValidElemT.

     CONST Brand     A TEXT, used to construct the brand for an instantiation.
*) 

(* This interface provides operations on sets whose members are of an ordinal 
   type.  It is written in a functional style.  It never mutates a set value, 
   (except for some internal lazy computation--not visible to clients), and 
   thus it sometimes is able to share heap objects.

   Its primary use pattern is where the set values can have widely
   varying sizes, you want a very large maximum size limit, but many 
   of the sets are expected to be much smaller than the maximum.  For
   this to happen, you probably want to instantiate only with INTEGER
   or WIDECHAR.  It will work with LONGINT, but only if its target-machine-
   dependent range is a subrange of INTEGER.  There is no space or time 
   performance benefit to instantiating with a subrange of the base type.

   If this does not fit your needs, you probably want to use
   Modula-3's builtin set type, or some other package.

   The set representations occupy variable-sized heap objects, just
   sufficient for the set value.  In the most general case, these use
   heap-allocated open arrays of machine words, with one bit per
   actual set member, plus some overhead, of course.

   If you compile with a later CM3 Modula-3 compiler and garbage
   collector that tolerate misaligned "pseudo" pointers, i.e, with the
   least significant bit set to one, you can set a boolean constant in
   the corresponding module OrdSets.mg.  This will cause it to utilize
   this Modula-3 implementation feature to store sufficiently small set 
   values entirely within the pointer word, avoiding the high space and 
   time overheads of heap allocation.  The CM3 5-8 compiler is sufficient.  
   SRC M3, PM3, EZM3, and earlier CM3 versions are not.  As of 2012-7-15,
   Pickles do not handle these.  Enable this with DoPseudoPointers, in 
   OrdSets.mg.  

   Up through 2013-02-17, Pickles will not tolerate this misaligned
   pointers.  Also, up through the same date, even without misaligned
   pointers, pickles will not correctly convert these sets between different
   word sizes.     
*) 

(* Thread safety: 

   I believe this module to be thread-safe, without needing mutual
   exclusion around calls to its visible procedures, except for a
   small chance of a performance bug involving simultaneous calls to
   Hash or simultaneous calls to Card, where some cached-lazy computation
   could be repeated unnecessarily.  This has had no testing. 
   A rationale is given in the corresponding module OrdSets.mg.
*)

; IMPORT Thread 
; IMPORT Word 
; IMPORT Wr 

; CONST Brand = "OrdSets1.0_of_" & Element . Brand 

; TYPE T <: REFANY 

; TYPE ElemT = Element . T 
; TYPE ValidElemT = Element . ValidElemT 
; CONST NullElem = Element . NullElem 

; TYPE ProcOfValidElem = PROCEDURE ( Elem : ValidElemT ) RAISES ANY 

; PROCEDURE Empty ( ) : T 
  (* Empty set. *) 

; PROCEDURE Singleton ( Elem : ElemT ) : T 
(* Singleton set containing just Elem.  Empty set if 
   Elem not in ValidElemT 
*) 

; PROCEDURE Range ( Lo , Hi : ElemT ) : T 
  (* Set containing all elements in the range [ Lo .. Hi ].  Empty set if
     either Lo or Hi is not in ValidElemT, or Lo > Hi  
  *) 

; PROCEDURE FromArray ( READONLY Elements : ARRAY OF ElemT ) : T 
  (* Set containing the valid members of Elements. 
     More efficient than repeated Includes or Unions of Singletons. 
  *) 

; PROCEDURE Union ( Set1 : T ; Set2 : T ) : T
(* Union of Set1 and Set2. *) 

; PROCEDURE Intersection ( Set1 : T ; Set2 : T ) : T 
(* Intersection of Set1 and Set2. *) 

; PROCEDURE Project ( Set : T ; Min , Max : ElemT ) : T 
  (* Remove elements outside the range Min .. Max *)

; PROCEDURE Difference ( Set1 : T ; Set2 : T ) : T 
  (* Set difference of Set1 minus Set2. *) 

; PROCEDURE SymDiff ( Set1 : T ; Set2 : T ) : T 
  (* Symmetric difference of Set1 and Set2. 
     IsElement(SymDiff(S1,S2),E) iff IsElement(S1,E) # IsElement(S2,E) 
  *) 

; PROCEDURE Include ( Set : T ; Elem : ElemT ) : T 
  (* Union of Set and Singleton(Elem), but often more efficient. *) 

; PROCEDURE Exclude ( Set : T ; Elem : ElemT ) : T 
  (* Difference of Set minus Singleton(Elem), but often more efficient. *) 

; PROCEDURE IsEmpty ( Set : T ) : BOOLEAN

; PROCEDURE Minimum ( Set : T ) : ElemT 
  (* Minimum valued element of Set.  NullElem, if Set is empty. *) 

; PROCEDURE Maximum ( Set : T ) : ElemT 
  (* Maximum valued element of Set.  NullElem, if Set is empty. *) 

; PROCEDURE ArbitraryMember ( Set : T ) : ElemT 
  (* An arbitrary member of Set.  NullElem, if Set is empty. *) 

; PROCEDURE ExtractArbitraryMember ( VAR Set : T ) : ElemT 
  (* Equivalent to: 
       WITH W = ArbitraryMember ( Set ) 
       DO IF W # NullElem THEN Set := Exclude ( Set , W ) END 
       ; RETURN W 
       END
     but faster.  
  *) 

; PROCEDURE Complement 
    ( Set : T ; UnivLo , UnivHi : ElemT := NullElem ) : T 
  (* Complement WRT a universe of [ UnivLo .. UnivHi ].
     The universe is first widened if necessary to cover Minimun(Set)
     ..Maximum(Set). 
     If Set is empty and exactly one of UnivLo,UnivHi is valid, Set is 
     complemented WRT Singleton(TheOneValidUnivBound).  Otherwise Empty()
     is complemented WRT Empty().     

     WARNING: This can create a *very* large heap object if the
              universe is large.  You probably want a universe having
              dynamically computed bounds that are far less
              extravagant than ValidElemT.  And if ValidElemT weren't
              large, you would probably be using some other set
              representation, such as Modula-3's builtin SET types.
              There is no Complement operation WRT ValidElemT, to make 
              it harder to inadvertently construct huge objects.
  *)   

; TYPE CardTyp = Word . T (* Cardinality.  Treat as unsigned but full range. *)

; PROCEDURE Card ( Set : T ) : CardTyp 
  (* Cardinality of Set. *) 

; PROCEDURE IsSubset ( Set1 : T ; Set2 : T ) : BOOLEAN

; PROCEDURE IsProperSubset ( Set1 : T ; Set2 : T ) : BOOLEAN

; PROCEDURE Equal ( Set1 , Set2 : T ) : BOOLEAN

; PROCEDURE Unequal ( Set1 : T ; Set2 : T ) : BOOLEAN

; PROCEDURE Disjoint ( Set1 , Set2 : T ) : BOOLEAN
  (* Set1 and Set2 are disjoint.  Usually faster than empty intersection, and
     never does any heap allocation. 
  *) 

; PROCEDURE Compare ( Set1 , Set2 : T ) : [ - 1 .. 1 ] (* <, =, >*)  
  (* Compare two sets according to an arbitrary but consistent total ordering
     on their abstract velues. 
  *) 

; TYPE HashTyp = Word . T 

; PROCEDURE Hash ( Set : T ) : HashTyp
  (* Hash(S) = 0 iff Equal(S,Empty()) *)  

; PROCEDURE IsElement ( Elem : ElemT ; Set : T ) : BOOLEAN

; PROCEDURE ForAllDo ( Set : T ; Proc : ProcOfValidElem ) 
  RAISES ANY
  (* Callback Proc(m) for every member m of T, in ascending order of ORD(m) *) 

; TYPE Iterator <: IteratorPublic 

; TYPE IteratorPublic 
  = OBJECT METHODS 
      current ( ) : ElemT 
      (* Return the current member of T.    
         NullElem when no more elements exist. 
      *) 
    ; advance ( ) 
      (* Advance the current member by one, in ascending order of ORD(m) *) 
    END (* IteratorPublic *) 

; PROCEDURE NewIterator ( Set : T ) : Iterator 
  (* Return a new Iterator, initialized to iterate over members m of T.
     Set the current member to the m with least ORD(m). *)  

; TYPE ElemImageFuncTyp = PROCEDURE ( Elem : ValidElemT ) : TEXT 

; PROCEDURE Image 
    ( Set : T 
    ; ElemImage : ElemImageFuncTyp 
    ; Prefix : TEXT := ""  
      (* If a new line is inserted, the following line will begin with this. *)
    ; LineLen : CARDINAL := 80
      (* Lines with more than one element or range will not exceed this. *) 
    ) 
  : TEXT  
  RAISES { Thread . Alerted , Wr . Failure } 
  (* A human readable image of a set, calling back ElemImage to display 
     elements, according to the following grammar: 
     Image ::= '{' List '}'
     List ::= { EorR / ',' } (* Comma-separated list of zero or more EorR's *)  
     EorR := Elem | Elem '..' Elem
  *) 

; EXCEPTION BadInvariant ( TEXT ) 

; PROCEDURE VerifySet ( Set : T ) RAISES { BadInvariant } 
  (* Verify that Set satisfies internal representation invariants. *) 

; END OrdSets 
. 


