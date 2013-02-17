  
(* -----------------------------------------------------------------------1- *)
(* File Sets.m3  Modula-3 source code.                                       *)
(* Copyright 2010 .. 2012, Rodney M. Bates.                                  *)
(* rbates@acm.org                                                            *)
(* Licensed under the Gnu Public License, version 2 or later.                *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically converted to Modula-3 and modified by 
   Rodney M. Bates, 2001, 2002, from Cocktail, reuse, Sets.mi, 
   which was originally written in Modula-2 and part of the reuse 
   library of the Cocktail tool package: 

   Author: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

(* $Id: Sets.m3,v 1.5 2013-02-17 23:20:33 jkrell Exp $ *)

(*
   $Log: not supported by cvs2svn $
*) 

(*
* Revision 1.1  2012-07-16 16:29:16  rodney
*
* Sets.i3 Sets.m3
*
* Sets is an old package that OrdSets replaces.  Used here to compare
* results between the packages, for testing OrdSets.
* 
 
 * RMB Aug 2001 Converted to Modula-3 
 
 * RMB July 98 Documentation/name changes. 
 
 * RMB 97/06/05 Changed ForAllDo and IsElement to not choke on NIL 
                BitsetPtr, since some tree dumps call 
                WriteSet for sets that are not in use and need 
                no allocated array. 
 
 * RMB 97/06/05 Added InitNullSet, since some tree dumps call 
                WriteSet for sets that are not in use and need 
                no allocated array. 
 
 * RMB 93/10/13 Rework types for WRL Modula-2. 
 
 * Revision 1.4  1991/11/21  14:33:17  grosch 
 * new version of RCS on SPARC 
 * 
 * Revision 1.3  90/05/30  17:08:45  grosch 
 * bug fixes in Complement and ReadSet 
 * 
 * Revision 1.2  89/09/20  11:50:33  grosch 
 * turned many FOR into WHILE loops to increase efficiency 
 * 
 * Revision 1.1  89/01/09  17:13:35  grosch 
 * added functions Size, Minimum, and Maximum 
 * 
 * Revision 1.0  88/10/04  11:47:13  grosch 
 * Initial revision 
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

MODULE Sets

; IMPORT FloatMode
; IMPORT Fmt
; IMPORT Lex
; IMPORT Rd
; IMPORT Thread 
; IMPORT Word
; IMPORT Wr

; CONST UnknownCardinality = LAST ( tInternalElmt )

; TYPE BitsetTyp = Word . T
; TYPE tInternalElmt = tElement

; REVEAL tSet = BRANDED "Sets.T" REF SetRecTyp

; TYPE ArrayOfBitset = ARRAY OF BitsetTyp
; TYPE RefArrayOfBitset = REF ArrayOfBitset 

; TYPE SetRecTyp
  = RECORD
      BitsetPtr : RefArrayOfBitset
    ; MaxElmt : tInternalElmt
    ; Card : tInternalElmt
    ; FirstElmt : tInternalElmt
    ; LastElmt : tInternalElmt
    END (* RECORD *)

; CONST AllOnes = Word . Not ( 0 )  

(* 
; CONST Bits
    = ARRAY [ 0 .. BITSIZE ( BitsetTyp ) - 1 ] OF BitsetTyp
        { Word . Shift ( 1 , 0 )
        , Word . Shift ( 1 , 1 )
        , Word . Shift ( 1 , 2 )
        , Word . Shift ( 1 , 3 )
        , Word . Shift ( 1 , 4 )
        , Word . Shift ( 1 , 5 )
        , Word . Shift ( 1 , 6 )
        , Word . Shift ( 1 , 7 )
        , Word . Shift ( 1 , 8 )
        , Word . Shift ( 1 , 9 )
        , Word . Shift ( 1 , 10 )
        , Word . Shift ( 1 , 11 )
        , Word . Shift ( 1 , 12 )
        , Word . Shift ( 1 , 13 )
        , Word . Shift ( 1 , 14 )
        , Word . Shift ( 1 , 15 )
        , Word . Shift ( 1 , 16 )
        , Word . Shift ( 1 , 17 )
        , Word . Shift ( 1 , 18 )
        , Word . Shift ( 1 , 19 )
        , Word . Shift ( 1 , 20 )
        , Word . Shift ( 1 , 21 )
        , Word . Shift ( 1 , 22 )
        , Word . Shift ( 1 , 23 )
        , Word . Shift ( 1 , 24 )
        , Word . Shift ( 1 , 25 )
        , Word . Shift ( 1 , 26 )
        , Word . Shift ( 1 , 27 )
        , Word . Shift ( 1 , 28 )
        , Word . Shift ( 1 , 29 )
        , Word . Shift ( 1 , 30 )
        , Word . Shift ( 1 , 31 )
        }
*) 
(* TODO: ^PORTABILITY: Make this word-size-independent.  Maybe just 
         replace Bits[I] everywhere by Word.Shift(1,I).  I think it is
         a leftover from Mocka Modula-2, where perhaps no Word.Shift 
         counterpart existed. 
*) 

; VAR Bits : ARRAY [ 0 .. BITSIZE ( BitsetTyp ) - 1 ] OF BitsetTyp 

; PROCEDURE InitBits ( ) 

   = BEGIN 
       FOR RI := 0 TO BITSIZE ( BitsetTyp ) - 1 
       DO 
         Bits [ RI ] := Word . Shift ( 1 , RI ) 
       END (* FOR *) 
     END InitBits 

; VAR GArrayCt : CARDINAL := 0 
; VAR GArrayElemCt : CARDINAL := 0 
; VAR GSetCt : CARDINAL := 0 

; PROCEDURE NewRefArrayOfBitset ( N : CARDINAL ) : RefArrayOfBitset 

  = BEGIN 
      INC ( GArrayCt ) 
    ; INC ( GArrayElemCt , N ) 
    ; RETURN NEW ( RefArrayOfBitset , N ) 
    END NewRefArrayOfBitset 

; PROCEDURE NeededBitsetCount ( ElementCt : tElement ) : CARDINAL 

  = BEGIN 
      RETURN 
        ( ElementCt + BITSIZE ( BitsetTyp )
          - ElementCt MOD BITSIZE ( BitsetTyp )
        )
        DIV BITSIZE ( BitsetTyp )
    END NeededBitsetCount

; PROCEDURE MakeSet ( VAR Set : tSet ; MaxElementCt : tElement := 256 )

  = VAR LBitsetCount : CARDINAL

  ; BEGIN (* MakeSet *)
      INC ( GSetCt ) 
    ; Set := NEW ( tSet )
    ; Set . MaxElmt := MaxElementCt - 1 
    ; LBitsetCount := NeededBitsetCount ( Set . MaxElmt ) 
    ; Set . BitsetPtr := NewRefArrayOfBitset ( LBitsetCount )
    ; AssignEmpty ( Set )
    END MakeSet

; PROCEDURE ReleaseSet ( VAR Set : tSet )

  = BEGIN (* ReleaseSet *)
      Set := NIL
    END ReleaseSet

; PROCEDURE Union ( VAR Set1 : tSet ; Set2 : tSet )

  = VAR N1 : CARDINAL 
  ; VAR N2 : CARDINAL 
  ; VAR LNewBitsetPtr : RefArrayOfBitset 

  ; BEGIN (* Union *)
      IF Set2 = NIL OR Set2 . BitsetPtr = NIL 
      THEN (* Set2 is empty, Set1 remains unchanged. *) 
      ELSIF Set1 = NIL OR Set1 . BitsetPtr = NIL 
      THEN (* Set1 is empty and Set2 is not. *) 
        Copy ( Set1 , Set2 )  
      ELSE (* Compute the result. *) 
        N1 := NUMBER ( Set1 . BitsetPtr ^ ) 
      ; N2 := NUMBER ( Set2 . BitsetPtr ^ )
      ; WHILE N2 > 0 AND Set2 . BitsetPtr ^ [ N2 - 1 ] = 0 
        DO DEC ( N2 ) 
        END (* WHILE *)  
      ; IF N2 > N1 
        THEN (* Must expand Set1. *) 
          LNewBitsetPtr := NewRefArrayOfBitset ( N2 ) 
        ; SUBARRAY ( LNewBitsetPtr ^ , 0 , N1 ) := Set1 . BitsetPtr ^ 
        ; FOR RI := N1 TO N2 - 1 
          DO 
            LNewBitsetPtr ^ [ RI ] := 0 
          END (* FoR *) 
        ; Set1 . BitsetPtr := LNewBitsetPtr 
        ; N1 := N2 
        END (* IF *) 
      ; FOR i := 0 TO N2 - 1
        DO WITH WBitset1 = Set1 . BitsetPtr ^ [ i ]
          DO
            WBitset1 := Word . Or ( WBitset1 , Set2 . BitsetPtr ^ [ i ] )
          END (* WITH *)
        END (* FOR *)
      ; Set1 . Card := UnknownCardinality
      ; Set1 . FirstElmt := MIN ( Set1 . FirstElmt , Set2 . FirstElmt )
      ; Set1 . LastElmt := MAX ( Set1 . LastElmt , Set2 . LastElmt )
      END (* IF *) 
    END Union

; PROCEDURE Difference ( VAR Set1 : tSet ; Set2 : tSet )

  = BEGIN (* Difference *) 
      IF Set1 = Set2 
      THEN Copy ( Set1 , Set1 ) 
      END (* IF *) 
    ; FOR i := 0 
          TO MIN ( NUMBER ( Set1 . BitsetPtr ^ )  
                 , NUMBER ( Set2 . BitsetPtr ^ ) 
                 ) 
             - 1 
      DO WITH WBitset1 = Set1 . BitsetPtr ^ [ i ]
        DO
          WBitset1
            := Word . And
                 ( WBitset1 , Word . Not ( Set2 . BitsetPtr ^ [ i ] )
                 )
        END (* WITH *)
      END (* FOR *)
    ; Set1 . Card := UnknownCardinality
(* CHECK: Is it OK for FirstElmt and LastElmt to be too wide a range? *)
    END Difference

; PROCEDURE Project ( VAR Set : tSet ; Min : tElement ; Max : tElement )
    (* Remove elements outside the range Min .. Max *)

  = VAR LFirstBitset : CARDINAL
  ; VAR LLastBitset : CARDINAL

  ; BEGIN (* Project *)
      IF Min > Max 
      THEN (* Project range is empty. *) 
        AssignEmpty ( Set ) 
      ELSE 
        IF Set . LastElmt < Min OR Set . FirstElmt > Max 
        THEN (* No overlap. *) 
          AssignEmpty ( Set ) 
        ELSE
          Max := MIN ( Max , Set . LastElmt )  
        ; LFirstBitset := Min DIV BITSIZE ( BitsetTyp )
        ; LLastBitset := Max DIV BITSIZE ( BitsetTyp )
        ; FOR FI := Set . FirstElmt DIV BITSIZE ( BitsetTyp ) 
                 TO LFirstBitset - 1
          DO Set . BitsetPtr ^ [ FI ] := 0
          END (* FOR *)
        ; IF Set . FirstElmt < Min
          THEN
            WITH WBitset = Set . BitsetPtr ^ [ LFirstBitset ]
            DO
              FOR FE := 0 TO Min MOD BITSIZE ( BitsetTyp ) - 1
              DO WBitset := Word . And ( WBitset , Word . Not ( Bits [ FE ] ) )
              END (* FOR *)
            END (* WITH *)
          END (* IF *)
        ; IF Max < Set . LastElmt
          THEN
            WITH WBitset = Set . BitsetPtr ^ [ LLastBitset ]
            DO
              FOR FE := Max MOD BITSIZE ( BitsetTyp ) + 1 
                  TO BITSIZE ( BitsetTyp ) - 1
              DO WBitset := Word . And ( WBitset , Word . Not ( Bits [ FE ] ) )
              END (* FOR *)
            END (* WITH *)
          END (* IF *)
        ; FOR FI := LLastBitset + 1 TO Set . LastElmt DIV BITSIZE ( BitsetTyp )
          DO Set . BitsetPtr ^ [ FI ] := 0
          END (* FOR *)
        ; Set . Card := UnknownCardinality
        END (* IF *) 
      END (* IF *) 
    END Project

; PROCEDURE Intersection ( VAR Set1 : tSet ; Set2 : tSet )

  = VAR N1 : CARDINAL 
  ; VAR N2 : CARDINAL 
  ; VAR NCommon : CARDINAL 

  ; BEGIN (* Intersection *)
      IF Set2 = NIL OR Set2 ^ . BitsetPtr = NIL 
      THEN (* Set2 is empty.  Make result empty too. *) 
        AssignEmpty ( Set1 ) 
      ELSIF Set1 = NIL OR Set1 ^ . BitsetPtr = NIL 
      THEN (* Set1 is empty and will remain unchanged. *) 
      ELSE 
        N1 := NUMBER ( Set1 ^ . BitsetPtr ^ )  
      ; N2 := NUMBER ( Set2 ^ . BitsetPtr ^ ) 
      ; NCommon := MIN ( N1 , N2 ) 
      ; FOR RI := 0 TO NCommon - 1 
        DO WITH WBitset1 = Set1 ^ . BitsetPtr ^ [ RI ]
          DO
            WBitset1 := Word . And ( WBitset1 , Set2 . BitsetPtr ^ [ RI ] )
          END (* WITH *)
        END (* FOR *)
      ; FOR RI := NCommon TO N1 - 1 
        DO Set1 ^ . BitsetPtr ^ [ RI ] := 0 
        END (* FOR *) 
      ; Set1 . Card := UnknownCardinality
      ; Set1 . FirstElmt := MAX ( Set1 . FirstElmt , Set2 . FirstElmt )
      ; Set1 . LastElmt := MIN ( Set1 . LastElmt , Set2 . LastElmt )
      END (* IF *) 
    END Intersection

; PROCEDURE SymDiff ( VAR Set1 : tSet ; Set2 : tSet )

  = BEGIN (* SymDiff *)
      FOR RI := 0 
          TO MIN ( NUMBER ( Set1 . BitsetPtr ^ )  
                 , NUMBER ( Set2 . BitsetPtr ^ ) 
                 ) 
             - 1 
      DO WITH WBitset1 = Set1 . BitsetPtr ^ [ RI ]
        DO
          WBitset1 := Word . Xor ( WBitset1 , Set2 . BitsetPtr ^ [ RI ] )
        END (* WITH *)
      END (* FOR *)
    ; Set1 . Card := UnknownCardinality
    ; Set1 . FirstElmt := MIN ( Set1 . FirstElmt , Set2 . FirstElmt )
    ; Set1 . LastElmt := MAX ( Set1 . LastElmt , Set2 . LastElmt )
    END SymDiff

; PROCEDURE Complement ( VAR Set : tSet )

  = VAR LN1 : CARDINAL 
  ; VAR LN2 : CARDINAL 
  ; VAR LOddBitCount : tElement
  ; VAR LBitsetPtr : RefArrayOfBitset  

  ; BEGIN (* Complement *)
      IF Set # NIL 
      THEN 
        LN2 := NeededBitsetCount ( Set ^ . MaxElmt ) 
      ; IF Set ^ . BitsetPtr = NIL 
        THEN (* Allocate all-new bitset. *) 
          LBitsetPtr := NewRefArrayOfBitset ( LN2 ) 
        ; FOR RI := 0 TO LN2 - 1 
          DO LBitsetPtr ^ [ RI ] := AllOnes  
          END (* FOR *) 
        ; Set ^ . BitsetPtr := LBitsetPtr 
        ; Set . FirstElmt := 0 
        ; Set . LastElmt := Set . MaxElmt
        ; Set . Card := Set . MaxElmt
        ELSE 
          LN1 := NUMBER ( Set . BitsetPtr ^ ) 
        ; LOddBitCount := Set . MaxElmt MOD BITSIZE ( BitsetTyp )
        ; IF LN1 < LN2 
          THEN (* Must expand to hold the result. *)  
            LBitsetPtr := NewRefArrayOfBitset ( LN2 ) 
          ; FOR RI := LN1 TO LN2 - 2 
            DO LBitsetPtr ^ [ RI ] := AllOnes  
            END (* FOR *) 
          ; Set ^ . BitsetPtr := LBitsetPtr 
          END (* IF *)  
(* FIX: Complete this. *) 
        ; FOR RI := 0 TO NUMBER ( Set . BitsetPtr ^ ) - 2
          DO WITH WBitset = Set . BitsetPtr ^ [ RI ]
            DO
              WBitset := Word . Not ( WBitset )
            END (* WITH *)
          END (* FOR *)
        ; IF LOddBitCount > 0
          THEN
            WITH WBitset 
                 = Set . BitsetPtr ^ [ NUMBER ( Set . BitsetPtr ^ ) - 1 ]
            DO
              WBitset
                := Word . And
                     ( Word . Minus ( Bits [ LOddBitCount ] , 1 )
                     , Word . Not ( WBitset )
                     )
            END (* WITH *)
          END (* IF *)
        ; IF Set . Card # UnknownCardinality
          THEN
            Set . Card := Set . MaxElmt + 1 - Set . Card
          END (* IF *)
        ; Set . FirstElmt := 0
        ; Set . LastElmt := Set . MaxElmt
        END (* IF *) 
      END (* IF *) 
    END Complement

; PROCEDURE Include ( VAR Set : tSet ; Elmt : tElement )

  = VAR LNeededBitsetCount : tElement 
  ; VAR LOldBitsetCount : tElement 
  ; VAR LNewBitsetPtr : RefArrayOfBitset
  ; VAR LNewBitset : BitsetTyp 

  ; BEGIN (* Include *)
      IF Set = NIL 
      THEN 
        MakeSet ( Set , Elmt + 1 ) 
      ; Set . FirstElmt := Elmt 
      ; Set . LastElmt := Elmt 
      ELSE 
        LNeededBitsetCount := NeededBitsetCount ( Elmt ) 
      ; IF Set . BitsetPtr = NIL 
        THEN 
          Set . BitsetPtr := NewRefArrayOfBitset ( LNeededBitsetCount )
        ; AssignEmpty ( Set )
        ; Set . FirstElmt := Elmt 
        ; Set . LastElmt := Elmt 
        ELSE
          LOldBitsetCount := NUMBER ( Set . BitsetPtr ^ )  
        ; IF LOldBitsetCount < LNeededBitsetCount 
          THEN (* Must expand Bitset array. *) 
            LNewBitsetPtr := NewRefArrayOfBitset ( LNeededBitsetCount ) 
          ; SUBARRAY ( LNewBitsetPtr ^ , 0 , LOldBitsetCount ) 
              := Set . BitsetPtr ^ 
          ; Set ^ . BitsetPtr := LNewBitsetPtr 
          END (* IF *) 
        ; Set . FirstElmt := MIN ( Set . FirstElmt , Elmt )
        ; Set . LastElmt := MAX ( Set . LastElmt , Elmt )
        END (* IF *) 
      END (* IF *) 
    ; WITH WBitset = Set . BitsetPtr ^ [ Elmt DIV BITSIZE ( BitsetTyp ) ]
      DO
        LNewBitset 
          := Word . Or ( WBitset , Bits [ Elmt MOD BITSIZE ( BitsetTyp ) ] )
      ; IF LNewBitset # WBitset 
        THEN 
          IF Set . Card # UnknownCardinality THEN INC ( Set . Card ) END 
        ; WBitset := LNewBitset
        END (* IF *) 
      END (* WITH *)
    END Include

; PROCEDURE Exclude ( VAR Set : tSet ; Elmt : tElement )

  = BEGIN (* Exclude *)
      WITH WBitset = Set . BitsetPtr ^ [ Elmt DIV BITSIZE ( BitsetTyp ) ]
      DO
        WBitset
          := Word . And
               ( WBitset
               , Word . Not ( Bits [ Elmt MOD BITSIZE ( BitsetTyp ) ] )
               )
      END (* WITH *)
    ; Set . Card := UnknownCardinality
    ; IF Elmt = Set . FirstElmt AND Elmt < Set . MaxElmt
      THEN
        INC ( Set . FirstElmt )
      END (* IF *)
    ; IF Elmt = Set . LastElmt AND Elmt > 0
      THEN
        DEC ( Set . LastElmt )
      END (* IF *)
    END Exclude

; PROCEDURE Card ( VAR Set : tSet ) : tElement

  = VAR LMin : tElement
  ; VAR LMax : tElement

  ; BEGIN (* Card *)
      IF Set . Card = UnknownCardinality
      THEN
        LMin := Minimum ( Set ) (* Just for side effect on FirstElmt *)
      ; LMax := Maximum ( Set ) (* Just for side effect on LastElmt *)
      ; Set . Card := 0
      ; FOR RI 
        := Set . FirstElmt
        TO Set . LastElmt
        DO IF IsElement ( RI , Set )
          THEN
            INC ( Set . Card )
          END (* IF *)
        END (* FOR *)
      END (* IF *)
    ; RETURN Set . Card
    END Card

; PROCEDURE Size ( VAR Set : tSet ) : tElement

  = BEGIN (* Size *)
      RETURN Set . MaxElmt
    END Size

; PROCEDURE Minimum ( VAR Set : tSet ) : tElement

  = VAR i : tElement

  ; BEGIN (* Minimum *)
      i := Set . FirstElmt
    ; WHILE i <= Set . LastElmt
      DO
        IF IsElement ( i , Set )
        THEN
          Set . FirstElmt := i 
        ; RETURN i
        END (* IF *)
      ; INC ( i )
      END (* WHILE *)
    ; RETURN 0
    END Minimum

; PROCEDURE Maximum ( VAR Set : tSet ) : tElement

  = VAR i : tInternalElmt

  ; BEGIN (* Maximum *)
      i := Set . LastElmt
    ; WHILE i >= Set . FirstElmt
      DO
        IF IsElement ( i , Set )
        THEN
          Set . LastElmt := i ; RETURN i
        END (* IF *)
      ; DEC ( i )
      END (* WHILE *)
    ; RETURN 0
    END Maximum

; PROCEDURE Select ( VAR Set : tSet ) : tElement

  = BEGIN (* Select *)
      RETURN Minimum ( Set )
    END Select

; PROCEDURE Extract ( VAR Set : tSet ) : tElement

  = VAR i : tElement

  ; BEGIN (* Extract *)
      i := Minimum ( Set ) 
    ; Exclude ( Set , i ) 
    ; RETURN i
    END Extract

; PROCEDURE IsSubset ( Set1 , Set2 : tSet ) : BOOLEAN

  = VAR i : tElement

  ; BEGIN (* IsSubset *)
      i := 0
    ; WHILE i < NUMBER ( Set1 . BitsetPtr ^ )
      DO
        IF Word . And
             ( Set1 . BitsetPtr ^ [ i ]
             , Word . Not ( Set2 . BitsetPtr ^ [ i ] )
             )
           # 0
        THEN
          RETURN FALSE
        END (* IF *)
      ; INC ( i )
      END (* WHILE *)
    ; RETURN TRUE
    END IsSubset

; PROCEDURE IsProperSubset ( Set1 , Set2 : tSet ) : BOOLEAN

  = BEGIN (* IsProperSubset *)
      RETURN IsSubset ( Set1 , Set2 ) AND IsNotEqual ( Set1 , Set2 )
    END IsProperSubset

; PROCEDURE IsEqual ( Set1 , Set2 : tSet ) : BOOLEAN

  = VAR LN1 : CARDINAL 
  ; VAR LN2 : CARDINAL 
  ; VAR LNTemp : CARDINAL 
  ; VAR LSetTemp : tSet 
  ; VAR i : tElement

  ; BEGIN (* IsEqual *)
      IF IsEmpty ( Set1 )  
      THEN RETURN IsEmpty ( Set2 ) 
      ELSIF IsEmpty ( Set2 ) 
      THEN RETURN FALSE 
      ELSE (* Both are nonempty => both have bitsets. *) 
        LN1 := NUMBER ( Set1 . BitsetPtr ^ ) 
      ; LN2 := NUMBER ( Set2 . BitsetPtr ^ ) 
      ; IF LN1 > LN2 
        THEN 
          LNTemp := LN1 
        ; LN1 := LN2 
        ; LN2 := LNTemp 
        ; LSetTemp := Set1
        ; Set1 := Set2
        ; Set2 := LSetTemp 
        END (* IF *) 
      ; i := 0
      ; WHILE i < LN1 
        DO
          IF Set1 . BitsetPtr ^ [ i ] # Set2 . BitsetPtr ^ [ i ]
          THEN
            RETURN FALSE
          END (* IF *)
        ; INC ( i )
        END (* WHILE *)
      ; WHILE i < LN2 
        DO 
          IF Set2 . BitsetPtr ^ [ i ] # 0 
          THEN
            RETURN FALSE
          END (* IF *)
        ; INC ( i )
        END (* WHILE *)
      ; RETURN TRUE
      END (* IF *) 
    END IsEqual

; PROCEDURE Hash ( Set : tSet ) : Word . T 

  = VAR LResult : Word . T 

  ; BEGIN 
      IF IsEmpty ( Set )  
      THEN RETURN 0  
      ELSE 
        LResult := 0 
      ; FOR RI := 0 TO NUMBER ( Set . BitsetPtr ^ ) - 1 
        DO 
          LResult 
            := Word . Xor 
                 ( LResult , Word . Rotate ( Set . BitsetPtr ^ [ RI ] , RI ) )
        END (* FOR *) 
      ; RETURN LResult 
      END (* IF *) 
    END Hash 

; PROCEDURE IsNotEqual ( Set1 , Set2 : tSet ) : BOOLEAN

  = BEGIN (* IsNotEqual *)
      RETURN NOT IsEqual ( Set1 , Set2 )
    END IsNotEqual

; PROCEDURE IsElement ( Elmt : tElement ; VAR Set : tSet ) : BOOLEAN

  = VAR LBitsetSs : CARDINAL 

  ; BEGIN (* IsElement *) 
      IF Set = NIL 
      THEN 
        RETURN FALSE 
      ELSIF Elmt < Set . FirstElmt 
            OR Elmt > Set . LastElmt 
            OR Set . BitsetPtr = NIL 
      THEN 
        RETURN FALSE 
      ELSE
        LBitsetSs := Elmt DIV BITSIZE ( BitsetTyp ) 
      ; IF LBitsetSs > LAST ( Set . BitsetPtr ^ ) 
        THEN 
          RETURN FALSE 
        ELSE 
          RETURN
            Word . And
              ( Set . BitsetPtr ^ [ LBitsetSs ]
              , Bits [ Elmt MOD BITSIZE ( BitsetTyp ) ]
              )
            # 0
        END (* IF *)
      END (* IF *)
    END IsElement

; PROCEDURE IsEmpty ( Set : tSet ) : BOOLEAN

  = VAR LMin : tElement
  ; VAR LMax : tElement
  ; VAR i : tElement

  ; BEGIN (* IsEmpty *)
      IF Set = NIL 
      THEN RETURN TRUE 
      ELSIF Set . FirstElmt <= Set . LastElmt
      THEN
        LMin := Set . FirstElmt DIV BITSIZE ( BitsetTyp )
      ; LMax := Set . LastElmt DIV BITSIZE ( BitsetTyp )
      ; i := LMin
      ; LOOP
          IF i > LMax
          THEN
            Set . FirstElmt := Set . LastElmt + 1 ; RETURN TRUE
          ELSIF Set . BitsetPtr ^ [ i ] # 0
          THEN
            RETURN FALSE
          ELSE
            Set . FirstElmt := i * BITSIZE ( BitsetTyp ) ; INC ( i )
          END (* IF *)
        END (* LOOP *)
      ELSE
        RETURN TRUE
      END (* IF *)
    END IsEmpty

; PROCEDURE ForAll ( Set : tSet ; Proc : ProcOftElementToBool ) : BOOLEAN
  RAISES ANY

  = BEGIN (* ForAll *)
      FOR i := Set . FirstElmt TO Set . LastElmt
      DO IF IsElement ( i , Set ) AND NOT Proc ( i )
        THEN
          RETURN FALSE
        END (* IF *)
      END (* FOR *)
    ; RETURN TRUE
    END ForAll

; PROCEDURE Exists ( Set : tSet ; Proc : ProcOftElementToBool ) : BOOLEAN
  RAISES ANY

  = BEGIN (* Exists *)
      FOR i := Set . FirstElmt TO Set . LastElmt
      DO IF IsElement ( i , Set ) AND Proc ( i )
        THEN
          RETURN TRUE
        END (* IF *)
      END (* FOR *)
    ; RETURN FALSE
    END Exists

; PROCEDURE Exists1 ( Set : tSet ; Proc : ProcOftElementToBool ) : BOOLEAN
  RAISES ANY

  = VAR n : tInternalElmt

  ; BEGIN (* Exists1 *)
      n := 0
    ; FOR i := Set . FirstElmt TO Set . LastElmt
      DO IF IsElement ( i , Set ) AND Proc ( i )
        THEN
          INC ( n )
        END (* IF *)
      END (* FOR *)
    ; RETURN n = 1
    END Exists1

; PROCEDURE Assign ( VAR Set1 : tSet ; Set2 : tSet )
    (* Set1 := Set2.  Set1 must be allocated and have the same size as Set2 *)

  = BEGIN (* Assign *)
      FOR i := 0 TO NUMBER ( Set1 . BitsetPtr ^ ) - 1
      DO Set1 . BitsetPtr ^ [ i ] := Set2 . BitsetPtr ^ [ i ]
      END (* FOR *)
    ; Set1 . Card := Set2 . Card
    ; Set1 . FirstElmt := Set2 . FirstElmt
    ; Set1 . LastElmt := Set2 . LastElmt
    ; Set1 . MaxElmt := Set2 . MaxElmt
    END Assign

; PROCEDURE Copy ( VAR Set1 : tSet ; Set2 : tSet )
  (* Set1 could be unallocated or the wrong max size, in which case,
     it will be (re)allocated with the same max size as Set2.
  *)

  = VAR LSet2MaxElmt : tElement 

  ; BEGIN (* Copy *)
(* TODO: Rationalize this whole module to fix the mess of mutable
         sets.
*) 
      IF Set2 = NIL 
      THEN Set1 := NIL 
      ELSE 
        LSet2MaxElmt := Set2 . MaxElmt 
      ; IF Set1 = NIL OR Set1 = Set2 OR Set1 . MaxElmt # LSet2MaxElmt
        THEN
          MakeSet ( Set1 , LSet2MaxElmt + 1 )
        ELSE 
        END (* IF *)
      ; Assign ( Set1 , Set2 )
      END (* IF *) 
    END Copy

; PROCEDURE AssignSingleton ( VAR Set : tSet ; Elmt : tElement )

  = BEGIN (* AssignSingleton *)
      AssignEmpty ( Set )
    ; Include ( Set , Elmt )
    ; Set . Card := 1
    ; Set . FirstElmt := Elmt
    ; Set . LastElmt := Elmt
    END AssignSingleton

; PROCEDURE AssignEmpty ( VAR Set : tSet )

  = BEGIN (* AssignEmpty *)
      IF Set # NIL 
      THEN
        IF Set . BitsetPtr # NIL 
        THEN  
          FOR i := 0 TO NUMBER ( Set . BitsetPtr ^ ) - 1
          DO Set . BitsetPtr ^ [ i ] := 0
          END (* FOR *)
        END (* IF *) 
      ; Set . Card := 0
      ; Set . FirstElmt := Set . MaxElmt
      ; Set . LastElmt := 0
      END (* IF *) 
    END AssignEmpty

; PROCEDURE ForAllDo ( Set : tSet ; Proc : ProcOftElement )
  RAISES ANY

  = BEGIN (* ForAllDo *)
      IF Set . BitsetPtr # NIL
      THEN
        FOR i := Set . FirstElmt TO Set . LastElmt
        DO IF IsElement ( i , Set )
          THEN
            Proc ( i )
          END (* IF *)
        END (* FOR *)
      END (* IF *)
    END ForAllDo

; PROCEDURE ReadSet ( Stream : Rd . T ; VAR Set : tSet )
  RAISES { Invalid , Rd . Failure , Thread . Alerted } 

  = VAR LElmt , card : tInternalElmt

  ; BEGIN (* ReadSet *)
      TRY 
        Lex . Skip ( Stream )
      ; Lex . Match ( Stream , "{" )
      ; AssignEmpty ( Set )
      ; card := 0
      ; LOOP
          Lex . Skip ( Stream )
        ; IF Rd . GetChar ( Stream ) = '}'
          THEN
            EXIT
          ELSE
            Rd . UnGetChar ( Stream )
          END (* IF *)
        ; LElmt := Lex . Int ( Stream )
        ; Include ( Set , LElmt )
        ; INC ( card )
        END (* LOOP *)
      ; Set . Card := card
      EXCEPT 
        Lex . Error 
      , Rd . EndOfFile 
      , FloatMode . Trap 
      => RAISE Invalid
      END (* TRY EXCEPT *) 
    END ReadSet

; PROCEDURE WriteSet ( Stream : Wr . T ; Set : tSet )
  RAISES { Wr . Failure , Thread . Alerted } 

  = PROCEDURE WriteElmt ( Elmt : tElement )

    = <* FATAL Wr . Failure *> 
      <* FATAL Thread . Alerted *> 
(* TODO: Propagate these out.  It requires generalizing ForAllDo to name them
         in its RAISES clause, which, for generality, should be just ANY,
         and that affects other uses of ForAllDo.  Note that WriteSet is 
         unused in Scheutz, as of 2006-12-18 *) 
      BEGIN (* WriteElmt *)
        Wr . PutChar ( Stream , ' ' )
      ; Wr . PutText ( Stream , Fmt . Int ( Elmt ) )
      END WriteElmt

  ; BEGIN (* WriteSet *)
      (* WriteS (f, "MaxElmt = "        ) ; WriteCard (f, MaxElmt        , 0); 
        WriteS (f, " Card = "          ) ; WriteCard (f, Card   , 0); 
        WriteS (f, " FirstElmt = "     ) ; WriteCard (f, FirstElmt , 0); 
        WriteS (f, " LastElmt = "      ) ; WriteCard (f, LastElmt       , 0); 
        WriteNl (f); 
     *)
      Wr . PutChar ( Stream , '{' )
    ; <* FATAL ANY *> (* Can't happen, WriteElmt *) 
      BEGIN ForAllDo ( Set , WriteElmt ) END (* Block *) 
    ; Wr . PutChar ( Stream , '}' )
    END WriteSet

; BEGIN (* Sets *)
    InitBits ( ) 
  END Sets
.
