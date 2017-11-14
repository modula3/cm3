(* ----------------------------------------------------------------------1- *)
(* File Main.m3 for Modula3 compiler test p032                              *)
(* Copyright 2017, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* Test passing of array parameters. *)

UNSAFE MODULE Main

; IMPORT Fmt , Stdio , Thread , Wr  

; TYPE Fixed = ARRAY [ 0 .. 3 ] OF INTEGER
; TYPE Open = ARRAY OF INTEGER

; CONST GInit = Fixed { 5 , 9 , 13 , 17 } 
; CONST GChanged = Fixed { 107 , 111 , 115 , 119 }

; VAR GTestCt : CARDINAL := 0 
; VAR GFailureCt : CARDINAL := 0 
; VAR GWrT : Wr . T := Stdio . stdout 

; PROCEDURE WL ( Msg : TEXT ) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      Wr . PutText ( GWrT , Msg ) 
    ; Wr . PutText ( GWrT , Wr . EOL ) 
    END WL

; PROCEDURE CheckAlias ( ShouldBe : BOOLEAN ; A1 , A2 : ADDRESS ; Msg : TEXT )
  = VAR Suffix : TEXT := "."
  ; BEGIN
      IF ( A1 = A2 ) # ShouldBe
      THEN
        Suffix := ", INCORRECT."
      ; INC ( GFailureCt ) 
      END
    ; IF A1 = A2
      THEN
        WL ( Msg & ", formals aliased" & Suffix )
      ELSE 
        WL ( Msg & ", formals not aliased" & Suffix )
      END (* IF *) 
    ; INC ( GTestCt ) 
    END CheckAlias

; PROCEDURE Check ( I : INTEGER ; Actual , Expected : INTEGER ; Msg : TEXT )
  = VAR Suffix : TEXT := "."
  ; BEGIN
      IF Expected # Actual
      THEN
        Suffix := ", INCORRECT."
      ; INC ( GFailureCt ) 
      END 
    ; WL ( Msg & ", subscript = " & Fmt . Int ( I )
           & ", actual = " & Fmt . Int ( Actual )
           & ", expected = " & Fmt . Int ( Expected ) 
           & Suffix 
         ) 
    ; INC ( GTestCt ) 
    END Check

(* Fixed arrays: *)

; PROCEDURE ValueFixed ( VALUE FFixed : Fixed ; VAR FAlias : Fixed )
    (* Assume FFixed and FAlias are passed the same actual.  
       Assume FFixed is passed GInit.
       FFixed should be unchanged (GInit) after changes to FAlias *)
  = CONST Label = "VALUE, fixed"
  ; BEGIN
      CheckAlias ( FALSE , ADR ( FFixed [ 0 ] ) , ADR ( FAlias [ 0 ] ) , Label ) 
    ; FOR I := FIRST ( Fixed ) TO LAST ( Fixed )
      DO
        Check ( I , FFixed [ I ] , GInit [ I ] , Label & ", initially" ) 
      END (* FOR *) 

    ; FOR I := FIRST ( Fixed ) TO LAST ( Fixed )
      DO
        FAlias [ I ] := GChanged [ I ] 
      END (* FOR *)
      
    ; FOR I := FIRST ( Fixed ) TO LAST ( Fixed )
      DO
        Check ( I , FFixed [ I ] , GInit [ I ] , Label & ", finally" ) 
      END (* FOR *) 
    END ValueFixed

; PROCEDURE VarFixed ( VAR FFixed : Fixed ; VAR FAlias : Fixed )
    (* Assume FFixed and FAlias are passed the same actual.  
       Assume FFixed is passed GInit.
       FFixed should reflect change to GChanged. *)
  = CONST Label = "VAR, fixed:" 
  ; BEGIN
      CheckAlias ( TRUE , ADR ( FFixed [ 0 ] ) , ADR ( FAlias [ 0 ] ) , Label ) 
    ; FOR I := FIRST ( Fixed ) TO LAST ( Fixed )
      DO
        Check ( I , FFixed [ I ] , GInit [ I ] , Label & ", initially" ) 
      END (* FOR *) 

    ; FOR I := FIRST ( Fixed ) TO LAST ( Fixed )
      DO
        FAlias [ I ] := GChanged [ I ] 
      END (* FOR *)
      
    ; FOR I := FIRST ( Fixed ) TO LAST ( Fixed )
      DO
        Check ( I , FFixed [ I ] , GChanged [ I ] , Label & ", finally" ) 
      END (* FOR *) 
    END VarFixed

; PROCEDURE ReadonlyFixed ( READONLY FFixed : Fixed ; VAR FAlias : Fixed )
    (* Assume FFixed and FAlias are passed the same actual.  
       Assume FFixed is passed GInit.
       FFixed should reflect change to GChanged. *)
  = CONST Label = "READONLY, fixed"
  ; BEGIN
      CheckAlias ( TRUE , ADR ( FFixed [ 0 ] ) , ADR ( FAlias [ 0 ] ) , Label ) 
    ; FOR I := FIRST ( Fixed ) TO LAST ( Fixed )
      DO
        Check ( I , FFixed [ I ] , GInit [ I ] , Label & ", initially" ) 
      END (* FOR *) 

    ; FOR I := FIRST ( Fixed ) TO LAST ( Fixed )
      DO
        FAlias [ I ] := GChanged [ I ] 
      END (* FOR *)
      
    ; FOR I := FIRST ( Fixed ) TO LAST ( Fixed )
      DO
        Check ( I , FFixed [ I ] , GChanged [ I ] , Label & ", finally" ) 
      END (* FOR *) 
    END ReadonlyFixed

; PROCEDURE WorkFixed ( )
  = VAR LVar : Fixed 
  ; BEGIN
    (* Illegal to pass GInit to FAlias. It must be designator. *) 

      WL ( "By VALUE, fixed: ")
    ; LVar := GInit 
    ; ValueFixed ( LVar , LVar )

    ; WL ( "By VAR, fixed:" )
    ; LVar := GInit 
    ; VarFixed ( LVar , LVar )

    ; WL ( "By READONLY, fixed:" )
    ; LVar := GInit 
    ; ReadonlyFixed ( LVar , LVar )

    END WorkFixed

(* Open arrays: *)

;   PROCEDURE ValueOpen ( <* NOWARN *> VALUE FOpen : Open ; VAR FAlias : Open )
    (* Assume FOpen and FAlias are passed the same actual.  
       Assume FOpen is passed a value equal to GInit.
       FOpen should be unchanged (GInit) after changes to FAlias *)
  = CONST Label = "VALUE, open"
  ; BEGIN
      CheckAlias ( FALSE , ADR ( FOpen [ 0 ] ) , ADR ( FAlias [ 0 ] ) , Label ) 
    ; FOR I := FIRST ( FOpen ) TO LAST ( FOpen )
      DO
        Check ( I , FOpen [ I ] , GInit [ I ] , Label & ", initially" ) 
      END (* FOR *) 

    ; FOR I := FIRST ( FOpen ) TO LAST ( FOpen )
      DO
        FAlias [ I ] := GChanged [ I ] 
      END (* FOR *)
      
    ; FOR I := FIRST ( FOpen ) TO LAST ( FOpen )
      DO
        Check ( I , FOpen [ I ] , GInit [ I ] , Label & ", finally" ) 
      END (* FOR *) 
    END ValueOpen

; PROCEDURE VarOpen ( VAR FOpen : Open ; VAR FAlias : Open )
    (* Assume FOpen and FAlias are passed the same actual.  
       Assume FOpen is passed a value equal to GInit.
       FOpen should reflect change to GChanged. *)
  = CONST Label = "VAR, open:" 
  ; BEGIN
      CheckAlias ( TRUE , ADR ( FOpen [ 0 ] ) , ADR ( FAlias [ 0 ] ) , Label ) 
    ; FOR I := FIRST ( FOpen ) TO LAST ( FOpen )
      DO
        Check ( I , FOpen [ I ] , GInit [ I ] , Label & ", initially" ) 
      END (* FOR *) 

    ; FOR I := FIRST ( FOpen ) TO LAST ( FOpen )
      DO
        FAlias [ I ] := GChanged [ I ] 
      END (* FOR *)
      
    ; FOR I := FIRST ( FOpen ) TO LAST ( FOpen )
      DO
        Check ( I , FOpen [ I ] , GChanged [ I ] , Label & ", finally" ) 
      END (* FOR *) 
    END VarOpen

; PROCEDURE ReadonlyOpen ( READONLY FOpen : Open ; VAR FAlias : Open )
    (* Assume FOpen and FAlias are passed the same actual.  
       Assume FOpen is passed a value equal to GInit.
       FOpen should reflect change to GChanged. *)
  = CONST Label = "READONLY, open"
  ; BEGIN
      CheckAlias ( TRUE , ADR ( FOpen [ 0 ] ) , ADR ( FAlias [ 0 ] ) , Label ) 
    ; FOR I := FIRST ( FOpen ) TO LAST ( FOpen )
      DO
        Check ( I , FOpen [ I ] , GInit [ I ] , Label & ", initially" ) 
      END (* FOR *) 

    ; FOR I := FIRST ( FOpen ) TO LAST ( FOpen )
      DO
        FAlias [ I ] := GChanged [ I ] 
      END (* FOR *)
      
    ; FOR I := FIRST ( FOpen ) TO LAST ( FOpen )
      DO
        Check ( I , FOpen [ I ] , GChanged [ I ] , Label & ", finally" ) 
      END (* FOR *) 
    END ReadonlyOpen

; PROCEDURE ReadonlyOpenNondesig ( READONLY FOpen : Open )
    (* Assume FOpen is passed a value equal to GInit. *)
  = CONST Label = "READONLY, open, nondesignator"
  ; BEGIN
      FOR I := FIRST ( FOpen ) TO LAST ( FOpen )
      DO
        Check ( I , FOpen [ I ] , GInit [ I ] , Label & ", initially" ) 
      END (* FOR *) 

    END ReadonlyOpenNondesig

; PROCEDURE NondesigFixed ( VAR FFixed : Fixed ) : Fixed
  = BEGIN
      RETURN FFixed
    END NondesigFixed 

; PROCEDURE NondesigOpen ( VAR FOpen : Open ) : Fixed
  = BEGIN
      RETURN FOpen
    END NondesigOpen 

; PROCEDURE WorkOpen ( )
  = VAR LVar : Fixed 
  ; BEGIN
    (* Illegal to pass GInit to FAlias. It must be designator. *) 

      WL ( "By VALUE, open: ")
    ; LVar := GInit 
    ; ValueOpen ( LVar , LVar )

    ; WL ( "By VAR, open:" )
    ; LVar := GInit 
    ; VarOpen ( LVar , LVar )

    ; WL ( "By READONLY, open:" )
    ; LVar := GInit 
    ; ReadonlyOpen ( LVar , LVar )

    ; WL ( "Array constructor nondesignator: ")
    ; ReadonlyOpenNondesig ( GInit )

    ; WL ( "Fixed function result nondesignator: ")
    ; LVar := GInit 
    ; ReadonlyOpenNondesig ( NondesigFixed ( LVar ) )

    ; WL ( "Open function result nondesignator: ")
    ; LVar := GInit 
    ; ReadonlyOpenNondesig ( NondesigOpen ( LVar ) )

    END WorkOpen

; BEGIN
    GTestCt := 0
  ; GFailureCt := 0
  ; WL ( "Test of array parameters." )
  ; WorkFixed ( )
  ; WorkOpen ( )
  ; WL ( Fmt . Int ( GTestCt ) & " tests performed." )
  ; IF GFailureCt = 0
    THEN
      WL ( "All tests succeeded." )
    ; WL ( "SUCCESS." )
    ELSE
      WL ( Fmt . Int ( GFailureCt ) & " tests FAILED." )
    ; WL ( "################### FAILURE #####################" )
    END (* IF *) 
  END Main
. 
