(* ----------------------------------------------------------------------1- *)
(* File Common.m3 for Modula3 compiler test p269                            *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

MODULE Common

; IMPORT Fmt 
; IMPORT Text 

(* EXPORTED: *) 
; PROCEDURE RepTypImage ( Value : RepTyp ) : TEXT
  = BEGIN
      CASE Value OF
      | RepTyp . Fixed => RETURN "Fixed" 
      | RepTyp . OpenContig => RETURN "OpenContig" 
      | RepTyp . OpenRemote => RETURN "OpenRemote"
      END
    END RepTypImage

(* EXPORTED: *) 
; PROCEDURE RepSetTypImage ( Reps : RepSetTyp ) : TEXT 
  = VAR Result : TEXT
  ; VAR First , Last : RepTyp
  ; VAR DisplayedCt : INTEGER 
  ; BEGIN
      DisplayedCt := 0 
    ; Result := "{"
    ; First := FIRST ( RepTyp ) 
    ; Last := LAST ( RepTyp )
    ; IF Last >= First
      THEN
        IF First IN Reps
        THEN
          Result := Result & RepTypImage ( First )
        ; INC ( DisplayedCt ) 
        END (*IF*) 
      ; FOR RE := VAL ( ORD ( First ) + 1 , RepTyp ) TO Last 
        DO IF RE IN Reps 
          THEN
            IF DisplayedCt > 0 THEN Result := Result & ", " END
          ; Result := Result & RepTypImage ( RE ) 
          ; INC ( DisplayedCt ) 
          END
        END
      END 
    ; RETURN Result & "}" 
    END RepSetTypImage

; PROCEDURE RepSetTypDescr ( Reps : RepSetTyp ) : TEXT 
  = VAR Result : TEXT
  ; VAR SingletonVal : RepTyp
  ; VAR EltCt : INTEGER
  ; BEGIN
      EltCt := 0
    ; FOR RE := FIRST ( RepTyp ) TO LAST ( RepTyp )  
      DO IF RE IN Reps
        THEN
          SingletonVal := RE 
        ; INC ( EltCt )
        END (*IF*)
      END (*FOR*)
    ; IF EltCt = 0 THEN Result := "{}"
      ELSIF EltCt = 1 THEN Result := RepTypImage ( SingletonVal )
      ELSE Result := "one of " & RepSetTypImage ( Reps )
      END (*IF*)
    ; RETURN Result 
    END RepSetTypDescr

; PROCEDURE SubsImage ( Subs : FullSubsTyp ; TopDepth : INTEGER ) : TEXT
  = VAR Result : TEXT

  ; BEGIN
      Result := "["
    ; IF TopDepth > 0
      THEN
        Result := Result & Fmt . Int ( Subs [ TopDepth ] )
      ; FOR RI := TopDepth - 1 TO 1 BY - 1 <*NOWARN*>
        (* This will sometimes have an empty range. *)
        DO
          Result := Result & "," & Fmt . Int ( Subs [ RI ] )
        END (*FOR*) 
      END (*IF*)
    ; Result := Result & "]"
    ; RETURN Result 
    END SubsImage 

(* List of module names that have knowable beginning address of their global
   constant area. *)
; CONST StringCt = 1000 (* No real need to make this expandable. *) 
; TYPE StringListTyp = ARRAY [ 0 .. StringCt ] OF TEXT

; VAR GModListCt : INTEGER := 0 
; VAR GModList : StringListTyp

(* EXPORTED: *)
; PROCEDURE NoteModName ( Name : TEXT )
  = BEGIN
      GModList [ GModListCt ] := Name
    ; INC ( GModListCt ) 
    END NoteModName 

(* EXPORTED: *)
; PROCEDURE IsInteresting ( ModName : TEXT ) : BOOLEAN
  = VAR LModNo : INTEGER
  ; BEGIN 
      IF ModName = NIL
      THEN RETURN FALSE
   (* ELSIF Text . Equal ( ModName , "ConstA4PA4PC4_M3" ) 
      THEN RETURN TRUE *) 
      ELSE
        LModNo := 0
      ; LOOP
          IF LModNo = GModListCt THEN EXIT END
        ; IF Text . Equal ( GModList [ LModNo ] , ModName )
          THEN RETURN TRUE
          END
        ; INC ( LModNo ) 
        END (*LOOP*) 
      ; RETURN FALSE
      END (*IF*) 
    END IsInteresting

(* EXPORTED: *)
; PROCEDURE IntProduct ( READONLY Vals : ARRAY OF INTEGER ) : INTEGER

  = VAR LResult : INTEGER
  
  ; BEGIN
      LResult := 1 
    ; FOR RI := FIRST ( Vals ) TO LAST ( Vals )
      DO LResult := LResult * Vals [ RI ] 
      END (*FOR*)
    ; RETURN LResult 
    END IntProduct 
    
; BEGIN
    GModListCt := 0 
  END Common
.


