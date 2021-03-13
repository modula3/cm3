(* ----------------------------------------------------------------------1- *)
(* File Common.m3 for Modula3 compiler test p280                            *)
(* Copyright 2020, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

UNSAFE MODULE Common

; IMPORT Fmt 
; IMPORT Stdio
; IMPORT Thread 
; IMPORT Wr  

; VAR GTestCt : INTEGER 
; VAR GFailureCt : INTEGER
; VAR WrT : Wr . T

; PROCEDURE Init ( )
  = BEGIN
      GTestCt := 0
    ; GFailureCt := 0
    ; WrT := Stdio . stdout 
    END Init 

; PROCEDURE Case ( )
  = BEGIN
      INC ( GTestCt ) 
    END Case 

; PROCEDURE Failure ( Msg : TEXT )
  = BEGIN
      Message ( Msg ) 
    ; INC ( GFailureCt ) 
    END Failure

; PROCEDURE Message ( Msg : TEXT )
  = <* FATAL Thread . Alerted , Wr . Failure *> BEGIN
      Wr . PutText ( WrT , Msg ) 
    ; Wr . PutText ( WrT , Wr . EOL )
    ; Wr . Flush ( WrT ) 
    END Message 

; PROCEDURE Report ( )
   = <* FATAL Thread . Alerted , Wr . Failure *> BEGIN
       Wr . PutText ( WrT , Fmt . Int ( GTestCt ) )
     ; Wr . PutText ( WrT , " cases tested." )
     ; Wr . PutText ( WrT , Wr.EOL )
     ; IF GFailureCt = 0
       THEN
         Wr . PutText ( WrT , "All succeeded." )
       ; Wr . PutText ( WrT , Wr.EOL )
       ELSE
         Wr . PutText ( WrT , Fmt . Int ( GFailureCt ) )
       ; Wr . PutText ( WrT , " of them FAILED." )
       ; Wr . PutText ( WrT , Wr.EOL )
       ; Wr . PutText ( WrT , "Overall test FAILED." )
       ; Wr . PutText ( WrT , Wr.EOL )
       END (* IF *)
     END Report
     
; PROCEDURE ArrayImage1D ( READONLY Val : Array1DOpen ) : TEXT 
  = VAR Result : TEXT
  ; BEGIN
      Result := "{"
    ; FOR RI := FIRST ( Val ) TO LAST ( Val )
      DO
        IF RI > 0 THEN Result := Result & "," END 
      ; Result := Result & Fmt . Int ( Val [ RI ] ) 
      END (*FOR*)
    ; Result := Result & "}" 
    ; RETURN Result 
    END ArrayImage1D 
    
; PROCEDURE ArrayImage2D ( READONLY Val : Array2DOpen ) : TEXT 
  = VAR Result : TEXT
  ; BEGIN
      Result := "{"
    ; FOR RI := FIRST ( Val ) TO LAST ( Val )
      DO
        IF RI > 0 THEN Result := Result & "," END 
      ; Result := Result & ArrayImage1D ( Val [ RI ] )  
      END (*FOR*)
    ; Result := Result & "}" 
    ; RETURN Result 
    END ArrayImage2D
    
; PROCEDURE ArrayImage3D ( READONLY Val : Array3DOpen ) : TEXT 
  = VAR Result : TEXT
  ; BEGIN
      Result := "{"
    ; FOR RI := FIRST ( Val ) TO LAST ( Val )
      DO
        IF RI > 0 THEN Result := Result & "," END 
      ; Result := Result & ArrayImage2D ( Val [ RI ] )  
      END (*FOR*)
    ; Result := Result & "}" 
    ; RETURN Result 
    END ArrayImage3D
    
; PROCEDURE Equal3D ( READONLY Left , Right : Array3DOpen ) : BOOLEAN 
  = VAR Result : BOOLEAN
  ;  BEGIN
      Result := Left = Right
    ; RETURN Result 
    END Equal3D

; TYPE IntRefTyp = UNTRACED REF INTEGER
; TYPE AddrRefTyp = UNTRACED REF ADDRESS 
; VAR InitialVal : INTEGER := Array3DVal [ 0 , 0 , 0 ] 

(* The ADR function, in the compiler and in m3gdb seems to be returning
   the address of the dope, when applied to OpenArrayVARFormal[0].  This 
   function attempts to convert a dope address to an elements address.
   It is not general, and uses inside information on the lengths and
   values used in arrays in this test program.
*)

; VAR InformStrippingDope := FALSE

; PROCEDURE StripDope ( Value : ADDRESS ; ExpLen : INTEGER ; Label : TEXT )
  : ADDRESS 
  = VAR IntRef : IntRefTyp
  ; VAR AddrRef : AddrRefTyp 
  ; BEGIN
      IntRef := LOOPHOLE ( Value , IntRefTyp )
    ; IF IntRef ^ = InitialVal
      THEN (* Looks like the zero-th element value.  Assume no dope. *)
        RETURN Value
      ELSE
        IntRef := LOOPHOLE ( Value + BYTESIZE ( ADDRESS ) , IntRefTyp )
      ; IF IntRef ^ = ExpLen 
        THEN (* Looks like a length field of dope.
                Use the dope's elements pointer. *)
          AddrRef := LOOPHOLE ( Value , AddrRefTyp )
        ; IF InformStrippingDope
          THEN
            Message ( Label & " stripping dope." ) 
          END 
        ; RETURN AddrRef ^
        ELSE (* Give up. *) 
          RETURN NIL 
        END (*IF*) 
      END (*IF*)
    END StripDope 

; BEGIN
    Init ( ) 
  END Common
.

