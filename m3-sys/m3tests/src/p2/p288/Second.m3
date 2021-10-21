(* ----------------------------------------------------------------------1- *)
(* File Second.m3 for Modula3 compiler test p288                            *)
(* Copyright 2021, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

MODULE Second

 EXPORTS Second , Named 

; IMPORT Display 
; IMPORT Named 

(* Defaults named in imported interface: *)
; TYPE Imp = RECORD
    A := Named . ImpA 
  ; R := Named . ImpR  
  ; S := Named . ImpS 
  END

; VAR GImp : Imp 
; PROCEDURE ImpP ( )
  = VAR LImp : Imp
  ; BEGIN
      Display . PutLine
        ( "Defaults named in interface imported by First.m3, second time:" )
    ; Display . PutLine ( "  Global variable:" )
    ; Display . Rec ( GImp ) 
    ; Display . PutLine ( "  Local variable:" )
    ; Display . Rec ( LImp )
    END ImpP

(* Defaults named in exported interface: *)
; TYPE Exp = RECORD
    A := ExpA
  ; R := ExpR 
  ; S := ExpS
  END

; VAR GExp : Exp 
; PROCEDURE ExpP ( )
  = VAR LExp : Exp
  ; BEGIN
      Display . PutLine
        ( "Defaults named in interface exported by First.m3, second time:" ) 
    ; Display . PutLine ( "  Global variable:" )
    ; Display . Rec ( GExp ) 
    ; Display . PutLine ( "  Local variable:" )
    ; Display . Rec ( LExp ) 
    END ExpP

; PROCEDURE Work ( )
  = BEGIN
      ImpP ( )
    ; ExpP ( ) 
    END Work 

; BEGIN
  END Second
.



