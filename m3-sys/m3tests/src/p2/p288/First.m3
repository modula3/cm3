(* ----------------------------------------------------------------------1- *)
(* File First.m3 for Modula3 compiler test p288                             *)
(* Copyright 2021, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

MODULE First

  EXPORTS First , Named 
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
        ( "Defaults named in interface imported by First.m3:" )
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
        ( "Defaults named in interface exported by First.m3:" ) 
    ; Display . PutLine ( "  Global variable:" )
    ; Display . Rec ( GExp ) 
    ; Display . PutLine ( "  Local variable:" )
    ; Display . Rec ( LExp ) 
    END ExpP

(* Defaults named in self module: *)
; CONST ModA = ARRAY [ 0 .. 2 ] OF INTEGER { 1 , 14 , 27 }
; CONST ModR = RECORD Fld0 , Fld1 , Fld2 : INTEGER END { 2 , 15 , 28 }
; CONST ModS = SET OF Display . Rng { 20 , 100 , 132 } 

; TYPE Mod = RECORD
    A := ModA
  ; R := ModR 
  ; S := ModS
  END


; VAR GMod : Mod 
; PROCEDURE ModP ( )
  = VAR LMod : Mod
  ; BEGIN
      Display . PutLine
        ( "Defaults named locally in module Named:" ) 
    ; Display . PutLine ( "  Global variable:" )
    ; Display . Rec ( GMod ) 
    ; Display . PutLine ( "  Local variable:" )
    ; Display . Rec ( LMod ) 
    END ModP 

(* Defaults anonymous in record fields. *)
; TYPE Anon = RECORD
    A := ARRAY [ 0 .. 2 ] OF INTEGER { 1 , 10 , 19 }
  ; R := RECORD Fld0 , Fld1 , Fld2 : INTEGER END { 2 , 11 , 20 }
  ; S := SET OF Display . Rng { 17 , 97 , 129 } 
  END 

; VAR GAnon : Anon 
; PROCEDURE AnonP ( )
  = VAR LAnon : Anon
  ; BEGIN
      Display . PutLine
        ( "Defaults anonymous in Named.m3:" ) 
    ; Display . PutLine ( "  Global variable:" )
    ; Display . Rec ( GAnon ) 
    ; Display . PutLine ( "  Local variable:" )
    ; Display . Rec ( LAnon ) 
    END AnonP 

(* Defaults named in same interface as the fields. *)
; VAR GSelfI : Named . Intf 
; PROCEDURE SelfP ( )
  = VAR LSelfI : Named . Intf
  ; BEGIN
      Display . PutLine
        ( "Defaults named locally in interface Named:" ) 
    ; Display . PutLine ( "  Global variable:" )
    ; Display . Rec ( GSelfI ) 
    ; Display . PutLine ( "  Local variable:" )
    ; Display . Rec ( LSelfI ) 
    END SelfP

; VAR GSelfI2 : Named . Intf2 
; PROCEDURE SelfP2 ( )
  = VAR LSelfI2 : Named . Intf2
  ; BEGIN
      Display . PutLine
        ( "Defaults named in interface Named, 2nd time:" ) 
    ; Display . PutLine ( "  Global variable:" )
    ; Display . Rec ( GSelfI2 ) 
    ; Display . PutLine ( "  Local variable:" )
    ; Display . Rec ( LSelfI2 ) 
    END SelfP2

; PROCEDURE Work ( )
  = BEGIN
      ImpP ( )
    ; ExpP ( ) 
    ; ModP ( ) 
    ; AnonP ( ) 
    ; SelfP ( ) 
    ; SelfP2 ( )
    END Work 

; BEGIN
  END First
.
