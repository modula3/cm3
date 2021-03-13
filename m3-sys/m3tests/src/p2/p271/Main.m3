UNSAFE MODULE Main
(* Test that a global constant, passed READONLY is
   passed by value, since it is not a designator. *)
(* Can't take ADR of a constant, because it is not a
   designator.  But it seems safe that, if the compiler
   ever makes a copy, it would not make the same copy
   in multiple places.
*) 
; IMPORT Stdio , Wr

; TYPE T = ARRAY OF INTEGER 
; CONST C = T { 17 , 29 , 107 } 
(*
; CONST D = SUBARRAY ( C , 1 , 2 ) (* "value is not constant" *) 
*)
; VAR WrT : Wr . T := Stdio . stdout

; PROCEDURE P ( READONLY F1 : T ; READONLY F2 : T := C )
  = BEGIN
      IF ADR ( F1 ) # ADR ( F2 )
      THEN
        Wr . PutText ( WrT , "OK" )
      ELSE
        Wr . PutText ( WrT , "BAD" )
      END
    ; Wr . PutText ( WrT , Wr . EOL )
    END P

; PROCEDURE Q ( READONLY F3 : T )
  = BEGIN
      P ( F3 ) 
    END Q 

; BEGIN
    Q ( C ) 
  END Main
. 
