(* ----------------------------------------------------------------------1- *)
(* File Support.m3 for Modula3 compiler test p012                           *)
(* Copyright 2017, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* Just to get distracting stuff out of the main program, in case
   its IRs need to be examined.
*) 

MODULE Support

; IMPORT Fmt , Stdio , Thread , Wr

; PROCEDURE WT ( TextVal : TEXT )
  (* Write TextVal on Stream.  TextVal = NIL is OK. *) 

  = <* FATAL Thread . Alerted , Wr . Failure *> BEGIN
      IF TextVal # NIL THEN
        Wr . PutText ( Stream , TextVal )
      END 
    END WT 

; PROCEDURE WI ( IntVal : INTEGER )
  (* Format and write IntVal on Stream. *) 

  = BEGIN
      WT ( Fmt . Int ( IntVal ) )  
    END WI 

; PROCEDURE WEOL ( )
  (* Write end-of-line on Stream. *) 

  = BEGIN
      WT ( Wr . EOL )  
    END WEOL

; BEGIN
    Stream := Stdio . stdout 
  END Support
.




