(* ----------------------------------------------------------------------1- *)
(* File SegList.m3 for Modula3 compiler test p269                           *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

UNSAFE MODULE SegList 

; PROCEDURE Compare ( a , b : T ) : [ - 1 .. 1 ]
  = BEGIN
      IF a . ConstHi < b . ConstHi THEN RETURN - 1  
      ELSIF a . ConstHi > b . ConstHi THEN RETURN 1
      ELSE RETURN 0
      END (*IF*) 
    END Compare 

; BEGIN
  END SegList
.



