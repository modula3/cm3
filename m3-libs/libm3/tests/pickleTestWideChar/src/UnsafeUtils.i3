(* -----------------------------------------------------------------------1- *)
(* File UnsafeUtils.i3 Modula-3 source code.                                 *)
(* Copyright 2010 .. 2016, Rodney M. Bates.                                  *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *) 
(* -----------------------------------------------------------------------2- *)

UNSAFE INTERFACE UnsafeUtils 

; IMPORT TextLiteral 

; PROCEDURE PatchTextLit ( T : TextLiteral . T ) 
  (* Very sleazy. We are mutating a sormally immutable text literal.  Probably
     all occurences of this literal value in its compilation will be using the
     mutated copy.  
     If WIDECHAR is unicode-sized, patch any occurences
     of the Unicode substition code U+FFFD to U+10FFFF
  *)   

; END UnsafeUtils
.
 


