(* Copyright (C) 1989, 1994 Digital Equipment Corporation                    *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Aug 11 12:50:19 PDT 1995 by detlefs                  *)
(*      modified on Fri Sep 23 09:32:13 PDT 1994 by heydon                   *)
(*      modified on Fri Mar 25 13:15:32 PST 1994 by kalsow                   *)
(*      modified on Tue Mar  3 18:37:31 PST 1992 by muller                   *)

INTERFACE TextF;

IMPORT Text;

TYPE CharArray = ARRAY OF CHAR;

REVEAL
  (* Remember, the report says that TEXT is predeclared and <: REFANY;
     just pretend that we have 
         TYPE
           TEXT <: REFANY
     in the Text interface. *)

  (* The array contains the characters of the text followed by a '\000' *)
 
  TEXT = BRANDED Text.Brand REF ARRAY OF CHAR;

PROCEDURE New (n: CARDINAL): TEXT RAISES {};
  (* create a new text capable of holding n characters. Note that its actual
     length as an array is n+1 because of the null termination.
     The characters at positions [0..n-1] are left undefined. The character
     at position n is '\000' *)

END TextF.
