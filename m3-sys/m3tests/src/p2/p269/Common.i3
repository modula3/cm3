(* ----------------------------------------------------------------------1- *)
(* File Common.i3 for Modula3 compiler test p269                            *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

INTERFACE Common

; TYPE MarkerTyp = ARRAY [ 0 .. 5 ] OF CHAR 

; CONST BegOfConstMarker
    = MarkerTyp { 'M' , 'a' , 'r' , 'k'  , 'e' , 'r' }
  (* An module that contains an actual use of this will have
     its compiled data at the beginning of its global constant
     area, and a pointer thereto in its global variable area,
     immediately following the RT0.ModuleInfo record.  Test
     code uses this to find beginnings of global constant areas
     of relevant modules. *) 

; TYPE RepTyp
  = { Fixed , OpenContig , OpenRemote } 

; TYPE RepSetTyp = SET OF RepTyp

; PROCEDURE RepTypImage ( Value : RepTyp ) : TEXT

; PROCEDURE RepSetTypImage ( Reps : RepSetTyp ) : TEXT
  (* A set image. *) 

; PROCEDURE RepSetTypDescr ( Reps : RepSetTyp ) : TEXT
  (* To display in a message. *) 

; TYPE FullSubsTyp = ARRAY [ 1 .. 6 ] OF INTEGER
  (* An array of subscript values for various dimensions. Unlike shapes,
     Unlike shapes, this is inside-out, i.e., Subs [ 1 ] is for the
     innermost dimension. *) 

; PROCEDURE SubsImage ( Subs : FullSubsTyp ; TopDepth : INTEGER ) : TEXT

; PROCEDURE NoteModName ( Name : TEXT )

; PROCEDURE IsInteresting ( ModName : TEXT ) : BOOLEAN

; PROCEDURE IntProduct ( READONLY Vals : ARRAY OF INTEGER ) : INTEGER

; END Common
.


