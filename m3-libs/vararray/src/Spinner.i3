
(* -----------------------------------------------------------------------1- *)
(* File Spinner.i3                                                           *)
(* Modula-3 source code.                                                     *)
(* Copyright 2012, Rodney M. Bates.                                          *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the Gnu Public License, version 2 or later.                *)
(* -----------------------------------------------------------------------2- *)

(* A command-line progress display, written to stderr. *)

(* Pass in counts of "ticks", any unit of progess you like.  
   Output changes every "spin", which is SpinCt ticks.  
   Output looks like this: 

       0....+....1....+....2....+....3....+....4....+....|
    5000....+....6....+....7....+....8....+....9....+....|
   10000....+....1....+....2....+....3....+....4....+....|
   15000....+....1....+....2....+.../
   15804  
 
   The characters of each line appear as spins happen.  
   The left column is actual tick count at beginning of line. 
   One line per 500 spins.  One digit per 100 spins.
   One '+' per odd 50 spins.  One '.' per 10 spins. 
   The spinner at the end of line rotates 1/8 turn every spin. 

*)  

INTERFACE Spinner 

; VAR SpinCt : CARDINAL := 1000 
  (* The visible spinner at the end of a progress line will rotate one notch
     only this many ticks.  Make it a power of 10, and don't change it while
     displaying progress.  
  *) 

; PROCEDURE ResetProgress ( ) 
  (* Start a new line of progress.  *) 

; PROCEDURE NoteProgress 
    ( VAR (* IN OUT *) Ticks : INTEGER ; Incr : CARDINAL := 1 ) 
  (* Call this to increment the count of whatever measure of progress.
     Don't increment your variable any other way. 
  *) 

; PROCEDURE ProgressLineIsEmpty ( ) : BOOLEAN 
  (* If you interrupt the progress display with other output, you probably
     want to emit a new line first, if this returns FALSE.
  *) 
 
; PROCEDURE RedisplayProgress ( ) 
  (* After you interrupt the progress display with other output, emit a new
     line, then call this to get the progress line redisplayed. 
  *) 
 
; PROCEDURE ShowExactProgress ( Ticks : INTEGER ) 
  (* At the end, call this to display a final count. *) 

; END Spinner 
. 
