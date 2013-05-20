
(* -----------------------------------------------------------------------1- *)
(* File Spinner.m3                                                           *)
(* Modula-3 source code.                                                     *)
(* Copyright 2012, Rodney M. Bates.                                          *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the Gnu Public License, version 2 or later.                *)
(* -----------------------------------------------------------------------2- *)

MODULE Spinner 

; IMPORT Fmt
; IMPORT Text
; IMPORT Thread
; IMPORT Stdio  
; IMPORT Wr 

; VAR PWrT : Wr . T 

; CONST TestCtPad = 8 

; CONST GLen = 80 
; VAR GProgressLine : ARRAY [ 0 .. GLen ] OF CHAR 
; VAR GProgressLen : CARDINAL := 0 

(* VISIBLE: *) 
; PROCEDURE ResetProgress ( ) 
  (* Start a new line of progress.  *) 

  = BEGIN 
      GProgressLen := 0
    END ResetProgress 

; PROCEDURE AppendProgress ( Val : TEXT ) 

  = VAR LLen : CARDINAL 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN
      LLen := Text . Length ( Val ) 
    ; IF GProgressLen > 0 
         AND GProgressLine [ GProgressLen - 1 ] = '\010' 
      THEN 
        DEC ( GProgressLen , 2 ) 
      END (* IF *) 
    ; Text . SetChars 
        ( SUBARRAY ( GProgressLine , GProgressLen , LLen ) , Val ) 
    ; INC ( GProgressLen , LLen ) 
    ; Wr . PutText ( PWrT , Val ) 
    ; Wr . Flush ( PWrT )    
    END AppendProgress 

(* VISIBLE: *) 
; PROCEDURE NoteProgress 
    ( VAR (* IN OUT *) Ticks : INTEGER ; Incr : CARDINAL := 1 ) 
  (* Call this to increment the count of whatever measure of progress.
     Don't increment your variable any other way. 
  *) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      LOOP 
        IF Incr = 0 
        THEN EXIT 
        ELSE 
          IF Ticks MOD ( 500 * SpinCt ) = 0 
          THEN (* New line. *) 
            ResetProgress ( ) 
          ; Wr . PutText ( PWrT , Wr . EOL ) 
          ; AppendProgress ( Fmt . Pad ( Fmt . Int ( Ticks ) , TestCtPad ) ) 
          ELSIF Ticks MOD ( 100 * SpinCt ) = 0 
          THEN 
            AppendProgress 
              ( Fmt . Int ( ( Ticks DIV ( 100 * SpinCt ) ) MOD 10 ) ) 
          ELSIF Ticks MOD ( 50 * SpinCt ) = 0 
          THEN 
            AppendProgress ( "+" ) 
          ELSIF Ticks MOD ( 10 * SpinCt ) = 0 
          THEN 
            AppendProgress ( "." )
          END (* IF *)
        ; CASE ( Ticks DIV SpinCt ) MOD 4 <* NOWARN *>  
          OF 0 => AppendProgress ( "/\010" )
          | 1 => AppendProgress ( "-\010" )
          | 2 => AppendProgress ( "\\\010" )
          | 3 =>AppendProgress ( "|\010" )
          END (* CASE *)
        ; INC ( Ticks ) 
        ; DEC ( Incr ) 
        END (* IF *) 
      END (* LOOP *) 
    END NoteProgress 

(* VISIBLE: *) 
; PROCEDURE ProgressLineIsEmpty ( ) : BOOLEAN 
  (* If you interrupt the progress display with other output, you probably
     want to emit a new line first, if this returns FALSE.
  *) 
 
  = BEGIN 
      RETURN GProgressLen = 0
    END ProgressLineIsEmpty

(* VISIBLE: *) 
; PROCEDURE RedisplayProgress ( ) 
  (* After you interrupt the progress display with other output, emit a new
     line, then call this to get the progress line redisplayed. 
  *) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      Wr . PutString ( PWrT , SUBARRAY ( GProgressLine , 0 , GProgressLen ) )
    ; Wr . Flush ( PWrT )    
    END RedisplayProgress 

(* VISIBLE: *) 
; PROCEDURE ShowExactProgress ( Ticks : INTEGER ) 
  (* At the end, call this to display a final count. *) 

  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      Wr . PutText ( PWrT , Wr . EOL ) 
    ; Wr . PutText ( PWrT , Fmt . Pad ( Fmt . Int ( Ticks ) , TestCtPad ) ) 
    ; Wr . Flush ( PWrT )    
    END ShowExactProgress

; BEGIN (* Spinner *) 
    PWrT := Stdio . stderr 
  ; ResetProgress ( ) 
  END Spinner 
. 
