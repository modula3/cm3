(* Copyright (C) Rodney M. Bates 2016. *)
(* rodney.m.bates@acm.org *) 
(* Licensed under the MIT License. *) 

MODULE UnsafeUniWr 

(* Writer for character stream with one of several encodings. *) 

; IMPORT Text 
; FROM Thread IMPORT Alerted 
; IMPORT UniCodec 
; FROM UniCodec IMPORT Range 
; FROM UniCodec IMPORT Widechar  
; IMPORT UniWr 
; IMPORT UniWrClass 
; IMPORT Wr 
; FROM Wr IMPORT Failure 

(* NOTE 2: When CHAR<:WIDECHAR, and Widechar=WIDECHAR, could remove unnecessary
           ORD and VAL conversions.  
           (But maybe it is better to just leave as-is, to provide long-term
           bootstrapping capability. *) 

(* EXPORTED: *) 
; PROCEDURE FastPutChar ( Stream : UniWr . T ; Ch : CHAR ) 
  RAISES { Failure , Alerted } 
  (* Encode Ch, using Enc(Stream), and write it to Sink(Stream) *) 
  
  = <* FATAL Range *> (* Can't happen. *)
    BEGIN 
      (* Dispatch to appropriate encoding procedure. *) 
      Stream . EncWideChar ( Stream . Sink , Ch ) 
    END FastPutChar 

(* EXPORTED: *) 
; PROCEDURE FastPutWideChar ( Stream : UniWr . T ; Wch : Widechar ) 
  RAISES { Range , Failure , Alerted } 
  (* Encode Wch, using Enc(Stream), and write it to Sink(Stream) *) 
  
  = BEGIN 
      (* Dispatch to appropriate encoding procedure. *) 
      Stream . EncWideChar ( Stream . Sink , Wch ) 
    END FastPutWideChar 

(* EXPORTED: *) 
; PROCEDURE FastPutString 
    ( Stream : UniWr . T ; READONLY ArrCh : ARRAY OF CHAR ) 
  RAISES { Failure , Alerted } 
  (* Encode each character of ArrCh, using Enc(Stream), and write it to 
     Sink(Stream) 
  *) 

  = <* FATAL Range *> (* Can't happen. *)
    BEGIN  
      FOR RI := 0 TO LAST ( ArrCh ) 
      DO Stream . EncWideChar 
           ( Stream . Sink 
           , (*Assignable:*) VAL ( ORD ( ArrCh [ RI ] ) , Widechar )  
(* 2 *) 
           ) 
      END (* FOR *) 
    END FastPutString 

(* EXPORTED: *) 
; PROCEDURE FastPutWideString 
    ( Stream : UniWr . T ; READONLY ArrWch : ARRAY OF Widechar ) 
  RAISES { Range , Failure , Alerted } 
  (* Encode each character of ArrWch, using Enc(Stream), and write it to 
     Sink(Stream) 
  *) 

  = BEGIN 
      FOR RI := 0 TO LAST ( ArrWch ) 
      DO Stream . EncWideChar ( Stream . Sink , ArrWch [ RI ] ) 
      END (* FOR *) 
    END FastPutWideString 

; TYPE ProcOfWideChar = PROCEDURE ( Wch : Widechar ) RAISES ANY 
; TYPE ProcOfString = PROCEDURE ( READONLY ArrCh : ARRAY OF CHAR  ) RAISES ANY 
; TYPE ProcOfWideString 
    = PROCEDURE ( READONLY ArrWch : ARRAY OF Widechar ) RAISES ANY 

; PROCEDURE TextForAllDo 
    ( t : TEXT
    ; VisitWch : ProcOfWideChar 
    ; <* UNUSED *> VisitString : ProcOfString 
    ; <* UNUSED *> VisitWideString: ProcOfWideString
    ) 
   RAISES ANY (* From the callbacks. *) 
(* TEMPORARY: Eventually, put this in Text and make it avoid allocate and
   copy. *)  
(* Execute a series of calls back on the Visit* callback procedures that 
   covers all the characters in t, in sequence.  Unlike GetChars and 
   GetWideChars, this avoids allocating and copying the characters of t 
   into a flat array. *)  

  = BEGIN 
      FOR RI := 0 TO Text . Length ( t ) - 1 
      DO VisitWch ( VAL ( ORD ( Text . GetWideChar ( t , RI ) ) , Widechar ) ) 
(* 2 *) 
      END (* FOR *) 
    END TextForAllDo 

(* EXPORTED: *) 
; PROCEDURE FastPutText ( Stream : UniWr . T ; String : TEXT ) 
  RAISES { Range , Failure , Alerted }
  (* Encode each character of String, using Enc(Stream), and write it to 
     Sink(Stream) 
  *) 

  = PROCEDURE VisitWideChar ( Wch : Widechar  ) 
    RAISES { Range , Failure , Alerted }

    = BEGIN 
        Stream . EncWideChar 
          ( Stream . Sink , VAL ( ORD ( Wch ) , Widechar ) ) 
      END VisitWideChar  

  ; PROCEDURE VisitString ( READONLY ArrCh : ARRAY OF CHAR ) 
    RAISES { Range , Failure , Alerted }

    = BEGIN 
        FOR RI := 0 TO LAST ( ArrCh ) 
        DO Stream . EncWideChar 
             ( Stream . Sink , VAL ( ORD ( ArrCh [ RI ] ) , Widechar ) ) 
        END (* FOR *) 
      END VisitString  

  ; PROCEDURE VisitWideString ( READONLY ArrWch : ARRAY OF Widechar ) 
    RAISES { Range , Failure , Alerted }

    = BEGIN 
        FOR RI := 0 TO LAST ( ArrWch ) 
        DO Stream . EncWideChar ( Stream . Sink , ArrWch [ RI ] ) 
        END (* FOR *) 
      END VisitWideString  

  ; BEGIN (* FastPutText *) 
      (* Temporary TextForAllDo: *)  
        (* String , VisitWideChar , VisitString , VisitWideString ) *)
      (* Text . ForAllDo: *) 
      Text . ForAllDo ( String , VisitString , VisitWideString ) 

(* TODO: We need a way to say <* FATAL ANY EXCEPT Range , Failure , Alerted *>
         to avoid extraneous warnings. 
*) 
    END FastPutText  

; BEGIN (* UnsafeUniWr *) 
  END UnsafeUniWr 
. 

