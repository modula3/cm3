(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * 
 * Author          : Blair MacIntyre
 * Created On      : Tue May 23 18:06:52 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Jun 18 19:00:34 1998
 * Update Count    : 49
 * 
 * $Source: /opt/cvs/cm3/m3-comm/events/src/EventRd.m3,v $
 * $Date: 2001-12-02 00:06:45 $
 * $Author: wagner $
 * $Revision: 1.1.1.1 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.4  1998/07/02 21:41:12  bm
 * small bug fixes
 *
 * Revision 1.3  1997/08/04 20:15:11  bm
 * Fixed BRANDs
 *
 * Revision 1.2  1996/11/21 22:41:12  bm
 * fixed header
 *
 * 
 * HISTORY
 *)
(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon Mar 21 13:15:19 PST 1994 by kalsow     *)
(*      modified on Fri Jun 18 11:34:44 PDT 1993 by wobber     *)
(*      modified on Thu May 20 15:22:41 PDT 1993 by swart      *)
(*      modified on Mon Apr 26 17:25:44 PDT 1993 by mcjones    *)
(*      modified on Thu Jul 11 20:58:41 1991 by muller         *)

MODULE EventRd;

IMPORT EventWr, EventWrF, RdClass, RdCopy, Rd, Wr, Thread, WrClass;
(* IMPORT IO, Fmt; *)

REVEAL
  T = Public BRANDED "EventRd.T" OBJECT
        cur_buf   : INTEGER    := 0;  (* index of the active buffer *)
        max_len   : INTEGER    := 0;  (* largest value of rd.cur possible *)
        n_buffers : INTEGER    := 0;  (* # of allocated buffers *)
        buffers   : BufferList := NIL;(* overflow array *)
        wr        : EventWr.T  := NIL;(* The EventWr.T use to init this. *)
      OVERRIDES
        seek   := Seek;
        length := Length;
        init   := Init;
      END;

TYPE
  BufferList = EventWrF.BufferList;

CONST
  BufferSize = EventWrF.BufferSize;

PROCEDURE Init (rd: T; wr: EventWr.T): T =
  (* VAR len := Wr.Length (wr); *)
  <*FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    (*
    IF (rd.buff = NIL) OR (len > NUMBER (rd.buff^)) THEN
      rd.buff := NEW(REF ARRAY OF CHAR, len);
    END;
    *)
    (* Copy the stuff from the writer. *)
    rd.wr := wr;
    rd.extMu := NEW(Thread.Mutex);

    IF wr # NIL THEN
      (* Make sure it's flushed. *)
      Wr.Flush(wr);

      rd.max_len := wr.max_len;
      rd.n_buffers := wr.n_buffers;
      rd.buffers := wr.buffers;

      (* Steal the buffers so that if the writer is init'd again, it
         gets new buffers. *)
      wr.buffers := NIL;
      wr.buff := NIL;
    ELSE
      IF rd.buffers = NIL THEN 
        rd.buffers := NEW(BufferList, 32); 
        FOR i := FIRST (rd.buffers^) TO LAST (rd.buffers^) DO
          rd.buffers[i] := NIL;
        END;
      END;
    END;

    rd.st := 0;
    rd.cur := 0;
    rd.closed := FALSE;
    rd.seekable := TRUE;
    rd.intermittent := FALSE;

    GotoBuffer(rd, 0);
    RETURN rd;
  END Init;

PROCEDURE ToWr (rd: T) : EventWr.T =
  VAR wr: EventWr.T;
  BEGIN
    IF rd = NIL OR rd.wr = NIL THEN RETURN NIL END;
    wr := rd.wr;

    (* Reinitialize the writer. *)
    wr.buffers := rd.buffers;
    EVAL wr.init();

    (* Drop references and return the wr. *)
    rd.wr := NIL;
    rd.buffers := NIL;
    rd.buff := NIL;

    RETURN wr;
  END ToWr;

PROCEDURE GotoBuffer (rd: T;  n: INTEGER) =
  VAR buf := n DIV BufferSize;
  BEGIN
    <* ASSERT buf <= LAST (rd.buffers^) *>
    rd.cur_buf := buf;
    rd.buff    := rd.buffers [buf];
    rd.lo      := buf * BufferSize;
    rd.hi      := MIN(rd.lo + BufferSize, rd.max_len);
  END GotoBuffer;

PROCEDURE New(t: EventWr.T): T = BEGIN RETURN NEW(T).init(t); END New;

PROCEDURE Length (rd: T): INTEGER RAISES {} =
  BEGIN
    RETURN rd.max_len;
  END Length;

PROCEDURE Seek (rd: T; pos: CARDINAL;
               <*UNUSED*> dontBlock: BOOLEAN): RdClass.SeekResult =
  BEGIN
    (* IO.Put(Fmt.F("EventRd.Seek(%s) => max=%s\n", Fmt.Unsigned(pos, 10),
                 Fmt.Int(rd.max_len))); *)
    IF pos >= rd.max_len THEN
      rd.cur := rd.max_len;
      GotoBuffer(rd, rd.cur);
      (* IO.Put(Fmt.F("EventRd.Seek(%s) => set cur=%s and return Eof\n",
                   Fmt.Int(pos), Fmt.Int(rd.cur)));  *)
      RETURN RdClass.SeekResult.Eof;
    ELSE
      IF (pos < rd.lo) OR (rd.hi <= pos) THEN GotoBuffer (rd, pos) END;
      rd.cur := pos;
      (* IO.Put(Fmt.F("EventRd.Seek(%s) => set cur=%s and return Ready\n",
                   Fmt.Unsigned(pos, 10), Fmt.Int(rd.cur))); *)
      RETURN RdClass.SeekResult.Ready; 
    END;
  END Seek;

PROCEDURE FromRd(rd: Rd.T; erd: T) : T 
  RAISES {Rd.Failure, Thread.Alerted} =
  VAR
    wr: EventWr.T := ToWr(erd);
    num : INTEGER;
    <*FATAL Wr.Failure *>
  BEGIN
    IF wr = NIL THEN
      wr := NEW(EventWr.T).init();
    END;
    num := RdCopy.ToWriter(rd, wr);

    (* IO.Put("Created EventRd from Read of " & 
       Fmt.Int(num) & " bytes\n"); *)
    IF erd = NIL THEN
      erd := NEW(T);
    END;
    RETURN erd.init(wr);
  END FromRd;

BEGIN
END EventRd.
