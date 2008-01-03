(*
* RdWrPipe.m3 -- a reader/writer pair that a directly connected together.
* Copyright (C) Blair MacIntyre 1995
* Author          : Blair MacIntyre
* Created On      : Mon Feb 20 17:43:14 1995
* Last Modified By: Blair MacIntyre
* Last Modified On: Wed Apr 19 17:20:20 1995
* Update Count    : 55
* Status          : Unknown, Use with caution!
*
* $Source: /opt/cvs/cm3/m3-comm/rdwr/test/john/src/RdWrPipe.m3,v $
* $Date: 2001-12-02 00:29:10 $
* $Author: wagner $
* $Revision: 1.1.1.1 $
*
* $Log: not supported by cvs2svn $
* Revision 1.1.1.1  1996/03/03 19:05:04  bm
* Imported Sources
*
*
* HISTORY
*)

MODULE RdWrPipe;

IMPORT Rd, Wr, RdClass, WrClass, Thread, Atom, AtomList, IO, Fmt;
(*UNUSED, but want to keep the following revelation honest.*)
IMPORT UnsafeWr, UnsafeRd;
FROM Thread IMPORT Alerted;

(* Since we need to use the Mutex properties of Rd.T and Wr.T, we
   should actually import UnsafeWr and UnsafeRd. We need to add the
   following revelations, as the comment in UnsafeRd points out, if we
   want to include both the Unsafe* and *Class interfaces. *)
REVEAL RdClass.Private <: MUTEX;
REVEAL WrClass.Private <: MUTEX;

CONST
TYPE
  (* the shared data between the reader and writer corresponds to the
     locking elements and the pointers into the shared buffer.  The closed
     flag is used for the reader/writer to communicate if they are
     close()d. *)

  (* If there is no data in the buffer, first_i = next_i, and the reader
     may block.  Otherwise, first_i points at the first character in the
     shared buffer available for reading, and next_i points at the the
     first character available for writing.  If (next_i + 1) MOD size =
     first_i, the buffer is full, and the writer may block.

     At any given time, rd.st will be first_i.  rd.cur <= rd.hi will fall
     in the area between first_i and next_i.

     wr.st will be next_i.  wr.cur <= wr.hi will fall in the free area
     between next_i and first_i.

     wr.flush() will sync the writer, so that next_i, wr.st and wr.lo will
     be advanced to correspond to wr.cur, "flushing" the buffered writer
     output so the reader can get at it. *)

  SharedData = RECORD
                 name: TEXT;		 (* an optional name *)
                 mu: Thread.Mutex;  (* for controlling access to this *)
                 cv: Thread.Condition;  (* when it's full or empty *)
                 first_i: CARDINAL   := 0; (* first index of data *)
                 next_i : CARDINAL   := 0; (* last index of part in use *)
                 size   : CARDINAL;  (* size of buffer *)
                 closed : BOOLEAN    := FALSE; (* is this buffer open? *)
               END;

  (* our reader and writer will share a buffer and an instance of the
     SharedData RECORD *)
  RdT = Rd.T OBJECT
          share: REF SharedData;
        OVERRIDES
          seek   := RdSeek;
          length := RdLength;
          close  := RdClose;
        END;

  WrT = Wr.T OBJECT
          share: REF SharedData;
        OVERRIDES
          seek  := WrSeek;
          close := WrClose;
          flush := WrFlush;
        END;

<*UNUSED*>
PROCEDURE RdChanged(t: TEXT; rd: RdT) = 
  BEGIN
    IO.Put(t & "("&rd.share.name&") changed: \n lo = " &
      Fmt.Unsigned(rd.lo) & ", hi = " &
      Fmt.Unsigned(rd.hi) & ", cur = " &
      Fmt.Unsigned(rd.cur) & ", st = " &
      Fmt.Unsigned(rd.st) & "\n share.first_i = " &
      Fmt.Unsigned(rd.share.first_i) & ", share.next_i = " &
      Fmt.Unsigned(rd.share.next_i) & ", share.size = " &
      Fmt.Unsigned(rd.share.size) & "\n");
  END RdChanged;

<*UNUSED*>
PROCEDURE WrChanged(t: TEXT; wr: WrT) = 
  BEGIN
    IO.Put(t & "("&wr.share.name&") changed: \n lo = " &
      Fmt.Unsigned(wr.lo) & ", hi = " &
      Fmt.Unsigned(wr.hi) & ", cur = " &
      Fmt.Unsigned(wr.cur) & ", st = " &
      Fmt.Unsigned(wr.st) & "\n share.first_i = " &
      Fmt.Unsigned(wr.share.first_i) & ", share.next_i = " &
      Fmt.Unsigned(wr.share.next_i) & ", share.size = " &
      Fmt.Unsigned(wr.share.size) & "\n");
  END WrChanged; 

PROCEDURE New (VAR rd       : Rd.T;
               VAR wr       : Wr.T;
                   buff_size: CARDINAL := BufferSize; nm : TEXT := NIL) =
  VAR
    info := NEW(REF SharedData, mu := NEW(Thread.Mutex),
                cv := NEW(Thread.Condition), size := buff_size);
    shared_buff := NEW(REF ARRAY OF CHAR, buff_size);
  BEGIN
    IF nm = NIL THEN nm := "" END;
    info.name := nm;
    rd := NEW(RdT, buff := shared_buff, share := info);
    wr := NEW(WrT, buff := shared_buff, share := info);

    rd.st := 0;
    rd.lo := 0;
    rd.cur := 0;
    rd.hi := 0;
    rd.closed := FALSE;
    rd.seekable := FALSE;
    rd.intermittent := TRUE;

    wr.st := 0;
    wr.lo := 0;
    wr.cur := 0;
    wr.hi := buff_size;
    wr.closed := FALSE;
    wr.seekable := FALSE;
    wr.buffered := TRUE;
  END New;

EXCEPTION Error;                 <*FATAL Error*>

PROCEDURE RdSeek (rd: RdT; pos: CARDINAL;
                  dontBlock: BOOLEAN): 
  RdClass.SeekResult RAISES {Alerted} =
  VAR do_signal := FALSE;
  BEGIN
    (* This file is not seekable, so only handle the special case. *)
    IF pos # rd.hi OR pos # rd.cur THEN RAISE Error; END;

    LOCK rd.share.mu DO
      (* Check the obvious: rd.st should correspond to the start of the
         data area of the shared buffer. *)
      <*ASSERT rd.st = rd.share.first_i *>

      (* cur_i is the position of rd.cur in the shared data buffer. *)
      WITH cur_i = (rd.st + rd.cur - rd.lo) MOD rd.share.size DO
        (* Set rd.lo to rd.cur, since we are done with all of the current
           buffer contents.  Move rd.st to cur_i, to keep things in sync in
           case we raise an Alert below and leave early. *)
        rd.lo := rd.cur;
        rd.st := cur_i;

        (* Before updating first_i, check if the buffer was full.  If so,
           then there may be blocked writers, so arrange to signal them.
           Since the buffer is completely full, we'll handle this special
           case explicitely. *)
        IF (rd.share.next_i + 1 MOD rd.share.size) = rd.share.first_i THEN
          do_signal := TRUE;

          (* move first_i to the next value to be read, freeing up the
             stuff that was already read. *)
          rd.share.first_i := cur_i;
        ELSE
          (* the buffer is not completely full, so we see if there is
             anything to be gotten.  First, set first_i, in case we are
             Alerted in the Wait below. *)
          rd.share.first_i := cur_i;

          (* As long as there is nothing more to read, wait. *)
          WHILE rd.share.first_i = rd.share.next_i DO
            (* Don't block if they don't want us to. *)
            IF dontBlock THEN RETURN RdClass.SeekResult.WouldBlock END;

            (* before blocking, check if the other end has closed *)
            IF rd.share.closed THEN RETURN RdClass.SeekResult.Eof END;

            (* wait for more data!  An Alert will pop us all the way
               out.  If the writer closes, we will be signaled, and
               go through the loop again, this time terminating above. *)
            Thread.AlertWait(rd.share.mu, rd.share.cv);
          END;
        END;

        IF rd.share.first_i < rd.share.next_i THEN
          (* move rd.hi to rd.share.next_i *)
          INC(rd.hi, rd.share.next_i - cur_i);
        ELSE
          (* available part of buffer wraps, so move rd.hi to one
             after the end. We will wrap next time. *) 
          INC(rd.hi, rd.share.size - cur_i);
        END;
        (* RdChanged(rd.share.name, rd); *)
      END;
    END;

    (* we signal outside the MUTEX, as suggested in the threads article, to
       prevent spurious context switches *)
    IF do_signal THEN Thread.Signal(rd.share.cv); END;
    RETURN RdClass.SeekResult.Ready;
  END RdSeek;

PROCEDURE RdLength (<*UNUSED*>rd: RdT): INTEGER =
  BEGIN
    RETURN -1;
  END RdLength;

PROCEDURE RdClose (rd: RdT) RAISES {} =
  BEGIN
    LOCK rd.share.mu DO
      IF NOT rd.share.closed THEN
        (* in case the writer is blocked *)
        Thread.Signal(rd.share.cv);
      END;
      rd.share.closed := TRUE; 
      rd.share := NIL;
    END;
  END RdClose;

PROCEDURE WrSeek (wr: WrT; pos: CARDINAL) RAISES {Wr.Failure, Alerted} =
  BEGIN
    (* This file is not seekable, so only handle the special case. *)
    IF pos # wr.hi OR pos # wr.cur THEN RAISE Error; END;

    (* first, call flush so that the shared buffer and wr are in sync.
       Also, flush will raise an exception if the reader has closed. *)
    wr.flush();

    LOCK wr.share.mu DO
      (* Check the obvious: wr.st should correspond to the start of the
         free area of the shared buffer. *)
      <*ASSERT wr.st = wr.share.next_i *>

      (* cur_i is the position of wr.cur in the shared data buffer. 
         Because of the flush() above, wr.cur = wr.lo, so cur_i =
         wr.st.  We'll leave it as it is here, for now, though. *)
      WITH cur_i = (wr.st + wr.cur - wr.lo) MOD wr.share.size DO
        WHILE (wr.share.next_i + 1 MOD wr.share.size) = wr.share.first_i DO
          (* Wait for more buffer space. An Alert will pop us all the
             way out. *)
          Thread.AlertWait(wr.share.mu, wr.share.cv)
        END;

        (* wr.st corresponds to the first part of the free buffer, starting
           at wr.share.next_i.  There will always be at least one "free"
           character, corresponding to wr.share.next_i, which always points
           at a valid spot in the buffer. *)
        IF wr.share.first_i <= wr.share.next_i THEN
          (* the rest of the buffer is open, since the first_i used is
             earlier in the buffer: in other words, the hole wraps around
             the end.  Mark everything to the end as available. *)
          INC(wr.hi, wr.share.size - cur_i);
        ELSE
          (* mark everything from the beginning of the buffer to 1 before
             first_i as available *)
          INC(wr.hi, wr.share.first_i - 1 - cur_i);
        END;
        (* WrChanged(wr.share.name, wr);*)
      END;
    END;
  END WrSeek;

PROCEDURE WrFlush (wr: WrT) RAISES {Wr.Failure} =
  VAR do_signal: BOOLEAN := FALSE;
  BEGIN
    LOCK wr.share.mu DO
      (* before wasting time doing any work, if the other end has
         closed then the write fails. *)
      IF wr.share.closed THEN 
        RAISE Wr.Failure(AtomList.List1(Atom.FromText("reader closed")));
      END;

      (* if there is anything to flush, let's flush! *)
      IF wr.cur > wr.lo THEN
        WITH cur_i = (wr.st + wr.cur - wr.lo) MOD wr.share.size DO
          IF wr.share.first_i = wr.share.next_i THEN
            (* The buffer was empty, so signal any blocked readers. *)
            do_signal := TRUE;
          END;

          (* We want to move next_i and wr.st ahead so that the buffered
             stuff is now "available" in the shared buffer.  So, we need to
             change next_i, wr.st to cur_i, and then move wr.lo to wr.cur,
             so that it corresponds to index of wr.st.  The case of cur_i
             having hit the end of the buffer, and needing to wrap is
             handled by the MOD above. *)
          wr.share.next_i := cur_i;
          wr.st := cur_i;
          wr.lo := wr.cur;
        END;
      END;
    END;
    IF do_signal THEN Thread.Signal(wr.share.cv); END;
  END WrFlush;

PROCEDURE WrClose (wr: WrT) RAISES {} =
  BEGIN
    LOCK wr.share.mu DO
      IF NOT wr.share.closed THEN
        (* in case the reader is blocked *)
        Thread.Signal(wr.share.cv);
      END;
      wr.share.closed := TRUE; 
      wr.share := NIL;
    END;
  END WrClose;

PROCEDURE ResetRdCounter (rd: Rd.T) =
  BEGIN
    TYPECASE rd OF
    | RdT (r) =>
        LOCK rd DO DEC(r.cur, r.lo); DEC(r.hi, r.lo); r.lo := 0; END;
    ELSE                         (* Skip *)
    END;
  END ResetRdCounter;

PROCEDURE ResetWrCounter (wr: Wr.T) =
  BEGIN
    TYPECASE wr OF
    | WrT (w) =>
        LOCK wr DO DEC(w.cur, w.lo); DEC(w.hi, w.lo); w.lo := 0; END;
    ELSE                         (* Skip *)
    END;
  END ResetWrCounter;

BEGIN
END RdWrPipe.
