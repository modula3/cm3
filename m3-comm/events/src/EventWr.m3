(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * See file COPYRIGHT-COLUMBIA for details.
 * 
 * Author          : Blair MacIntyre
 * Created On      : Tue May 23 18:01:38 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 12:30:01 1997
 * Update Count    : 17
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1.1.1  2001/12/02 00:06:45  wagner
 * Blair MacIntyre's events library
 *
 * Revision 1.3  1997/08/04 20:15:13  bm
 * Fixed BRANDs
 *
 * Revision 1.2  1996/11/21 22:49:10  bm
 * fixed header
 *
 * 
 * HISTORY
 *)
(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu May 20 15:22:42 PDT 1993 by swart      *)
(*      modified on Mon Mar 23 08:13:46 PST 1992 by kalsow     *)
(*      modified on Thu Jan 25 10:33:49 1990 by muller         *)

UNSAFE MODULE EventWr EXPORTS EventWr;

IMPORT WrClass, EventWrF;

(* The writer's contents are stored in a sequence of fixed length
   buffers.  The single buffer that the class-independent portions
   of Wr manipulate is called "active".  All buffers are all stored
   in wr.buffers.  The space in wr.buffers is doubled when needed.
*)
      
REVEAL
  T = EventWrF.T BRANDED "EventWr.T" OBJECT
      OVERRIDES
        seek   := Seek;
        close  := Close;
        length := Length;
        init := Init;
        flush := Flush;
      END;

TYPE
  Buffer     = EventWrF.Buffer;
  BufferList = EventWrF.BufferList;

CONST
  BufferSize = EventWrF.BufferSize;

PROCEDURE Init (wr: T): T =
  BEGIN
    WrClass.Lock(wr);
    TRY
      (* May already be allocated, if we are reusing wr. *)
      IF wr.buffers = NIL THEN 
        wr.buffers := NEW(BufferList, 32); 
        FOR i := FIRST (wr.buffers^) TO LAST (wr.buffers^) DO
          wr.buffers[i] := NIL;
        END;
      END;
      wr.st := 0;
      wr.cur := 0;
      wr.max_len := 0;
      wr.closed := FALSE;
      wr.seekable := TRUE;
      wr.buffered := TRUE;
      GotoBuffer(wr, wr.cur);
    FINALLY
      WrClass.Unlock(wr);
    END;
    RETURN wr;
  END Init;

PROCEDURE New(): T = BEGIN RETURN NEW(T).init(); END New;

PROCEDURE Length (wr: T): CARDINAL RAISES {} =
  BEGIN
    wr.max_len := MAX (wr.max_len, wr.cur);
    RETURN wr.max_len;
  END Length;

PROCEDURE Seek(wr: T; n: CARDINAL) RAISES {} =
  BEGIN
    (* capture the current length of the writer *)
    wr.max_len := MAX (wr.max_len, wr.cur);

    (* make sure we don't seek beyond the end of the writer *)
    n := MIN (n, wr.max_len);

    IF (n < wr.lo) OR (wr.hi <= n) THEN GotoBuffer (wr, n) END;
    wr.cur := n;
  END Seek;

PROCEDURE Flush(wr: T) =
  BEGIN
    Seek(wr, wr.cur);
  END Flush;

PROCEDURE GotoBuffer (wr: T;  n: INTEGER) =
  VAR buf := n DIV BufferSize;
  BEGIN
    WHILE (buf > LAST (wr.buffers^)) DO ExpandBufferPool (wr) END;
    WHILE (wr.n_buffers <= buf) DO
      (* May already be allocated, if we are reusing wr. *)
      IF wr.buffers [wr.n_buffers] = NIL THEN
        wr.buffers [wr.n_buffers] := NEW (Buffer, BufferSize);
      END;
      INC (wr.n_buffers);
    END;
    wr.cur_buf := buf;
    wr.buff    := wr.buffers [buf];
    wr.lo      := buf * BufferSize;
    wr.hi      := wr.lo + BufferSize;
  END GotoBuffer;

PROCEDURE ExpandBufferPool (wr: T) =
  VAR new := NEW (BufferList, 2 * NUMBER (wr.buffers^));
  BEGIN
    SUBARRAY (new^, 0, wr.n_buffers) := wr.buffers^;
    FOR i := LAST(wr.buffers^)+1 TO LAST (new^) DO
      new[i] := NIL;
    END;
    wr.buffers := new;
  END ExpandBufferPool;

PROCEDURE Close(wr: T) RAISES {} =
  BEGIN
    wr.buff      := NIL;
    wr.cur_buf   := 0;
    wr.n_buffers := 0;
  END Close;

PROCEDURE ToChars(wr: T): REF ARRAY OF CHAR =
  VAR result: REF ARRAY OF CHAR;  len, start, n_full: INTEGER;
  BEGIN
    WrClass.Lock(wr);
    TRY
      (* capture the current length of the writer *)
      len := MAX (wr.max_len, wr.cur);

      (* allocate the result and count the buffersp *)
      result := NEW (REF ARRAY OF CHAR, len);
      n_full := len DIV BufferSize;

      (* copy the full buffers *)
      start  := 0; (* current character index *)
      FOR i := 0 TO n_full - 1 DO
        SUBARRAY (result^, start, BufferSize) := wr.buffers[i]^;
        INC (start, BufferSize);
      END;

      (* copy the remaining partial buffer *)
      len := len - start;
      IF (len > 0) THEN
        SUBARRAY (result^, start, len)
            := SUBARRAY (wr.buffers[n_full]^, 0, len);
      END;

      (* reset the length, pointers and buffers *)
      wr.max_len := 0;
      wr.cur := 0;
      GotoBuffer (wr, 0);
    FINALLY 
      WrClass.Unlock(wr)
    END;
    RETURN result;
  END ToChars;

BEGIN
END EventWr.
