(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu May 20 15:22:42 PDT 1993 by swart      *)
(*      modified on Mon Mar 23 08:13:46 PST 1992 by kalsow     *)
(*      modified on Thu Jan 25 10:33:49 1990 by muller         *)

MODULE TextWr;

IMPORT WrClass, TextF;

(* The writer's contents are stored in a sequence of fixed length
   buffers.  The single buffer that the class-independent portions
   of Wr manipulate is called "active".  All buffers are all stored
   in wr.buffers.  The space in wr.buffers is doubled when needed.
*)
      
REVEAL
  T = Public BRANDED OBJECT
        cur_buf   : INTEGER    := 0;  (* index of the active buffer *)
        max_len   : INTEGER    := 0;  (* largest value of wr.cur ever seen *)
        n_buffers : INTEGER    := 0;  (* # of allocated buffers *)
        buffers   : BufferList := NIL;(* overflow array *)
      OVERRIDES
        seek   := Seek;
        close  := Close;
        length := Length;
        init := Init;
      END;

TYPE
  Buffer     = REF ARRAY OF CHAR;
  BufferList = REF ARRAY OF Buffer;

CONST
  Slop = 24; (* enough so that a buffer doesn't overflow an allocator page *)
  BufferSize = 1024 - Slop;


PROCEDURE Init (wr: T): T =
  BEGIN
    WrClass.Lock(wr);
    TRY
      IF wr.buffers = NIL THEN wr.buffers := NEW(BufferList, 32); END;
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

PROCEDURE GotoBuffer (wr: T;  n: INTEGER) =
  VAR buf := n DIV BufferSize;
  BEGIN
    WHILE (buf > LAST (wr.buffers^)) DO ExpandBufferPool (wr) END;
    WHILE (wr.n_buffers <= buf) DO
      wr.buffers [wr.n_buffers] := NEW (Buffer, BufferSize);
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
    wr.buffers := new;
  END ExpandBufferPool;

PROCEDURE Close(wr: T) RAISES {} =
  BEGIN
    wr.buff      := NIL;
    wr.cur_buf   := 0;
    wr.n_buffers := 0;
    wr.buffers   := NIL;
  END Close;

PROCEDURE ToText(wr: T): TEXT =
  VAR result: TEXT;  len, start, n_full: INTEGER;
  BEGIN
    WrClass.Lock(wr);
    TRY
      (* capture the current length of the writer *)
      len := MAX (wr.max_len, wr.cur);

      (* allocate the result and count the buffersp *)
      result := TextF.New (len);
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
  END ToText;

BEGIN
END TextWr.
