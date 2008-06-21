(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Dec  8 09:46:59 PST 1994 by kalsow     *)

(* An "Wx.T" is an in-memory, append-only buffer.  It's cheaper
   to write strings and numbers to an "Wx.T" than a "Wr.T".

   It's an unchecked runtime error to concurrently call procedures
   in this interface on a single "Wx.T".
*)

INTERFACE Wx;

IMPORT Wr, Thread, TCP;

TYPE
  T <: T_; T_ = OBJECT
  METHODS
    init    (drain: TCP.T): T;
    flush   ()                            RAISES {Wr.Failure, Thread.Alerted};
    put     (a, b, c, d: TEXT := NIL)     RAISES {Wr.Failure, Thread.Alerted};
    putChar (ch: CHAR)                    RAISES {Wr.Failure, Thread.Alerted};
    putInt  (i: INTEGER)                  RAISES {Wr.Failure, Thread.Alerted};
    putStr  (READONLY x: ARRAY OF CHAR)   RAISES {Wr.Failure, Thread.Alerted};
    putSub  (txt: TEXT;  start: INTEGER;  len := LAST(INTEGER))
                                          RAISES {Wr.Failure, Thread.Alerted};
    toText  (): TEXT;
  END;

END Wx.
(* 
   Given a "Wx.T" or writer "wx",

   "wx.init(t)" drops whatever is in "wx"'s internal buffer and
   attaches the drain "t".  If "t" is NIL, "wx" will accumulate
   everything that it receives.  Otherwise, when its internal
   buffer fills, "t.put" will be called to empty the buffer.

   "wx.flush()" empties "wx"'s internal buffer to its attached
   drain.

   "wx.put..." writes data to "wx"'s internal buffer.

   "wx.toText()" returns the accumulated contents of "wx"'s
   buffer.  If "wx" has a non-"NIL" drain attached the contents
   of the buffer are unpredictable.
*)

