(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Nov 20 20:59:20 PST 1992 by meehan     *)
(*      modified on Tue Jun 16 13:16:23 PDT 1992 by muller     *)
(*      modified on Wed May  3 11:10:09 PDT 1989 by mbrown     *)
(*      modified on Tue Oct 11 19:06:48 1988 by chan           *)

INTERFACE MTextRd;

IMPORT Rd, MText, Thread;

TYPE
  T <: Public;
  Public = Rd.T OBJECT
           METHODS
             init (m         : MText.T  := NIL;
                   start     : CARDINAL := 0;
                   rangeStart: CARDINAL := 0;
                   rangeEnd  : CARDINAL := LAST (CARDINAL);
                   reverse   : BOOLEAN  := FALSE            ): T
                   RAISES {Rd.Failure, Thread.Alerted}
           END;

(* "rd.init(m, ...)" initializes a seekable, non-intermittent reader for the
   "MText m".

   It is intended that this method be called more than once, to re-initialize
   a reader (and avoid re-allocating storage for "rd.buff"); in this case, it
   is permitted for "m" to be "NIL", and the default value is the "MText" that
   was previously used by the reader.

   The reader's source may be a proper subsequence of the underlying "MText",
   and the reader can be initialized to read in reverse order.

   "src(rd)" comprises the characters of "m" that are contained in the
   interval
|  [first(rd) .. last(rd)]
   where
|  first(rd)) = rangeStart
   and
|  last(rd) = MIN (rangeEnd, MText.Length(m)) - 1]

|  len(rd) = MAX (last(rd) - first(rd) + 1, 0)

   In a forward reader ("reverse = FALSE"), "Rd.GetChar" delivers characters
   from "start" to "last(rd)".  "Rd.Index" returns the index relative to
   "first(rd)".  Hence just after a forward reader "rd" is initialized,
|  cur(rd) = start - first(rd)

   In a reverse reader, "Rd.GetChar" delivers characters from "start-1" (not
   "start") to "first(rd)".  "Rd.Index" returns the index relative to
   "last(rd)+1".  Hence just after a reverse reader "rd" is initialized,
|  cur(rd) = last(rd) + 1 - start

   Thus the value returned by "Rd.Index" increases by 1 with each call to
   "Rd.GetChar", regardless of the reader's direction.

   The coordinate system used by "Rd.Seek" is consistent with "Rd.Index".

   If the underlying "MText" is modified, the reader is invalid and should be
   re-initialized.   

   "rd.init" calls "Rd.Seek(rd, rd.cur)", which can raise "Rd.Failure" or
   "Thread.Alerted".

   Synchronization: it is not possible to have several readers reading the
   mtext concurrently.  The reason is that a read to a file node causes the
   mtext data structure to change.  If you don't use file nodes, then it's
   safe. (JRM doesn't believe this.)

*)

PROCEDURE New (m         : MText.T;
               start     : CARDINAL  := 0;
               rangeStart: CARDINAL  := 0;
               rangeEnd  : CARDINAL  := LAST (CARDINAL);
               reverse   : BOOLEAN   := FALSE            ): T
  RAISES {Rd.Failure, Thread.Alerted};
(* This is equivalent to
| NEW (T).init (m, start, rangeStart, rangeEnd, reverse, alertable)
*)

END MTextRd.
