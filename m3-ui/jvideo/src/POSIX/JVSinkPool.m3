(* Copyright (C) 1989, 1993 Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Fri Jan 27 15:07:26 PST 1995 by msm      *)
(*      modified on Sun Oct 24 17:58:22 PDT 1993 by sfreeman *)

MODULE JVSinkPool;

IMPORT JVFromSource, Jvs, JVSink, OSError, Text, Thread;

TYPE
  Elt = RECORD
          hostname: TEXT;
          quality : JVSink.Quality;
          sink    : JVSink.T;
          delay   : CARDINAL;
          next    : EltRef           := NIL;
        END;
  EltRef = REF Elt;

PROCEDURE GetSink (hostname  : TEXT;
                   quality   : JVSink.Quality := JVSink.DefaultQuality;
                   create                     := TRUE;
                   maxBuffers: CARDINAL       := 2;
                   delay     : CARDINAL                                 ):
  JVSink.T RAISES {OSError.E, Thread.Alerted} =
  VAR res: JVSink.T := NIL;
  BEGIN
    WITH elt = SearchList(hostname, quality, delay) DO
      IF elt # NIL THEN RETURN elt.sink; END;
    END;
    IF create THEN
      res := NEW(JVSink.T).init(
               hostname, quality, maxBuffers,
               NEW(JVFromSource.Factory).init(NEW(Jvs.T).init()), delay);

      (* add to list *)
      WITH elt = NEW(EltRef, hostname := hostname, quality := quality,
                     delay := delay, sink := res) DO
        LOCK mutex DO elt.next := list; list := elt; END;
      END;
    END;
    RETURN res;
  END GetSink;

(* -- internal procedures -- *)

PROCEDURE SearchList (hostname: TEXT;
                      quality : JVSink.Quality;
                      delay   : CARDINAL        ): EltRef =
  (* look for an element which matches params.  Return NIL if none *)
  VAR curr := list;
  BEGIN
    LOCK mutex DO
      WHILE curr # NIL DO
        IF ((hostname = NIL AND curr.hostname = NIL)
              OR (hostname # NIL AND curr.hostname # NIL
                    AND Text.Equal(hostname, curr.hostname)))
             AND quality = curr.quality AND delay = curr.delay THEN
          EXIT;
        END;
        curr := curr.next;
      END;
    END;
    RETURN curr;
  END SearchList;

VAR
  mutex         := NEW(MUTEX);
  list : EltRef := NIL;
BEGIN

END JVSinkPool.
