(* Copyright (C) 1989, 1990, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Thu Oct 21 17:20:35 PDT 1993 by sfreeman *)

(* this is the "nice" interface to Jva.i3 which provides connections to the
   local audio server.  These Ts are shared if possible and the connection
   only closed if the T has no more clients. *)

MODULE JVAudio;

IMPORT AtomList, Jv, Jva, OSError, Refany, Text, TextRefTbl, Thread, WeakRef;

REVEAL
  T = Jva.T BRANDED OBJECT
        hostname: TEXT;
        count   : CARDINAL := 0;
        open               := FALSE;
      OVERRIDES
        close := Close;
      END;

PROCEDURE Close (t: T) =
  VAR ref: Refany.T;
  BEGIN
    LOCK t DO
      IF t.count > 1 THEN
        DEC(t.count);
      ELSIF t.count = 1 THEN
        DEC(t.count);
        IF t.open THEN
          Jv.T.close(t);
          LOCK mutex DO
            EVAL tbl.delete(t.hostname, ref); END;
        END;
      END;
    END;
  END Close;

PROCEDURE New (hostname: TEXT): T RAISES {OSError.E, Thread.Alerted} =
  VAR
    ref: Refany.T;
    res: T;
  BEGIN
    IF hostname = NIL OR Text.Empty(hostname) THEN
      RAISE OSError.E(AtomList.List1(Jva.invalidHostname));
    END;
    LOCK mutex DO
      IF tbl.get(hostname, ref) THEN
        res := NARROW(ref, T);
      ELSE
        (* create a new one *)
        res := NEW(T).init();
        res.connect(hostname);
        EVAL WeakRef.FromRef(res, CleanUp);
        EVAL tbl.put(hostname, res);
      END;
    END;
    LOCK res DO
      res.open := TRUE;
      res.hostname := hostname;
      INC(res.count);
    END;
    RETURN res;
  END New;

(* -- weak ref stuff -- *)
PROCEDURE CleanUp (<*UNUSED*> READONLY w: WeakRef.T; r: REFANY) =
  BEGIN
    WITH t = NARROW(r, T) DO
      (* don't need to lock t now *)
      IF t.open THEN Jv.T.close(t); END;
    END;
  END CleanUp;

VAR
  mutex := NEW(Thread.Mutex);
  tbl   := NEW(TextRefTbl.Default).init(3);
BEGIN
END JVAudio.
