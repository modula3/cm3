(* Copyright (C) 1989, 1993 Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Fri Jan 27 15:07:26 PST 1995 by msm      *)
(*      modified on Sun Oct 24 17:59:22 PDT 1993 by sfreeman *)

MODULE JVDecompPool;

IMPORT JVBuffer, JVDecomp, JVSink, JVSinkPool, Jvs, OSError, Text, Thread;

TYPE
  Elt = RECORD
          sourceHost: TEXT;
          quality   : JVSink.Quality;
          dparams   : Jvs.DcmpParams;
          cmap      : Jvs.ColormapInfo;
          decomp    : JVDecomp.T;
          next      : EltRef             := NIL;
	  subtype   : CARDINAL;
        END;
  EltRef = REF Elt;


VAR jvs: Jvs.T := NIL;

PROCEDURE GetDecomp (hostname: TEXT;
                     quality : JVSink.Quality := JVSink.DefaultQuality;
                     READONLY dparams: Jvs.DcmpParams;
                     READONLY cmap   : Jvs.ColormapInfo;
                              create                      := TRUE;
                     maxSinkBuffs, maxDecompBuffs: CARDINAL := 2;
                     decompFactory: JVBuffer.Factory := NIL;
                     decompServer : Jvs.T            := NIL;
                     delay        : CARDINAL;
		     subtype      : CARDINAL): JVDecomp.T
  RAISES {OSError.E, Thread.Alerted} =
  VAR localparams := dparams;
  (* with output sizes converted for jv board alignment *)
  BEGIN
    (* get acceptable outX, outY *)
    IF jvs = NIL THEN jvs := NEW(Jvs.T).init(); END;
    EVAL jvs.setDecompress(localparams);

    WITH elt = SearchList(hostname, quality, localparams, cmap, subtype) DO
      IF elt # NIL THEN RETURN elt.decomp; END;
    END;
    IF NOT create THEN RETURN NIL; END;

    WITH sink = JVSinkPool.GetSink(
                  hostname, quality, create, maxSinkBuffs, delay),
         res = NEW(JVDecomp.T).init(sink, dparams, cmap, maxDecompBuffs,
                                    decompFactory, decompServer),

         elt = NEW(EltRef, sourceHost := hostname, quality := quality,
                   dparams := localparams, cmap := cmap, decomp := res,
		   subtype := subtype) DO
      <* ASSERT sink # NIL *>
      LOCK mutex DO elt.next := list; list := elt; END;
      RETURN res;
    END;
  END GetDecomp;

(* -- internal procedures -- *)

PROCEDURE SearchList (         sourceHost: TEXT;
                               quality   : JVSink.Quality;
                      READONLY dparams   : Jvs.DcmpParams;
                      READONLY cmap      : Jvs.ColormapInfo;
			       subtype   : CARDINAL): EltRef =
  (* look for an element which matches params.  Return NIL if none *)
  PROCEDURE TextEqual (t1, t2: TEXT): BOOLEAN =
    BEGIN
      RETURN (t1 = NIL AND t2 = NIL)
               OR (t1 # NIL AND t2 # NIL AND Text.Equal(t1, t2));
    END TextEqual;
  VAR curr := list;
  BEGIN
    LOCK mutex DO
      WHILE curr # NIL DO
        IF TextEqual(sourceHost, curr.sourceHost)
             AND quality = curr.quality AND dparams = curr.dparams
             AND cmap.id = curr.cmap.id
             AND cmap.nColors = curr.cmap.nColors
             AND cmap.monochrome = curr.cmap.monochrome
             AND TextEqual(cmap.displayName, curr.cmap.displayName) 
	     AND subtype = curr.subtype THEN
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
END JVDecompPool.
