(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jan 31 10:13:48 PST 1995 by kalsow  *)
(*      modified on Thu Jan 21 09:01:03 PST 1993 by steveg  *)
(*      modified on Thu Dec 10 19:00:36 PST 1992 by msm     *)
<*PRAGMA LL*>

MODULE JoinCMap;

IMPORT ScrnColorMap, JoinScreen;

REVEAL
  Oracle = ScrnColorMap.Oracle BRANDED OBJECT
    st: JoinScreen.T;
  OVERRIDES
    standard := ColorMapDefault;
    new := ColorMapNew;
    list := ColorMapList;
    lookup := ColorMapLookup
  END;

PROCEDURE New(st: JoinScreen.T): Oracle =
  BEGIN
    RETURN NEW(Oracle, st := st)
  END New;

PROCEDURE ColorMapNew (<* UNUSED *> orc      : Oracle;
                       <* UNUSED *> nm       : TEXT := NIL;
                       <* UNUSED *> preLoaded       := TRUE ): ScrnColorMap.T =
  BEGIN
    Crash(); <*ASSERT FALSE*>
    (* IF orc.st.sts = NIL THEN Crash(); END; RETURN
       orc.st.sts[0].cmap.new(nm, preLoaded); *)
  END ColorMapNew;

PROCEDURE ColorMapDefault (<* UNUSED *> orc: Oracle): ScrnColorMap.T
  RAISES {} =
  BEGIN
    Crash(); <*ASSERT FALSE*>
    (* IF orc.st.sts = NIL THEN Crash(); END; RETURN
       orc.st.sts[0].cmap.standard(); *)
  END ColorMapDefault;
  
PROCEDURE ColorMapList (<* UNUSED *> orc       : Oracle;
                        <* UNUSED *> pat       : TEXT;
                        <* UNUSED *> maxResults: CARDINAL):
  REF ARRAY OF TEXT RAISES {} =
  BEGIN
    Crash(); <*ASSERT FALSE*>
    (* IF orc.st.sts = NIL THEN Crash(); END; RETURN
       orc.st.sts[0].cmap.list(pat, maxResults); *)
  END ColorMapList;
  
PROCEDURE ColorMapLookup (<* UNUSED *> orc: Oracle; <* UNUSED *> pat: TEXT):
  ScrnColorMap.T RAISES {} =
  BEGIN
    Crash(); <*ASSERT FALSE*>
    (* IF orc.st.sts = NIL THEN Crash(); END; RETURN
       orc.st.sts[0].cmap.lookup(pat); *)
  END ColorMapLookup;

EXCEPTION FatalError;

PROCEDURE Crash () =
  <*FATAL FatalError*>
  BEGIN
    RAISE FatalError
  END Crash;
  
BEGIN
END JoinCMap.

