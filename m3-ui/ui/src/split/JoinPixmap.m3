(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jan 31 09:41:52 PST 1995 by kalsow  *)
(*      modified on Mon Jun  6 04:18:00 PDT 1994 by msm     *)
<*PRAGMA LL*>

MODULE JoinPixmap;

IMPORT ScrnPixmap, JoinScreen, Pixmap, Palette, PlttFrnds, Rect, VBTRep,
       STypeMapSeq, IntRefTbl, TrestleComm, ScreenType, STypeMap;

PROCEDURE New(st: JoinScreen.T): Oracle =
  BEGIN
    RETURN NEW(Oracle, st := st)
  END New;

PROCEDURE ResolvePix (st: JoinScreen.T; pm: Pixmap.T) =
  VAR i: INTEGER; t := st.succ(NIL, i);
  BEGIN
    WHILE t # NIL DO EVAL Palette.ResolvePixmap(t, pm); t := st.succ(t, i) END
  END ResolvePix;

PROCEDURE Apply (           st: JoinScreen.T;
                 <*UNUSED*> cl: Palette.PixmapClosure;
                            pm: Pixmap.T               ): ScrnPixmap.T =
  VAR res := st.pixmaps[pm.pm];
  BEGIN
    ResolvePix(st, pm);
    IF res = NIL OR res = PlttFrnds.noPixmap THEN
      res := NEW(T, id := 2 * pm.pm + 1, st := st)
    END;
    VAR
      i: INTEGER;
      t          := st.succ(NIL, i);
    BEGIN
      IF t = NIL THEN
        res.depth := 1;
        res.bounds := Rect.Empty
      ELSE
        WITH tpm = Palette.ResolvePixmap(t, pm) DO
          res.depth := tpm.depth;
          res.bounds := tpm.bounds
        END
      END
    END;
    RETURN res
  END Apply;

REVEAL
  Oracle = ScrnPixmap.Oracle BRANDED OBJECT
             st: JoinScreen.T;
             jpms: IntRefTbl.Default := NIL;
             pmcount: CARDINAL := 2 (* non-zero, positive, and even *)
           (*
           OVERRIDES
             load    := Register;
             list    := List;
             lookup  := Lookup;
             builtIn := BuiltIn
           *)
           END;

REVEAL
  T = ScrnPixmap.T BRANDED OBJECT
        st: JoinScreen.T;
        map: STypeMapSeq.T := NIL;
      OVERRIDES
      (*
        localize := Localize;
        unload   := Unload
      *)
        free := Free;
      END;

(*
PROCEDURE PixmapRegister(orc: PixmapOracle; 
                         READONLY pm: ScrnPixmap.Raw; 
                         nm: TEXT := NIL): ScrnPixmap.T 
    RAISES {TrestleComm.Failure} =
  BEGIN
    IF orc.st.sts = NIL THEN Crash(); END;
    RETURN orc.st.sts[0].pixmap.load(pm, nm);
  END PixmapRegister;

PROCEDURE PixmapList(orc: PixmapOracle;  pat: TEXT;
                     maxResults: CARDINAL := 1) : REF ARRAY OF TEXT 
    RAISES {TrestleComm.Failure} =
  BEGIN
    IF orc.st.sts = NIL THEN Crash(); END;
    RETURN orc.st.sts[0].pixmap.list(pat, maxResults);
  END PixmapList;

PROCEDURE PixmapLookup(orc: PixmapOracle;
                       name: TEXT) : ScrnPixmap.T
    RAISES {TrestleComm.Failure} =
  BEGIN
    IF orc.st.sts = NIL THEN Crash(); END;
    RETURN orc.st.sts[0].pixmap.lookup(name);
  END PixmapLookup;
  
PROCEDURE PixmapBuiltIn(orc: PixmapOracle;
                        pm: Pixmap.Predefined): ScrnPixmap.T =
  BEGIN
    IF orc.st.sts = NIL THEN Crash(); END;
    RETURN orc.st.sts[0].pixmap.builtIn(pm);
  END PixmapBuiltIn;

EXCEPTION FatalError;

PROCEDURE Crash () =
  <* FATAL FatalError *>
  BEGIN
    RAISE FatalError;
  END Crash;
*)

VAR mu := NEW(MUTEX);

PROCEDURE Free (pm: T) RAISES {TrestleComm.Failure} =
  VAR
    map : STypeMapSeq.T;
    elem: STypeMap.T;
    o   : Oracle        := pm.st.pixmap;
    ra: REFANY;
  BEGIN
    LOCK mu DO
      map := pm.map;
      pm.map := NIL;
      IF o.jpms # NIL THEN EVAL o.jpms.delete(pm.id, ra) END
    END;
    IF map = NIL THEN RETURN END;
    WHILE map.size() # 0 DO
      elem := map.remlo();
      IF elem.pm # NIL THEN elem.pm.free() END
    END
  END Free;

PROCEDURE Create (st: JoinScreen.T; READONLY clip: Rect.T): T =
  VAR res: T := NEW(T, depth := st.depth, bounds := clip, st := st);
      o: Oracle := st.pixmap;
  BEGIN
    LOCK mu DO
      IF st # st.bits THEN res.id := o.pmcount ELSE res.id := -o.pmcount END;
      INC(o.pmcount, 2);
      IF o.jpms = NIL THEN o.jpms := NEW(IntRefTbl.Default).init() END;
      EVAL o.jpms.put(res.id, res)
    END;
    RETURN res
  END Create;

PROCEDURE AddPixmap(p: T; st: ScreenType.T; pm: ScrnPixmap.T) =
  VAR elem := STypeMap.T{st := st, pm := pm};
  BEGIN
    LOCK mu DO
      IF p.map = NIL THEN p.map := NEW(STypeMapSeq.T).init() END;
      p.map.addhi(elem)
    END
  END AddPixmap;

PROCEDURE Resolve (jst: JoinScreen.T; pst: ScreenType.T; n: INTEGER):
  ScrnPixmap.T =
  VAR p: T; o: Oracle := jst.pixmap; ra: REFANY; elem: STypeMap.T;
  BEGIN
    LOCK mu DO
      IF o.jpms = NIL OR NOT o.jpms.get(n, ra) THEN RETURN NIL END;
      p := ra;
      IF p = NIL OR p.map = NIL THEN RETURN NIL END;
      FOR i := 0 TO p.map.size() - 1 DO
        elem := p.map.get(i);
        IF elem.st = pst THEN RETURN elem.pm END
      END;
      RETURN NIL
    END
  END Resolve;

BEGIN END JoinPixmap.

