(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Mon Mar  6 17:48:42 PST 1995 by msm     *)
(*      modified on Thu Apr 29 11:04:24 PDT 1993 by mjordan *)
(*      modified on Fri Apr  2 14:44:49 PST 1993 by steveg *)
(* modified on Mon Feb 24 13:57:19 PST 1992 by muller *)
(* modified on Fri Sep 6 17:25:31 PDT 1991 by gnelson *)
<*PRAGMA LL*>

(* Every installed VBT has a DpyFilter above it to catch messages telling
   the window to move to a new display. *)

MODULE DpyFilter;

IMPORT VBT, InstalledVBT, Trestle, TrestleComm, TextRefTbl,
       OSError, Point, MiscDetail, VBTClass, Thread, Split, Rd, FileRd,
       Env, Text, JoinParent, JoinedVBT, TrestleImpl;

REVEAL
  T = Public BRANDED OBJECT
        enabled         := TRUE;
        pass   : TEXT := NIL;
      OVERRIDES
        misc := Misc;
        init := Init
      END;

VAR
  mu     := NEW(Thread.Mutex);
  inited := FALSE;

PROCEDURE Init (v: T; ch: JoinedVBT.T; enabled: BOOLEAN): T =
  VAR fn: TEXT;
  CONST DefaultFile = "~/.pa_new_display";
  BEGIN
    LOCK mu DO
      IF NOT inited THEN
        ChangeDisplay := VBT.GetMiscCodeType("ChangeDisplay");
        AddDisplay := VBT.GetMiscCodeType("AddDisplay");
        inited := TRUE;
        fn := Env.Get("PA_NEW_DISPLAY");
	IF fn = NIL THEN fn := DefaultFile END;
        TRY
          (* !!! fn := Filename.ExpandTilde(fn); *)
          VAR rd := FileRd.Open(fn);
          BEGIN
            DefaultPassword := Rd.GetLine(rd);
            Rd.Close(rd)
          END
        EXCEPT
          (* Filename.Error, *) OSError.E, Rd.Failure, Rd.EndOfFile, Thread.Alerted =>
        END
      END
    END;
    v.pass := DefaultPassword;
    v.enabled := enabled;
    EVAL JoinParent.T.init(v, ch);
    RETURN v
  END Init;

PROCEDURE New (ch: JoinedVBT.T; enabled := TRUE): T =
  VAR res := NEW(T);
  BEGIN
    EVAL res.init(ch, enabled);
    RETURN res
  END New;

VAR trsls := NEW(TextRefTbl.Default).init();

PROCEDURE Connect (servers: REF ARRAY OF TEXT): Trestle.T =
  VAR
    res: Trestle.T;
    ra : REFANY;
  BEGIN
    IF servers = NIL THEN RETURN NIL END;
    FOR i := FIRST(servers^) TO LAST(servers^) DO
      IF trsls.get(servers[i], ra) THEN RETURN ra END
    END;
    FOR i := FIRST(servers^) TO LAST(servers^) DO
      TRY
        res := Trestle.Connect(servers[i]);
        IF res # NIL THEN EVAL trsls.put(servers[i], res); RETURN res END
      EXCEPT
        TrestleComm.Failure =>  (* skip *)
      END
    END;
    RETURN NIL
  END Connect;

VAR
  DefaultPassword: TEXT := NIL;

PROCEDURE Misc (v: T; READONLY cd: VBT.MiscRec) =
  <* FATAL Split.NotAChild *>
  BEGIN
    IF v.enabled AND (cd.type = ChangeDisplay OR cd.type = AddDisplay) THEN
      VAR
        m   : Message := MiscDetail.ToRef(cd.detail[0]);
        d             := MiscDetail.ToRef(cd.detail[1]);
        trsl          := Connect(m.newDisplay);
      BEGIN
        IF trsl # NIL AND v.pass = NIL
             OR m.oldAuth # NIL AND Text.Equal(m.oldAuth, v.pass) THEN
          VAR
            decor := TrestleImpl.GetDecor(v);
            w := InstalledVBT.NewParent(v);
          BEGIN
            IF w = NIL THEN m.status := FALSE; RETURN END;
            w.pass := m.newAuth;
            IF w.pass # NIL AND Text.Equal(w.pass, "") THEN
              w.pass := NIL
            END;
            IF d # NIL THEN VBT.PutProp(w, d) END;
            TRY
              trsl.attach(w);
              IF decor # NIL THEN
                TrestleImpl.InnerDecorate(trsl, w, decor)
              END;
              IF m.iconic THEN
                trsl.iconize(w)
              ELSE
                trsl.overlap(w, m.screen, Point.T{m.x, m.y})
              END;
              m.status := TRUE;
              IF cd.type = ChangeDisplay AND v.parent # NIL THEN
                Split.Delete(v.parent, v)
              END
            EXCEPT
              TrestleComm.Failure => m.status := FALSE; JoinParent.Rem(w)
            END
          END
        ELSE
          m.status := FALSE
        END
      END
    ELSE
      Public.misc(v, cd)
    END
  END Misc;

PROCEDURE Parent (v: VBT.T): T =
  BEGIN
    LOOP TYPECASE v OF T (p) => RETURN p ELSE v := v.parent END END
  END Parent;

PROCEDURE SetEnabled (ch: VBT.T; enabled := TRUE) =
  VAR v := Parent(ch);
  BEGIN
    IF v # NIL THEN v.enabled := enabled END
  END SetEnabled;

PROCEDURE GetEnabled (ch: VBT.T): BOOLEAN =
  VAR v := Parent(ch);
  BEGIN
    IF v # NIL THEN RETURN v.enabled ELSE RETURN TRUE END
  END GetEnabled;

BEGIN
END DpyFilter.


