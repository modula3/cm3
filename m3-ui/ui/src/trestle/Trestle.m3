(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Fri Mar 31 15:09:46 PST 1995 by msm      *)
(*      modified on Tue Jan 31 09:48:57 PST 1995 by kalsow   *)
(*      modified on Thu Apr 29 11:14:17 PDT 1993 by mjordan  *)
(*      modified on Thu Apr 22 15:02:39 PDT 1993 by steveg   *)
(*      modified on Mon Feb 24 13:56:12 PST 1992 by muller   *)
(*      modified on Sat Nov  2 15:59:41 PST 1991 by gnelson  *)
<*PRAGMA LL*>

UNSAFE MODULE Trestle EXPORTS Trestle, TrestleImpl;

IMPORT Thread, Env, TrestleClass, VBT, TrestleComm, Params, VBTClass,
       ScrnColorMap, Point, Rect, Region, ScrnPixmap, Split,
       VBTRep, JoinParent, ProperSplit, InstallQueue, InstalledVBT,
       TrestleConf, TrestleOS;

FROM TrestleClass IMPORT InstallRef, Decoration;

REVEAL 
  TrestleClass.RootVBT = ProperSplit.T BRANDED OBJECT END;
  
(*EXCEPTION Unimplemented;*)

REVEAL User = TrestleConf.User BRANDED OBJECT trsl: T END;

REVEAL
  T = TPublic BRANDED OBJECT
        lastQuery: IParent     := NIL;
        lastId: ScreenID := NoScreen;
        lastDelta: Point.T;
      OVERRIDES
        delete := DeleteDefault;
      END;

VAR
  <* LL >= {TrestleClass.connectMu} *>
  default: T := NIL;
  defaultUser := "Unknown user";
  initMu := NEW(MUTEX);
  inited, confEnabled := FALSE;

PROCEDURE SetConfCtl(t: T; on: BOOLEAN) =
  BEGIN
    t.confEnabled := on
  END SetConfCtl;

PROCEDURE DeleteDefault(t: T; ch: VBT.T) =
  <* FATAL Split.NotAChild *>
  BEGIN
    Split.Delete(t, ch)
  END DeleteDefault;

PROCEDURE Init() =
  BEGIN
    IF NOT inited THEN
      LOCK initMu DO
      	IF NOT inited THEN
          TrestleOS.Init();
	  (* Add other classes of window system here *)
          TrestleConf.Init(CreateUser);
          defaultUser := Env.Get("ARGOUSER");
          IF defaultUser = NIL THEN defaultUser := TrestleOS.UserName() END;
          confEnabled := Env.Get("ARGOENABLED") # NIL;
          inited := TRUE
        END
      END
    END
  END Init;
  
PROCEDURE DeleteHook (ch: VBT.T) =
  VAR ir: InstallRef := VBT.GetProp(ch, TYPECODE(InstallRef));
  BEGIN
    DEC(ir.installCount);
    IF ir.installCount = 0 THEN Thread.Broadcast(ir.c) END;
    VBT.RemProp(ch, TYPECODE(InstallRef))
  END DeleteHook;

TYPE
  InstallObject = Closure OBJECT
    trsl: T;
    v: VBT.T;
    dec: Decoration
  OVERRIDES
    apply := DoInstall
  END;

PROCEDURE Install(
    v: VBT.T;
    applName: TEXT := NIL;
    inst: TEXT := NIL;
    windowTitle: TEXT := NIL;
    iconTitle: TEXT := NIL;
    bgColorR: REAL := 1.0;
    bgColorG: REAL := 1.0;
    bgColorB: REAL := 1.0;
    iconWindow: VBT.T := NIL;
    trsl: T := NIL)
  RAISES {TrestleComm.Failure} =
  BEGIN
    PreAttach(v, trsl);
    Fork(NEW(InstallObject, trsl := trsl, v := v,
             dec := NEW(Decoration, 
               applName := applName, inst := inst,
               windowTitle := windowTitle, iconTitle := iconTitle,
               bgColorR := bgColorR, bgColorG := bgColorG, 
               bgColorB := bgColorB, iconWindow := iconWindow)))
  END Install;

PROCEDURE Decorate (v          : VBT.T;
                    instance   : TEXT    := NIL;
                    windowTitle: TEXT    := NIL;
                    iconTitle  : TEXT    := NIL;
                    bgColorR   : REAL    := -1.0;
                    bgColorG   : REAL    := -1.0;
                    bgColorB   : REAL    := -1.0;
                    applName   : TEXT    := NIL;
                    iconWindow : VBT.T   := NIL   )
  RAISES {TrestleComm.Failure} =
  VAR
    ch  : VBT.T;
    trsl: T;
  BEGIN
    IF RootChild(v, trsl, ch) THEN
      InnerDecorate(
        trsl, ch, NEW(Decoration, applName := applName, inst := instance,
                      windowTitle := windowTitle, iconTitle := iconTitle,
                      bgColorR := bgColorR, bgColorG := bgColorG,
                      bgColorB := bgColorB, iconWindow := iconWindow))
    END
  END Decorate;

PROCEDURE GetDecor (ch: VBT.T): Decoration =
  VAR res: Decoration := VBT.GetProp(ch, TYPECODE(Decoration));
  BEGIN
    RETURN res
  END GetDecor;

TYPE Join = InstalledVBT.Join OBJECT app: App END;

PROCEDURE InnerDecorate (trsl: T; ch: VBT.T; new: Decoration)
  RAISES {TrestleComm.Failure} =
  VAR old := GetDecor(ch);
  BEGIN
    IF old = NIL THEN
      IF new.applName = NIL THEN new.applName := Params.Get(0) END;
      IF new.windowTitle = NIL THEN
        IF new.inst = NIL THEN
          new.windowTitle := new.applName
        ELSE
          new.windowTitle := new.applName & " " & new.inst
        END
      END;
      IF new.iconTitle = NIL THEN
        IF new.inst = NIL THEN
          new.iconTitle := new.applName
        ELSE
          new.iconTitle := new.inst
        END
      END;
      IF new.inst = NIL THEN new.inst := Env.Get("WINSTANCE"); END;
      IF new.inst = NIL THEN new.inst := "" END;
      IF new.bgColorR < 0.0 THEN new.bgColorR := 1.0 END;
      IF new.bgColorG < 0.0 THEN new.bgColorG := 1.0 END;
      IF new.bgColorB < 0.0 THEN new.bgColorB := 1.0 END
    ELSE
      IF new.applName = NIL THEN new.applName := old.applName END;
      IF new.windowTitle = NIL THEN new.windowTitle := old.windowTitle END;
      IF new.iconTitle = NIL THEN new.iconTitle := old.iconTitle END;
      IF new.iconWindow = NIL THEN new.iconWindow := old.iconWindow END;
      IF new.inst = NIL THEN new.inst := old.inst END;
      IF new.bgColorR < 0.0 THEN new.bgColorR := old.bgColorR END;
      IF new.bgColorG < 0.0 THEN new.bgColorG := old.bgColorG END;
      IF new.bgColorB < 0.0 THEN new.bgColorB := old.bgColorB END
    END;
    TYPECASE ch OF
      IParent(ip) =>
        VAR
          j: Join := JoinParent.Child(ip);
          app := j.app;
          trs: T;
          jch: VBT.T;
          par := JoinParent.Succ(j, NIL);
          allFail := par # NIL;
        BEGIN
          app.decorated := TRUE;
          WHILE par # NIL DO
            VBT.PutProp(par, new);
            IF RootChild(par, trs, jch) THEN
              TRY
                trs.decorate(jch, old, new);
                allFail := FALSE;
               EXCEPT
                 TrestleComm.Failure =>
               END (* TRY *)
            END;
            par := JoinParent.Succ(j, par)
          END;
          IF allFail THEN RAISE TrestleComm.Failure END
        END
    ELSE
      VBT.PutProp(ch, new);
      trsl.decorate(ch, old, new)
    END
  END InnerDecorate;

PROCEDURE GetDecoration(v: VBT.T;
  VAR instance, windowTitle, iconTitle, applName: TEXT;
  VAR bgColorR, bgColorG, bgColorB: REAL;
  VAR iconWindow: VBT.T): BOOLEAN =
  VAR trsl: T; ch: VBT.T; old: Decoration; BEGIN
    IF NOT RootChild(v, trsl, ch) THEN RETURN FALSE END;
    old := GetDecor(ch);
    IF old = NIL THEN RETURN FALSE END;
    instance := old.inst;
    windowTitle := old.windowTitle;
    iconTitle := old.iconTitle;
    applName := old.applName;
    bgColorR := old.bgColorR;
    bgColorG := old.bgColorG;
    bgColorB := old.bgColorB;
    iconWindow := old.iconWindow;
    RETURN TRUE
  END GetDecoration;

PROCEDURE RootChild (v: VBT.T; VAR trsl: T; VAR ch: VBT.T): BOOLEAN =
  BEGIN
    LOOP
      TYPECASE v.parent OF
        NULL => RETURN FALSE
      | T (tr) => trsl := tr; ch := v; RETURN TRUE
      ELSE
        v := v.parent
      END
    END
  END RootChild;

PROCEDURE DoInstall (self: InstallObject) =
  VAR
    trsl: T;
    ch  : VBT.T;
  BEGIN
    TRY
      InnerAttach(self.v, self.trsl)
    EXCEPT
      TrestleComm.Failure => Delete(self.v); RETURN
    END;
    TRY
      IF RootChild(self.v, trsl, ch) THEN
        InnerDecorate(trsl, ch, self.dec)
      END;
      MoveNear(self.v, NIL);
    EXCEPT
      TrestleComm.Failure => Delete(self.v)
    END;
  END DoInstall;

PROCEDURE PreAttach (v: VBT.T; VAR trsl: T)
  RAISES {TrestleComm.Failure} <* LL.sup <= VBT.mu *> =
  BEGIN
    IF trsl = NIL THEN trsl := Default() END;
    LOCK TrestleClass.closeMu DO
      IF trsl.closed THEN RAISE TrestleComm.Failure END
    END;
    LOCK v DO
      VAR ir: InstallRef := VBTClass.GetProp(v, TYPECODE(InstallRef));
      BEGIN
        IF ir = NIL THEN
          ir :=
            NEW(InstallRef, installCount := 1, c := NEW(Thread.Condition));
          VBTClass.PutProp(v, ir)
        ELSE
          INC(ir.installCount)
        END
      END
    END
  END PreAttach;

PROCEDURE Attach(v: VBT.T; trsl: T) RAISES {TrestleComm.Failure} =
  BEGIN
    PreAttach(v, trsl);
    InnerAttach(v, trsl)
  END Attach;

TYPE AppState = {Init, Run, Destroy};
     AppCond = {Unknown, Overlap, MoveNear, Iconize, Offscreen};

REVEAL
  App = AppPublic BRANDED OBJECT
          installOK                      := FALSE;
          state                          := AppState.Init;
          cond                           := AppCond.Unknown;
          condpal  : App                 := NIL;
          id       : ScreenID;
          delta    : Point.T             := Point.Origin;
          join     : InstalledVBT.Join;
          decorated                      := FALSE;
        OVERRIDES
          add      := AddApp;
          delete   := DeleteApp;
          suspend  := SuspendApp;
          activate := ActivateApp;
        END;

TYPE
  NoConfApp = App OBJECT
              OVERRIDES
                init    := NoConfInit;
                destroy := NoConfDestroy
              END;

PROCEDURE NoConfInit(a: App; u: TrestleConf.User) =
  BEGIN
    a.add(u);
    a.activate(u)
  END NoConfInit;

PROCEDURE NoConfDestroy(a: App) =
  BEGIN
    a.suspend(a.primary);
    a.delete(a.primary)
  END NoConfDestroy; 

TYPE
  IParent = InstalledVBT.T OBJECT
              active           := FALSE;
              iconic           := FALSE;
              evermapped       := FALSE;
            OVERRIDES
              screenOf := IParentScreenOf;
              reshape  := IParentReshape;
              rescreen := IParentRescreen
            END;

PROCEDURE IParentScreenOf (v: IParent; ch: VBT.T; READONLY pt: Point.T):
  ScreenOfRec =
  VAR res := InstalledVBT.T.screenOf(v, ch, pt);
  BEGIN
    IF res.trsl # NIL THEN
      res.trsl.lastQuery := v;
      res.trsl.lastId := res.id;
      res.trsl.lastDelta := Point.Sub(res.q, pt)
    END;
    RETURN res
  END IParentScreenOf;

PROCEDURE IParentReshape (v: IParent; READONLY cd: VBT.ReshapeRec) =
  VAR j: Join := JoinParent.Child(v);
  BEGIN
    IF NOT Rect.IsEmpty(cd.new) THEN
      v.iconic := FALSE
    ELSIF v.active AND j # NIL AND j.app.cond # AppCond.Iconize THEN
      v.iconic := TRUE
    END;
    InstalledVBT.T.reshape(v, cd)
  END IParentReshape;

PROCEDURE IParentRescreen (v: IParent; READONLY cd: VBT.RescreenRec) =
  BEGIN
    IF v.active THEN v.iconic := TRUE END;
    InstalledVBT.T.rescreen(v, cd)
  END IParentRescreen;

PROCEDURE InnerAttach(v: VBT.T; trsl: T) 
  RAISES {TrestleComm.Failure} <* LL = {VBT.mu} *> =
  VAR app: App; join := NEW(Join);
  BEGIN
    InstalledVBT.InitChild(join, v, DeleteHook);
    IF trsl.confEnabled THEN
      app := NEW(App, join := join)
    ELSE
      app := NEW(NoConfApp, join := join)
    END;
    join.app := app;
    app.primary := trsl.conf;
    app.init(trsl.conf);
    IF NOT app.installOK THEN RAISE TrestleComm.Failure END;
    app.state := AppState.Run
  END InnerAttach;

PROCEDURE AddApp (a: App; cuser: TrestleConf.User) =
  BEGIN
    IF cuser # NIL AND a.state # AppState.Destroy THEN
      WITH user = NARROW(cuser, User),
           trsl = user.trsl            DO
        TRY
          VAR
            par               := NEW(IParent);
            decor: Decoration := NIL;
          BEGIN
            VAR
              trs: T;
              ch : VBT.T;
            BEGIN
              IF a.state = AppState.Run AND RootChild(a.join, trs, ch) THEN
                decor := GetDecor(ch)
              END
            END;
            InstalledVBT.InitParent(par, a.join);
            IF user = a.primary THEN
              JoinParent.SetInput(a.join, par)
            END;
            trsl.attach(par);
            a.installOK := TRUE;
            IF a.state = AppState.Run THEN
              IF decor # NIL THEN
                VBT.PutProp(par, decor);
                trsl.decorate(par, NIL, decor)
              END;
              IF a.cond = AppCond.Offscreen THEN
              ELSIF a.cond # AppCond.Unknown THEN
                trsl.iconize(par)
              END
            END
          END
        EXCEPT
          TrestleComm.Failure =>
        END
      END
    END
  END AddApp;

PROCEDURE Delete (v: VBT.T) RAISES {} =
  VAR
    trsl: T;
    ch  : VBT.T;
  BEGIN
    IF RootChild(v, trsl, ch) THEN
      TYPECASE ch OF
        IParent (ip) =>
          VAR
            join: Join := JoinParent.Child(ip);
            app := join.app;
          BEGIN
            IF app = NIL THEN
              trsl.delete(ch)
            ELSE
              app.state := AppState.Destroy;
              app.destroy()
            END
          END
      ELSE
        trsl.delete(ch)
      END
    END
  END Delete;

PROCEDURE ChildApp(v: VBT.T): App =
  VAR
    trsl: T;
    ch  : VBT.T;
  BEGIN
    IF RootChild(v, trsl, ch) THEN
      TYPECASE ch OF
        IParent (ip) =>
          VAR
            join: Join := JoinParent.Child(ip);
            app := join.app;
          BEGIN
            RETURN app
          END
      ELSE
          RETURN NIL
      END
    ELSE
      RETURN NIL
    END
  END ChildApp;

PROCEDURE LocateTrslForUser (a: App; user: User; VAR par: IParent): T =
  VAR
    trsl: T;
    ch  : VBT.T;
    prnt: IParent := NIL;
  BEGIN
    IF user # NIL AND RootChild(a.join, trsl, ch) THEN
      prnt := JoinParent.Succ(a.join, NIL);
      LOOP
        IF prnt = NIL THEN EXIT END;
        IF RootChild(prnt, trsl, ch) THEN
          IF user.trsl = trsl THEN par := prnt; RETURN trsl END
        END;
        prnt := JoinParent.Succ(a.join, prnt)
      END
    END;
    par := NIL;
    RETURN NIL
  END LocateTrslForUser;

PROCEDURE DeleteApp (a: App; cuser: TrestleConf.User) =
  VAR
    par : IParent;
    trsl          := LocateTrslForUser(a, cuser, par);
  BEGIN
    IF trsl # NIL THEN trsl.delete(par) END
  END DeleteApp;

PROCEDURE SuspendApp (a: App; cuser: TrestleConf.User) =
  VAR
    par : IParent;
    trsl          := LocateTrslForUser(a, cuser, par);
  BEGIN
    IF trsl # NIL THEN
      par.active := FALSE;
      IF a.cond # AppCond.Offscreen THEN
        TRY trsl.iconize(par) EXCEPT TrestleComm.Failure => END
      END
    END
  END SuspendApp;

PROCEDURE ActivateApp (a: App; cuser: TrestleConf.User) =
  VAR
    par, wpar: IParent;
    trsl               := LocateTrslForUser(a, cuser, par);
  BEGIN
    IF trsl # NIL THEN
      par.active := TRUE;
      IF a.state = AppState.Run AND a.cond # AppCond.Unknown THEN
        TRY
          IF a.cond = AppCond.Offscreen THEN
          ELSIF par.iconic THEN  (* skip *)
          ELSIF par.evermapped OR a.condpal = a THEN
            trsl.moveNear(par, par);
            par.evermapped := TRUE
          ELSIF a.cond = AppCond.MoveNear THEN
            IF a.condpal = NIL
                 OR trsl # LocateTrslForUser(a.condpal, cuser, wpar) THEN
              trsl.moveNear(par, NIL)
            ELSE
              trsl.moveNear(par, wpar);
              par.evermapped := TRUE
            END
          ELSIF a.cond = AppCond.Overlap THEN
            IF a.condpal = NIL
                 OR trsl # LocateTrslForUser(a.condpal, cuser, wpar)
                 OR wpar = NIL THEN
              trsl.overlap(par, a.id, a.delta);
              par.evermapped := TRUE
            ELSIF NOT wpar.iconic THEN
              WITH sor = ScreenOf(wpar, Rect.NorthWest(VBT.Domain(wpar))) DO
                trsl.overlap(par, sor.id, Point.Add(sor.q, a.delta))
              END;
              par.evermapped := TRUE
            END
          END
        EXCEPT
          TrestleComm.Failure =>
        END
      END;
      par.evermapped := TRUE;
    END;
  END ActivateApp;

PROCEDURE ScreenOf(  
  v: VBT.T;
  READONLY pt: Point.T): ScreenOfRec RAISES {} =
  BEGIN
    LOCK v DO
      WITH p = v.parent DO
        IF p = NIL THEN 
          RETURN ScreenOfRec{id := NoScreen, q := pt, 
                   trsl := NIL, dom := Rect.Empty}
        ELSE
          RETURN p.screenOf(v, pt)
        END
      END
    END
  END ScreenOf;

PROCEDURE Overlap (v: VBT.T; id: ScreenID; READONLY nw: Point.T)
  RAISES {TrestleComm.Failure} =
  VAR
    trsl: T;
    ch  : VBT.T;
  BEGIN
    IF NOT RootChild(v, trsl, ch) THEN RETURN END;
    TYPECASE ch OF
      IParent (ip) =>
        VAR
          j         : Join    := JoinParent.Child(ip);
          a                   := j.app;
          par       : IParent := JoinParent.Succ(j, NIL);
          trs       : T;
          jch       : VBT.T;
          allFail             := par # NIL;
          menuParent          := trsl.lastQuery;
          menu := menuParent # NIL AND NOT a.decorated AND id = trsl.lastId
                    AND id # NoScreen;
          menuNW: Point.T;
        BEGIN
          a.cond := AppCond.Overlap;
          IF menu THEN
            menuNW := Point.Sub(nw, Point.Add(Rect.NorthWest(
                                                VBT.Domain(menuParent)),
                                              trsl.lastDelta));
            VAR j: Join := JoinParent.Child(menuParent); BEGIN
              IF j # NIL THEN a.condpal := j.app ELSE a.condpal := NIL END
            END;
            a.delta := menuNW
          ELSE
            a.condpal := NIL;
            a.delta := nw
          END;
          a.id := id;
          WHILE par # NIL DO
            IF RootChild(par, trs, jch) THEN
              TRY
                IF par.active THEN
                  IF NOT menu THEN
                    trs.overlap(jch, id, nw);
                    par.evermapped := TRUE
                  ELSE
                    VAR match: IParent;
                    BEGIN
                      IF LocateParent(trs, menuParent, match) THEN
                        IF match.iconic THEN
                          par.evermapped := FALSE
                        ELSE
                          WITH sor = ScreenOf(match, Rect.NorthWest(
                                                       VBT.Domain(match))) DO
                            trs.overlap(
                              jch, sor.id, Point.Add(sor.q, menuNW));
                            par.evermapped := TRUE
                          END
                        END
                      END
                    END
                  END
                ELSE
                  trs.iconize(jch);
                  par.evermapped := FALSE
                END;
                allFail := FALSE
              EXCEPT
                TrestleComm.Failure =>
              END
            END;
            par := JoinParent.Succ(j, par)
          END;
          IF allFail THEN RAISE TrestleComm.Failure END
        END
    ELSE
      trsl.overlap(ch, id, nw)
    END
  END Overlap;

PROCEDURE LocateParent (t: T; v: IParent; VAR m: IParent): BOOLEAN =
  VAR
    j  : Join  := JoinParent.Child(v);
    trs: T;
    ch : VBT.T;
  BEGIN
    m := JoinParent.Succ(j, NIL);
    WHILE m # NIL DO
      IF RootChild(m, trs, ch) AND trs = t THEN RETURN TRUE END;
      m := JoinParent.Succ(j, m)
    END;
    RETURN FALSE
  END LocateParent;

PROCEDURE Iconize (v: VBT.T) RAISES {TrestleComm.Failure} =
  VAR
    trsl: T;
    ch  : VBT.T;
  BEGIN
    IF NOT RootChild(v, trsl, ch) THEN RETURN END;
    TYPECASE ch OF
      IParent (ip) =>
        VAR
          j      : Join    := JoinParent.Child(ip);
          a      := j.app;
          par    : IParent := JoinParent.Succ(j, NIL);
          trs    : T;
          jch    : VBT.T;
          allFail          := par # NIL;
        BEGIN
          a.cond := AppCond.Iconize;
          WHILE par # NIL DO
            IF RootChild(par, trs, jch) THEN
              TRY
                trs.iconize(jch);
                allFail := FALSE
              EXCEPT
                TrestleComm.Failure =>
              END
            END;
            par := JoinParent.Succ(j, par)
          END;
          IF allFail THEN RAISE TrestleComm.Failure END
        END
    ELSE
      trsl.iconize(ch)
    END
  END Iconize;

PROCEDURE MoveNear (v, w: VBT.T) RAISES {TrestleComm.Failure} =
  VAR
    trsl: T;
    ch  : VBT.T;
  BEGIN
    IF NOT RootChild(v, trsl, ch) THEN RETURN END;
    TYPECASE ch OF
      IParent (ip) =>
        VAR
          j      : Join    := JoinParent.Child(ip);
          a                := j.app;
          wa     : App     := NIL;
          par    : IParent := JoinParent.Succ(j, NIL);
          wpar   : IParent := NIL;
          trs    : T;
          jch    : VBT.T;
          allFail          := par # NIL;
        BEGIN
          a.cond := AppCond.MoveNear;
          IF w # NIL AND RootChild(w, trs, w) AND ISTYPE(w, IParent) THEN
            wa := NARROW(JoinParent.Child(w), Join).app
          END;
          a.condpal := wa;
          WHILE par # NIL DO
            IF RootChild(par, trs, jch) THEN
              TRY
                IF par.active THEN
                  IF wa # NIL
                       AND LocateTrslForUser(wa, trs.conf, wpar) = trs THEN
                    trs.moveNear(jch, wpar)
                  ELSE
                    trs.moveNear(jch, NIL)
                  END;
                  par.evermapped := TRUE
                ELSE
                  trs.iconize(jch);
                  par.evermapped := FALSE
                END (* IF *);
                allFail := FALSE
              EXCEPT
                TrestleComm.Failure =>
              END
            END;
            par := JoinParent.Succ(j, par)
          END;
          IF allFail THEN RAISE TrestleComm.Failure END
        END
    ELSE
      trsl.moveNear(ch, w)
    END
  END MoveNear;

PROCEDURE InstallOffscreen(
    v: VBT.T;
    width, height: CARDINAL;
    st: VBT.ScreenType) RAISES {TrestleComm.Failure} =
  VAR trsl: T; ch: VBT.T; a: App; BEGIN
    IF NOT RootChild(v, trsl, ch) OR ch.st # NIL THEN Crash() END;
    WHILE st # NIL AND ISTYPE(st, VBTRep.OffscreenType) DO
      st := NARROW(st, VBTRep.OffscreenType).st
    END;
    IF ISTYPE(ch, IParent) THEN
      a := NARROW(JoinParent.Child(ch), Join).app;
      a.cond := AppCond.Offscreen
    END;
    trsl.installOffscreen(ch, width, height, st)
  END InstallOffscreen;
 
VAR 
  mu := NEW(MUTEX);
  c := NEW(Thread.Condition);
  workQ := InstallQueue.Empty;
  (* workQ is protected by mu, and contains the closure that need to
     be activated.  c is signalled when workQ becomes non-empty.  *)
  worker, pinger: Thread.T := NIL;
  (* worker is the Thread handle of the thread doing Work, or NIL.
     pinger is the Thread handle of the thread timing out Work, or NIL.  *)

TYPE Closure = InstallQueue.Closure;

PROCEDURE Fork(cl: Closure) =
  VAR mustSignal: BOOLEAN;
  BEGIN
    LOCK mu DO
      mustSignal := InstallQueue.IsEmpty(workQ);
      InstallQueue.Insert(workQ, cl);
      IF worker = NIL THEN 
        worker := Thread.Fork(NEW(Thread.SizedClosure, apply := Work, 
          stackSize := 20000)) 
      END;
      IF pinger = NIL THEN 
        pinger := Thread.Fork(NEW(Thread.SizedClosure, apply := Ping, 
          stackSize := 20000)) 
      END
    END;
    IF mustSignal THEN Thread.Signal(c) END
  END Fork;

PROCEDURE Work(<*UNUSED*>self: Thread.Closure): REFANY RAISES {} =
  <*FATAL InstallQueue.Exhausted*>
  VAR cl: Closure; live: INTEGER;
  BEGIN
    LOOP
      live := 2;
      LOCK mu DO
        WHILE InstallQueue.IsEmpty(workQ) AND live # 0 DO 
          Thread.Wait(mu, c); DEC(live)
        END;
        IF InstallQueue.IsEmpty(workQ) THEN worker := NIL; RETURN NIL END;
        cl := InstallQueue.Remove(workQ); 
      END;
      LOCK VBT.mu DO cl.apply() END
    END
  END Work;

PROCEDURE Ping(<*UNUSED*>self: Thread.Closure): REFANY RAISES {} =
  BEGIN
    LOOP
      LOCK mu DO
        IF worker = NIL THEN pinger := NIL; RETURN NIL END
      END;
      Thread.Pause(5.0D0);
      Thread.Signal(c)
    END
  END Ping;

PROCEDURE SetDefault (t: T) =
  BEGIN
    Init();
    LOCK TrestleClass.connectMu DO default := t END
  END SetDefault;

PROCEDURE Default (): T RAISES {TrestleComm.Failure} =
  BEGIN
    Init();
    LOCK TrestleClass.connectMu DO
      LOCK TrestleClass.closeMu DO
        IF default # NIL AND NOT default.closed THEN RETURN default END
      END;
      default := TrestleClass.Connect(NIL);
      default.confEnabled := confEnabled;
      default.conf := NEW(User, displayName := default.trestleId(),
                          name := defaultUser, trsl := default);
      default.conf.register();
      RETURN default
    END
  END Default;

PROCEDURE Connect(inst: TEXT := NIL): T RAISES {TrestleComm.Failure} =
  <* LL.sup <= TrestleClass.connectMu *>
  VAR res: T;
  BEGIN
    Init();
    IF inst = NIL THEN 
      RETURN Default()
    ELSE
      res := TrestleClass.Connect(inst);
      res.confEnabled := confEnabled;
      res.conf := NEW(User, displayName := res.trestleId(),
                      name := defaultUser, trsl := res);
      res.conf.register();
      RETURN res
    END
  END Connect;

PROCEDURE CreateUser (user, display: TEXT): TrestleConf.User =
  VAR trsl: T;
  BEGIN
    TRY
      trsl := TrestleClass.Connect(display);
      trsl.confEnabled := confEnabled;
      trsl.conf :=
        NEW(User, name := user, displayName := display, trsl := trsl);
      RETURN trsl.conf
    EXCEPT
      TrestleComm.Failure => RETURN NIL
    END
  END CreateUser;

PROCEDURE AwaitDelete(v: VBT.T) = 
  VAR ir: InstallRef;
  BEGIN
    LOCK VBT.mu DO
      ir := VBT.GetProp(v, TYPECODE(InstallRef));
      IF ir = NIL THEN RETURN END;
      WHILE ir.installCount > 0 DO Thread.Wait(VBT.mu, ir.c) END
    END
  END AwaitDelete;
  
<* UNUSED *>
PROCEDURE SetColorMap(v: VBT.T; cm: ScrnColorMap.T) RAISES {} = 
  VAR trsl: T; ch: VBT.T; BEGIN
    IF RootChild(v, trsl, ch) THEN trsl.setColorMap(ch, cm) END
  END SetColorMap;

PROCEDURE Capture(
    id: ScreenID;
    READONLY clip: Rect.T;
    VAR  br: Region.T;
    trsl: T := NIL)
    : ScrnPixmap.T
  RAISES {TrestleComm.Failure} = BEGIN 
    IF trsl = NIL THEN trsl := Default() END;
    RETURN trsl.captureScreen(id, clip, br)
  END Capture;

PROCEDURE GetScreens(trsl: T := NIL): ScreenArray RAISES {TrestleComm.Failure} =
  BEGIN 
    IF trsl = NIL THEN trsl := Default() END;
    RETURN trsl.getScreens() 
  END GetScreens;

PROCEDURE AllCeded(trsl: T := NIL): BOOLEAN RAISES {TrestleComm.Failure} = 
  BEGIN 
    IF trsl = NIL THEN trsl := Default() END;
    RETURN trsl.allCeded() 
  END AllCeded;

PROCEDURE TickTime(trsl: T := NIL): INTEGER = 
  BEGIN 
    TRY
      IF trsl = NIL THEN trsl := Default() END;
      RETURN trsl.tickTime() 
    EXCEPT
      TrestleComm.Failure => RETURN 1
    END
  END TickTime;

PROCEDURE UpdateChalk (ch: VBT.T) =
  VAR trs: T;
  BEGIN
    IF ch = NIL OR NOT RootChild(ch, trs, ch) OR trs = NIL THEN RETURN END;
    TYPECASE ch OF
      NULL =>                    (*skip*)
    | InstalledVBT.T (iv) =>
        VAR
          ch    := JoinParent.Child(iv);
          p     := JoinParent.Succ(ch, NIL);
          chalk := trs.trestleId(iv);
        BEGIN
          WHILE p # NIL DO
            VAR trsl: T := p.parent;
            BEGIN
              IF trsl # NIL THEN trsl.updateChalk(p, chalk) END;
              p := JoinParent.Succ(ch, p)
            END
          END
        END
    ELSE
    END
  END UpdateChalk;

PROCEDURE UpdateBuddies (ch: VBT.T) =
  BEGIN
    TYPECASE ch OF
      NULL =>                    (*skip*)
    | InstalledVBT.T (iv) =>
        VAR
          ch                            := JoinParent.Child(iv);
          p                             := JoinParent.Succ(ch, NIL);
          cnt                           := 0;
          trsls, ids: REF ARRAY OF TEXT;
        BEGIN
          WHILE p # NIL DO INC(cnt); p := JoinParent.Succ(ch, p) END;
          trsls := NEW(REF ARRAY OF TEXT, cnt);
          ids := NEW(REF ARRAY OF TEXT, cnt);
          p := JoinParent.Succ(ch, NIL);
          cnt := 0;
          WHILE p # NIL DO
            VAR trsl: T := p.parent;
            BEGIN
              IF trsl = NIL THEN
                trsls[cnt] := "";
                ids[cnt] := "0"
              ELSE
                trsls[cnt] := trsl.trestleId(p);
                ids[cnt] := trsl.windowId(p)
              END;
              INC(cnt);
              p := JoinParent.Succ(ch, p)
            END
          END;
          p := JoinParent.Succ(ch, NIL);
          WHILE p # NIL DO
            VAR trsl: T := p.parent;
            BEGIN
              IF trsl # NIL THEN trsl.updateBuddies(p, trsls^, ids^) END;
              p := JoinParent.Succ(ch, p)
            END
          END
        END
    ELSE
    END
  END UpdateBuddies;
  
EXCEPTION FatalError;

PROCEDURE Crash() =
  <*FATAL FatalError*>
  BEGIN
    RAISE FatalError
  END Crash;

(* PROCEDURE SetScreens(
    sa: ScreenArray;
    trsl: T := NIL)
    : BOOLEAN
  RAISES {TrestleComm.Failure, Unimplemented} = BEGIN RAISE Unimplemented END SetScreens; *)

(* PROCEDURE Swap(v, w: VBT.T) RAISES {TrestleComm.Failure, Unimplemented} = BEGIN RAISE Unimplemented END Swap; *)

(* PROCEDURE GetName(v: VBT.T): TEXT RAISES {TrestleComm.Failure} = 
  BEGIN RAISE Unimplemented END GetName; *)

(* PROCEDURE NameList(
    nm: TEXT;
    trsl: T := NIL)
    : REF ARRAY OF TEXT
  RAISES {TrestleComm.Failure} = BEGIN RAISE Unimplemented END NameList; *)

(* PROCEDURE MoveNearByName(v: VBT.T; nm: TEXT) RAISES {TrestleComm.Failure} = 
  BEGIN RAISE Unimplemented END MoveNearByName; *)

(* PROCEDURE SwapByName(
    v: VBT.T;
    nm: TEXT)
  RAISES {TrestleComm.Failure, Unimplemented} = 
  BEGIN RAISE Unimplemented END SwapByName; *)

(* PROCEDURE DeleteByName(nm: TEXT; trsl: T := NIL) RAISES {TrestleComm.Failure} = BEGIN RAISE Unimplemented END DeleteByName; *)

(* PROCEDURE TakeOver(
    id: ScreenID;
    v: VBT.T;
    trsl: T := NIL)
  RAISES {TrestleComm.Failure, Unimplemented} = 
  BEGIN RAISE Unimplemented END TakeOver; *)

(* PROCEDURE Restore(
    id: ScreenID;
    v: VBT.T)
  RAISES {TrestleComm.Failure, Unimplemented} = 
  BEGIN RAISE Unimplemented END Restore; *)

(* PROCEDURE TakeOverMouse(
    id: ScreenID;
    v: VBT.T;
    trsl: T := NIL)
  RAISES {TrestleComm.Failure} = 
  BEGIN RAISE Unimplemented END TakeOverMouse; *)

(* PROCEDURE ReleaseMouse(id: ScreenID; v: VBT.T) RAISES {TrestleComm.Failure} = 
  BEGIN RAISE Unimplemented END ReleaseMouse; *)

(* PROCEDURE SetHighlight(
    id: ScreenID;
    READONLY r: Rect.T;
    border: CARDINAL;
    trsl: T := NIL)
  RAISES {TrestleComm.Failure} = BEGIN RAISE Unimplemented END SetHighlight; *)

(* PROCEDURE AddParent(
    prnt: VBT.T;
    id: ScreenID;
    trsl: T := NIL)
  RAISES {TrestleComm.Failure, Unimplemented} = 
  BEGIN RAISE Unimplemented END AddParent; *)

(* PROCEDURE RemParent(
    prnt: VBT.T;
    id: ScreenID;
    trsl: T := NIL)
  RAISES {TrestleComm.Failure, Unimplemented} = 
  BEGIN RAISE Unimplemented END RemParent; *)

(* PROCEDURE WarpCursor(
    id: ScreenID;
    READONLY pt: Point.T;
    trsl: T := NIL)
  RAISES {TrestleComm.Failure} = BEGIN RAISE Unimplemented END WarpCursor; *)

(* PROCEDURE LastCeded(
    trsl: T := NIL)
    : VBT.TimeStamp
  RAISES {TrestleComm.Failure, Unimplemented} = 
  BEGIN RAISE Unimplemented END LastCeded; *)

(* PROCEDURE GetParameters(trsl: T := NIL): Parameters 
  RAISES {TrestleComm.Failure} = BEGIN RAISE Unimplemented END GetParameters; *)

(* PROCEDURE SetParameters(
    p: Parameters;
    trsl: T := NIL)
  RAISES {TrestleComm.Failure} = BEGIN RAISE Unimplemented END SetParameters;
*)

BEGIN END Trestle.
