(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Thu Mar 16 17:11:09 PST 1995 by msm *)
(* modified on Mon Feb 24 13:59:46 PST 1992 by muller *)
<*PRAGMA LL*>

UNSAFE MODULE XConfCtl EXPORTS XConfCtl, TrestleConf;

IMPORT VBT, Trestle, XClient, TrestleImpl, RefSeq, Text, X, TrestleOnX, 
  XProperties, TrestleComm;

REVEAL
  User = UserPublic BRANDED OBJECT
           equate, next: User := NIL;
         OVERRIDES
           register := RegisterUser;
         END;

REVEAL
  App = AppPublic BRANDED OBJECT
          users: RefSeq.T;
        OVERRIDES
          init    := InitApp;
          destroy := DestroyApp;
        END;

VAR mutex := NEW(MUTEX);

PROCEDURE InitApp (self: App; user: User) =
  BEGIN
    user := RootUser(user);
    self.users := NEW(RefSeq.T).init();
    self.users.addhi(user);
    self.add(user);
    self.activate(user);
  END InitApp;

PROCEDURE DestroyApp (self: App) =
  VAR i: CARDINAL := 0; n := self.users.size(); u: User;
  BEGIN
    WHILE i < n DO
      u := self.users.get(i);
      self.suspend(u);
      self.delete(u);
      INC(i)
    END
  END DestroyApp;

VAR Users := NEW(RefSeq.T).init();

PROCEDURE RegisterUser (self: User) =
  BEGIN
    LOCK mutex DO
      FOR i := 0 TO Users.size()-1 DO
        IF self = Users.get(i) THEN RETURN END
      END;
      Users.addhi(self);
      RETURN
    END
  END RegisterUser;

VAR
  userCreate: UserProc;          (* Trestle-supplied proc to create User
                                    objects *)

PROCEDURE Init (createUser: UserProc) =
  BEGIN
    userCreate := createUser
  END Init;

PROCEDURE RootUser (u: User): User =
  BEGIN
    LOCK mutex DO
      WHILE u # NIL AND u.equate # NIL DO u := u.equate END
    END;
    RETURN u
  END RootUser;

PROCEDURE UserFromHost (host: TEXT; createOK: BOOLEAN): User =
  VAR
    res: User     := NIL;
    i  : CARDINAL := 0;
    n  : CARDINAL;
  BEGIN
    LOCK mutex DO
      n := Users.size();
      WHILE i < n DO
        res := Users.get(i);
        IF Text.Equal(res.displayName, host) THEN
          i := n
        ELSE
          res := NIL;
          INC(i)
        END;
      END;
    END;
    IF res = NIL AND createOK THEN
      res := userCreate(host, host);
      IF res # NIL THEN RegisterUser(res) END
    END;
    RETURN RootUser(res)
  END UserFromHost;

PROCEDURE RemoveUser(VAR list: RefSeq.T; user: User) =
  VAR i: CARDINAL := 0; n := list.size();
  BEGIN
    WHILE i < n DO
      IF list.get(i) = user THEN
        list.put(i, list.gethi());
        EVAL list.remhi();
        RETURN
      END;
      INC(i)
    END
  END RemoveUser;

TYPE Mode = {None, Join, Leave, Activate, Deactivate, JoinAndActivate, Define};

  (* Syntax: if MuxAgent = "TrestleCop" then we send the following set the
     CPORT to a message of the following kinds:
|       +host1,host2,... (add hosts and activate)
|       -host1,host2,... (deactivate and delete)
|       !host1,host2,... (add host, inactive)
|       ^host1,host2,... (activate host)
|       /host1,host2,... (deactivate host)
|       =host1,host2,... (host1 is a synonym for the controlling user)
     Commands are not null-terminated, although nulls are permitted and
     ignored.  Commands may be concatenated. *)

CONST
  sep = ARRAY Mode OF CHAR{';', '!', '-', '^', '/', '+', '='};
  Sep = SET OF CHAR{';', '!', '-', '^', '/', '+', '=', ',', '\000'};

PROCEDURE Act (app: TrestleImpl.App; mode: Mode; host: TEXT) =
  VAR
    user := UserFromHost(
              host, mode = Mode.Join OR mode = Mode.JoinAndActivate);
  BEGIN
    IF user = NIL THEN RETURN END;
    CASE mode OF
      Mode.None =>
    | Mode.Join => app.users.addhi(user); app.add(user)
    | Mode.Leave =>
        app.suspend(user);
        app.delete(user);
        RemoveUser(app.users, user)
    | Mode.Activate => app.activate(user)
    | Mode.Deactivate => app.suspend(user);
    | Mode.JoinAndActivate => app.add(user); app.activate(user)
    | Mode.Define =>
        LOOP
          VAR root := RootUser(app.primary);
          BEGIN
            user := RootUser(user);
            IF user = root THEN EXIT END;
            LOCK mutex DO
              IF user.equate = NIL THEN user.equate := root; EXIT END
            END
          END
        END
    END
  END Act;

PROCEDURE Parse (app: TrestleImpl.App; READONLY str: ARRAY OF CHAR) =
  VAR
    start, end: CARDINAL := 0;
    mode                 := Mode.None;
  BEGIN
    WHILE end < NUMBER(str) DO
      IF str[end] IN Sep THEN
        IF start < end AND mode # Mode.None THEN
          Act(app, mode, Text.FromChars(SUBARRAY(str, start, end - start)))
        END;
        FOR i := FIRST(Mode) TO LAST(Mode) DO
          IF str[end] = sep[i] THEN mode := i END
        END;
        start := end + 1
      END;
      INC(end)
    END;
    IF start < end AND mode # Mode.None THEN
      Act(app, mode, Text.FromChars(SUBARRAY(str, start, end - start)))
    END
  END Parse;

PROCEDURE Process (v: VBT.T) =
  VAR
    app : TrestleImpl.App;
    ch  : VBT.T;
    trsl: Trestle.T;
    type: X.Atom;
    str : REF ARRAY OF CHAR;
    fmt : INTEGER;
    win: X.Drawable;
  BEGIN
    LOCK VBT.mu DO
      IF NOT TrestleImpl.RootChild(v, trsl, ch) THEN RETURN END;
      app := TrestleImpl.ChildApp(ch);
      IF app = NIL OR trsl = NIL THEN RETURN END;
      win := TrestleOnX.Drawable(ch);
      TRY
        TYPECASE trsl OF
          XClient.T (xconn) =>
            TrestleOnX.Enter(xconn);
            TRY
              IF NOT XProperties.GetProp(
                       xconn, win, XClient.ToAtom(xconn, "XMUX_CPORT"),
                       type, str, fmt) THEN
                RETURN
              END;
            FINALLY
              TrestleOnX.Exit(xconn)
            END;
        ELSE
        END
      EXCEPT
        TrestleComm.Failure =>
      END;
      IF fmt # 8 THEN RETURN END;
      Parse(app, str^)
    END
  END Process;

BEGIN
END XConfCtl. 

