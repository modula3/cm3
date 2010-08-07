(*---------------------------------------------------------------------------*)
MODULE Main;

IMPORT CVS, Msg, Text, TextSeq, Params, Process;
IMPORT AbstractPathname AS APN;

PROCEDURE ListOut(list : TextSeq.T) =
  BEGIN
    FOR i := 0 TO list.size() - 1 DO
      Msg.V(list.get(i));
    END;
  END ListOut;

(*---------------------------------------------------------------------------*)
PROCEDURE ListStates() =
  VAR
    list : TextSeq.T;
  BEGIN
    (* list states of actual directory *)
    list := CVS.StateList(APN.New("."));
    ListOut(list);
  END ListStates;

(*---------------------------------------------------------------------------*)
PROCEDURE ListTags(prefix : TEXT) =
  VAR
    list : TextSeq.T;
  BEGIN
    list := CVS.Tags(APN.New("."), prefix);
    ListOut(list);
  END ListTags;

(*---------------------------------------------------------------------------*)
PROCEDURE Update(rev : TEXT) =
  VAR
    res : TEXT;
  BEGIN
    IF CVS.Update(APN.New("."), rev, res) THEN
      Msg.V(res);
      Msg.V("update completed successfully");
    ELSE
      Msg.Error("update failed");
    END;
  END Update; 

(*---------------------------------------------------------------------------*)
PROCEDURE ListStickyTags() =
  VAR
    list : TextSeq.T;
  BEGIN
    list := CVS.CurrentStickyTags(APN.New("."));
    ListOut(list);
  END ListStickyTags;

(*---------------------------------------------------------------------------*)
PROCEDURE Modified() =
  VAR
    res : TEXT;
  BEGIN
    IF CVS.Modified(APN.New("."), res) THEN
      Msg.V("YES");
    ELSE
      Msg.V("NO");
    END;
    Msg.V(res);
  END Modified; 

(*---------------------------------------------------------------------------*)
PROCEDURE UpToDate() =
  VAR
    res : TEXT;
  BEGIN
    IF CVS.UpToDate(APN.New("."), res) THEN
      Msg.V("YES");
    ELSE
      Msg.V("NO");
    END;
    Msg.V(res);
  END UpToDate; 

(*---------------------------------------------------------------------------*)
PROCEDURE Commit() =
  BEGIN
    IF CVS.Commit(APN.New("."), "automatic commit by test program") THEN
      Msg.V("commit okay");
    ELSE
      Msg.Error("commit failed");
    END;
  END Commit; 

(*---------------------------------------------------------------------------*)
PROCEDURE CommitI() =
  BEGIN
    IF CVS.Commit(APN.New("."), NIL, NIL) THEN
      Msg.V("commit okay");
    ELSE
      Msg.Error("commit failed");
    END;
  END CommitI; 

(*---------------------------------------------------------------------------*)
VAR (* Main *)
  par1 : TEXT;
BEGIN (* Main *)
  Msg.vFlag := TRUE;

  IF Params.Count < 2 THEN
    ListStates();
    Process.Exit(0);
  END;

  par1 := Params.Get(1);
  IF Text.Equal(par1, "tags") THEN
    IF Params.Count > 2 THEN
      ListTags(Params.Get(2));
    ELSE
      ListTags("all");
    END;
  ELSIF Text.Equal(par1, "up") THEN
    IF Params.Count > 2 THEN
      Update(Params.Get(2));
    ELSE
      Update("head");
    END;
  ELSIF Text.Equal(par1, "sticky") THEN
    ListStickyTags();
  ELSIF Text.Equal(par1, "modified") THEN
    Modified();
  ELSIF Text.Equal(par1, "uptodate") THEN
    UpToDate();
  ELSIF Text.Equal(par1, "checkin") THEN
    Commit();
  ELSIF Text.Equal(par1, "commit") THEN
    CommitI();
  ELSE
    Msg.Error("unknown command");
  END;
END Main.
