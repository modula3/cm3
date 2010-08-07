(*---------------------------------------------------------------------------*)
MODULE CompactClient;

IMPORT NetObj, Rd, Thread, TextTextTbl;
IMPORT RsrcService, MsgX, MsgIF;

(*---------------------------------------------------------------------------*)
REVEAL
  T = Default BRANDED "CompactClient Interface 0.0" OBJECT
    host       :  TEXT := "localhost";
    address    :  NetObj.Address;
    service    :  RsrcService.T;
    data       :  TEXT;
    env        :  TextTextTbl.T;
    msg        :  MsgIF.T := NIL;
    eb         := EB.Ignore;
  METHODS
    connect() := Connect;
    err(m : TEXT) := Err;
    log(m : TEXT) := Log;
  OVERRIDES
    init := Init;
    rsrcText := RsrcText;
    rsrcRd := RsrcRd;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE Err(self : T; m : TEXT) =
  BEGIN
    IF self.eb = EB.Ignore THEN RETURN END;
    IF self.eb = EB.Warn THEN
      MsgX.Error(self.msg, m);
    ELSIF self.eb = EB.Fail THEN
      MsgX.Fatal(self.msg, m);
    END;
  END Err;

(*---------------------------------------------------------------------------*)
PROCEDURE Log(self : T; m : TEXT) =
  BEGIN
    MsgX.V(self.msg, m);
  END Log;

(*---------------------------------------------------------------------------*)
PROCEDURE Connect(self : T) =
  VAR
    netobj     :  NetObj.T;
  BEGIN
    self.log("locating agent (service broker) at " & self.host);
    TRY
      self.address := NetObj.Locate(self.host);
    EXCEPT
      NetObj.Invalid => self.err("cannot resolve hostname " & self.host);
      RETURN;
    | NetObj.Error => self.err("cannot connect to host " & self.host); RETURN;
    | Thread.Alerted => self.err("lookup interrupted"); RETURN;
    END;
    self.log("importing service");
    TRY
      netobj := NetObj.Import("ComPactRsrcService", self.address);
      IF netobj = NIL THEN
        self.err("service ComPactRsrcService not found");
      ELSE
        self.service := NARROW(netobj, RsrcService.T);
        self.connected := TRUE;
      END;
    EXCEPT
    | NetObj.Error => self.err("cannot import service from host " & self.host);
    | Thread.Alerted => self.err("import interrupted");
    END;
  END Connect;

(*---------------------------------------------------------------------------*)
PROCEDURE Init(self : T; host := "localhost"; msgif : MsgIF.T := NIL; 
               eb := EB.Warn) : T =
  BEGIN
    self.host := host;
    self.msg := msgif;
    self.eb := eb;
    self.connect();
    RETURN self;
  END Init;

(*---------------------------------------------------------------------------*)
PROCEDURE RsrcText(self : T; name : TEXT; env : TextTextTbl.T := NIL) : TEXT =
  VAR data : TEXT;
  BEGIN
    IF NOT self.connected THEN self.connect() END;
    IF NOT self.connected THEN RETURN NIL END;
    TRY
      data := self.service.getRsrcAsText(name, env);
      RETURN data;
    EXCEPT
      RsrcService.E(e) => self.err("server error: " & e);
    | NetObj.Error => self.err("cannot import service from host " & self.host);
    | Thread.Alerted => self.err("import interrupted");
    END;
    RETURN NIL;
  END RsrcText;

(*---------------------------------------------------------------------------*)
PROCEDURE RsrcRd(self : T; name : TEXT; env : TextTextTbl.T := NIL) : Rd.T =
  VAR rd : Rd.T;
  BEGIN
    IF NOT self.connected THEN self.connect() END;
    IF NOT self.connected THEN RETURN NIL END;
    TRY
      rd := self.service.getRsrcReader(name, env);
      RETURN rd;
    EXCEPT
      RsrcService.E(e) => self.err("server error: " & e);
    | NetObj.Error => self.err("cannot import service from host " & self.host);
    | Thread.Alerted => self.err("import interrupted");
    END;
    RETURN NIL;
  END RsrcRd;

(*---------------------------------------------------------------------------*)
PROCEDURE RemoteRsrcText(VAR cl :  T; 
                         host   :  TEXT;
                         name   :  TEXT; 
                         env    :  TextTextTbl.T := NIL;
                         msgif  :  MsgIF.T := NIL; 
                         eb     := EB.Warn) : TEXT =
  BEGIN
    IF cl = NIL THEN
      cl := NEW(T).init(host, msgif, eb);
    ELSE
      cl.eb := eb;
    END;
    RETURN cl.rsrcText(name, env);
  END RemoteRsrcText;

(*---------------------------------------------------------------------------*)
PROCEDURE RemoteRsrcRd(VAR cl :  T; 
                       host   :  TEXT;
                       name   :  TEXT; 
                       env    :  TextTextTbl.T := NIL;
                       msgif  :  MsgIF.T := NIL; 
                       eb     := EB.Warn) : Rd.T =
  BEGIN
    IF cl = NIL THEN
      cl := NEW(T).init(host, msgif, eb);
    ELSE
      cl.eb := eb;
    END;
    RETURN cl.rsrcRd(name, env);
  END RemoteRsrcRd;

BEGIN
END CompactClient.
