(*---------------------------------------------------------------------------*)
INTERFACE RsrcService;

IMPORT NetObj, Rd, Thread, TextSeq, TextTextTbl;

(*---------------------------------------------------------------------------*)
EXCEPTION E(TEXT);

(*---------------------------------------------------------------------------*)
TYPE
  T = NetObj.T OBJECT
  METHODS
    readConfig(fn : TEXT)
      RAISES {NetObj.Error, E, Thread.Alerted};

    setRsrcPath(name : TEXT; rp : TextSeq.T)
      RAISES {NetObj.Error, E, Thread.Alerted};

    getRsrcPath(name : TEXT) : TextSeq.T
      RAISES {NetObj.Error, E, Thread.Alerted};

    getRsrcReader(name : TEXT; env : TextTextTbl.T := NIL) : Rd.T
      RAISES {NetObj.Error, E, Thread.Alerted};

    getRsrcAsText(name : TEXT; env : TextTextTbl.T := NIL) : TEXT
      RAISES {NetObj.Error, E, Thread.Alerted};
  END;

END RsrcService.
