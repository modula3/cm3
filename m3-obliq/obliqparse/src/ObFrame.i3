(* Copyright 1991 Digital Equipment Corporation.  *) 
(* Distributed only by permission.                *) 

INTERFACE ObFrame; 
IMPORT SynLocation, SynScan, TextRd, Pathname, ObErr, ObTree, ObLib,
       ObValue, Obliq;
IMPORT NetObj, Thread, Fingerprint;

TYPE
  Env=Obliq.Env;

  Name =
    SynLocation.Located BRANDED OBJECT name: TEXT END;
  NameList =
    SynLocation.Located BRANDED OBJECT first: TEXT; rest: NameList END;

  Quit =
    ObTree.Phrase BRANDED OBJECT
    END;
  Load =
    ObTree.Phrase BRANDED OBJECT
      name: TEXT;
    END;
  Import =
    ObTree.Phrase BRANDED OBJECT
      name: TEXT;
    END;
  Module =
    ObTree.Phrase BRANDED OBJECT
      name, for: TEXT; imports: NameList;
    END;
  AddHelp =
    ObTree.Phrase BRANDED OBJECT
      name, sort, short, long: TEXT;
    END;
  EndModule =
    ObTree.Phrase BRANDED OBJECT
      ideList: NameList;
    END;
  Establish =
    ObTree.Phrase BRANDED OBJECT
      name, for: TEXT;
    END;
  Save =
    ObTree.Phrase BRANDED OBJECT
      name: TEXT;
    END;
  Delete =
    ObTree.Phrase BRANDED OBJECT
      name: TEXT;
    END;
  Qualify =
    ObTree.Phrase BRANDED OBJECT
      ideList: NameList;
    END;

  FrameOpCode =
    ObLib.OpCode OBJECT
      val: ObValue.Val;
    END;
  OpCodeHandle = NetObj.T OBJECT METHODS
    getOpCodes(ts: Fingerprint.T): REF ObLib.OpCodes 
       RAISES {NetObj.Error, Thread.Alerted};
  END;

PROCEDURE Setup();

PROCEDURE LoadFile(sc: SynScan.T; filename: Pathname.T; 
  complain: BOOLEAN:=TRUE) RAISES {ObErr.Fail};

PROCEDURE ModuleFrame(sc: SynScan.T; name, for: TEXT; 
  imports: NameList; env: Env) RAISES {ObErr.Fail};

PROCEDURE ModuleEnd(sc: SynScan.T; ideList: NameList) RAISES {ObErr.Fail};

PROCEDURE ImportFrame(sc: SynScan.T; name: TEXT; env: Env) RAISES {ObErr.Fail};

PROCEDURE EstablishFrame(name, for: TEXT; env: Env): Env 
  RAISES {ObErr.Fail};

PROCEDURE SaveFrame(name,for: TEXT; env: Env): Env 
  RAISES {ObErr.Fail};

PROCEDURE DeleteFrame(name: TEXT; env: Env): Env RAISES {ObErr.Fail};

PROCEDURE QualifyFrame(env: Env; ideList: NameList): Env RAISES {ObErr.Fail};

PROCEDURE AddHelpFrame(name, sort, short, long: TEXT; 
                          env: Env) RAISES {ObErr.Fail};

VAR (* READONLY after initialization *) 
  SearchPathSeparator: CHAR;

TYPE SearchPath = OBJECT
    first: Pathname.T;
    rest: SearchPath;
  END;

VAR searchPath: SearchPath;

PROCEDURE FmtSearchPath(path: SearchPath): TEXT;

PROCEDURE LexSearchPath(rd: TextRd.T): SearchPath;

END ObFrame.

