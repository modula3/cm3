(* Copyright 1991 Digital Equipment Corporation.  *) 
(* Distributed only by permission.                *) 

INTERFACE ObFrame; 
IMPORT SynLocation, SynScan, TextRd, Pathname, ObErr, ObTree, ObLib,
       ObValue, Obliq, SynWr;
IMPORT NetObj, Thread, Fingerprint;

TYPE
  Env = Obliq.Env;

  Name = SynLocation.Located BRANDED "ObFrame.Name" OBJECT name: TEXT END;
  NameList = SynLocation.Located BRANDED "ObFrame.NameList" OBJECT
               first: TEXT;
               rest : NameList
             END;

  Quit = ObTree.Phrase BRANDED "ObFrame.Quit" OBJECT END;
  Load = ObTree.Phrase BRANDED "ObFrame.Load" OBJECT name: TEXT;  END;
  Import = ObTree.Phrase BRANDED "ObFrame.Import" OBJECT name: TEXT;  END;
  Module = ObTree.Phrase BRANDED "ObFrame.Module" OBJECT
             name, for: TEXT;
             imports  : NameList;
           END;
  AddHelp = ObTree.Phrase BRANDED "ObFrame.AddHelp" OBJECT
              name, sort, short, long: TEXT;
            END;
  EndModule = ObTree.Phrase BRANDED "ObFrame.EndModule" OBJECT
                ideList: NameList;
              END;
  Establish =
    ObTree.Phrase BRANDED "ObFrame.Establish" OBJECT name, for: TEXT;  END;
  Save = ObTree.Phrase BRANDED "ObFrame.Save" OBJECT name: TEXT;  END;
  Delete = ObTree.Phrase BRANDED "ObFrame.Delete" OBJECT name: TEXT;  END;
  Qualify =
    ObTree.Phrase BRANDED "ObFrame.Qualify" OBJECT ideList: NameList;  END;

  FrameOpCode = ObLib.OpCode OBJECT val: ObValue.Val;  END;
  OpCodeHandle = NetObj.T OBJECT
                 METHODS
                   getOpCodes (ts: Fingerprint.T): REF ObLib.OpCodes
                               RAISES {NetObj.Error, Thread.Alerted};
                 END;

PROCEDURE Setup();

PROCEDURE LoadFile(sc: SynScan.T; filename: Pathname.T; 
  complain: BOOLEAN:=TRUE) RAISES {ObErr.Fail};

PROCEDURE ModuleFrame(sc: SynScan.T; name, for: TEXT; 
  imports: NameList; env: Env) RAISES {ObErr.Fail};

PROCEDURE ModuleEnd(sc: SynScan.T; ideList: NameList) RAISES {ObErr.Fail};

PROCEDURE ImportFrame(sc: SynScan.T; name: TEXT; env: Env) RAISES {ObErr.Fail};

PROCEDURE EstablishFrame(wr: SynWr.T; name, for: TEXT; env: Env): Env 
  RAISES {ObErr.Fail};

PROCEDURE SaveFrame(wr: SynWr.T; name,for: TEXT; env: Env): Env 
  RAISES {ObErr.Fail};

PROCEDURE DeleteFrame(wr: SynWr.T; name: TEXT; 
                      env: Env): Env RAISES {ObErr.Fail};

PROCEDURE QualifyFrame(wr: SynWr.T; env: Env; 
                       ideList: NameList): Env RAISES {ObErr.Fail};

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

