(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
MODULE ObParseFrame;
IMPORT SynLocation, SynParse, MetaParser, ObFrame;

  PROCEDURE SelectText(p: SynParse.T; index: INTEGER): TEXT  =
    BEGIN
      RETURN NARROW(p.stack[index], MetaParser.TextTemp).text;
    END SelectText;

  PROCEDURE BuildPhraseQuit(<*UNUSED*>self: SynParse.Action; 
                            <*UNUSED*>p: SynParse.T; 
                            <*UNUSED*>base: INTEGER; 
                            READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN 
      NEW(ObFrame.Quit, location:=SynLocation.NewLineLocation(info));
  END BuildPhraseQuit;

  PROCEDURE BuildPhraseLoadName(<*UNUSED*>self: SynParse.Action;
                                p: SynParse.T; base: INTEGER; 
                                READONLY info: SynLocation.Info): 
    SynParse.Tree =
  BEGIN
    RETURN 
      NEW(ObFrame.Load, location:=SynLocation.NewLineLocation(info),
        name:=SelectText(p, base+1) & ".obl");
  END BuildPhraseLoadName;

  PROCEDURE BuildPhraseLoadString(<*UNUSED*>self: SynParse.Action; 
                                  p: SynParse.T;
      base: INTEGER; READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN 
      NEW(ObFrame.Load, location:=SynLocation.NewLineLocation(info),
        name:=SelectText(p, base+1));
  END BuildPhraseLoadString;

  PROCEDURE BuildPhraseImport(<*UNUSED*>self: SynParse.Action; p: SynParse.T;
      base: INTEGER; READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN 
      NEW(ObFrame.Import, location:=SynLocation.NewLineLocation(info),
        name:=SelectText(p, base+1));
  END BuildPhraseImport;

  PROCEDURE BuildPhraseModule(<*UNUSED*>self: SynParse.Action; p: SynParse.T;
      base: INTEGER; READONLY info: SynLocation.Info): SynParse.Tree =
  VAR name, for: TEXT;
  BEGIN
    name := SelectText(p, base+1);
    IF p.stack[base+3] = NIL 
    THEN for := name;
    ELSE for := SelectText(p, base+3);
    END;
    RETURN 
      NEW(ObFrame.Module, location:=SynLocation.NewLineLocation(info),
        name:=name, for:=for, imports:=p.stack[base+5]);
  END BuildPhraseModule;

  PROCEDURE BuildPhraseEndModule(<*UNUSED*>self: SynParse.Action;
                                 <*UNUSED*>p: SynParse.T;
                                 <*UNUSED*>base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN 
      NEW(ObFrame.EndModule,
        location:=SynLocation.NewLineLocation(info));
  END BuildPhraseEndModule;

  PROCEDURE BuildImportList(<*UNUSED*>self: SynParse.Action; p: SynParse.T;
      base: INTEGER; READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN 
      NEW(ObFrame.NameList, location:=SynLocation.NewLineLocation(info),
        first:=SelectText(p, base+1),
	rest:=p.stack[base+2]);
  END BuildImportList;

  PROCEDURE BuildImportListSingle(<*UNUSED*>self: SynParse.Action;
                                  p: SynParse.T;
      base: INTEGER; READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN 
      NEW(ObFrame.NameList, location:=SynLocation.NewLineLocation(info),
        first:=SelectText(p, base+1), rest:=NIL);
  END BuildImportListSingle;

  PROCEDURE BuildImportListNil(<*UNUSED*>self: SynParse.Action; 
                               <*UNUSED*>p: SynParse.T;
                               <*UNUSED*>base: INTEGER; 
                               <*UNUSED*>READONLY info: SynLocation.Info):
    SynParse.Tree  =
  BEGIN
    RETURN NIL;
  END BuildImportListNil;

  PROCEDURE BuildPhraseEstablish(<*UNUSED*>self: SynParse.Action;
                                 p: SynParse.T;
      base: INTEGER; READONLY info: SynLocation.Info): SynParse.Tree  =
  VAR name, for: TEXT;
  BEGIN
    name := SelectText(p, base+1);
    IF p.stack[base+3] = NIL 
    THEN for := name;
    ELSE for := SelectText(p, base+3);
    END;
    RETURN 
      NEW(ObFrame.Establish, location:=SynLocation.NewLineLocation(info),
        name:=name, for:=for);
  END BuildPhraseEstablish;

  PROCEDURE BuildPhraseDelete(<*UNUSED*>self: SynParse.Action; p: SynParse.T;
      base: INTEGER; READONLY info: SynLocation.Info): SynParse.Tree  =
  BEGIN
    RETURN 
      NEW(ObFrame.Delete, location:=SynLocation.NewLineLocation(info),
        name:=SelectText(p, base+1));
  END BuildPhraseDelete;

  PROCEDURE BuildPhraseSave(<*UNUSED*>self: SynParse.Action; p: SynParse.T;
      base: INTEGER; READONLY info: SynLocation.Info): SynParse.Tree  =
  BEGIN
    RETURN 
      NEW(ObFrame.Save, location:=SynLocation.NewLineLocation(info),
        name:=SelectText(p, base+1));
  END BuildPhraseSave;

  PROCEDURE BuildPhraseQualify(<*UNUSED*>self: SynParse.Action; 
                               <*UNUSED*>p: SynParse.T;
                               <*UNUSED*>base: INTEGER; 
                           READONLY info: SynLocation.Info): SynParse.Tree  =
  BEGIN
    RETURN 
      NEW(ObFrame.Qualify,
        location:=SynLocation.NewLineLocation(info));
  END BuildPhraseQualify;

PROCEDURE RegisterActions(actions: MetaParser.ActionTable)  =
  BEGIN
    MetaParser.Register("BuildPhraseQuit", BuildPhraseQuit, actions);
    MetaParser.Register("BuildPhraseLoadName", BuildPhraseLoadName, actions);
    MetaParser.Register("BuildPhraseLoadString", BuildPhraseLoadString, actions);
    MetaParser.Register("BuildPhraseImport", BuildPhraseImport, actions);
    MetaParser.Register("BuildPhraseEstablish", BuildPhraseEstablish, actions);
    MetaParser.Register("BuildPhraseDelete", BuildPhraseDelete, actions);
    MetaParser.Register("BuildPhraseSave", BuildPhraseSave, actions);
    MetaParser.Register("BuildPhraseQualify", BuildPhraseQualify, actions);
    MetaParser.Register("BuildPhraseModule", BuildPhraseModule, actions);
    MetaParser.Register("BuildPhraseEndModule", BuildPhraseEndModule, actions);
    MetaParser.Register("BuildImportList", BuildImportList, actions);
    MetaParser.Register("BuildImportListSingle", BuildImportListSingle, actions);
    MetaParser.Register("BuildImportListNil", BuildImportListNil, actions);
  END RegisterActions;

PROCEDURE Setup()  =
  BEGIN
  END Setup;

BEGIN
END ObParseFrame.
