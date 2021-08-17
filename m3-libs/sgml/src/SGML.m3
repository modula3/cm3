(*  SGML parser library                                                    *)
(*  Copyright (C) 1997 Michel Dagenais                                     *)
(*                                                                         *)
(*  This library is free software; you can redistribute it and/or          *)
(*  modify it under the terms of the GNU Library General Public            *)
(*  License as published by the Free Software Foundation; either           *)
(*  version 2 of the License, or (at your option) any later version.       *)
(*                                                                         *)
(*  This library is distributed in the hope that it will be useful,        *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of         *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      *)
(*  Library General Public License for more details.                       *)
(*                                                                         *)
(*  You should have received a copy of the GNU Library General Public      *)
(*  License along with this library; if not, write to the Free             *)
(*  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.     *)
(*                                                                         *)
(*  For more information on this program, contact Michel Dagenais at       *)
(*  dagenais@vlsi.polymtl.ca or Electrical Eng. Dept., Ecole Polytechnique *)
(*  P.O. Box 6079, Station A, Montreal, Quebec, Canada, H3C 3A7.           *)

MODULE SGML EXPORTS SGML, SGMLRep;

IMPORT SGML, SGMLC, SGMLCScanner, Text, RefSeq, TextRefTbl, TextTextTbl, Atom,
    ASCII, FileRd, TextRd, Pathname, FS, Process, OSError, 
    IO, Rd, FSM, Fmt, MxConfig;

REVEAL

  SGML.Parser = ParserPublicRep BRANDED OBJECT
      options: SGML.ParserOptions;
      warn: Warnings;
      programName: TEXT;
      files: REF ARRAY OF TEXT;
      rds: REF ARRAY OF Rd.T;
      e: Err;
      fileNo: CARDINAL;
      inhibit: BOOLEAN := TRUE;
      defn: Definitions;
      cache: Cache;
      stateStack: RefSeq.T;
      currState: StateFrame;
    OVERRIDES
      init := InitParser;
      run := RunParser;
      halt := HaltParser;
      inhibitMessages := InhibitMessages;
      subdocumentParser := SubdocumentParser;
      newParser := NewParser;
    END;

  SGML.Application = SGML.ApplicationPublic BRANDED OBJECT
      parser: SGML.Parser;
    OVERRIDES
      getDetailedLocation := GetDetailedLocation;
      init := ApplicationInit;
    END;

(* The parser maintains tables of defined entities (parameter, general
   and doctype), of public names for some of these entities,
   and of elements. Each element type has a finite state machine describing
   the content it accepts. Since elements may nest, the current state for
   the finite state machines of all currently nested elements must be kept
   in a stack of state frames. *)

  StateFrame = BRANDED REF RECORD
      name: Atom.T;
      omitE: BOOLEAN;
      currNode: FSM.Node;
      fsm: FSM.T;
    END;

TYPE

  Definitions = RECORD
      elements: TextRefTbl.Default;
      attlists: TextRefTbl.Default;
      notations: TextRefTbl.Default;
      pEntities: TextRefTbl.Default;
      gEntities: TextRefTbl.Default;
      dEntities: TextRefTbl.Default;
      publics: TextTextTbl.Default;
    END;

  Cache = REF RECORD
      catalog: Definitions;
      dtd: TextRefTbl.Default;
    END;

  Err = SGMLC.ErrHandler BRANDED OBJECT
      num: CARDINAL := 0;
      application: SGML.Application;
    OVERRIDES
      error := GenError
    END;

  EntityResolver = SGMLCScanner.EntityResolver OBJECT
      parser: SGML.Parser;
    OVERRIDES
      resolve := ResolveParameterEntity;
    END;

(* The error handler and entity resolver allow the parser and scanner to
   report errors and query for entities. *)

  Warnings = RECORD
      mixed, sgmldecl, should, default, duplicate, undefined, unclosed,
      empty, net, unusedParam, notationSysid: BOOLEAN := FALSE;
    END;

VAR
  StartAtom := Atom.FromText("!INIT");

(* Report the error to the application. *)

PROCEDURE GenError(e : Err ; <*UNUSED*>line : CARDINAL ; 
      <*UNUSED*>col : CARDINAL ; msg : TEXT) =
  BEGIN
    INC(e.num);
    e.application.error(SGML.ErrorEvent{e.application.parser.p.offset(),
        SGML.ErrorType.OtherError,msg});
  END GenError ;

(* Convert a string of 0-9 digits to the associated character. *)

PROCEDURE CharRefToCode(t: TEXT; VAR c: SGML.CharCode): BOOLEAN =
  VAR
    car: CHAR;
    val: CARDINAL;
    length := Text.Length(t);
  BEGIN
    IF length < 3 THEN RETURN FALSE; END;

    IF Text.Equal(t,"&amp;") THEN
      c := ORD('&');
      RETURN TRUE;
    ELSIF Text.GetChar(t,0) = '&' AND Text.GetChar(t,1) = '#' AND
        Text.GetChar(t,length - 1) = ';' THEN
      val := 0;
      IF Text.GetChar(t,2) = 'x' OR Text.GetChar(t,2) = 'X' THEN
        FOR i := 3 TO Text.Length(t) - 2 DO
          car := Text.GetChar(t,i);
          IF car IN ASCII.Digits THEN
            val := (16 * val) + (ORD(car) - ORD('0'));
          ELSE
            car := ASCII.Upper[car];
            IF car < 'A' OR car > 'F' THEN RETURN FALSE; END;
            val := (16 * val) + 10 + (ORD(car) - ORD('A'));
          END;
          IF val >= LAST(SGML.CharCode) THEN RETURN FALSE; END;
        END;
      ELSE
        FOR i := 2 TO Text.Length(t) - 2 DO
          val := (10 * val) + (ORD(Text.GetChar(t,i)) - ORD('0'));
          IF val >= LAST(SGML.CharCode) THEN RETURN FALSE; END;
        END;
      END;
      c := val;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END CharRefToCode;

(* Allocate all the parser tables. Provide default values. *)

PROCEDURE InitParser(self: SGML.Parser; 
    options: SGML.ParserOptions; <*NOWARN*>
    programName: TEXT; files: REF ARRAY OF TEXT; 
    rds: REF ARRAY OF Rd.T := NIL): SGML.Parser =
  BEGIN
    self.options := options;
    CopyTextArray(self.options.addCatalog,"catalog");
    CopyTextArray(self.options.includeParam);
    CopyTextArray(self.options.enableWarning);
    InitWarnings(self);
    CopyTextArray(self.options.addSearchDir,MxConfig.Get("PKG_USE") & 
        MxConfig.HOST_PATH_SEP & "sgml" & MxConfig.HOST_PATH_SEP & "src" &
        MxConfig.HOST_PATH_SEP & "dtd");
    CopyTextArray(self.options.activateLink);
    CopyTextArray(self.options.architecture);
    self.programName := programName;

    self.attributes := NEW(REF ARRAY OF SGML.Attribute, 15);
    self.dataChunks := NEW(REF ARRAY OF SGML.CdataChunk, 15);

    self.stateStack := NEW(RefSeq.T);

    self.files := NEW(REF ARRAY OF TEXT,NUMBER(files^));
    self.files^ := files^;
    self.rds := rds;

    self.e := NEW(Err);
    self.s := NEW(SGMLC.Scanner);
    self.p := NEW(ParserPlus, parser := self);

    self.cache := NEW(Cache, dtd := NIL);

    RETURN self;
  END InitParser;

PROCEDURE NewParser(self: SGML.Parser; files: REF ARRAY OF TEXT; 
    rds: REF ARRAY OF Rd.T := NIL): SGML.Parser =
  VAR
    new := NEW(SGML.Parser);
  BEGIN
    new.options := self.options;
    new.warn := self.warn;
    new.programName := self.programName;

    new.attributes := NEW(REF ARRAY OF SGML.Attribute, 15);
    new.dataChunks := NEW(REF ARRAY OF SGML.CdataChunk, 15);

    new.stateStack := NEW(RefSeq.T);

    new.files := NEW(REF ARRAY OF TEXT,NUMBER(files^));
    new.files^ := files^;
    new.rds := rds;

    new.e := NEW(Err);
    new.s := NEW(SGMLC.Scanner);
    new.p := NEW(ParserPlus, parser := new);

    new.cache := self.cache;

    RETURN new;
  END NewParser;

PROCEDURE InitWarnings(self: SGML.Parser) =
  BEGIN
    FOR i := 0 TO LAST(self.options.enableWarning^) DO
      WITH w = self.options.enableWarning[i] DO
        IF Text.Equal(w,"mixed") THEN self.warn.mixed := TRUE;
        ELSIF Text.Equal(w,"sgmldecl") THEN self.warn.sgmldecl := TRUE;
        ELSIF Text.Equal(w,"should") THEN self.warn.should := TRUE;
        ELSIF Text.Equal(w,"default") THEN self.warn.default := TRUE;
        ELSIF Text.Equal(w,"duplicate") THEN self.warn.duplicate := TRUE;
        ELSIF Text.Equal(w,"undefined") THEN self.warn.undefined := TRUE;
        ELSIF Text.Equal(w,"empty") THEN self.warn.empty := TRUE;
        ELSIF Text.Equal(w,"net") THEN self.warn.net := TRUE;
        ELSIF Text.Equal(w,"unused-param") THEN self.warn.unusedParam := TRUE;
        ELSIF Text.Equal(w,"notation-sysid") THEN 
          self.warn.notationSysid := TRUE;
        ELSIF Text.Equal(w,"min-tag") THEN 
          self.warn.unclosed := TRUE;
          self.warn.empty := TRUE;
          self.warn.net := TRUE;
        ELSIF Text.Equal(w,"all") THEN 
          self.warn := Warnings{TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
              TRUE,TRUE};
        END;
      END;
    END;
  END InitWarnings;

(* Initialize all the parser tables. Start parsing calling back the
   application for each significant chunk parsed. *)

PROCEDURE RunParser(self: SGML.Parser; a: SGML.Application): CARDINAL =
  VAR
    rd: Rd.T;
  BEGIN
    self.e.num := 0;
    self.e.application := a;
    self.application := a;
    a.parser := self;

    (* Once for all parsers using the same options (paths and catalogs)
       we need to fill the definitions with the catalogs content. The
       few builtin entities are inserted as well. Then, the resulting set
       of definitions is saved in the cache, to be reused for all files
       within the same parser and for all parsers sharing the same options
       (obtained with newParser()). *)

    IF self.cache.dtd = NIL THEN

      (* Start with clean tables for the definitions. *)

      self.defn.elements := NEW(TextRefTbl.Default).init();
      self.defn.attlists := NEW(TextRefTbl.Default).init();
      self.defn.notations := NEW(TextRefTbl.Default).init();
      self.defn.pEntities :=  NEW(TextRefTbl.Default).init();
      self.defn.gEntities :=  NEW(TextRefTbl.Default).init();
      self.defn.dEntities :=  NEW(TextRefTbl.Default).init();
      self.defn.publics :=  NEW(TextTextTbl.Default).init();

      (* Initialize the builtin entities *)

      AddElement(self,"#PCDATA",NIL,NIL,FALSE,TRUE);

      AddEntity(self,NEW(REF SGML.Entity, name := "amp", 
          internalText := "&amp;",
          dataType := SGML.EntityDataType.Sgml, 
          declType := SGML.EntityDeclType.General));
      AddEntity(self,NEW(REF SGML.Entity, name := "gt", 
          internalText := ">",
          dataType := SGML.EntityDataType.Sgml, 
          declType := SGML.EntityDeclType.General));
      AddEntity(self,NEW(REF SGML.Entity, 
          name := "lt", internalText := "<",
          dataType := SGML.EntityDataType.Sgml, 
          declType := SGML.EntityDeclType.General));
      AddEntity(self,NEW(REF SGML.Entity, name := "quot",
          internalText := "\"", dataType := SGML.EntityDataType.Sgml, 
          declType := SGML.EntityDeclType.General));

      (* Parse the catalog files. *)

      self.s := self.s.initSimple(self.e);
      self.s.pushState(SGMLCScanner.State.Catalog);
      FOR i := 0 TO LAST(self.options.addCatalog^) DO
        TRY
          ParseCatalog(self,self.options.addCatalog[i],
              Process.GetWorkingDirectory());
        EXCEPT ELSE
          a.error(SGML.ErrorEvent{0,SGML.ErrorType.OtherError,
              "Error processing " & self.options.addCatalog[i]});
        END;
      END;
      self.s.popState();

      (* Initialize the cache and save the definitions in it. *)

      self.cache.dtd := NEW(TextRefTbl.Default).init();
      CopyDefinitions(self.defn,self.cache.catalog);
    END;

    (* For each file, start with the catalogs definitions saved in the
       cache and parse the file. *)

    FOR i := 0 TO LAST(self.files^) DO
      self.fileNo := i;
      TRY
        CopyDefinitions(self.cache.catalog,self.defn);
        self.stateStack := self.stateStack.init();
        self.currState := NIL;
        self.doctype := NIL;
        self.markup := FALSE;
        self.s := self.s.initSimple(self.e);

        IF self.rds = NIL THEN rd := FileRd.Open(self.files[i]);
        ELSE rd := self.rds[i];
        END;

        self.s.pushFile(self.files[i],rd);
        self.s.setEntityResolver(NEW(EntityResolver, parser := self));
        self.p := self.p.init(self.s, self.e);
        self.p.parse();
      EXCEPT
      ELSE
        a.error(SGML.ErrorEvent{0,SGML.ErrorType.OtherError,
            "Cannot open " & self.files[i]});
      END;
    END;
    RETURN self.e.num;
  END RunParser;

(* Parse another file in the current context. The same parser state
   is kept (entities, elements...). *)

PROCEDURE SubParse(self: SGML.Parser; name: TEXT; rd: Rd.T) =
  VAR
    oldS: SGMLC.Scanner;
    oldP: SGMLC.Parser;
  BEGIN
    oldS := self.s;
    oldP := self.p;
    self.s := NEW(SGMLC.Scanner);
    self.p := NEW(ParserPlus, parser := self);
    self.s := self.s.initSimple(self.e);
    self.s.inMarkupDecl(self.markup);
    self.s.pushFile(name,rd);
    self.s.setEntityResolver(NEW(EntityResolver, parser := self));
    self.p := self.p.init(self.s, self.e);
    TRY self.p.parse(); EXCEPT ELSE END;
    self.s := oldS;
    self.p := oldP;
  END SubParse;

(* The doctype tells which Dtd to use. The Dtd is parsed as a subfile. *)

PROCEDURE ParseDtd(parser: SGML.Parser; externalId: SGML.ExternalId) =
  VAR
    rd: Rd.T;
    dtdId: TEXT;
    tmp: REFANY;
    defn: REF Definitions;
  BEGIN

    (* When a default document type exists, use it. Otherwise complain and
       use HTML. *)

    IF parser.doctype = NIL THEN
      parser.doctype := parser.options.defaultDoctype;
      IF parser.doctype = NIL THEN
        parser.doctype := "HTML";
        parser.application.error(SGML.ErrorEvent{parser.p.offset(),SGML.
            ErrorType.Info, "Document Type not declared, HTML assumed"});
      END;
    END;

    parser.application.startDtd(SGML.StartDtdEvent{parser.p.offset(),
        parser.doctype,externalId});

    (* Some Dtd are fairly long to parse. The corresponding definitions
       are saved in the cache and reused the second time a Dtd
       is requested. The key to uniquely identity Dtds is the system ID. *)

    dtdId := GetEntity(parser,parser.doctype,SGML.EntityDeclType.Doctype).
        externalId.generatedSystemId;
    IF parser.cache.dtd.get(dtdId,tmp) THEN
      defn := NARROW(tmp,REF Definitions);
      CopyDefinitions(defn^,parser.defn);
    ELSE
      rd := ResolveEntity(parser,parser.doctype,SGML.EntityDeclType.Doctype);
      IF rd = NIL THEN
        parser.application.error(SGML.ErrorEvent{0,SGML.ErrorType.
            OtherError,"Cannot open Dtd for DocType " & parser.doctype});
      ELSE
        parser.markup := TRUE;
        SubParse(parser,parser.doctype,rd);
        parser.markup := FALSE;
        defn := NEW(REF Definitions);
        CopyDefinitions(parser.defn,defn^);
        EVAL parser.cache.dtd.put(dtdId,defn);
      END;
    END;

    parser.application.endDtd(SGML.EndDtdEvent{parser.p.offset(),
        parser.doctype});
  END ParseDtd;

(* Not implemented yet. Awaiting requests for it! *)

PROCEDURE HaltParser(<*UNUSED*>self: SGML.Parser) =
  BEGIN
    <* ASSERT FALSE *> (* Perhaps force EOF into the scanner... *)
  END HaltParser;

PROCEDURE InhibitMessages(self: SGML.Parser; inhibit: BOOLEAN) =
  BEGIN
    self.inhibit := inhibit;
  END InhibitMessages;

(* Not implemented yet. Awaiting requests for it. *)

PROCEDURE SubdocumentParser(self: SGML.Parser; <*UNUSED*>systemId: TEXT): 
    SGML.Parser =
  BEGIN
    <*ASSERT FALSE*>
    RETURN self; <*NOWARN*>
  END SubdocumentParser;

PROCEDURE ApplicationInit(self: SGML.Application): SGML.Application =
  BEGIN
    RETURN self;
  END ApplicationInit;

(* Unfortunately the detailed location does not carry all the information.
   It allows for one full set of data within a file plus an entity name
   and offset. What we have in reality is a stack of entities when
   nested parameter entities are encountered. *)

PROCEDURE GetDetailedLocation(self: SGML.Application; 
    <*UNUSED*>pos: SGML.Position): SGML.DetailedLocation =
  VAR
    p := self.parser.p;
    s := self.parser.s;
    input: SGMLCScanner.Input;
    nl: SGML.DetailedLocation;
  BEGIN
    IF s.inputStack.size() = 0 THEN
      nl.byteOffset := p.offset();
      nl.lineNumber := p.line();
      nl.columnNumber := p.column();
      nl.entityOffset := 0;
      nl.entityName := NIL;
      nl.filename := s.input.name;
    ELSE
      input := s.inputStack.get(0);
      nl.lineNumber := input.currentLine;
      nl.columnNumber := input.currentCol;
      nl.byteOffset := input.offset;
      nl.entityOffset := p.offset();
      nl.entityName := s.input.name;
      nl.filename := input.name;
    END;
    RETURN nl;
  END GetDetailedLocation;

(* We are reusing the same scanner for catalogs as for SGML files. *)

PROCEDURE ParseCatalog(parser: SGML.Parser; name, dir: TEXT) =
  VAR
    ss: SGMLC.ScanSymbol;
    publicId, systemId: TEXT;
    e: REF SGML.Entity;
  BEGIN
    PushCatalogFile(parser,name,dir);
    TRY
      LOOP
        parser.s.get(ss);
        CASE ss.sym OF
        | SGMLC.Symbol.Eof => 
            EXIT;
        | SGMLC.Symbol.CATALOGkw =>
            parser.s.get(ss);
            PushCatalogFile(parser,ss.string,
                Pathname.Prefix(parser.s.input.name));
        | SGMLC.Symbol.PUBLICkw =>
            parser.s.get(ss);
            publicId := ss.string;
            parser.s.get(ss);
            systemId := ss.string;
            EVAL parser.defn.publics.put(publicId, systemId);
        | SGMLC.Symbol.SGMLDECLkw =>
            parser.s.get(ss);
        | SGMLC.Symbol.DOCTYPEkw =>
            e := NEW(REF SGML.Entity);
            parser.s.get(ss);
            e.name := ss.string;
            e.declType := SGML.EntityDeclType.Doctype;
            e.dataType := SGML.EntityDataType.Sgml;
            e.internalText := NIL;
            parser.s.get(ss);
            e.externalId := SGML.ExternalId{ss.string,NIL,NIL};
            AddEntity(parser,e);
        ELSE
          IF ss.string = NIL THEN ss.string := ""; END;
          parser.application.error(SGML.ErrorEvent{0,
              SGML.ErrorType.OtherError, "Invalid catalog entry " & 
              ss.string});
        END;
      END;
    EXCEPT ELSE
      parser.application.error(SGML.ErrorEvent{0,
          SGML.ErrorType.OtherError, "Premature end of file in catalog"});
    END;
  END ParseCatalog;

(* Find the catalog file along the available paths. *)

PROCEDURE PushCatalogFile(parser: SGML.Parser; name, dir: TEXT) =
  VAR
    fullName: TEXT;
    rd: Rd.T;
  BEGIN
    fullName := FindFile(name,dir,parser.options.addSearchDir);
    rd := NIL;
    IF fullName # NIL THEN rd := IO.OpenRead(fullName); 
    ELSE fullName := name; 
    END;

    IF rd = NIL THEN
      parser.application.error(SGML.ErrorEvent{0,SGML.ErrorType.OtherError,
          "Cannot open catalog " & fullName});
    ELSE
      parser.s.pushFile(fullName, rd);
    END;
  END PushCatalogFile;

(* Insert a new element in the table of defined elements. There may
   already be an entry for this element holding the description of its
   attributes. *)

PROCEDURE AddElement(parser: SGML.Parser; name: TEXT; content: REFANY;
    m: FSM.T; omitS, omitE: BOOLEAN) =
  VAR
    tmp: REFANY;
  BEGIN
    IF parser.defn.elements.get(name,tmp) AND parser.warn.duplicate THEN
      parser.application.error(SGML.ErrorEvent{0,SGML.ErrorType.OtherError,
          "Redefinition of element " & name});
    ELSE
      EVAL parser.defn.elements.put(name,NEW(ElementDesc, name := name,
          content := content, fsm := m, omitS := omitS, omitE := omitE));
    END;
  END AddElement;

(* Add the specified attributes to the list for the element. *)

PROCEDURE AddAttributes(parser: SGML.Parser; name: TEXT; 
    attributes: TextRefTbl.T) =
  VAR
    tmp: REFANY;
    iter: TextRefTbl.Iterator;
    t: TEXT;
    attlist: TextRefTbl.T;
  BEGIN
    IF parser.defn.attlists.get(name,tmp) THEN
      attlist := NARROW(tmp,TextRefTbl.T);
      iter := attlist.iterate();
      WHILE iter.next(t,tmp) DO
        EVAL attributes.put(t,tmp);
      END;
    END;

    EVAL parser.defn.attlists.put(name,attributes);
  END AddAttributes;

(* Add the entity into the appropriate table (general, parameter, doctype).
   For external entities, any missing components (system id, generated 
   system id) are computed using the "publics" table and the search paths. *)

PROCEDURE AddEntity(p: SGML.Parser; e: REF SGML.Entity) =
  VAR
    exist: BOOLEAN;
  BEGIN
    IF e.externalId.systemId = NIL AND e.externalId.publicId # NIL THEN
      EVAL p.defn.publics.get(e.externalId.publicId,e.externalId.systemId);
    END;
    IF e.externalId.systemId # NIL THEN
      e.externalId.generatedSystemId := FindFile(e.externalId.systemId,
          Pathname.Prefix(p.s.input.name),p.options.addSearchDir);
    END;
    CASE e.declType OF
    | SGML.EntityDeclType.General => exist := p.defn.gEntities.put(e.name,e);
    | SGML.EntityDeclType.Parameter => exist := p.defn.pEntities.put(e.name,e);
    | SGML.EntityDeclType.Doctype => exist := p.defn.dEntities.put(e.name,e);
    ELSE <* ASSERT FALSE *>
    END;
    IF exist AND p.warn.duplicate THEN
      p.application.error(SGML.ErrorEvent{0,SGML.ErrorType.OtherError,
          "Redefinition of entity " & e.name});
    END;
  END AddEntity;

(* Add the notation declaration in the table. *)

PROCEDURE AddNotation(parser: SGML.Parser; n: REF SGML.Notation) =
  BEGIN
    EVAL parser.defn.notations.put(n.name,n);
  END AddNotation;

(* Check that the DataChunk buffer is large enough. *)

PROCEDURE IncData(p: SGML.Parser) =
  BEGIN
    INC(p.nbData);
    IF p.nbData > LAST(p.dataChunks^) THEN
      WITH new = NEW(REF ARRAY OF SGML.CdataChunk,NUMBER(p.dataChunks^) * 2) DO
        SUBARRAY(new^,0,NUMBER(p.dataChunks^)) := p.dataChunks^;
        p.dataChunks := new;
      END;
    END;
  END IncData;

(* Check that the attribute buffer is large enough. *)

PROCEDURE IncAttr(p: SGML.Parser) =
  BEGIN
    INC(p.nbAttr);
    IF p.nbAttr > LAST(p.attributes^) THEN
      WITH new = NEW(REF ARRAY OF SGML.Attribute,NUMBER(p.attributes^) * 2) DO
        SUBARRAY(new^,0,NUMBER(p.attributes^)) := p.attributes^;
        p.attributes := new;
      END;
    END;
  END IncAttr;

(* Find a parameter entity from the table. *)

PROCEDURE ResolveParameterEntity(self: EntityResolver; name: TEXT): Rd.T =
  BEGIN
    RETURN ResolveEntity(self.parser,name,SGML.EntityDeclType.Parameter);
  END ResolveParameterEntity;

(* Find an entity of the specified type from the corresponding table and
   open a reader for it. *)

PROCEDURE ResolveEntity(self: SGML.Parser; name: TEXT; 
    type: SGML.EntityDeclType): Rd.T =
  VAR
    e: REF SGML.Entity;
  BEGIN
    e := GetEntity(self,name,type);

    IF e # NIL THEN
      IF e.internalText # NIL THEN RETURN TextRd.New(e.internalText); END;
      IF e.externalId.generatedSystemId = NIL THEN RETURN NIL; END;
      RETURN IO.OpenRead(e.externalId.generatedSystemId);
    ELSE
      RETURN NIL;
    END;
  END ResolveEntity;

PROCEDURE GetEntity(self: SGML.Parser; name: TEXT; type: SGML.EntityDeclType):
    REF SGML.Entity =
  VAR
    tmp: REFANY;
    found: BOOLEAN;
  BEGIN
    CASE type OF
    | SGML.EntityDeclType.General => 
        found := self.defn.gEntities.get(name,tmp);
    | SGML.EntityDeclType.Parameter => 
        found := self.defn.pEntities.get(name,tmp);
    | SGML.EntityDeclType.Doctype => 
        found := self.defn.dEntities.get(name,tmp);
    ELSE <* ASSERT FALSE *>
    END;

    IF found THEN RETURN NARROW(tmp, REF SGML.Entity);
    ELSE RETURN NIL;
    END;
  END GetEntity;

(* Look for a file along the search paths or in the current directory. *)

PROCEDURE FindFile(name, cwd: TEXT; dirs: REF ARRAY OF TEXT): TEXT =
  VAR
    absName: TEXT;
  BEGIN
    IF Pathname.Absolute(name) THEN RETURN name; END;

    TRY
      absName := Pathname.Join(cwd,name,NIL);
      EVAL FS.Status(absName);
      RETURN absName;
    EXCEPT OSError.E => END;

    FOR i := 0 TO LAST(dirs^) DO
      TRY
        absName := Pathname.Join(dirs[i],name,NIL);
        EVAL FS.Status(absName);
        RETURN absName;
      EXCEPT OSError.E => END;
    END;
    RETURN NIL;
  END FindFile;

(* Initialize the stack of FSM states which keep track of what was received
   in which nested element. To start, we assume we are in an element (!INIT)
   which expects a single element named after the document type. *)

PROCEDURE StartContent(parser: SGML.Parser) =
  VAR
    fsm: FSM.T;
  BEGIN
    FSM.New(fsm,Atom.FromText(parser.doctype));
    FSM.Wrap(fsm); <*NOWARN*> (* This FSM is valid *)
    parser.currState := NEW(StateFrame, name := StartAtom, omitE := FALSE,
        currNode := FSM.StartNode(fsm), fsm := fsm);
    parser.pcdata := FALSE;
  END StartContent;

(* This is the end. Any currently opened element should allow an
   end tag next, and the end tag should be optional. *)

PROCEDURE ExplainNode(fsm: FSM.T; id: CARDINAL; edges: REF ARRAY OF FSM.Edge; 
    else, skip: FSM.Node): TEXT =
  VAR
    t: TEXT;
  BEGIN
    t := "Current state is node " & Fmt.Unsigned(id) & 
    " with possible transitions:\n";

    FOR i := 0 TO LAST(edges^) DO
      t := t & "    event " & Atom.ToText(edges[i].label) & " leads to node " &
      Fmt.Unsigned(FSM.NodeId(fsm,edges[i].destination)) & "\n";
    END;

    IF else # NIL THEN
      t := t & "    else leads to node " & Fmt.Unsigned(FSM.NodeId(fsm,else)) &
      "\n";
    END;

    IF skip # NIL THEN
      t := t & "    skip leads to node " & Fmt.Unsigned(FSM.NodeId(fsm,skip)) &
      "\n";
    END;
    RETURN t;
  END ExplainNode;

PROCEDURE ExplainFSMError(parser: SGML.Parser; error: SGML.ErrorType; 
    t: TEXT) =
  VAR
    explanation := "\n";
    id: CARDINAL;
    skip, else: FSM.Node;
    edges: REF ARRAY OF FSM.Edge;
    fsm := parser.currState.fsm;
    node := parser.currState.currNode;
  BEGIN
    (* This is a bit verbose, output only when warnings enabled *)
    IF parser.warn.unclosed THEN
      WHILE node # NIL DO
        FSM.NodeContent(fsm, node, id, edges, else, skip);
        explanation := explanation & ExplainNode(fsm, id, edges, else, skip);
        node := skip;
      END;
    END;

    parser.application.error(SGML.ErrorEvent{0,error,t & explanation});
  END ExplainFSMError;

PROCEDURE EndContent(parser: SGML.Parser) =
  BEGIN
    LOOP
      IF parser.stateStack.size() = 0 THEN RETURN; END;
      IF NOT(FSM.Exit(parser.currState.currNode) AND 
             parser.currState.omitE) THEN
        ExplainFSMError(parser,SGML.ErrorType.OtherError,
            "Premature end of document missing end tag: " & 
            Atom.ToText(parser.currState.name));
      END;
      IF parser.warn.unclosed THEN
        parser.application.error(SGML.ErrorEvent{0,SGML.ErrorType.Warning,
            "Omitted </" & Atom.ToText(parser.currState.name) & ">"}); 
      END;
      parser.application.endElement(SGML.EndElementEvent{parser.p.offset(),
          Atom.ToText(parser.currState.name)});
      parser.currState := parser.stateStack.remhi();
    END;
  END EndContent;

(* A new element starts. *)

PROCEDURE StartElement(parser: SGML.Parser; 
    READONLY e: SGML.StartElementEvent) =
  BEGIN
    parser.pcdata := FALSE;
    StartElement2(parser,e);
  END StartElement;

PROCEDURE StartData(parser: SGML.Parser) =
  BEGIN
    IF NOT parser.pcdata THEN
      StartElement2(parser,SGML.StartElementEvent{0,"#PCDATA",
          SGML.ElementContentType.Empty,FALSE,NIL});
    END;
  END StartData;

PROCEDURE StartElement2(parser: SGML.Parser; 
    READONLY e: SGML.StartElementEvent) =
  VAR
    nextNode: FSM.Node;
    name := Atom.FromText(e.gi);
    empty := e.contentType = SGML.ElementContentType.Empty;
    element: ElementDesc;
    tmp: REFANY;
    expect: Atom.T;
    ne: SGML.StartElementEvent;
  BEGIN
    LOOP
      (* The element is accepted within the current element. *)

      IF FSM.Enter(parser.currState.currNode,name,nextNode) THEN
        IF NOT parser.defn.elements.get(e.gi,tmp) THEN
          parser.application.error(SGML.ErrorEvent{0,SGML.ErrorType.
              OtherError,"Unknown element " & e.gi});
          RETURN;
        END;
        parser.currState.currNode := nextNode;
        element := tmp;
        IF empty THEN RETURN; END;
        parser.stateStack.addhi(parser.currState);
        parser.currState := NEW(StateFrame, name := name, 
            omitE := element.omitE, currNode := FSM.StartNode(element.fsm),
            fsm := element.fsm);
        RETURN;

      (* Perhaps the current element should end (end tag allowed next and is
         optional). Then the new element may become allowed. *)

      ELSIF FSM.Exit(parser.currState.currNode) AND parser.currState.omitE AND
          parser.stateStack.size() > 1 THEN
        IF parser.warn.unclosed THEN
          parser.application.error(SGML.ErrorEvent{0,SGML.ErrorType.Warning,
              "Omitted </" & Atom.ToText(parser.currState.name) & ">"}); 
        END;

        parser.application.endElement(SGML.EndElementEvent{parser.p.offset(),
            Atom.ToText(parser.currState.name)});
        parser.currState := parser.stateStack.remhi();

      (* Perhaps a single type of element is accepted here and its start
         tag is optional, in which case it is provided. Then the new element
         may become allowed. *)

      ELSE
        expect := FSM.Expect(parser.currState.currNode);
        IF expect # NIL AND parser.defn.elements.get(Atom.ToText(expect),tmp) 
            AND NARROW(tmp,ElementDesc).omitS THEN
          EVAL FSM.Enter(parser.currState.currNode,expect,nextNode);
          parser.currState.currNode := nextNode;
          element := tmp;
          ne.pos := parser.p.offset();
          ne.gi := element.name;
          IF element.content # EmptyAtom THEN
            ne.contentType := SGML.ElementContentType.Mixed;
          ELSE
            ne.contentType := SGML.ElementContentType.Empty;
          END;
          ne.included := FALSE;
          ne.attributes := NEW(REF ARRAY OF SGML.Attribute, 0);
          IF parser.warn.unclosed THEN
          parser.application.error(SGML.ErrorEvent{0,SGML.ErrorType.Warning,
              "Omitted <" & ne.gi & ">"}); 
          END;

          parser.application.startElement(ne);
          parser.stateStack.addhi(parser.currState);
          parser.currState := NEW(StateFrame, name := expect, 
              omitE := element.omitE, currNode := FSM.StartNode(element.fsm),
              fsm := element.fsm);
        ELSE
          ExplainFSMError(parser,SGML.ErrorType.OtherError,
              "Misplaced tag " & e.gi & " within element " &
              Atom.ToText(parser.currState.name));
          RETURN;
        END;
      END;
    END;
  END StartElement2;

(* End element received. *)

PROCEDURE EndElement(parser: SGML.Parser; n: TEXT) =
  VAR
    name := Atom.FromText(n);
    mayExit := FSM.Exit(parser.currState.currNode);
    ne: SGML.StartElementEvent;
  BEGIN
    parser.pcdata := FALSE;
    LOOP
      (* Correct end tag received. *)

      IF parser.currState.name = name THEN

        (* The element content is not complete! *)
        IF NOT mayExit THEN
          ExplainFSMError(parser,SGML.ErrorType.OtherError,
              "Premature end tag " & n);
        END;

        parser.currState := parser.stateStack.remhi();
        RETURN;

      (* Incorrect end tag. Perhaps an indication of missing omitted
         end tags. Check if the current element is complete and its
         end tag optional. If so, generate the omitted end tag and
         retry at the next level, in the loop. *)

      ELSIF mayExit AND parser.currState.omitE AND 
          parser.stateStack.size() > 1 THEN
        IF parser.warn.unclosed THEN
          parser.application.error(SGML.ErrorEvent{0,SGML.ErrorType.Warning,
              "Omitted </" & Atom.ToText(parser.currState.name) & ">"}); 
        END;

        parser.application.endElement(SGML.EndElementEvent{parser.p.offset(),
            Atom.ToText(parser.currState.name)});
        parser.currState := parser.stateStack.remhi();

      (* This end tag just should not be there. Create a dummy start tag
         just to keep these balanced. *)

      ELSE
        ExplainFSMError(parser,SGML.ErrorType.OtherError,
            "Misplaced end tag " & n & " within element " &
            Atom.ToText(parser.currState.name));

        ne.pos := parser.p.offset();
        ne.gi := n;
        ne.contentType := SGML.ElementContentType.Empty;
        ne.included := FALSE;
        ne.attributes := NEW(REF ARRAY OF SGML.Attribute, 0);
        parser.application.startElement(ne);
        RETURN;
      END;
    END;
  END EndElement;


VAR
  emptyArray := NEW(REF ARRAY OF TEXT,0);

(* Copy a text array, use an empty array if NIL, and append "add1"
   if non NIL. Used to copy optional parsing options, merged with a
   possible default value "add1". *)

PROCEDURE CopyTextArray(VAR a: REF ARRAY OF TEXT; add1: TEXT := NIL) =
  VAR
    newArray: REF ARRAY OF TEXT;
    size: CARDINAL;
  BEGIN
    IF a = NIL THEN a := emptyArray; END;
    size := NUMBER(a^);
    IF add1 # NIL THEN INC(size); END;
    newArray := NEW(REF ARRAY OF TEXT,size);
    SUBARRAY(newArray^,0,NUMBER(a^)) := a^;
    IF add1 # NIL THEN newArray[LAST(newArray^)] := add1; END;
    a := newArray;
  END CopyTextArray;

PROCEDURE CopyTextRefTbl(tbl: TextRefTbl.T): TextRefTbl.Default =
  VAR
    new := NEW(TextRefTbl.Default).init();
    iter := tbl.iterate();
    k: TEXT;
    v: REFANY;
  BEGIN
    WHILE iter.next(k,v) DO EVAL new.put(k,v); END;
    RETURN new;
  END CopyTextRefTbl;

PROCEDURE CopyTextTextTbl(tbl: TextTextTbl.T): TextTextTbl.Default =
  VAR
    new := NEW(TextTextTbl.Default).init();
    iter := tbl.iterate();
    k, v: TEXT;
  BEGIN
    WHILE iter.next(k,v) DO EVAL new.put(k,v); END;
    RETURN new;
  END CopyTextTextTbl;

PROCEDURE CopyDefinitions(READONLY from: Definitions; VAR to: Definitions) =
  BEGIN
    to.elements := CopyTextRefTbl(from.elements);
    to.attlists := CopyTextRefTbl(from.attlists);
    to.notations := CopyTextRefTbl(from.notations);
    to.pEntities := CopyTextRefTbl(from.pEntities);
    to.gEntities := CopyTextRefTbl(from.gEntities);
    to.dEntities := CopyTextRefTbl(from.dEntities);
    to.publics := CopyTextTextTbl(from.publics);
  END CopyDefinitions;

PROCEDURE DumpDefinitions(this: Parser) =
  BEGIN
  END DumpDefinitions;

BEGIN
  (* Preallocated values to indicate the type of marked section. *)
  CDataParam := NEW(REF ARRAY OF SGML.MarkedSectionParam, 1);
  IncludeParam := NEW(REF ARRAY OF SGML.MarkedSectionParam, 1);
  IgnoreParam := NEW(REF ARRAY OF SGML.MarkedSectionParam, 1);
  CDataParam[0].type := SGML.MarkedSectionParamType.CData;
  IncludeParam[0].type := SGML.MarkedSectionParamType.Include;
  IgnoreParam[0].type := SGML.MarkedSectionParamType.Ignore;
  CDataParam[0].entityName := "CData";
  IncludeParam[0].entityName := "Include";
  IgnoreParam[0].entityName := "Ignore";
  PCDataAtom := Atom.FromText("#PCDATA");
  EmptyAtom := Atom.FromText("EMPTY");
END SGML.

