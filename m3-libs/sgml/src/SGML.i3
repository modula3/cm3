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

(* A Parser object processes the specified sgml files, and calls methods on
   a user defined Application object for each significant parsing event.
   The user defined Application object overrides the methods to react
   appropriately to these events (e.g. print back a modified sgml file,
   construct an abstract syntax tree...). *)

INTERFACE SGML;

IMPORT Rd;

TYPE
  ParserOptions = RECORD
      showOpenEntities, showOpenElements, outputCommentDecls,
      outputMarkedSections, outputGeneralEntities, 
      mapCatalogDocument: BOOLEAN := FALSE;
      defaultDoctype: TEXT;
      addCatalog, includeParam, enableWarning, addSearchDir,
      activateLink, architecture: REF ARRAY OF TEXT := NIL;
    END;

(* These options define the behavior of the parser.

   "ShowOpenEntities" and "showOpenElements" produce information about
   the corresponding entity and element when parsing error messages are
   issued. "OutputCommentDecls", "outputMarkedSections" and 
   "outputGeneralEntities" determine if the parser produces events 
   when these SGML constructs are encountered.

   "DefaultDoctype" specifies the document type definition to use when
   no DOCTYPE tag is found.

   "AddCatalog" adds the specified file names as SGML DTD catalogs.
   "IncludeParam" defines the specified names as parameter entities
   set to "INCLUDE" (ENTITY % param "INCLUDE"); this way, sections
   of sgml files which were "IGNORE" by default may be changed to
   "INCLUDE" just by setting this option. "EnableWarning" enables the
   named warnings: "mixed" mixed content model which does not allow #PCDATA, 
   "sgmldecl" dubious constructs in SGML declarations, 
   "should" non followed ISO 8879 recommendations, 
   "default" defaulted references,
   "duplicate" duplicate entity declarations, 
   "undefined" undefined elements used in the DTD, 
   "unclosed" unclosed start and end tags, 
   "empty" empty start and end tags, "net" net-enabling start and end tags, 
   "min-tag" minimized start and end tags (equivalent to unclosed, empty 
   and net), "unused-map" defined but unused short reference maps, 
   "unused-param" defined but unused parameter entities, 
   "notation-sysid" notation for which no system identifier could be
   generated, "all" equivalent to all the above,
   "no-idref" do not warn about unresolved references, "no-significant" 
   do not warn about non significant characters in literals. *)

  Parser <: ParserPublic;

  ParserPublic = OBJECT METHODS
      init(options: ParserOptions; programName: TEXT; 
          files: REF ARRAY OF TEXT; rds: REF ARRAY OF Rd.T := NIL): Parser;
      run(a: Application): CARDINAL;
      halt();
      inhibitMessages(inhibit: BOOLEAN);
      subdocumentParser(systemId: TEXT): Parser;
      newParser(files: REF ARRAY OF TEXT; rds: REF ARRAY OF Rd.T := NIL): 
          Parser;
    END;

(* The call "p.init(o,p,f,r)" initializes a parser with options "o",
   program name "p" (used in error messages), and "f" the array of names
   of files to be parsed. When "r" is not specified, files from "f" are
   opened. Otherwise, "f" is used for file names but the actual input is
   taken from the readers in "r".

   The call "p.run(a)" starts parsing the files and calls back the
   Application object "a" for each parsing event. It returns the number
   of errors encountered once the parsing is through.

   The call "p.halt()" stops the parsing, causing the run method to return.
   It is usually called from one the the Application object methods.

   The call "p.inhibitMessages(b)" disables error and warning messages when
   b is TRUE.

   The call "p.subdocumentParser(s)" creates a new parser ready to process
   "s" which identifies a subdocument in the context of the file currently
   parsed by "p".

   The call "p.newParser(f,r)" returns a parser using the same options as
   "p" but ready to process a new set of files defined by "f" and "r".
   Since the options are the same (catalog name, search paths...), caching
   of parsed document type definitions may occur for a significant speedup.

*)

  Application <: ApplicationPublic;

  ApplicationPublic = OBJECT METHODS
      init(): Application;
      appInfo(READONLY e: AppinfoEvent);
      startDtd(READONLY e: StartDtdEvent);
      endDtd(READONLY e: EndDtdEvent);
      endProlog(READONLY e: EndPrologEvent);
      startElement(READONLY e: StartElementEvent);
      endElement(READONLY e: EndElementEvent);
      data(READONLY e: DataEvent);
      sdata(READONLY e: SdataEvent);
      pi(READONLY e: PiEvent);
      externalDataEntityRef(READONLY e: ExternalDataEntityRefEvent);
      subdocEntityRef(READONLY e: SubdocEntityRefEvent);
      nonSgmlChar(READONLY e: NonSgmlCharEvent);
      commentDecl(READONLY e: CommentDeclEvent);
      markedSectionStart(READONLY e: MarkedSectionStartEvent);
      markedSectionEnd(READONLY e: MarkedSectionEndEvent);
      ignoredChars(READONLY e: IgnoredCharsEvent);
      generalEntity(READONLY e: GeneralEntityEvent);
      error(READONLY e: ErrorEvent);
      openEntityChange();
      getDetailedLocation(pos: Position): DetailedLocation;
    END;

(* An instance of the "Application" type, or one of its descendant type, is
   passed to a Parser and receives the parsing information as methods
   being called back. Each of these methods receives a corresponding
   parsing event structure.

   The call "a.init()" initializes "a", before it is used for parsing.

   The call "a.getDetailedLocation(pos)" returns detailed information about
   the location of "pos" within the currently parsed entity. It may only be
   called from within one of the other methods.

   The other methods are called by the Parser and are overidden in Application
   type descendants to perform the desired work. "AppInfo" is called when the
   APPINFO section of the SGML declaration is encountered, "startDtd" upon
   encountering the Document Type Definition (DTD), "endDtd" at the end of 
   the DTD, "endProlog" at the end of the prolog (local markup declarations), 
   "startElement" when a start element tag is found, "endElement" for a 
   real or implied end element tag, "data" for character data (CDATA) 
   within elements or marked sections, "sdata" for special character 
   data (SDATA like bitmap images), "pi" for a processing instruction, 
   "externalDataEntityRef" for a reference to an external data entity, 
   "subdocEntityRef" for a reference to a subdoc entity, 
   "nonSgmlChar" for non SGML conforming characters, "commentDecl" for
   a sequence of comments, "markedSectionStart" at the beginning of a marked
   section, "markedSectionEnd" at the end of a marked section, "ignoredChars"
   for character data within an "IGNORE" marked section, "generalEntity"
   for a general entity definition (this occurs within the prolog except
   for undefined entities which when referenced are set to the default
   entity content), "error" upon encountering a parsing error, and 
   "openEntityChange" each time the currently opened entity changes. *)


PROCEDURE CharRefToCode(t: TEXT; VAR c: CharCode): BOOLEAN;

(* While the input files only contain 8 bits ISO-8859 character codes, larger
   16 bits UNICODE codes may be specified by (decimal or hexadecimal) character
   references. For this reason, all such 16 bits codes are kept
   "escaped" as character references. Moreover, the special ampersand
   character (&amp;) is also kept as an entity reference throughout the
   processing. This allows all the processing to use ordinary TEXT elements
   which are limited to 8 bits characters. The call "CharRefToCode(t,c)"
   return "TRUE" when a valid character reference is received in "t" 
   and returns the corresponding code in "c". A valid character reference is
   either &amp;amp;, or &amp;#decimalNumber;, or &amp;#xHexaNumber;,
   with the "number" within the interval 0..65535). This procedure is
   typically used by applications to process 16 bits characters escaped 
   as character references in Sdata events. *)

TYPE
  CharCode = [0..65535];

  Position = CARDINAL;

  ExternalId = RECORD
      systemId: TEXT;
      publicId: TEXT;
      generatedSystemId: TEXT;
    END;
  (* Depending on the type of external identifier, each of these 
     fields may or may not be available (non NIL). At least one should 
     be non NIL. *)

  Notation = RECORD
      name: TEXT;
      externalId: ExternalId;
    END;
  (* A named notation with the corresponding external identifier. *)

  EntityDataType = { Sgml, CData, SData, NData, Subdoc, Pi };

  EntityDeclType = { General, Parameter, Doctype, Linktype };

  Entity = RECORD
      name: TEXT;
      dataType: EntityDataType;
      declType: EntityDeclType;
      internalText: TEXT;
      (* Following valid if internalText is NIL *)
      externalId: ExternalId;
      attributes: REF ARRAY OF Attribute;
      notation: Notation;
    END;
  (* For an internal entity, the replacement text is found in "internalText".
     For external entities, an external identifier, attributes and a notation
     are provided. *)

  AttributeType = { Invalid, Implied, CData, Tokenized };

  AttributeDefaulted = { Specified, Definition, Current };

  CdataChunk = RECORD
      nonSgmlChar: CHAR;
      data: TEXT;
      entityName: TEXT;
    END;
  (* For an SDATA entity reference, entityName is the entity name and data
     the replacement text. For normal data, entityName is NIL and data
     contains the character data. For non SGML conforming characters,
     data and entityName are NIL and nonSgmlChar contains the character. *)

  Attribute = RECORD
      name: TEXT;
      type: AttributeType;
      defaulted: AttributeDefaulted;
      cdataChunks: REF ARRAY OF CdataChunk;
      tokens: TEXT;
      isId: BOOLEAN;
      isGroup: BOOLEAN;
      entities: REF ARRAY OF Entity;
      notation: Notation;
    END;
  (* If the attribute type is Cdata, the value is found in "cdataChunks",
     otherwise if the type is Tokenized, the value is found in "tokens".
     For an attribute type NOTATION notation is defined, ENTITY
     or ENTITIES entities is defined. The field isId is TRUE for an attribute
     of type ID. *)

  (* The event structures all contain a position which may be used to
     obtain detailed position information. *)

  PiEvent = RECORD
      pos: Position;
      data: TEXT;
      entityName: TEXT;
    END;
  (* The content of the processing instruction is in data. If it was
     an entity reference, the entityName is provided (non NIL). *)

  ElementContentType = { Empty, CData, RCData, Mixed, Element };

  StartElementEvent = RECORD
      pos: Position;
      gi: TEXT;
      contentType: ElementContentType;
      included: BOOLEAN;
      attributes: REF ARRAY OF Attribute;
    END;
  (* The element type (tag name) is in gi. *)
      
  EndElementEvent = RECORD
      pos: Position;
      gi: TEXT;
    END;
  (* The element type is in gi. *)

  DataEvent = RECORD
      pos: Position;
      data: TEXT;
    END;

  SdataEvent = RECORD
      pos: Position;
      text: TEXT;
      entityName: TEXT;
    END;
  (* Reference to an internal sdata entity. The replacement text is in text
     and the referenced entity in entityName. *)

  ExternalDataEntityRefEvent = RECORD
      pos: Position;
      entity: Entity;
    END;

  SubdocEntityRefEvent = RECORD
      pos: Position;
      entity: Entity;
    END;

  NonSgmlCharEvent = RECORD
      pos: Position;
      c: CHAR;
    END;

  ErrorType = { Info, Warning, Quantity, IDRef, Capacity, OtherError };

  ErrorEvent = RECORD
      pos: Position;
      type: ErrorType;
      message: TEXT;
    END;

  AppinfoEvent = RECORD
      pos: Position;
      string: TEXT;
    END;

  StartDtdEvent = RECORD
      pos: Position;
      name: TEXT;
      (* If it does not have an external ID all names within will be NIL *)
      externalId: ExternalId;
    END;

  EndDtdEvent = RECORD
      pos: Position;
      name: TEXT;
    END;

  EndPrologEvent = RECORD
      pos: Position;
    END;

  GeneralEntityEvent = RECORD
      entity: Entity;
    END;

  CommentDeclEvent = RECORD
      pos: Position;
      comments: REF ARRAY OF TEXT;
      seps: REF ARRAY OF TEXT;
    END;

  MarkedSectionStatus = { Include, RCData, CData, Ignore };

  MarkedSectionParamType = { Temp, Include, RCData, CData, Ignore, EntityRef };

  MarkedSectionParam = RECORD
      type: MarkedSectionParamType;
      entityName: TEXT;
    END;

  MarkedSectionStartEvent = RECORD
      pos: Position;
      status: MarkedSectionStatus;
      params: REF ARRAY OF MarkedSectionParam;
    END;

  MarkedSectionEndEvent = RECORD
      pos: Position;
      status: MarkedSectionStatus;
    END;

  IgnoredCharsEvent = RECORD
      pos: Position;
      data: TEXT;
    END;

  DetailedLocation = RECORD
      lineNumber: CARDINAL;
      columnNumber: CARDINAL;
      byteOffset: CARDINAL;
      entityOffset: CARDINAL;
      entityName: TEXT;
      filename: TEXT;
    END;

(* Debugging *)

PROCEDURE DumpDefinitions(this: Parser);


END SGML.

