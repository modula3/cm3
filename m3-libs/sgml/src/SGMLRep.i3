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

INTERFACE SGMLRep;

(* This interface adds to the SGML parser object a number of fields
   required to store information about the current state and defined
   elements, attributes and entities. It also defines procedures
   used during the parsing process. *)

IMPORT SGML, SGMLC, RefSeq, TextSeq, TextRefTbl, Atom, FSM;

REVEAL
  SGML.Parser <: ParserPublicRep;

TYPE
  StateFrame <: REFANY;

  ParserPublicRep = SGML.ParserPublic OBJECT
      application: SGML.Application;
      p: SGMLC.Parser;
      s: SGMLC.Scanner;
      attributes: REF ARRAY OF SGML.Attribute;
      nbAttr: CARDINAL;
      dataChunks: REF ARRAY OF SGML.CdataChunk;
      nbData: CARDINAL;
      doctype: TEXT;
      markup: BOOLEAN;
      pcdata: BOOLEAN;
    END;

(* The "attributes" and "dataChunks" fields store intermediate
   results during the parsing. *)

PROCEDURE ParseDtd(parser: SGML.Parser; externalId: SGML.ExternalId);

(* Start parsing the document type definition, specified by the
   external identifier. *)

PROCEDURE AddElement(parser: SGML.Parser; name: TEXT; content: REFANY;
    m: FSM.T; omitS, omitE: BOOLEAN);

(* Insert a new element definition. *)

PROCEDURE AddEntity(p: SGML.Parser; e: REF SGML.Entity);

(* Insert a new entity definition. *)

PROCEDURE AddNotation(p: SGML.Parser; n: REF SGML.Notation);

(* Insert a new notation definition. *)

PROCEDURE AddAttributes(parser: SGML.Parser; name: TEXT; 
    attributes: TextRefTbl.T);

(* Insert a new attributes definition for the named element. *)

PROCEDURE GetEntity(self: SGML.Parser; name: TEXT; type: SGML.EntityDeclType):
    REF SGML.Entity;

(* Get the specified named "name" of type "type". *)

PROCEDURE IncData(p: SGML.Parser);

(* Prepare the "dataChunks" buffer to add one data chunk. *)

PROCEDURE IncAttr(p: SGML.Parser);

(* Prepare the "attributes" buffer to add one attribute. *)

PROCEDURE StartContent(parser: SGML.Parser);

(* Initialize the element stack before parsing the document content. *)

PROCEDURE EndContent(parser: SGML.Parser);

(* End of document content, check that no unended element remains.
   Add omitted tags as needed. *)

PROCEDURE StartElement(parser: SGML.Parser; 
    READONLY e: SGML.StartElementEvent);

(* Begin a new element in the document content. Add omitted tags as needed. *)

PROCEDURE EndElement(parser: SGML.Parser; n: TEXT);

(* End an element in the document content. Add omitted tags as needed. *)

PROCEDURE StartData(parser: SGML.Parser);

(* Some character data found (data or sdata) *)

PROCEDURE CopyTextRefTbl(tbl: TextRefTbl.T): TextRefTbl.Default;

(* Copy a TextRefTbl *)

TYPE
  ParserPlus = SGMLC.Parser OBJECT
      parser: SGML.Parser;
    END;

  ElementDesc = REF RECORD
      name: TEXT;
      content: REFANY;
      fsm: FSM.T;
      omitS, omitE: BOOLEAN;
    END;

  (* The original content describing expression is stored in "content".
     The corresponding FSM is in fsm. Fields "omitS" and "omitE" tell if the
     start and end tags may be omitted. *)

  AttributeDesc = REF RECORD
      name: TEXT;
      content: REFANY;
      default: REF ARRAY OF SGML.CdataChunk;
      required, implied, fixed := FALSE;
    END;

(* The types "Enumeration", "Choice", "Seq", "Optional", "Repeat" and
   "RepeatPlus" are used for the content description expression. *)

  Enumeration = TextSeq.T BRANDED OBJECT END;

  Choice = RefSeq.T BRANDED OBJECT END;

  Seq = RefSeq.T BRANDED OBJECT END;

  Optional = BRANDED OBJECT r: REFANY; END;

  Repeat = BRANDED OBJECT r: REFANY; END;

  RepeatPlus = BRANDED OBJECT r: REFANY; END;

  NotationType = RefSeq.T BRANDED OBJECT END;

  RefType = { Hex, Char, Name };

VAR (* CONST *)
  CDataParam: REF ARRAY OF SGML.MarkedSectionParam;
  IncludeParam: REF ARRAY OF SGML.MarkedSectionParam;
  IgnoreParam: REF ARRAY OF SGML.MarkedSectionParam;
  PCDataAtom: Atom.T;
  EmptyAtom: Atom.T;

CONST 
  Debug = FALSE;

END SGMLRep.
