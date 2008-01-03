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


MODULE SGMLPrint;

IMPORT Wr, SGML, SGMLElementSeq, Text, Thread, Fmt;

REVEAL
  T = Public BRANDED OBJECT
      parseMode: ParseMode;
    OVERRIDES
      init := Init;
      appInfo := AppInfo;
      startDtd := StartDtd;
      endDtd := EndDtd;
      endProlog := EndProlog;
      startElement := StartElement;
      endElement := EndElement;
      data := Data;
      sdata := SData;
      pi := Pi;
      externalDataEntityRef := ExternalDataEntityRef;
      subdocEntityRef := SubdocEntityRef;
      nonSgmlChar := NonSgmlChar;
      commentDecl := CommentDecl;
      markedSectionStart := MarkedSectionStart;
      markedSectionEnd := MarkedSectionEnd;
      ignoredChars := IgnoredChars;
      generalEntity := GeneralEntity;
      error := Error;
      openEntityChange:= OpenEntityChange;
    END;

TYPE
  ParseMode = {Normal, RCData, CData};

  (* OutputState = {PendingNL, Other, StartTag}; *)

<*FATAL Thread.Alerted*>
<*FATAL Wr.Failure*>

(* For each parser event, print the associated SGML file content.
   The output may differ slightly from the input since several
   input variations produce the same parsing events. For instance,
   omitted end tags are detected by the parser and inserted. Since
   they are reported as parsing events they always appear on output.

   The output corresponding to the content of each "event" structure
   is not necessarily obvious because of the flexibility/complexity
   of SGML. *)

PROCEDURE AppInfo(<*UNUSED*>self: T; 
    <*UNUSED*>READONLY e: SGML.AppinfoEvent) =
  VAR
  BEGIN
  END AppInfo;

PROCEDURE StartDtd(self: T; 
    READONLY e: SGML.StartDtdEvent) =
  VAR
  BEGIN
    IF e.externalId.systemId # NIL OR e.externalId.publicId # NIL OR
        e.externalId.generatedSystemId # NIL THEN
      Wr.PutText(self.wr,"<!DOCTYPE " & e.name & " ");
      IF e.externalId.publicId # NIL THEN
        Wr.PutText(self.wr,"PUBLIC \"" & e.externalId.publicId & "\"");
      ELSE
        Wr.PutText(self.wr,"SYSTEM");
      END;
      IF e.externalId.systemId # NIL THEN
        Wr.PutText(self.wr," \"" & e.externalId.systemId & "\"");
      END;
      Wr.PutText(self.wr,">\n");
    END;
  END StartDtd;

PROCEDURE EndDtd(<*UNUSED*>self: T; 
      <*UNUSED*>READONLY e: SGML.EndDtdEvent) =
  VAR
  BEGIN
  END EndDtd;

PROCEDURE EndProlog(<*UNUSED*>self: T; 
      <*UNUSED*>READONLY e: SGML.EndPrologEvent) =
  VAR
  BEGIN
  END EndProlog;

PROCEDURE StartElement(self: T; 
      READONLY e: SGML.StartElementEvent) =
  VAR
  BEGIN
    IF self.stack.size() > 0 AND 
        self.stack.gethi().contentType = SGML.ElementContentType.Element THEN
      Wr.PutText(self.wr,"\n");
    END;

    self.stack.addhi(e);

    IF self.parseMode # ParseMode.Normal THEN
      Wr.PutText(self.wr,"]]><! INCLUDE [");
    END;

    CASE e.contentType OF
    | SGML.ElementContentType.CData => self.parseMode := ParseMode.CData;
    | SGML.ElementContentType.RCData => self.parseMode := ParseMode.RCData;
    ELSE
      self.parseMode := ParseMode.Normal;
    END;
    Wr.PutText(self.wr,"<" & e.gi);
    FOR i := 0 TO LAST(e.attributes^) DO
      WITH a = e.attributes[i] DO
        CASE a.type OF
        | SGML.AttributeType.CData =>
            IF a.defaulted # SGML.AttributeDefaulted.Definition THEN
              Wr.PutText(self.wr," " & a.name & "=" & "\"");
              FOR j := 0 TO LAST(a.cdataChunks^) DO
                IF a.cdataChunks[j].entityName # NIL THEN
                  EntityRef(self,a.cdataChunks[j].entityName);
                ELSIF a.cdataChunks[j].data # NIL THEN
                  PrintChars(self,a.cdataChunks[j].data);
                ELSE
                  Wr.PutText(self.wr,"&#" & 
                      Fmt.Int(ORD(a.cdataChunks[j].nonSgmlChar)) & ";");
                END;
              END;
              Wr.PutText(self.wr,"\"");
            END;
        | SGML.AttributeType.Tokenized =>
            IF a.defaulted # SGML.AttributeDefaulted.Definition THEN
              IF a.isGroup AND Text.Equal(a.name,a.tokens) THEN
                Wr.PutText(self.wr," " & a.tokens);
              ELSE
                Wr.PutText(self.wr," " & a.name & "=" & "\"" & a.tokens & 
                    "\"");
              END;
            END;
        ELSE
        END;
      END;
    END;
    Wr.PutText(self.wr,">");
  END StartElement;

PROCEDURE PrintChars(self: T; t: TEXT) =
  VAR
    c: CHAR;
  BEGIN
    FOR i := 0 TO Text.Length(t) - 1 DO
      c := Text.GetChar(t,i);
      IF self.parseMode = ParseMode.CData THEN
        Wr.PutChar(self.wr,c);
      ELSE
        CASE c OF
        | '"' => Wr.PutText(self.wr,"&quot;");
        (* | '&' => Wr.PutText(self.wr,"&amp;"); *)
        | '>' => Wr.PutText(self.wr,"&gt;");
        | '<' => Wr.PutText(self.wr,"&lt;");
        ELSE
          IF c IN nonSGML THEN
            Wr.PutText(self.wr,"&#" & Fmt.Int(ORD(c)) & ";");
          ELSE
            IF c = '\r' THEN c := '\n'; END;
            Wr.PutChar(self.wr,c);
          END;
        END;
      END;
    END;
  END PrintChars;

PROCEDURE EndElement(self: T; 
    READONLY e: SGML.EndElementEvent) =
  VAR
    element: SGML.StartElementEvent := SGML.StartElementEvent{e.pos,e.gi,
        SGML.ElementContentType.Element,FALSE,NIL};
  BEGIN
    IF self.stack.size() > 0 THEN
      element := self.stack.remhi();
    END;

    IF element.contentType = SGML.ElementContentType.Element THEN
      Wr.PutText(self.wr,"\n");
    END;

    IF element.contentType # SGML.ElementContentType.Empty THEN
      Wr.PutText(self.wr,"</" & e.gi & ">");
    END;

    self.parseMode := ParseMode.Normal;
  END EndElement;

PROCEDURE Data(self: T; READONLY e: SGML.DataEvent) =
  BEGIN
    PrintChars(self,e.data);
  END Data;

PROCEDURE SData(self: T; READONLY e: SGML.SdataEvent) =
  BEGIN
    EntityRef(self,e.entityName);
  END SData;

PROCEDURE Pi(self: T; READONLY e: SGML.PiEvent) =
  BEGIN
    IF e.entityName # NIL THEN
      EntityRef(self,e.entityName);
    ELSE
      Wr.PutText(self.wr,"<?" & e.data & ">");
    END;
  END Pi;

PROCEDURE ExternalDataEntityRef(self: T; 
    READONLY e: SGML.ExternalDataEntityRefEvent) =
  BEGIN
    EntityRef(self,e.entity.name);
  END ExternalDataEntityRef;

PROCEDURE SubdocEntityRef(self: T; 
    READONLY e: SGML.SubdocEntityRefEvent) =
  BEGIN
    EntityRef(self,e.entity.name);
  END SubdocEntityRef;

PROCEDURE NonSgmlChar(self: T; 
    READONLY e: SGML.NonSgmlCharEvent) =
  BEGIN
    Wr.PutText(self.wr,"&#" & Fmt.Int(ORD(e.c)) & ";");
  END NonSgmlChar;

PROCEDURE CommentDecl(self: T; 
    READONLY e: SGML.CommentDeclEvent) =
  BEGIN
    Wr.PutText(self.wr,"<!");
    FOR i := 0 TO LAST(e.comments^) DO
      Wr.PutText(self.wr,"--" & e.comments[i] & "--");
      IF i < LAST(e.comments^) THEN Wr.PutText(self.wr,e.seps[i]); END;
    END;
    Wr.PutText(self.wr,">");
  END CommentDecl;

PROCEDURE MarkedSectionStart(self: T; 
    READONLY e: SGML.MarkedSectionStartEvent) =
  BEGIN
    Wr.PutText(self.wr,"<![");
    FOR i := 0 TO LAST(e.params^) DO
      CASE e.params[i].type OF
      | SGML.MarkedSectionParamType.Temp => Wr.PutText(self.wr," TEMP");
      | SGML.MarkedSectionParamType.Include => Wr.PutText(self.wr," INCLUDE");
      | SGML.MarkedSectionParamType.RCData => Wr.PutText(self.wr," RCDATA");
      | SGML.MarkedSectionParamType.CData => Wr.PutText(self.wr," CDATA");
      | SGML.MarkedSectionParamType.Ignore => Wr.PutText(self.wr," IGNORE");
      | SGML.MarkedSectionParamType.EntityRef => Wr.PutText(self.wr," %" &
            e.params[i].entityName & ";");
      END;
    END;
    CASE e.status OF
    | SGML.MarkedSectionStatus.CData => self.parseMode := ParseMode.CData;
    | SGML.MarkedSectionStatus.RCData => self.parseMode := ParseMode.RCData;
    ELSE
    END;
    Wr.PutText(self.wr," [");
  END MarkedSectionStart;

PROCEDURE MarkedSectionEnd(self: T; 
    <*UNUSED*>READONLY e: SGML.MarkedSectionEndEvent) =
  BEGIN
    Wr.PutText(self.wr,"]]>");
    self.parseMode := ParseMode.Normal;
  END MarkedSectionEnd;

PROCEDURE IgnoredChars(self: T; 
    READONLY e: SGML.IgnoredCharsEvent) =
  BEGIN
    Wr.PutText(self.wr,e.data);
  END IgnoredChars;

PROCEDURE EntityRef(self: T; name: TEXT) =
  BEGIN
    Wr.PutText(self.wr,"&" & name & ";");
  END EntityRef;

PROCEDURE GeneralEntity(self: T; 
    READONLY e: SGML.GeneralEntityEvent) =
  VAR
    c: CHAR;
  BEGIN
    IF e.entity.internalText # NIL AND
        Text.Length(e.entity.internalText) = 1 AND 
        e.entity.dataType # SGML.EntityDataType.SData THEN
      c := Text.GetChar(e.entity.internalText,0);
      IF c = '<' OR c = '>' OR c = '&' OR c = '"' THEN 
        PrintChars(self,e.entity.internalText);
      END;
    END;
  END GeneralEntity;

PROCEDURE Error(self: T; READONLY e: SGML.ErrorEvent) =
  VAR
    position := self.getDetailedLocation(e.pos);
  BEGIN
    Wr.PutText(self.wr,"<!-- ERROR " & e.message & " in file " &
        position.filename & " line " & Fmt.Int(position.lineNumber) &
        " column " & Fmt.Int(position.columnNumber));
    IF position.entityName # NIL THEN
      Wr.PutText(self.wr," in entity " & position.entityName & " offset " &
          Fmt.Int(position.entityOffset));
    END;
    Wr.PutText(self.wr," -->\n");
  END Error;

PROCEDURE OpenEntityChange(<*UNUSED*>self: T) =
  VAR
  BEGIN
  END OpenEntityChange;

PROCEDURE Init(self: T): T =
  VAR
  BEGIN
    self.parseMode := ParseMode.Normal;
    self.stack := NEW(SGMLElementSeq.T).init();
    EVAL SGML.Application.init(self);
    RETURN self;
  END Init;

VAR
  isSGML, nonSGML := SET OF CHAR{};

BEGIN
  isSGML := isSGML + SET OF CHAR{'\r', '\n', '\t'} + 
      SET OF CHAR{' '..VAL(126,CHAR)} +
      SET OF CHAR{VAL(160,CHAR)..VAL(254,CHAR)};
  nonSGML := SET OF CHAR{FIRST(CHAR)..LAST(CHAR)} - isSGML;
END SGMLPrint.
