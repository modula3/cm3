(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* created by steveg *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 14:50:25 1997
 *)

MODULE HTTPControlValue EXPORTS HTTPControlValue, Main;

IMPORT App, FloatMode, Fmt, HTTP, HTTPControl, Lex, Rd, Text, TextRd,
       TextExtras, Thread, Wr;

<* PRAGMA LL *>

REVEAL
  BooleanValue = BooleanValuePublic BRANDED
  "HTTPControlValue.BooleanValue" OBJECT
                 OVERRIDES
                   getText       := BooleanGetText;
                   get           := BooleanGetNull;
                   setText       := BooleanSetText;
                   set           := BooleanSetNull;
                   setDefault    := BooleanSetDefault;
                   writeFormItem := BooleanWriteFormItem;
                 END;

PROCEDURE BooleanGetText (self: BooleanValue; req: HTTP.Request): TEXT
  RAISES {HTTPControl.NotAuthorized} =
  BEGIN
    IF self.get(req) THEN RETURN "TRUE" ELSE RETURN "FALSE" END;
  END BooleanGetText;

PROCEDURE BooleanSetText (self: BooleanValue;
                          req : HTTP.Request;
                          v   : TEXT;
                          log : App.Log       )
  RAISES {App.Error, HTTPControl.NotAuthorized} =
  BEGIN
    IF TextExtras.CIEqual("FALSE", v) THEN
      self.set(req, FALSE);
    ELSIF TextExtras.CIEqual("TRUE", v) THEN
      self.set(req, TRUE);
    ELSE
      log.log(Fmt.F("Bad value (%s) for boolean item: %s", v, self.label),
              App.LogStatus.Error);
    END;
  END BooleanSetText;

PROCEDURE BooleanGetNull (<* UNUSED *> self: BooleanValue;
                          <* UNUSED *> req : HTTP.Request  ): BOOLEAN =
  BEGIN
    <* ASSERT FALSE *>
  END BooleanGetNull;

PROCEDURE BooleanSetNull (<* UNUSED *> self: BooleanValue;
                          <* UNUSED *> req : HTTP.Request;
                          <* UNUSED *> v   : BOOLEAN       ) =
  BEGIN
    <* ASSERT FALSE *>
  END BooleanSetNull;

PROCEDURE BooleanSetDefault (self: BooleanValue;
                             req : HTTP.Request;
                             log : App.Log       )
  RAISES {App.Error, HTTPControl.NotAuthorized} =
  BEGIN
    self.set(req, FALSE);
    IF App.Verbose() THEN
      log.log(Fmt.F("setting default \"FALSE\" value: %s", self.label),
              App.LogStatus.Verbose);
    END;
  END BooleanSetDefault;

PROCEDURE BooleanWriteFormItem (self: BooleanValue;
                                req : HTTP.Request;
                                wr  : Wr.T;
                                log : App.Log       )
  RAISES {App.Error, HTTPControl.NotAuthorized} =
  VAR checked: TEXT;
  BEGIN
    TRY
      IF self.editable THEN
        IF self.get(req) THEN checked := "checked" ELSE checked := "" END;
        Wr.PutText(
          wr,
          Fmt.F(
            "%s <input type=checkbox name=%s value=true %s> %s %s<BR>\n",
            self.leader, self.id, checked, self.label, self.trailer));
      ELSE
        Wr.PutText(wr, Fmt.F("%s %s: %s %s<BR>\n", self.leader, self.label,
                             self.getText(req), self.trailer));
      END;
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
        log.log(
          "Problem writing boolean item to browser", App.LogStatus.Error);
    END;
  END BooleanWriteFormItem;

REVEAL
  ChoiceValue = ChoiceValuePublic BRANDED
  "HTTPControlValue.ChoiceValue" OBJECT
                OVERRIDES
                  init          := ChoiceInit;
                  getText       := ChoiceGetText;
                  get           := ChoiceGetNull;
                  setText       := ChoiceSetText;
                  set           := ChoiceSetNull;
                  setDefault    := ChoiceSetDefault;
                  writeFormItem := ChoiceWriteFormItem;
                END;

PROCEDURE ChoiceInit (self: ChoiceValue; READONLY names: ARRAY OF TEXT):
  ChoiceValue =
  BEGIN
    self.names := NEW(REF ARRAY OF TEXT, NUMBER(names));
    self.names^ := names;
    RETURN self;
  END ChoiceInit;

PROCEDURE ChoiceGetText (self: ChoiceValue; req: HTTP.Request): TEXT
  RAISES {HTTPControl.NotAuthorized} =
  BEGIN
    RETURN self.names[self.get(req)];
  END ChoiceGetText;

PROCEDURE ChoiceSetText (self: ChoiceValue;
                         req : HTTP.Request;
                         v   : TEXT;
                         log : App.Log       )
  RAISES {App.Error, HTTPControl.NotAuthorized} =
  BEGIN
    FOR i := 0 TO LAST(self.names^) DO
      IF Text.Equal(v, self.names[i]) THEN
        self.set(req, i, log);
        RETURN;
      END;
    END;
    log.log(Fmt.F("Bad value (%s) for choice item: %s", v, self.label),
            App.LogStatus.Error);
  END ChoiceSetText;

PROCEDURE ChoiceGetNull (<* UNUSED *> self: ChoiceValue;
                         <* UNUSED *> req : HTTP.Request ): INTEGER =
  BEGIN
    <* ASSERT FALSE *>
  END ChoiceGetNull;

PROCEDURE ChoiceSetNull (<* UNUSED *> self: ChoiceValue;
                         <* UNUSED *> req : HTTP.Request;
                         <* UNUSED *> v   : INTEGER;
                         <* UNUSED *> log : App.Log       ) =
  BEGIN
    <* ASSERT FALSE *>
  END ChoiceSetNull;

PROCEDURE ChoiceSetDefault (self: ChoiceValue;
                            req : HTTP.Request;
                            log : App.Log       )
  RAISES {App.Error, HTTPControl.NotAuthorized} =
  BEGIN
    self.set(req, 0, log);
    IF App.Verbose() THEN
      log.log(Fmt.F("setting default %s choice value: %s", self.names[0],
                    self.label), App.LogStatus.Verbose);
    END;
  END ChoiceSetDefault;

PROCEDURE ChoiceWriteFormItem (self: ChoiceValue;
                               req : HTTP.Request;
                               wr  : Wr.T;
                               log : App.Log       )
  RAISES {App.Error, HTTPControl.NotAuthorized} =
  VAR
    selected       := self.get(req);
    checked : TEXT;
  BEGIN
    TRY
      IF self.editable THEN
        Wr.PutText(wr, Fmt.F("%s %s: ", self.leader, self.label));
        FOR i := 0 TO LAST(self.names^) DO
          IF i = selected THEN checked := "checked" ELSE checked := "" END;
          Wr.PutText(
            wr, Fmt.F("<input type=radio name=%s value=%s %s> %s \n",
                      self.id, self.names[i], checked, self.names[i]));
        END;
        Wr.PutText(wr, Fmt.F(" %s<BR>", self.trailer));
      ELSE
        Wr.PutText(wr, Fmt.F("%s %s: %s %s<BR>\n", self.leader, self.label,
                             self.getText(req), self.trailer));
      END;
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
        log.log(
          "Problem writing choice item to browser", App.LogStatus.Error);
    END;
  END ChoiceWriteFormItem;

(* a form value is a link to another form *)
REVEAL
  FormValue = FormValuePublic BRANDED "HTTPControlValue.FormValue" OBJECT
              OVERRIDES
                init          := FormValueInit;
                getText       := FormGetText;
                get           := FormGet;
                setText       := FormSetText;
                set           := FormSet;
                writeFormItem := FormWriteFormItem;
              END;

PROCEDURE FormValueInit (self: FormValue; name, url: TEXT): FormValue =
  BEGIN
    self.name := name;
    self.url := url;
    RETURN self;
  END FormValueInit;

PROCEDURE FormGetText (self: FormValue; <* UNUSED *> req: HTTP.Request):
  TEXT =
  BEGIN
    RETURN self.form.name()
  END FormGetText;

PROCEDURE FormSetText (self: FormValue;
                       req : HTTP.Request;
                       v   : TEXT;
                       log : App.Log       )
  RAISES {App.Error, HTTPControl.NotAuthorized} =
  VAR form := HTTPControl.FormLookup(v);
  BEGIN
    IF form # NIL THEN
      self.set(req, form);
    ELSE
      log.log(Fmt.F("no form with name: %s", v), App.LogStatus.Error);
    END;
  END FormSetText;

PROCEDURE FormGet (self: FormValue; <* UNUSED *> req: HTTP.Request):
  HTTPControl.Form =
  BEGIN
    RETURN self.form;
  END FormGet;

PROCEDURE FormSet (             self: FormValue;
                   <* UNUSED *> req : HTTP.Request;
                                v   : HTTPControl.Form) =
  BEGIN
    self.form := v;
  END FormSet;

PROCEDURE FormWriteFormItem (             self: FormValue;
                             <* UNUSED *> req : HTTP.Request;
                                          wr  : Wr.T;
                                          log : App.Log       )
  RAISES {App.Error} =
  BEGIN
    TRY
      Wr.PutText(
        wr, Fmt.F("<A HREF=\"%s\">%s</A><BR>\n", self.url, self.name));
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
        log.log(
          "Problem writing form item to browser", App.LogStatus.Error);

    END;
  END FormWriteFormItem;

REVEAL
  IntegerValue = IntegerValuePublic BRANDED
  "HTTPControlValue.IntegerValue" OBJECT
                 OVERRIDES
                   getText       := IntegerGetText;
                   get           := IntegerGetNull;
                   setText       := IntegerSetText;
                   set           := IntegerSetNull;
                   writeFormItem := IntegerWriteFormItem;
                 END;

PROCEDURE IntegerGetText (self: IntegerValue; req: HTTP.Request): TEXT
  RAISES {HTTPControl.NotAuthorized} =
  BEGIN
    RETURN Fmt.Int(self.get(req));
  END IntegerGetText;

PROCEDURE IntegerSetText (self: IntegerValue;
                          req : HTTP.Request;
                          v   : TEXT;
                          log : App.Log       )
  RAISES {App.Error, HTTPControl.NotAuthorized} =
  BEGIN
    TRY
      self.set(req, Lex.Int(TextRd.New(v)), log)
    EXCEPT
    | Rd.Failure, Lex.Error, FloatMode.Trap, Thread.Alerted =>
        log.log(
          Fmt.F("Bad integer value (%s) for field: %s", v, self.label),
          App.LogStatus.Error);
    END;
  END IntegerSetText;

PROCEDURE IntegerGetNull (<* UNUSED *> self: IntegerValue;
                          <* UNUSED *> req : HTTP.Request  ): INTEGER =
  BEGIN
    <* ASSERT FALSE *>
  END IntegerGetNull;

PROCEDURE IntegerSetNull (<* UNUSED *> self: IntegerValue;
                          <* UNUSED *> req : HTTP.Request;
                          <* UNUSED *> v   : INTEGER;
                          <* UNUSED *> log : App.Log       ) =
  BEGIN
    <* ASSERT FALSE *>
  END IntegerSetNull;

PROCEDURE IntegerWriteFormItem (self: IntegerValue;
                                req : HTTP.Request;
                                wr  : Wr.T;
                                log : App.Log       )
  RAISES {App.Error, HTTPControl.NotAuthorized} =
  BEGIN
    TRY
      IF self.editable THEN
        Wr.PutText(
          wr,
          Fmt.F(
            "%s %s: <input type=text name=%s maxlength=1000 size=10 value=%s> %s<BR>\n",
            self.leader, self.label, self.id, self.getText(req),
            self.trailer));
      ELSE
        Wr.PutText(wr, Fmt.F("%s %s: %s %s<BR>", self.leader, self.label,
                             self.getText(req), self.trailer));
      END;
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
        log.log(
          "Problem writing iteger item to browser", App.LogStatus.Error);

    END;
  END IntegerWriteFormItem;

REVEAL
  RealValue = RealValuePublic BRANDED "HTTPControlValue.RealValue" OBJECT
              OVERRIDES
                getText       := RealGetText;
                get           := RealGetNull;
                setText       := RealSetText;
                set           := RealSetNull;
                writeFormItem := RealWriteFormItem;
              END;

PROCEDURE RealGetText (self: RealValue; req: HTTP.Request): TEXT
  RAISES {HTTPControl.NotAuthorized} =
  BEGIN
    RETURN Fmt.Real(self.get(req));
  END RealGetText;

PROCEDURE RealSetText (self: RealValue;
                       req : HTTP.Request;
                       v   : TEXT;
                       log : App.Log       )
  RAISES {App.Error, HTTPControl.NotAuthorized} =
  BEGIN
    TRY
      self.set(req, Lex.Real(TextRd.New(v)), log)
    EXCEPT
    | Rd.Failure, Lex.Error, FloatMode.Trap, Thread.Alerted =>
        log.log(
          Fmt.F("Bad integer value (%s) for field: %s", v, self.label),
          App.LogStatus.Error);
    END;
  END RealSetText;

PROCEDURE RealGetNull (<* UNUSED *> self: RealValue;
                       <* UNUSED *> req : HTTP.Request): REAL =
  BEGIN
    <* ASSERT FALSE *>
  END RealGetNull;

PROCEDURE RealSetNull (<* UNUSED *> self: RealValue;
                       <* UNUSED *> req : HTTP.Request;
                       <* UNUSED *> v   : REAL;
                       <* UNUSED *> log : App.Log       ) =
  BEGIN
    <* ASSERT FALSE *>
  END RealSetNull;

PROCEDURE RealWriteFormItem (self: RealValue;
                             req : HTTP.Request;
                             wr  : Wr.T;
                             log : App.Log       )
  RAISES {App.Error, HTTPControl.NotAuthorized} =
  BEGIN
    TRY
      IF self.editable THEN
        Wr.PutText(
          wr,
          Fmt.F(
            "%s %s <input type=text name=%s maxlength=1000 size=10 value=%s> %s<BR>\n",
            self.leader, self.label, self.id, self.getText(req),
            self.trailer));
      ELSE
        Wr.PutText(wr, Fmt.F("%s %s: %s %s<BR>", self.leader, self.label,
                             self.getText(req), self.trailer));
      END;
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
        log.log(
          "Problem writing real item to browser", App.LogStatus.Error);

    END;
  END RealWriteFormItem;

REVEAL
  TextValue = TextValuePublic BRANDED "HTTPControlValue.TextValue" OBJECT
              OVERRIDES
                getText       := TextGetText;
                get           := TextGetNull;
                setText       := TextSetText;
                set           := TextSetNull;
                writeFormItem := TextWriteFormItem;
              END;

PROCEDURE TextGetText (self: TextValue; req: HTTP.Request): TEXT
  RAISES {HTTPControl.NotAuthorized} =
  BEGIN
    RETURN self.get(req);
  END TextGetText;

PROCEDURE TextSetText (self: TextValue;
                       req : HTTP.Request;
                       v   : TEXT;
                       log : App.Log       )
  RAISES {App.Error, HTTPControl.NotAuthorized} =
  BEGIN
    self.set(req, HTTP.DecodeTextForHTML(v, log), log);
  END TextSetText;

PROCEDURE TextGetNull (<* UNUSED *> self: TextValue;
                       <* UNUSED *> req : HTTP.Request): TEXT =
  BEGIN
    <* ASSERT FALSE *>
  END TextGetNull;

PROCEDURE TextSetNull (<* UNUSED *> self: TextValue;
                       <* UNUSED *> req : HTTP.Request;
                       <* UNUSED *> v   : TEXT;
                       <* UNUSED *> log : App.Log       ) =
  BEGIN
    <* ASSERT FALSE *>
  END TextSetNull;

PROCEDURE TextWriteFormItem (self: TextValue;
                             req : HTTP.Request;
                             wr  : Wr.T;
                             log : App.Log       )
  RAISES {App.Error, HTTPControl.NotAuthorized} =
  BEGIN
    TRY
      IF self.editable THEN
        IF self.scrollable THEN
          Wr.PutText(wr, Fmt.FN(
            "%s %s <TEXTAREA name=%s rows=%s cols=%s>%s</TEXTAREA>%s<BR>\n",
            ARRAY OF TEXT{
                    self.leader, self.label, self.id,
                    Fmt.Int(self.rows), Fmt.Int(self.columns),
                    HTTP.EncodeTextForHTML(self.getText(req)), self.trailer}));
        ELSE
          Wr.PutText(wr, Fmt.FN(
            "%s %s <input type=text name=\"%s\" maxlength=%s size=%s value=\"%s\"> %s<BR>\n",
            ARRAY OF TEXT{
                    self.leader, self.label, self.id,
                    Fmt.Int(self.maxLength), Fmt.Int(self.size),
                    HTTP.EncodeTextForHTML(self.getText(req)), self.trailer}));
        END;
      ELSE
        Wr.PutText(wr, Fmt.F("%s %s: %s %s", self.leader, self.label,
                             HTTP.EncodeTextForHTML(self.getText(req)),
                             self.trailer));
      END;
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
        log.log(
          "Problem writing text item to browser", App.LogStatus.Error);

    END;
  END TextWriteFormItem;

REVEAL
  URLValue = URLValuePublic BRANDED "HTTPControlValue.URLValue" OBJECT
               url: TEXT;
             OVERRIDES
               init          := URLInit;
               getText       := URLGetText;
               get           := URLGetNull;
               setText       := URLSetText;
               set           := URLSetNull;
               writeFormItem := URLWriteFormItem;
             END;

PROCEDURE URLInit (self: URLValue; url: TEXT): URLValue =
  BEGIN
    self.url := url;
    RETURN self;
  END URLInit;

PROCEDURE URLGetText (self: URLValue; req: HTTP.Request): TEXT
  RAISES {HTTPControl.NotAuthorized} =
  BEGIN
    RETURN self.get(req);
  END URLGetText;

PROCEDURE URLSetText (self: URLValue;
                      req : HTTP.Request;
                      v   : TEXT;
                      log : App.Log       )
  RAISES {App.Error, HTTPControl.NotAuthorized} =
  BEGIN
    self.set(req, v, log);
  END URLSetText;

PROCEDURE URLGetNull (self: URLValue; <* UNUSED *> req: HTTP.Request):
  TEXT =
  BEGIN
    RETURN self.url;
  END URLGetNull;

PROCEDURE URLSetNull (             self: URLValue;
                      <* UNUSED *> req : HTTP.Request;
                                   v   : TEXT;
                      <* UNUSED *> log : App.Log       ) =
  BEGIN
    self.url := v;
  END URLSetNull;

PROCEDURE URLWriteFormItem (self: URLValue;
                            req : HTTP.Request;
                            wr  : Wr.T;
                            log : App.Log       )
  RAISES {App.Error, HTTPControl.NotAuthorized} =
  BEGIN
    TRY
      Wr.PutText(
        wr, Fmt.F("%s <A HREF=\"%s\">%s</A> %s<BR>\n", self.leader,
                  self.getText(req), self.label, self.trailer));
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
        log.log("Problem writing URL item to browser", App.LogStatus.Error);
    END;
  END URLWriteFormItem;

REVEAL
  ImageValue = ImageValuePublic BRANDED "HTTPControlValue.ImageValue" OBJECT
               OVERRIDES
                 getText       := ImageGetText;
                 get           := ImageGetNull;
                 setText       := ImageSetText;
                 set           := ImageSetNull;
                 writeFormItem := ImageWriteFormItem;
               END;

PROCEDURE ImageGetText (self: ImageValue; req: HTTP.Request): TEXT
  RAISES {HTTPControl.NotAuthorized} =
  BEGIN
    RETURN self.get(req).toText();
  END ImageGetText;

PROCEDURE ImageSetText (self: ImageValue;
                        req : HTTP.Request;
                        v   : TEXT;
                        log : App.Log       )
  RAISES {App.Error, HTTPControl.NotAuthorized} =
  BEGIN
    self.set(req, NEW(HTTP.URL).init(v, log), log);
  END ImageSetText;

PROCEDURE ImageGetNull (<* UNUSED *> self: ImageValue;
                        <* UNUSED *> req : HTTP.Request): HTTP.URL =
  BEGIN
    <* ASSERT FALSE *>
  END ImageGetNull;

PROCEDURE ImageSetNull (<* UNUSED *> self: ImageValue;
                        <* UNUSED *> req : HTTP.Request;
                        <* UNUSED *> v   : HTTP.URL;
                        <* UNUSED *> log : App.Log       ) =
  BEGIN
    <* ASSERT FALSE *>
  END ImageSetNull;

PROCEDURE ImageWriteFormItem (self: ImageValue;
                              req : HTTP.Request;
                              wr  : Wr.T;
                              log : App.Log       )
  RAISES {App.Error, HTTPControl.NotAuthorized} =
  BEGIN
    TRY
      Wr.PutText(
        wr, Fmt.F("%s <IMG ALT=\"%s\" SRC=\"%s\"> %s<BR>\n", self.leader,
                  self.label, self.getText(req), self.trailer));
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
        log.log("Problem writing URL item to browser", App.LogStatus.Error);
    END;
  END ImageWriteFormItem;

REVEAL
  TableValue = TableValuePublic BRANDED "HTTPControlValue.TableValue" OBJECT
               OVERRIDES
                 getText       := TableGetText;
                 get           := TableGet;
                 setText       := TableSetText;
                 set           := TableSet;
                 setValues     := TableSetValues;
                 writeFormItem := TableWriteFormItem;
               END;

PROCEDURE TableGetText (<* UNUSED *> self: TableValue;
                        <* UNUSED *> req : HTTP.Request): TEXT =
  BEGIN
    RETURN "<table>";
  END TableGetText;

PROCEDURE TableSetText (<* UNUSED *> self: TableValue;
                        <* UNUSED *> req : HTTP.Request;
                        <* UNUSED *> v   : TEXT;
                                     log : App.Log       ) RAISES {App.Error} =
  BEGIN
    log.log("Cant set table text", App.LogStatus.Error);
  END TableSetText;

PROCEDURE TableGet (self: TableValue; <* UNUSED *> req: HTTP.Request):
  Table =
  BEGIN
    RETURN self.table;
  END TableGet;

PROCEDURE TableSet (             self : TableValue;
                    <* UNUSED *> req  : HTTP.Request;
                                 table: Table         ) =
  BEGIN
    self.table := table;
  END TableSet;

PROCEDURE TableWriteFormItem (self: TableValue;
                              req : HTTP.Request;
                              wr  : Wr.T;
                              log : App.Log       )
  RAISES {App.Error, HTTPControl.NotAuthorized} =
  BEGIN
    TRY
      Wr.PutText(
        wr, Fmt.F("<TABLE BORDER><CAPTION>%s</CAPTION>\n", self.caption));
      FOR i := 0 TO LAST(self.table^) DO
        Wr.PutText(wr, "<TR>");
        FOR j := 0 TO LAST(self.table[i]^) DO
          Wr.PutText(wr, "<TH>");
          self.table[i, j].writeFormItem(req, wr, log);
        END;
        Wr.PutText(wr, "\n");
      END;
      Wr.PutText(wr, "</TABLE>");
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
        log.log("Problem writing URL item to browser", App.LogStatus.Error);
    END;
  END TableWriteFormItem;

PROCEDURE TableSetValues (self : TableValue;
                          req  : HTTP.Request;
                          query: HTTP.FormQuery;
                          log  : App.Log         )
  RAISES {App.Error, HTTPControl.NotAuthorized} =
  VAR
    field: HTTP.Field;
    value: HTTPControl.Value;
  BEGIN
    FOR i := 0 TO LAST(self.table^) DO
      FOR j := 0 TO LAST(self.table[i]^) DO
        value := self.table[i, j];
        IF value.editable THEN
          field := query.lookupField(value.id);
          IF field = NIL THEN
            value.setDefault(req, log);
          ELSE
            value.setText(req, field.value, log);
          END;
        END;
      END;
    END;
  END TableSetValues;

REVEAL
  MessageValue = MessageValuePublic BRANDED
  "HTTPControlValue.MessageValue" OBJECT
                   msg: TEXT;
                 OVERRIDES
                   init          := MessageInit;
                   getText       := MessageGetText;
                   writeFormItem := MessageWriteFormItem;
                 END;

PROCEDURE MessageInit (self: MessageValue; msg: TEXT): MessageValue =
  BEGIN
    self.msg := msg;
    self.editable := FALSE;
    self.label := "";
    self.id := "";
    RETURN self;
  END MessageInit;

PROCEDURE MessageGetText (             self: MessageValue;
                          <* UNUSED *> req : HTTP.Request  ): TEXT =
  BEGIN
    RETURN self.msg;
  END MessageGetText;

PROCEDURE MessageWriteFormItem (             self: MessageValue;
                                <* UNUSED *> req : HTTP.Request;
                                             wr  : Wr.T;
                                             log : App.Log       )
  RAISES {App.Error} =
  BEGIN
    TRY
      Wr.PutText(wr, self.msg);
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
        log.log(
          "Problem writing boolean item to browser", App.LogStatus.Error);
    END;
  END MessageWriteFormItem;

BEGIN
END HTTPControlValue.
