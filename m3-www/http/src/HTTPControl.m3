(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Created by steveg *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 14:50:35 1997
 *)

MODULE HTTPControl EXPORTS HTTPControl;

IMPORT App, Fmt, HTTP, HTTPApp, HTTPControlValue, Rd, Text, TextWr, Thread,
       Wr;

<* PRAGMA LL *>

VAR
  rootForm: StaticForm;
  mu                   := NEW(MUTEX);

REVEAL
  Form = FormPublic BRANDED "HTTPControl.Form" OBJECT
           nameF: TEXT;
         OVERRIDES
           init    := FormInitDefault;
           accept  := FormAcceptDefault;
           respond := FormRespondDefault;
           name    := FormNameDefault;
         END;

PROCEDURE FormInitDefault (self: Form; name: TEXT): Form =
  BEGIN
    self.nameF := name;
    RETURN self;
  END FormInitDefault;

PROCEDURE FormNameDefault (self: Form): TEXT =
  BEGIN
    RETURN self.nameF;
  END FormNameDefault;


PROCEDURE FormAcceptDefault (<* UNUSED *> self   : Form;
                             <* UNUSED *> request: HTTP.Request;
                             <* UNUSED *> path   : TEXT;
                             <* UNUSED *> VAR (* OUT *) acceptState: REFANY):
  BOOLEAN =
  BEGIN
    RETURN FALSE;
  END FormAcceptDefault;

PROCEDURE FormRespondDefault (<* UNUSED *> self   : Form;
                              <* UNUSED *> request: HTTP.Request;
                              <* UNUSED *> query  : HTTP.FormQuery;
                              <* UNUSED *> wr     : Wr.T;
                              <* UNUSED *> log    : App.Log;
                              <* UNUSED *> READONLY acceptState: REFANY) =
  BEGIN
  END FormRespondDefault;

REVEAL
  Iterator = IteratorPublic BRANDED "HTTPControl.Iterator" OBJECT
             OVERRIDES
               next := IteratorDefaultNext;
             END;

PROCEDURE IteratorDefaultNext (<* UNUSED *> self: Iterator): Value =
  BEGIN
    RETURN NIL;
  END IteratorDefaultNext;

TYPE
  ValuesList = REF RECORD
                     head: Value;
                     tail: ValuesList;
                   END;

  Values = OBJECT
             head, tail: ValuesList;
           METHODS
             init (): Values                            := ValuesInit;
             add  (value: Value; tail: BOOLEAN := TRUE) := ValuesAddValue;
           END;

PROCEDURE ValuesInit (self: Values): Values =
  BEGIN
    self.head := NIL;
    self.tail := NIL;
    RETURN self;
  END ValuesInit;

PROCEDURE ValuesAddValue (self: Values; value: Value; tail: BOOLEAN) =
  VAR elem := NEW(ValuesList, head := value);
  BEGIN
    LOCK mu DO
      IF self.head = NIL THEN
        self.head := elem;
        self.tail := elem;
      ELSIF tail THEN
        self.tail.tail := elem;
        self.tail := elem;
      ELSE
        elem.tail := self.head;
        self.head := elem;
      END;
    END;
  END ValuesAddValue;

REVEAL
  StaticForm = StaticFormPublic BRANDED "HTTPControl.StaticForm" OBJECT
                 values       : Values;
                 urlSF, urlSet: TEXT;
               OVERRIDES
                 init     := StaticFormInit;
                 url      := StaticFormURL;
                 addValue := StaticFormAddValue;
                 accept   := StaticFormAccept;
                 respond  := StaticFormRespond;
                 iterate  := StaticFormIterate;
               END;

PROCEDURE StaticFormInit (self     : StaticForm;
                          name, url: TEXT;
                          register : BOOLEAN     ): StaticForm =
  BEGIN
    IF Text.GetChar(url, 0) = '/' THEN url := Text.Sub(url, 1); END;
    self.urlSF := url;
    self.urlSet := url & "Set";
    self.values := NEW(Values).init();
    EVAL Form.init(self, name);
    IF register THEN RegisterForm(self, name, url, TRUE); END;
    RETURN self;
  END StaticFormInit;

PROCEDURE StaticFormURL (self: StaticForm): TEXT =
  BEGIN
    RETURN self.urlSF;
  END StaticFormURL;

PROCEDURE StaticFormAddValue (self: StaticForm; value: Value) =
  BEGIN
    self.values.add(value);
  END StaticFormAddValue;

PROCEDURE StaticFormAccept (             self   : StaticForm;
                            <* UNUSED *> request: HTTP.Request;
                                         path   : TEXT;
                            VAR (* OUT *) acceptState: REFANY): BOOLEAN =
  BEGIN
    IF Text.Equal(path, self.urlSF) THEN
      acceptState := NEW(StaticFormAcceptState, set := FALSE);
      RETURN TRUE;
    ELSIF Text.Equal(path, self.urlSet) THEN
      acceptState := NEW(StaticFormAcceptState, set := TRUE);
      RETURN TRUE;
    END;
    RETURN FALSE;
  END StaticFormAccept;

REVEAL
  Value = ValuePublic BRANDED "HTTPControl.Value" OBJECT
          OVERRIDES
            setText       := SetTextNull;
            getText       := GetTextNull;
            setDefault    := SetDefaultNull;
            writeFormItem := WriteFormItemNull;
          END;

PROCEDURE SetTextNull (<* UNUSED *> self: Value;
                       <* UNUSED *> req : HTTP.Request;
                       <* UNUSED *> v   : TEXT;
                       <* UNUSED *> log : App.Log       ) =
  BEGIN
    <* ASSERT FALSE *>
  END SetTextNull;

PROCEDURE GetTextNull (<* UNUSED *> self: Value;
                       <* UNUSED *> req : HTTP.Request): TEXT =
  BEGIN
    <* ASSERT FALSE *>
  END GetTextNull;

PROCEDURE SetDefaultNull (             value: Value;
                          <* UNUSED *> req  : HTTP.Request;
                                       log  : App.Log       )
  RAISES {App.Error} =
  BEGIN
    log.log(
      Fmt.F("No default value for: %s", value.label), App.LogStatus.Error);
  END SetDefaultNull;

PROCEDURE WriteFormItemNull (<* UNUSED *> value: Value;
                             <* UNUSED *> req  : HTTP.Request;
                             <* UNUSED *> wr   : Wr.T;
                             <* UNUSED *> log  : App.Log       ) =
  BEGIN
    <* ASSERT FALSE *>
  END WriteFormItemNull;

REVEAL
  ContainerValue = ContainerValuePublic BRANDED
  "HTTPControl.ContainerValue" OBJECT
                   OVERRIDES
                     setValues := ContainerValueSetValuesNull;
                   END;

PROCEDURE ContainerValueSetValuesNull (<* UNUSED *> self : ContainerValue;
                                       <* UNUSED *> req  : HTTP.Request;
                                       <* UNUSED *> query: HTTP.FormQuery;
                                       <* UNUSED *> log  : App.Log         ) =
  BEGIN
  END ContainerValueSetValuesNull;

(* return the next editable value in the list *)
PROCEDURE NextEditableValue (iterator: Iterator): Value =
  VAR value: Value := iterator.next();
  BEGIN
    LOCK mu DO
      WHILE value # NIL AND NOT value.editable DO
        value := iterator.next();
      END;
    END;
    RETURN value;
  END NextEditableValue;

PROCEDURE SetValues (req  : HTTP.Request;
                     form : Form;
                     query: HTTP.FormQuery;
                     log  : App.Log         )
  RAISES {App.Error, NotAuthorized} =
  VAR
    field     : HTTP.Field;
    iterValues             := form.iterate();
    value                  := NextEditableValue(iterValues);
  BEGIN
    (* Plan: Iterate through the editable values.  If there is a matching
       query value the set it from that or else default it... *)
    WHILE value # NIL DO
      IF ISTYPE(value, ContainerValue) THEN
        NARROW(value, ContainerValue).setValues(req, query, log);
      ELSE
        field := query.lookupField(value.id);
        IF field # NIL THEN
          value.setText(req, field.value, log);
          IF App.Verbose() THEN
            log.log(
              Fmt.F("field: %s value: %s setting value: %s", field.name,
                    field.value, value.label), App.LogStatus.Verbose);
          END;
        ELSE
          value.setDefault(req, log);
        END;
      END;
      value := NextEditableValue(iterValues);
    END;
  END SetValues;

PROCEDURE StaticFormRespond (         form       : StaticForm;
                                      request    : HTTP.Request;
                                      query      : HTTP.FormQuery;
                                      wr         : Wr.T;
                                      log        : App.Log;
                             READONLY acceptState: REFANY          )
  RAISES {App.Error, NotAuthorized} =
  VAR
    set        := FALSE;
    iterValues := form.iterate();
    value      := iterValues.next();
  BEGIN
    IF acceptState # NIL THEN
      set := NARROW(acceptState, StaticFormAcceptState).set;
    END;
    IF set THEN
      (* enumerate fields and set values *)
      IF App.Verbose() THEN
        log.log(Fmt.F("Query request: %s", query.toText()),
                App.LogStatus.Verbose);
      END;
      SetValues(request, form, query, log);
    END;
    TRY
      IF form.title = NIL THEN
        Wr.PutText(wr, "<HTML><BODY>");
      ELSE
        Wr.PutText(wr, Fmt.F("<HTML><HEAD><TITLE>%s</TITLE></HEAD><BODY>",
                   HTTP.EncodeTextForHTML(form.title)));
      END;

      Wr.PutText(wr, Fmt.F("<FORM METHOD=POST Action=%s>\n", form.urlSet));
      WHILE value # NIL DO
        value.writeFormItem(request, wr, log);
        LOCK mu DO value := iterValues.next(); END;
      END;
      IF form.hasSubmitButton THEN
        Wr.PutText(wr, "<P><INPUT TYPE=submit VALUE=Submit><INPUT TYPE=reset>");
      END;
      Wr.PutText(wr, "</FORM></BODY></HTML>");
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
        log.log("Problem writing form to browser", App.LogStatus.Error);
    END;
  END StaticFormRespond;

TYPE
  StaticFormIterator = Iterator OBJECT
                         set     : BOOLEAN;
                         values  : ValuesList;
                         row, col: INTEGER;
                       OVERRIDES
                         next := StaticFormNext;
                       END;

PROCEDURE StaticFormIterate (self: StaticForm): Iterator =
  BEGIN
    RETURN NEW(StaticFormIterator, values := self.values.head, row := -1);
  END StaticFormIterate;

PROCEDURE StaticFormNext (self: StaticFormIterator): Value =
  VAR res: Value;
  BEGIN
    IF self.values = NIL THEN
      RETURN NIL
      (*
          ELSIF self.set AND
                TYPEOF(self.values.head) = HTTPControlValue.TableValue THEN
            table := self.values.head;
            IF self.row = -1 THEN
              self.row := 0;
              self.col := 0;
            ELSE
              INC(self.col);
            END;
            IF self.col > LAST(table.table[self.row]^) THEN
              INC(self.row);
              self.col := 0;
            END;
            IF self.row > LAST(table.table^) THEN
              self.row := -1;
              self.values := self.values.tail;
              RETURN StaticFormNext(self);
            END;
            RETURN table.table[self.row, self.col];
      *)
    ELSE
      res := self.values.head;
      self.values := self.values.tail;
      RETURN res;
    END;
  END StaticFormNext;

TYPE
  FormsList = REF RECORD
                    head: Form;
                    tail: FormsList;
                  END;

  Forms = OBJECT
            head, tail: FormsList;
          METHODS
            init (): Forms                           := FormsInit;
            add  (form: Form; tail: BOOLEAN := TRUE) := FormsAddForm;
          END;

PROCEDURE FormsInit (self: Forms): Forms =
  BEGIN
    self.head := NIL;
    self.tail := NIL;
    RETURN self;
  END FormsInit;

PROCEDURE FormsAddForm (self: Forms; form: Form; tail: BOOLEAN) =
  VAR elem := NEW(FormsList, head := form);
  BEGIN
    LOCK mu DO
      IF self.head = NIL THEN
        self.head := elem;
        self.tail := elem;
      ELSIF tail THEN
        self.tail.tail := elem;
        self.tail := elem;
      ELSE
        elem.tail := self.head;
        self.head := elem;
      END;
    END;
  END FormsAddForm;

VAR registeredForms := NEW(Forms).init();

PROCEDURE RegisterForm (form: Form; name, url: TEXT; addToRoot: BOOLEAN) =
  VAR value: HTTPControlValue.FormValue;
  BEGIN
    registeredForms.add(form);
    IF addToRoot AND form # rootForm AND rootForm # NIL THEN
      value := NEW(HTTPControlValue.FormValue, editable := TRUE,
                   form := form).init(name, url);
      rootForm.addValue(value);
    END;
  END RegisterForm;

TYPE
  RequestHandler = HTTPApp.RequestHandler OBJECT
                   OVERRIDES
                     accept  := Accept;
                     request := Request;
                   END;

PROCEDURE FindForm (              request: HTTP.Request;
                                  path   : TEXT;
                    VAR (* OUT *) fas    : REFANY        ): Form =
  VAR forms: FormsList;
  BEGIN
    LOCK mu DO
      forms := registeredForms.head;
      WHILE forms # NIL DO
        IF forms.head.accept(request, path, fas) THEN
          RETURN forms.head;
        END;
        forms := forms.tail;
      END;
    END;
    RETURN NIL;
  END FindForm;

PROCEDURE FormLookup (name: TEXT): Form =
  VAR forms: FormsList;
  BEGIN
    LOCK mu DO
      forms := registeredForms.head;
      WHILE forms # NIL DO
        IF Text.Equal(forms.head.name(), name) THEN RETURN forms.head; END;
        forms := forms.tail;
      END;
    END;
    RETURN NIL;
  END FormLookup;

TYPE
  AcceptState = REF RECORD
                      query          : HTTP.FormQuery;
                      form           : Form;
                      formAcceptState: REFANY;
                    END;

PROCEDURE Accept (<* UNUSED *>               self       : RequestHandler;
                                             request    : HTTP.Request;
                  <* UNUSED *>               serverData : REFANY;
                               VAR (* OUT *) acceptState: REFANY;
                  <* UNUSED *>               log        : App.Log         ):
  BOOLEAN =
  VAR
    path           : TEXT;
    query          : HTTP.FormQuery;
    form           : Form;
    formAcceptState: REFANY;
  BEGIN
    IF request.url.local(HTTPApp.AnyService) THEN
      IF request.method = HTTP.Method.Get
           OR request.method = HTTP.Method.Head THEN
        TRY
          query := NEW(HTTP.FormQuery).init(request.url.query);
        EXCEPT
        | HTTP.BadFormQuery =>
        END;
      END;
      path := request.url.path;
      form := FindForm(request, path, formAcceptState);
      IF form # NIL THEN
        acceptState := NEW(AcceptState, query := query, form := form,
                           formAcceptState := formAcceptState);
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END Accept;

(* form fields are named val<i> and correspond to the i'th editable value
   in the form. *)
PROCEDURE Request (<* UNUSED *> self       : RequestHandler;
                                request    : HTTP.Request;
                   <* UNUSED *> serverData : REFANY;
                                acceptState: REFANY;
                   <* UNUSED *> rd         : Rd.T;
                                wr         : Wr.T;
                                log        : App.Log         )
  RAISES {App.Error} =
  VAR
    as    : AcceptState := acceptState;
    tempWr              := TextWr.New();
  BEGIN
    WITH reply = NEW(
                   HTTP.Reply, code := HTTP.StatusCode[HTTP.StatusType.OK],
                   reason := HTTP.StatusReason[HTTP.StatusType.OK]) DO
      EVAL reply.addField(
             NEW(HTTP.Field).init(
               HTTP.FieldName[HTTP.FieldType.Content_Type], "text/html"));
      IF App.Verbose() THEN
        log.log(reply.toText(NIL, log), App.LogStatus.Verbose);
      END;
      reply.write(tempWr, HTTP.DefaultStyle(reply.version), log);
    END;
    IF request.method = HTTP.Method.Post THEN
      TRY
        as.query := NEW(HTTP.FormQuery).init(request.postData);
      EXCEPT
      | HTTP.BadFormQuery =>
          log.log(Fmt.F("Bad form query in request: %s",
                        request.toText(
                          HTTP.DefaultStyle(request.version), TRUE, log)),
                  App.LogStatus.Error);
      END;
    END;
    TRY
      as.form.respond(request, as.query, tempWr, log, as.formAcceptState);
      Wr.PutText(wr, TextWr.ToText(tempWr));
    EXCEPT
    | Thread.Alerted, Wr.Failure =>
        log.log(
          "HTTPControl.Request: unexpected error", App.LogStatus.Error);
    | NotAuthorized =>
        WITH pi = HTTP.GetProgramInfo() DO
          HTTP.ReplyUnauthorized(wr, pi.authType, pi.authRealm, log);
        END;
    END;
  END Request;

PROCEDURE RootForm (): StaticForm =
  BEGIN
    RETURN rootForm;
  END RootForm;

PROCEDURE AddToForm (form: StaticForm; subForm: Form; name, url: TEXT) =
  BEGIN
    form.addValue(
      NEW(HTTPControlValue.FormValue, form := subForm).init(name, url));
  END AddToForm;

BEGIN
  rootForm := NEW(StaticForm).init("", "/");

  HTTPApp.RegisterRequestHandler(HTTPApp.AnyPort, 
    NEW(RequestHandler, priority := HTTPApp.RequestPriority.Low));
END HTTPControl.

