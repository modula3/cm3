
MODULE WebContact EXPORTS Main;

(* A simple web-based application. *)

IMPORT HTTPApp, HTTPControl, HTTPControlValue, App; 
(* Import Web Application framework. *)

IMPORT Text, TextTextTbl;
FROM IO IMPORT Put;

(* Create two fields for names and e-mail addresses. Each is displayed
   on an automatically generated form, and the callback procedures
   are run when the form is submitted. *)

VAR
  name, email: TEXT := "";

(* Define a text control, "name_value", and its Get and Set, and Default operations. *)

VAR
  name_value := NEW(HTTPControlValue.TextValue,
		leader := "<strong>Name: </strong>",  id := "name",
		set := SetName, get := GetName, setDefault := Default);

PROCEDURE SetName(self: HTTPControlValue.TextValue; val: TEXT; log: App.Log) RAISES {App.Error} = 
BEGIN
  name := val;
  IF db.get(name, email) THEN
    email := "";
  END;
END SetName;

PROCEDURE GetName(self: HTTPControlValue.TextValue): TEXT =
BEGIN
  RETURN name;
END GetName;


(* Define another text control "email_value", and its Get, Set, and Default
   operations. *)

VAR
  email_value := NEW(HTTPControlValue.TextValue,
			leader := "<strong> Email:</strong>", id := "email",
			set := SetEmail, get := GetEmail, setDefault := Default);

PROCEDURE GetEmail (self: HTTPControlValue.TextValue): TEXT = 
BEGIN
  Log ("getting email");
  RETURN email;
END GetEmail;

PROCEDURE SetEmail (self: HTTPControlValue.TextValue;
		     val: TEXT; log : App.Log) RAISES {App.Error} = 

BEGIN
  IF Text.Empty (val) THEN
    IF NOT db.get(name, email) THEN
      email := "";
    END;
  ELSE
    EVAL db.put(name, val);
  END;
END SetEmail;

PROCEDURE Default (<* UNUSED *>x : HTTPControlValue.TextValue; <* UNUSED *>log : App.Log ) =
BEGIN
END Default;


(* "root" is the root of the HTTP server.
   "db" is a text->text table for mapping names to email addresses. *)

VAR
  root : HTTPControl.StaticForm := HTTPControl.RootForm();
  db := NEW(TextTextTbl.Default).init();

BEGIN

(* Initialize root default options. *)

    root.hasSubmitButton := TRUE;
    root.title := "Contact Database";

(* Add title. *)

    root.addValue(NEW(HTTPControlValue.MessageValue).init("\n" &
 							  "<H2>Contact Database</H2>"));

(* Add the two text fields. *)

    root.addValue(name_value);
    root.addValue(email_value);
                
(* Serve at port # 80. If there is a problem, raise "App.Error". *)

    TRY
      HTTPApp.Serve(80); (* Unhandled: App.Error *)
    EXCEPT
      App.Error => IO.Put ("A problem occured\n");
    END;

 END WebContact.
