(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Fri Mar 15 14:27:38 PST 1996 by steveg *)

INTERFACE HTTPControl;

(* HTTPControl provides an interface for applictions to 
   be controlled via an HTTP form interface.

   Values are registered with the interface and are called
   when the user changes the value.

   HTTPControl registers an HTTPApp.RequestHandler that handles
   requests for local URLs.

  *)

IMPORT
  App, HTTP, Wr;

TYPE
  Form <: FormPublic;
  FormPublic = OBJECT
  METHODS
    init(name: TEXT): Form;
    name(): TEXT;
    (* "name" gives the name of the form *)
    accept(request: HTTP.Request; 
           localPath: TEXT;
           VAR (* OUT *) acceptState: REFANY): BOOLEAN;
    (* returns TRUE if this form handles "request". 
       "localPath" is the path portion of the request's URL.
    *)
    respond(request: HTTP.Request; query: HTTP.FormQuery; 
            wr: Wr.T; log: App.Log;
            READONLY acceptState: REFANY) RAISES {App.Error};
    (* "request" is the parsed request header. 
       "query" is the parsed query of submitted form.
       "wr" is the writer for the outgoing response header and result.
    *)
    iterate(): Iterator;
    (* return an iterator for the values of the form *)
  END;

TYPE
  Iterator <: IteratorPublic;
  IteratorPublic = OBJECT
  METHODS
    next(): Value;
    (* return the next value of the form.  return NIL if there are
       no more values.  the result is not defined if a value is
       added while the iterator is active. *)
  END;

PROCEDURE RegisterForm (form     : Form;
                        name, url: TEXT;
                        addToRoot: BOOLEAN := TRUE);
(* Register the form so that the form.accept method is called to
   see if the form handles the request.  If "addToRoot" the form
   is added to the root form. *)

PROCEDURE FormLookup(name: TEXT): Form;
(* Return the registered form with the name "name" *)

TYPE
  StaticFormAcceptState = REF RECORD
    set: BOOLEAN;
  END;

TYPE
  StaticForm <: StaticFormPublic;
  StaticFormPublic = Form OBJECT
    title           : TEXT    := NIL;
    hasSubmitButton : BOOLEAN := TRUE;
  METHODS
    init(name, url: TEXT;
         register := TRUE): StaticForm;
    (* "url" is the local url of the form on the control server.  A "url" of 
       "xxx" or "/xxx" corresponds to the URL: http://<server>:<port>/xxx.  
       The "action" of the static form is URL: http://<server>:<port>/xxxSet

       The "acceptState" field of the "respond" method is a 
       StaticFormAcceptState value indicating whether this is an initial
       call or an attempt to set the form's values.

       The default implementation of "init" calls RegisterForm with
       the form and "url" under "name". *)

    url(): TEXT;
    (* "url" gives the local URL for a request of the form *)
    addValue(value: Value);    
  END;
  (* A StaticForm is a form that has a fixed URL for its address and
     fixed contents (made up of values).
   *)

PROCEDURE RootForm(): StaticForm;
(* The default form answers to the URL: http://<server>:<port>/ *)

PROCEDURE AddToForm(form: StaticForm; subForm: Form; name, url: TEXT);
(* Equivalent to form.addValue(NEW(FormValue, form := form).init(url)) *)

TYPE
  Value <: ValuePublic;
  ValuePublic = OBJECT
    id: TEXT;
    leader, label, trailer: TEXT := "";
    editable: BOOLEAN := TRUE;
  METHODS
    setText(v: TEXT; log: App.Log) RAISES {App.Error};
    getText(): TEXT;
    setDefault(log: App.Log) RAISES {App.Error};
    writeFormItem(wr: Wr.T; log: App.Log) RAISES {App.Error};
  END;

TYPE
  ContainerValue <: ContainerValuePublic;
  ContainerValuePublic = Value OBJECT
  METHODS
    setValues(query: HTTP.FormQuery; log: App.Log) RAISES {App.Error};
  END;

(* All URLs referring to control forms are protected by the basic
   HTTP authorization scheme:

   http://www.w3.org/pub/WWW/Protocols/HTTP1.0/draft-ietf-http-spec.html#BasicAA
   
   If no user is given, then the empty string "" is used.

   If an authorization password is given as an argument then that
   password is used.  If no password is given (NIL), then HTTPControl
   generates one randomly and writes it to stderr (presumably, only
   the user running the application is able to view and know the
   random password).  If an empty password ("") is given then no
   authorization is required to access the control forms.

   The arguments for the password are:

   switch      env              config      default
   -password   HTTP_PASSWORD    password:   NIL
   -user       USER             user:       ""
*)

END HTTPControl.
