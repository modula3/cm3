(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Tue Feb  4 10:14:13 PST 1997 by steveg *)

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

EXCEPTION
  NotAuthorized;

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
            READONLY acceptState: REFANY) RAISES {App.Error, NotAuthorized};
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
(* Equivalent to form.addValue(NEW(FormValue, form := subForm).init(url)) *)

TYPE
  Value <: ValuePublic;
  ValuePublic =
    OBJECT
      id                    : TEXT;
      leader, label, trailer: TEXT    := "";
      editable              : BOOLEAN := TRUE;
    METHODS
      setText (req: HTTP.Request; v: TEXT; log: App.Log)
               RAISES {App.Error, NotAuthorized};
      getText    (req: HTTP.Request): TEXT RAISES {NotAuthorized};
      setDefault (req: HTTP.Request; log: App.Log) RAISES {App.Error, NotAuthorized};
      writeFormItem (req: HTTP.Request; wr: Wr.T; log: App.Log)
                     RAISES {App.Error, NotAuthorized};
    END;

TYPE
  ContainerValue <: ContainerValuePublic;
  ContainerValuePublic =
    Value OBJECT
    METHODS
      setValues (req: HTTP.Request; query: HTTP.FormQuery; log: App.Log)
                 RAISES {App.Error, NotAuthorized};
    END;

END HTTPControl.
