(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Mon Mar 18 16:00:16 PST 1996 by steveg *)

INTERFACE HTTPControlValue;

IMPORT
  App, HTTP, HTTPControl;

TYPE
  Value = HTTPControl.Value;

TYPE
  BooleanValue <: BooleanValuePublic;
  BooleanValuePublic = Value OBJECT
  METHODS
    get(): BOOLEAN;
    set(v: BOOLEAN);
  END;
  (* clients must subtype a BooleanValue to provide get and set methods *)

  ChoiceValue <: ChoiceValuePublic;
  ChoiceValuePublic = Value OBJECT
    names: REF ARRAY OF TEXT;
  METHODS
    init(READONLY names: ARRAY OF TEXT): ChoiceValue;
    get(): INTEGER;
    set(v: INTEGER; log: App.Log) RAISES {App.Error};
  END;
  (* clients must subtype a ChoiceValue to provide get and set methods *)

  FormValue <: FormValuePublic;
  FormValuePublic = Value OBJECT
    form: HTTPControl.Form;
    name, url: TEXT;
  METHODS
    init(name, url: TEXT): FormValue;
    get(): HTTPControl.Form;
    set(v: HTTPControl.Form);
  END;

  ImageValue <: ImageValuePublic;
  ImageValuePublic = Value OBJECT
  METHODS
    get(): HTTP.URL;
    set(v: HTTP.URL; log: App.Log) RAISES {App.Error};
  END;

  IntegerValue <: IntegerValuePublic;
  IntegerValuePublic = Value OBJECT
  METHODS
    get(): INTEGER;
    set(v: INTEGER; log: App.Log) RAISES {App.Error};
  END;
  (* clients must subtype an IntegerValue to provide get and set methods *)

  MessageValue <: MessageValuePublic;
  MessageValuePublic = Value OBJECT
  METHODS
    init(msg: TEXT): MessageValue;
  END;
  (* a non-editable value that display "msg" *)

  RealValue <: RealValuePublic;
  RealValuePublic = Value OBJECT
  METHODS
    get(): REAL;
    set(v: REAL; log: App.Log) RAISES {App.Error};
  END;
  (* clients must subtype an RealValue to provide get and set methods *)

  TextValue <: TextValuePublic;
  TextValuePublic = Value OBJECT
    scrollable : BOOLEAN  := FALSE;
    size       : CARDINAL := 30;   (* non-scroll option -- width *)
    maxLength  : CARDINAL := 1000; (* non-scroll, max length *)
    rows       : CARDINAL := 8;    (* scrollable, default rows *)
    columns    : CARDINAL := 80;   (* scrollable, default columns *)
  METHODS
    get(): TEXT;
    set(v: TEXT; log: App.Log) RAISES {App.Error};
  END;
  (* clients must subtype an TextValue to provide get and set methods *)

  URLValue <: URLValuePublic;
  URLValuePublic = Value OBJECT
  METHODS
    init(url: TEXT): URLValue;
    get(): TEXT;
    set(v: TEXT; log: App.Log) RAISES {App.Error};
  END;

TYPE
  Rows = REF ARRAY OF Columns;
  Columns = REF ARRAY OF Value;
  Table = Rows;

  TableValue <: TableValuePublic;
  TableValuePublic = HTTPControl.ContainerValue OBJECT 
    caption: TEXT;
    table: Table;
  METHODS
    get(): Table;
    set(v: Table) RAISES {App.Error};
  END;

END HTTPControlValue.
