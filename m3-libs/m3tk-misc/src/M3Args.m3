(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)

MODULE M3Args EXPORTS M3Args, M3ArgsCL;

(* This implementation uses Args and also assumes that it is working
   from a command line decoding. It amalgamates all the keywords into
   a single template, and does a single decode. M3ArgsCL.Reset can
   be called to redo the decoding.
*)

IMPORT Text, TextExtras, Args, Err, ASCII, RefList, RefListSort;

TYPE
  ArgState = OBJECT
    name: TEXT;
    nameAndKind: TEXT;
    usage: TEXT;
    shared: BOOLEAN;
  END;

  FlagArgState = ArgState BRANDED OBJECT END; 
  StringArgState = ArgState BRANDED OBJECT END;
  StringListArgState = ArgState BRANDED OBJECT END;
  PrefixArgState = ArgState BRANDED OBJECT END;

REVEAL
  T = BRANDED REF RECORD
    toolName, toolDescription, version: TEXT;
    master: BOOLEAN;
    argList: RefList.T;
  END;
   
VAR
  toolList_g: RefList.T;   (* list of all registered tools *)
  master_g: T := NIL;      (* current master *)
  args_g: RECORD
    init: BOOLEAN;    (* have we done Args.NewTemplate/Args.Decode? *) 
    cl: REF Args.T;   (* Command line *)
    template: Args.Template;
    handle: Args.Handle;
    keyString: TEXT;
    help, identify: BOOLEAN;
  END;

CONST
  IndentLength = 24;
  Indent = "                         ";

EXCEPTION
  DuplicateArg; (* no non-shared duplicates allowed *)
  ClashingShortform;

PROCEDURE New(toolName, toolDescription, version: TEXT;
    master := FALSE): T RAISES {} =
  VAR
    t: T;
  BEGIN
    t := NEW(T); 
    t.toolName := toolName;
    t.toolDescription := toolDescription;
    t.version := version;
    t.master := master;
    t.argList := NIL;
    IF master THEN
      toolList_g := RefList.Cons(t, toolList_g); master_g := t;
    ELSE toolList_g := RefList.AppendD(toolList_g, RefList.List1(t));
    END;
    RETURN t;
  END New;

PROCEDURE SetMaster(t: T): T RAISES {}=
  PROCEDURE Compare(e1: REFANY; <*UNUSED*> e2: REFANY): [-1..1]=
    BEGIN
      IF e1 = t THEN RETURN -1 ELSE RETURN 1 END;
    END Compare;

  VAR r := master_g;
  BEGIN
    toolList_g := RefListSort.SortD(toolList_g, Compare);
    master_g := t;
    RETURN r;
  END SetMaster;

PROCEDURE Usage(t: T) RAISES {} =
  VAR 
    al: RefList.T;
    a: ArgState;
    l: INTEGER;
  BEGIN
    Err.Print(t.toolDescription, Err.Severity.Comment);
    al := t.argList;
    WHILE al # NIL DO
      a := al.head;
      Err.Print("-", Err.Severity.Continue, FALSE);
      Err.Print(a.nameAndKind, Err.Severity.Continue, FALSE);
      l := Text.Length(a.nameAndKind);
      REPEAT Err.Print(" ", Err.Severity.Continue, FALSE); INC(l)
      UNTIL l >= IndentLength;
      Err.Print(a.usage, Err.Severity.Continue, FALSE);
      Err.Print("", Err.Severity.Continue);
      al := al.tail;
    END; (* while *)
    Err.Print("", Err.Severity.Continue)
  END Usage;

PROCEDURE RegisterFlag(
    t: T;
    argName: TEXT;
    usage: TEXT;
    shared := FALSE) RAISES {} =
  BEGIN
    RegisterArg(NEW(FlagArgState), t, argName, usage, Opt.Optional, shared);
  END RegisterFlag;

PROCEDURE RegisterString(
    t: T;
    argName: TEXT;
    usage: TEXT;
    opt: Opt := Opt.Optional;
    shared := FALSE) 
    RAISES {} =
  BEGIN
    RegisterArg(NEW(StringArgState), t, argName, usage, opt, shared);
  END RegisterString;

PROCEDURE RegisterStringList(
    t: T;
    argName: TEXT;
    usage: TEXT;
    opt: Opt := Opt.Optional;
    shared := FALSE) 
    RAISES {} =
  BEGIN
    RegisterArg(NEW(StringListArgState), t, argName, usage, opt, shared);
  END RegisterStringList;

PROCEDURE RegisterPrefix(
    t:T;
    argName: TEXT;
    usage: TEXT;
    opt: Opt := Opt.Optional;
    shared := FALSE) RAISES {}=
  BEGIN
    RegisterArg(NEW(PrefixArgState), t, argName, usage, opt, shared);
  END RegisterPrefix;

PROCEDURE RegisterArg(a: ArgState; t: T; argName: TEXT;
    usage: TEXT;
    opt: Opt;
    shared := FALSE) RAISES {} =
  VAR 
    shortForm, nameAndKind: TEXT;
  BEGIN
    a.name := argName;
    a.usage := ExpandNL(usage);
    nameAndKind := ArgsArgName(argName, a, opt, shortForm);
    a.nameAndKind:= nameAndKind;
    a.shared := shared; 
    IF IsDuplicated(argName, shortForm, ISTYPE(a, PrefixArgState), shared) THEN
      IF NOT shared THEN 
        <*FATAL DuplicateArg*> BEGIN RAISE DuplicateArg END;
      END;
    ELSE
      args_g.keyString := args_g.keyString & a.nameAndKind;
      args_g.keyString := args_g.keyString & " ";
    END;
    t.argList:= RefList.AppendD(t.argList, RefList.List1(a));
  END RegisterArg;

<*INLINE*> PROCEDURE ExpandNL(t: TEXT): TEXT RAISES {}=
  VAR
    index: CARDINAL := 0;
  BEGIN
    LOOP
      IF TextExtras.FindChar(t, '\n', index) THEN
      	t := TextExtras.Extract(t, 0, index+1) & Indent &
	     TextExtras.Extract(t, index+1, Text.Length(t));
        INC(index);
      ELSE
      	EXIT
      END; (* if *)
    END; (* loop *)
    RETURN t;
  END ExpandNL;

PROCEDURE Help(t: T; preamble := TRUE) RAISES {} =
  BEGIN
    Setup(t);
    IF preamble THEN HelpPreamble(t); END;
    Usage(t);
  END Help;

PROCEDURE HelpPreamble(t: T; ) RAISES {} =
  BEGIN
    Setup(t);
    Err.Print(
  "Keywords - \'/f\' boolean flag. \'/l\' space separated list of values.\n" &
  "           \'/1\' single value. \'/r\' means mandatory.\n" &
  "           \'/p\' means positional argument (keyword can be omitted).\n" &
  "Capitalisation (and \'=short\') indicates alternative shortened form.\n",
      Err.Severity.Continue);
  END HelpPreamble;

PROCEDURE CheckHelp(display := TRUE): BOOLEAN RAISES {} =
  VAR
    tl: RefList.T; t: T;
  BEGIN
    Setup(NIL);
    IF args_g.help OR args_g.identify THEN
      IF display THEN
        tl := toolList_g;
        IF tl # NIL AND args_g.help THEN HelpPreamble(t) END;
        WHILE tl # NIL DO
          t := tl.head;
          SetName(t);
          IF args_g.identify THEN
            Err.Print("Version " & t.version, Err.Severity.Comment);
          END;
          IF args_g.help THEN Usage(t); END;
          tl := tl.tail;
        END; (* while *)
      END;
      RETURN TRUE
    ELSE
      RETURN FALSE
    END;
  END CheckHelp;

PROCEDURE Setup(t: T) RAISES {} =
  BEGIN
    SetName(t);
    ArgsInit();
  END Setup;

PROCEDURE ArgsInit() RAISES {} =
  BEGIN
    IF NOT args_g.init THEN
      args_g.cl := Args.CommandLine();
      <*FATAL Args.BadTemplate*>
      BEGIN
        args_g.template := Args.NewTemplate(args_g.keyString);
      END;
      ArgsDecode();
    END;
  END ArgsInit;

PROCEDURE ArgsDecode() RAISES {}=
  BEGIN
    Args.Standard(args_g.cl^, args_g.help, args_g.identify);
    args_g.handle := Args.Decode(args_g.template, args_g.cl^, TRUE);
    args_g.init := TRUE;
  END ArgsDecode;


PROCEDURE Reset(cl: REF Args.T) RAISES {}=
  BEGIN
    args_g.cl := cl;
    ArgsDecode();    
  END Reset;


PROCEDURE SetName(t: T) RAISES {} =
  VAR name: TEXT;
  BEGIN
    IF t = NIL THEN name := "m3args" ELSE name := t.toolName; END;
    EVAL Err.SetProgramName(name);
  END SetName;

PROCEDURE Find(t: T): BOOLEAN RAISES {} =
  BEGIN
    Setup(t);
    IF Args.Good(args_g.handle) THEN
      RETURN TRUE;
    ELSE
      Err.Print("Bad args - use \'-help\' if in need of help", 
          Err.Severity.Warning);
      RETURN FALSE;
    END;
  END Find;

PROCEDURE ArgsArgName(s: TEXT; 
    a: ArgState; opt: Opt;
    VAR (*out*) shortForm: TEXT): TEXT RAISES {} =
  VAR
    ns: TEXT;    
    l, index, lindex: CARDINAL;
    shortFormArray: REF ARRAY OF CHAR;
    ch: CHAR;
  BEGIN
    l := Text.Length(s);
    shortFormArray := NEW(REF ARRAY OF CHAR, l);
    index := 0; lindex := 0;
    WHILE index < l DO
      ch := Text.GetChar(s, index);
      IF ch IN ASCII.Uppers THEN
        shortFormArray[lindex] := ASCII.Lower[ch];
        INC(lindex);
      END;
      INC(index);
    END; (* while *)
    shortForm := Text.FromChars(SUBARRAY(shortFormArray^, 0, lindex));
    (* check and ignore if short form = long form *)
    IF TextExtras.CIEqual(s, shortForm) THEN
      lindex := 0;
    END; (* if *)
    IF lindex > 0 THEN
      ns := s & "=" & shortForm;
    ELSE
      ns := s;
    END; (* if *)
    TYPECASE a OF <*NOWARN*>
    | FlagArgState =>
        ns := Text.Cat(ns, "/f");
    | StringListArgState =>
        ns := Text.Cat(ns, "/l");
    | StringArgState =>
        ns := ns & "/1";
        IF opt = Opt.Required THEN
          ns := Text.Cat(ns, "/r");
        END;
    | PrefixArgState => 
        ns := ns & "/l/x"
    END;
    IF opt = Opt.Positional THEN ns := Text.Cat(ns, "/p") END;
    RETURN ns;
  END ArgsArgName;

PROCEDURE GetFlag(<*UNUSED*> t: T; s: TEXT): BOOLEAN RAISES {} =
  BEGIN
    TRY
      RETURN Args.Flag(args_g.handle, s)
    EXCEPT
    | Args.BadEnquiry => <*ASSERT FALSE*>
    END; 
  END GetFlag;

PROCEDURE GetString(<*UNUSED*> t: T; s: TEXT): TEXT 
    RAISES {} =
  BEGIN
    TRY
      RETURN Args.Single(args_g.handle, s);
    EXCEPT
    | Args.BadEnquiry => <*ASSERT FALSE*>
    END; 
  END GetString;

PROCEDURE GetStringList(<*UNUSED*> t: T; s: TEXT): REF ARRAY OF TEXT 
    RAISES {} =
  BEGIN
    TRY
      RETURN Args.Value(args_g.handle, s);
    EXCEPT
    | Args.BadEnquiry => <*ASSERT FALSE*>
    END; 
  END GetStringList;

PROCEDURE GetPrefix(<*UNUSED*> t: T; s: TEXT): REF ARRAY OF TEXT 
    RAISES {} =
  BEGIN
    TRY
      RETURN Args.Value(args_g.handle, s);
    EXCEPT
    | Args.BadEnquiry => <*ASSERT FALSE*>
    END; 
  END GetPrefix;

PROCEDURE SetFlag(<*UNUSED*> t: T; s: TEXT; f: BOOLEAN) RAISES {} =
  VAR v: REF ARRAY OF TEXT;
  BEGIN
    IF f THEN v := NEW(REF ARRAY OF TEXT, 0) ELSE v := NIL END;
    TRY
      Args.Bind(args_g.handle, s, v, TRUE);
    EXCEPT
    | Args.BadBinding => <*ASSERT FALSE*>
    END;
  END SetFlag;

PROCEDURE SetString(<*UNUSED*> t: T; s: TEXT; val: TEXT) 
    RAISES {} =
  VAR v: REF ARRAY OF TEXT;
  BEGIN
    v := NEW(REF ARRAY OF TEXT, 1); v[0] := val;
    TRY
      Args.Bind(args_g.handle, s, v, TRUE);
    EXCEPT
    | Args.BadBinding => <*ASSERT FALSE*>
    END;
  END SetString;

PROCEDURE SetStringList(<*UNUSED*> t: T; s: TEXT; sl: REF ARRAY OF TEXT) 
    RAISES {} =
  BEGIN
    TRY
      Args.Bind(args_g.handle, s, sl, TRUE);
    EXCEPT
    | Args.BadBinding => <*ASSERT FALSE*>
    END;
  END SetStringList;

PROCEDURE SetPrefix(<*UNUSED*> t: T; s: TEXT; sl: REF ARRAY OF TEXT) 
    RAISES {} =
  BEGIN
    TRY
      Args.Bind(args_g.handle, s, sl, TRUE);
    EXCEPT
    | Args.BadBinding => <*ASSERT FALSE*>
    END;
  END SetPrefix;

PROCEDURE SetStringAsList(<*UNUSED*> t: T; s: TEXT; sl: TEXT) RAISES {} =
  VAR
    start, end, l: CARDINAL;
    count := 0;
    v: REF ARRAY OF TEXT;
  BEGIN
    start := 0; end := 0; 
    l := Text.Length(sl);
    LOOP
      IF TextExtras.FindCharSet(sl, ASCII.Set{' ', ','}, end) THEN END;
      IF end >= l THEN EXIT END;
      start := end+1; end := start;
      INC(count);
    END;
    v := NEW(REF ARRAY OF TEXT, count);
    start := 0; end := 0; count := 0;
    LOOP
      IF TextExtras.FindCharSet(sl, ASCII.Set{' ', ','}, end) THEN END;
      v[count] := TextExtras.Extract(sl, start, end);
      INC(count);
      IF end >= l THEN EXIT END;
      start := end+1; end := start;
    END;
    TRY
      Args.Bind(args_g.handle, s, v, TRUE);
    EXCEPT
    | Args.BadBinding => <*ASSERT FALSE*>
    END;
  END SetStringAsList;

PROCEDURE IsDuplicated(argName, shortForm: TEXT;
                       isPrefix: BOOLEAN; shared: BOOLEAN): BOOLEAN=
  PROCEDURE IsPrefixOf(t, pre: TEXT): BOOLEAN=
    VAR
      index: CARDINAL := 0;
    BEGIN
      RETURN TextExtras.FindSub(t, pre, index) AND index = 0
    END IsPrefixOf;

  VAR 
    tl, al: RefList.T; t: T;
    a: ArgState;
    hasShort: BOOLEAN;
    result: BOOLEAN := FALSE;
  BEGIN
    hasShort := NOT Text.Equal(shortForm, "");
    tl := toolList_g;
    WHILE tl # NIL DO
      t := tl.head;
      al := t.argList;
      WHILE al # NIL DO
        a := al.head;
        (* both the full name and the short form must be unique *)
        IF isPrefix THEN
          IF IsPrefixOf(a.name, argName) OR
            (hasShort AND IsPrefixOf(ShortFormOf(a.nameAndKind), argName)) THEN
            <*FATAL ClashingShortform*> BEGIN RAISE ClashingShortform; END;
          END; (* if *)
        ELSIF TextExtras.CIEqual(argName, a.name) THEN
          IF NOT(shared AND a.shared) THEN result := TRUE; END;
        ELSIF (hasShort AND (TextExtras.CIEqual(shortForm, 
                            ShortFormOf(a.nameAndKind)))) THEN
            <*FATAL ClashingShortform*> BEGIN RAISE ClashingShortform; END;
        END; (* if *)
        al := al.tail;
      END; (* while *)
      tl := tl.tail;
    END;
    RETURN result;
  END IsDuplicated;

PROCEDURE ShortFormOf(nameAndKind: TEXT): TEXT RAISES {} =
  VAR
    index, sindex: CARDINAL;
    nameAndShort: TEXT;
  BEGIN
    index := 0;
    IF TextExtras.FindChar(nameAndKind, '=', index) THEN
      sindex := index+1;
      IF TextExtras.FindChar(nameAndKind, '/', index) THEN
        nameAndShort := TextExtras.Extract(nameAndKind, sindex, index);
        RETURN nameAndShort;
      END;
    END;   
    RETURN "";
  END ShortFormOf;

BEGIN
  args_g.init := FALSE; args_g.keyString := "";
END M3Args.
