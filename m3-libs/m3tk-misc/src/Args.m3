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
(**)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)


MODULE Args;

IMPORT Text, ASCII, Fmt, CITextRefTbl, TextList, Convert, Params;
IMPORT TextExtras;


CONST
  KeywordPrefix = '-';
  KeywordPrefixText = "-";

  Spacer = ' ';
  AlternativeSep = '=';
  TypeSep = '/';


TYPE
  Key = OBJECT
    quota: CARDINAL := 1;
    exact := TRUE;
    required := FALSE;
    positional := FALSE;
    prefix := FALSE;
    index: CARDINAL;
    names: TextList.T := NIL;
    cghack: REF INTEGER;
  END;

  Error = {None,
     RequiredArgMissing, TooFewArgs, TooManyArgs, KeyAppearsMoreThanOnce};


REVEAL
  Template = BRANDED OBJECT
    table: CITextRefTbl.T;
    count: CARDINAL := 0;
    keys: REF ARRAY OF Key := NIL;
  END;

  Handle = BRANDED OBJECT
    errors := 0;
    template: Template;
    values: REF ARRAY OF REF ARRAY OF TEXT;
    errorList: REF ARRAY OF Error := NIL;
    leftOver: TextList.T := NIL;
  END;


(* Utilities *)

<*INLINE*> PROCEDURE AddRear(VAR (*inout*) l: TextList.T; t: Text.T)=
  BEGIN
    (* Append text to TextList.T *)
    WITH tl = TextList.List1(t) DO
      IF l = NIL THEN l := tl
      ELSE
        l := TextList.AppendD(l, tl);
      END;
    END
  END AddRear;


<*INLINE*> PROCEDURE Copy(READONLY args: T): REF T RAISES {}=
  VAR
    new := NEW(REF ARRAY OF TEXT, NUMBER(args));
  BEGIN
    new^ := args;
    RETURN new;
  END Copy;


<*INLINE*> PROCEDURE Concatenate(READONLY args1, args2: T): REF T RAISES {}=
  VAR
    length1 := NUMBER(args1);
    length2 := NUMBER(args2);
    new := NEW(REF ARRAY OF TEXT, length1 + length2);
  BEGIN
    SUBARRAY(new^, 0, length1) := args1;
    SUBARRAY(new^, length1, length2) := args2;
    RETURN new;
  END Concatenate;


(* Parsing key strings and building a template *)

PROCEDURE EnterKeyName(
    table: CITextRefTbl.T;
    t: Text.T;
    k: Key)
    RAISES {BadTemplate}=
  BEGIN
    (* Add keyword to text hash table; no duplicates allowed *)
    IF table.put(t, k) THEN
      RAISE BadTemplate;
    END; (* if *)
  END EnterKeyName;


<*INLINE*> PROCEDURE EnterKeyNames(
    template: Template;
    key: Key)
    RAISES {BadTemplate}=
  BEGIN
    (* Make key accessible via hash table; its names are entered both with and
     without the special keyword prefix character *)
    VAR
      te: TextList.T := key.names;
    BEGIN
      WHILE te # NIL DO
        EnterKeyName(template.table, te.head, key);
        EnterKeyName(template.table, KeywordPrefixText & te.head, key);
        te := te.tail;
      END;
    END;
  END EnterKeyNames;


<*INLINE*> PROCEDURE CheckKeyTypeValid(
    t, longForm: Text.T)
    RAISES {BadTemplate}=
  BEGIN
    IF Text.Length(t) > 1 AND NOT TextExtras.CIEqual(t, longForm) THEN
      RAISE BadTemplate;
    END; (* if *)
  END CheckKeyTypeValid;


PROCEDURE KeyType(
    key: Key;
    t: Text.T)
    RAISES {BadTemplate}=
  VAR
    ch := ASCII.Upper[Text.GetChar(t, 0)];
  BEGIN
    IF ch = 'R' THEN
      CheckKeyTypeValid(t, "required");
      key.required := TRUE;
    ELSIF ch = 'L' THEN
      CheckKeyTypeValid(t, "list");
      key.quota := LAST(CARDINAL);
      key.exact := FALSE;
    ELSIF ch = 'F' THEN
      CheckKeyTypeValid(t, "flag");
      key.quota := 0;
    ELSIF ch = 'X' THEN
      CheckKeyTypeValid(t, "prefix");
      key.prefix := TRUE;
    ELSIF ch = 'P' THEN
      CheckKeyTypeValid(t, "positional");
      key.positional := TRUE;
    ELSIF ch IN ASCII.Digits THEN
      VAR used: INTEGER; 
          buffer := NEW(REF ARRAY OF CHAR, Text.Length(t));
      BEGIN
        Text.SetChars(buffer^, t);
        key.quota := Convert.ToUnsigned(buffer^, used);
        IF used # NUMBER(buffer^) THEN RAISE BadTemplate END;
        key.exact := (ch # '0');
      END
    ELSE
      RAISE BadTemplate;
    END;
  END KeyType;


PROCEDURE GetItem(
    t: Text.T;
    terminators: SET OF CHAR;
    limit: CARDINAL;
    VAR pos: CARDINAL;
    VAR item: Text.T)
    : CHAR
    RAISES {BadTemplate}=
  VAR
    start := pos;
  BEGIN
    (* Extract next alphanumeric item and return terminating character or
     '\000' if we have reached 'limit'. The value of 'item' returned is
     always at least one character long and alphanumeric *)
    EVAL TextExtras.FindCharSet(t, ASCII.Asciis - ASCII.AlphaNumerics, pos);
    IF pos = start THEN RAISE BadTemplate END;
    item := Text.Sub(t, start, pos - start);
    IF pos >= limit THEN
      RETURN '\000';
    ELSE
      WITH ch = Text.GetChar(t, pos) DO
        INC(pos);
        IF pos >= limit OR NOT ch IN terminators THEN RAISE BadTemplate END;
        RETURN ch;
      END;
    END;
  END GetItem;


PROCEDURE ParseKey(
    template: Template;
    t: Text.T;
    start, end: CARDINAL)
    RAISES {BadTemplate}=
  CONST
    BothSeps = SET OF CHAR {AlternativeSep, TypeSep};
    JustTypeSep = SET OF CHAR {TypeSep};
  VAR
    key := NEW(Key, index := template.count);
    pos := start;
    item: Text.T;
    ch: CHAR;
  BEGIN
    (* Build list of the alternative names of the key *)
    REPEAT
      ch := GetItem(t, BothSeps, end, pos, item);
      IF NOT Text.GetChar(item, 0) IN ASCII.Letters THEN
        RAISE BadTemplate;
      END;
      AddRear(key.names, item);
    UNTIL ch # AlternativeSep;

    (* Discover the type of the key *)
    WHILE ch = TypeSep DO
      ch := GetItem(t, JustTypeSep, end, pos, item);
      KeyType(key, item);
    END;
      
    (* Add the key names to the template *)
    EnterKeyNames(template, key);
  END ParseKey;


PROCEDURE NewTemplate(t: Text.T): Template RAISES {BadTemplate}=
  VAR
    template := NEW(Template, table := NEW(CITextRefTbl.Default).init());
  BEGIN
    (* Parse 't' and build up hash table containing all keys *)
    VAR
      start, pos: CARDINAL := 0;
    BEGIN
      WHILE TextExtras.FindCharSet(t, ASCII.Asciis - ASCII.Set {Spacer}, pos) DO
        start := pos;
        WITH last = NOT TextExtras.FindChar(t, Spacer, pos) DO
          ParseKey(template, t, start, pos);
          INC(template.count);
          IF last THEN EXIT ELSE INC(pos) END;
        END;
      END;
    END;

    (* Build array so the keys can be accessed by index *)
    IF template.count > 0 THEN
      VAR
        i := template.table.iterate();
        key: Text.T;
        value: REFANY;
      BEGIN
        template.keys := NEW(REF ARRAY OF Key, template.count);
        WHILE i.next(key, value) DO
          WITH key = NARROW(value, Key) DO
            template.keys[key.index] := key;
          END;
        END;
      END;
    END;

    RETURN template;
  END NewTemplate;


(* Decoding arguments and building the argument handle *)

PROCEDURE LooksLikeKeyword(t: Text.T): BOOLEAN RAISES {}=
  BEGIN
    RETURN Text.Length(t) >= 2 AND Text.GetChar(t, 0) = KeywordPrefix AND
        Text.GetChar(t, 1) IN ASCII.Letters;
  END LooksLikeKeyword;


PROCEDURE IsKeyword(
    h: Handle;
    t: Text.T;
    VAR key: Key;
    VAR tMinusPrefix: TEXT)
    : BOOLEAN
    RAISES {}=
  VAR
    ref: REFANY;
  BEGIN
    IF h.template.table.get(t, ref) THEN
      key := NARROW(ref, Key);
      RETURN TRUE;
    ELSE
      (* might be a prefix arg *)
      VAR iter := h.template.table.iterate();
          name, uname: TEXT; val: REFANY; index: CARDINAL;
          ut := ToUpper(t);
      BEGIN
        WHILE iter.next(name, val) DO
          index := 0; key := val; uname := ToUpper(name);
          IF TextExtras.FindSub(ut, uname, index) AND index = 0 AND
             key.prefix THEN
            tMinusPrefix := TextExtras.Extract(t, Text.Length(name),
                                               Text.Length(t));
            RETURN TRUE;
          END
        END;
        RETURN FALSE;
      END
    END;
  END IsKeyword;

PROCEDURE ToUpper(t: TEXT): TEXT=
  VAR
    l := Text.Length(t);
    x := NEW(REF ARRAY OF CHAR, l);
  BEGIN
    FOR i := 0 TO l-1 DO
      x[i] := ASCII.Upper[Text.GetChar(t, i)];
    END;
    RETURN Text.FromChars(x^);
  END ToUpper;

PROCEDURE CheckedArgValue(t: Text.T): Text.T RAISES {}=
  VAR
    length := Text.Length(t);
  BEGIN
    IF length >= 2 AND Text.GetChar(t, 0) = KeywordPrefix AND
        Text.GetChar(t, 1) = KeywordPrefix THEN
      RETURN Text.Sub(t, 1, length - 1);
    ELSE
      RETURN t;
    END; (* if *)
  END CheckedArgValue;


PROCEDURE NewErrorList(number: CARDINAL): REF ARRAY OF Error RAISES {}=
  VAR
    new := NEW(REF ARRAY OF Error, number);
  BEGIN
    FOR i := 0 TO number - 1 DO new[i] := Error.None END;
    RETURN new;
  END NewErrorList;


<*INLINE*> PROCEDURE NoteError(h: Handle; key: Key; newError: Error) RAISES {}=
  BEGIN
    IF h.errorList = NIL THEN
      h.errorList := NewErrorList(h.template.count);
    END;
    WITH error =  h.errorList[key.index] DO
      IF error = Error.None THEN
        error := newError;
        INC(h.errors);
      END;
    END;
  END NoteError;


<*INLINE*> PROCEDURE MoveListOfArgs(
    VAR from: ARRAY OF TEXT)
    : REF ARRAY OF TEXT
    RAISES {}=
  VAR
    number := NUMBER(from);
    new := NEW(REF ARRAY OF TEXT, number);
  BEGIN
    FOR i := 0 TO number - 1 DO
      WITH f = from[i] DO
        new[i] := CheckedArgValue(f);
        f := NIL;
      END;
    END;
    RETURN new;
  END MoveListOfArgs;


VAR
  null_g := NEW(REF ARRAY OF TEXT, 0);


PROCEDURE BindValue(h: Handle; key: Key; VAR args: T) RAISES {}=
  VAR
    quota := key.quota;
    error := Error.None;
  BEGIN
    WITH value = h.values[key.index] DO

      IF value # NIL AND NOT key.prefix THEN
        error := Error.KeyAppearsMoreThanOnce;
      ELSIF NUMBER(args) > quota THEN
        error := Error.TooManyArgs;
      ELSIF key.exact AND NUMBER(args) < quota THEN
        error := Error.TooFewArgs;
      END;

      IF error = Error.None THEN
        IF key.prefix THEN
          VAR new: REF ARRAY OF TEXT; length: CARDINAL;
          BEGIN
            IF value = NIL THEN length := 1;
            ELSE
              length := NUMBER(value^) + 1;
            END;
            new := NEW(REF ARRAY OF TEXT, length);
            FOR i := 0 TO length-2 DO
              new[i] := value[i];
            END;
            new[length-1] := args[0];
            value := new;
          END
        ELSE
          value := MoveListOfArgs(args);
        END
      ELSE
        IF value = NIL THEN value := null_g END;
        NoteError(h, key, error);
        FOR i := 0 TO LAST(args) DO args[i] := NIL END;
      END;

    END;
  END BindValue;


PROCEDURE FindNextKeyword(
    READONLY args: T;
    pos: CARDINAL;
    VAR argCount: CARDINAL)
    : CARDINAL
    RAISES {}=
  VAR
    countArgs := TRUE;
  BEGIN
    argCount := 0;
    LOOP
      IF pos >= NUMBER(args) THEN RETURN pos END;
      WITH arg = args[pos] DO
        IF arg = NIL THEN
          countArgs := FALSE;
        ELSIF LooksLikeKeyword(arg) THEN
          RETURN pos;
        ELSE
          IF countArgs THEN INC(argCount) END;
        END;
      END;
      INC(pos);
    END;
  END FindNextKeyword;


PROCEDURE KeywordArgs(h: Handle; VAR args: T) RAISES {}=
  VAR
    argCount: CARDINAL;
    i := FindNextKeyword(args, 0, argCount);
  BEGIN
    WHILE i <= LAST(args) DO
      WITH arg = args[i] DO
        INC(i);
        VAR
          key: Key;
          next := FindNextKeyword(args, i, argCount);
          VAR tMinusPrefix: TEXT := NIL;
        BEGIN
          IF IsKeyword(h, arg, key, tMinusPrefix) THEN
            arg := NIL;
            IF key.prefix THEN
              VAR oneArg := NEW(REF ARRAY OF TEXT, 1);
              BEGIN
                IF tMinusPrefix = NIL THEN tMinusPrefix := "" END;
                oneArg[0] := tMinusPrefix;
                BindValue(h, key, oneArg^);
              END;
              (* dont consume following args *)
              IF argCount # 0 THEN next := i; END;
            ELSE
              IF i + argCount > LAST(args) AND key.quota < argCount THEN
                argCount := key.quota;
              END;
              WITH argsForKey = SUBARRAY(args, i, argCount) DO
                BindValue(h, key, argsForKey);
              END;
            END
          END;
          i := next;
        END;
      END;
    END;
  END KeywordArgs;


PROCEDURE FindTrailingArgs(
    READONLY args: T;
    VAR pos: CARDINAL)
    : BOOLEAN
    RAISES {}=
  VAR
    search := NUMBER(args);
  BEGIN
    LOOP
      IF search = 0 THEN
        EXIT;
      ELSE
        DEC(search);
      END;
      WITH arg = args[search] DO
        IF arg = NIL THEN
          INC(search); (* So 'search' points after the NIL *)
          EXIT;
        ELSIF LooksLikeKeyword(arg) THEN
          RETURN FALSE;
        ELSE
          (* loop *)
        END;
      END;
    END;
    (* Exit to here means we hit the start of the argument array or a NIL
     argument before hitting any keywords. If there are any arguments after
     this point we have found trailing arguments *)
    IF search < NUMBER(args) THEN
      pos := search;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END FindTrailingArgs;


PROCEDURE PositionalArgs(h: Handle; VAR args: T) RAISES {}=
  VAR
    aPos: CARDINAL := 0;
    limit: CARDINAL;
  BEGIN
    (* First check that we have some arguments to bind; only arguments at the
     head and tail of the argument array can be bound positionally *)
    EVAL FindNextKeyword(args, 0, limit);
    IF limit = 0 THEN
      IF NOT FindTrailingArgs(args, aPos) THEN RETURN END;
      limit := NUMBER(args);
    END;

    (* Iterate the keys, looking for those which can be bound positionally *)
    WITH keys = h.template.keys^ DO
      FOR kPos := FIRST(keys) TO LAST(keys) DO
        WITH key = keys[kPos] DO

          (* Check if we can bind positionally to this keyword *)
          IF key.positional AND key.quota # 0 THEN

            (* If is already bound then we terminate processing of
             positional args *)
            IF h.values[key.index] # NIL THEN RETURN END;

            (* Check we have some values to bind *)
            IF aPos = limit THEN
              IF limit < NUMBER(args) AND FindTrailingArgs(args, aPos) THEN
                (* We found some more at the tail of the argument array *)
                limit := NUMBER(args);
              ELSE
                RETURN; (* We've run out *)
              END;
            END;

            (* Bind as many values as we can *)
            WITH argCount = MIN(limit - aPos, key.quota) DO
              BindValue(h, key, SUBARRAY(args, aPos, argCount));
              INC(aPos, argCount);
            END;

          END;

        END;
      END;
    END;
  END PositionalArgs;


<*INLINE*> PROCEDURE CheckRequiredArgsPresent(h: Handle) RAISES {}=
  BEGIN
    WITH keys = h.template.keys^ DO
      FOR i := FIRST(keys) TO LAST(keys) DO
        WITH key = keys[i] DO
          IF key.required AND h.values[i] = NIL THEN
            NoteError(h, key, Error.RequiredArgMissing);
          END;
        END;
      END;
    END;
  END CheckRequiredArgsPresent;


<*INLINE*> PROCEDURE CheckAllArgsDecoded(h: Handle; VAR args: T) RAISES {}=
  VAR
    afterKeyword := FALSE;
  BEGIN
    FOR i := FIRST(args) TO LAST(args) DO
      WITH arg = args[i] DO
        IF arg # NIL THEN
          WITH looksLikeKeyword = LooksLikeKeyword(arg) DO
            IF looksLikeKeyword OR NOT afterKeyword THEN
              AddRear(h.leftOver, arg);
              INC(h.errors);
            END;
            IF looksLikeKeyword THEN afterKeyword := TRUE END;
          END;
          arg := NIL;
        ELSE
          afterKeyword := FALSE;
        END;
      END;
    END;
  END CheckAllArgsDecoded;


PROCEDURE Decode(
    template: Template;
    VAR args: T;
    all := TRUE)
    : Handle
    RAISES {}=
  VAR
    h := NEW(Handle, template := template,
        values := NEW(REF ARRAY OF REF ARRAY OF TEXT, template.count));
  BEGIN
    FOR i := 0 TO h.template.count - 1 DO h.values[i] := NIL END;

    IF h.template.count > 0 THEN
      KeywordArgs(h, args);
      IF all THEN PositionalArgs(h, args) END;
      CheckRequiredArgsPresent(h);    
    END;

    IF all THEN CheckAllArgsDecoded(h, args) END;

    RETURN h;
  END Decode;


PROCEDURE Good(h: Handle): BOOLEAN RAISES {}=
  BEGIN
    RETURN h.errors = 0;
  END Good;


EXCEPTION
  Fatal;


<*INLINE*> PROCEDURE InternalValue(
    h: Handle;
    name: Text.T;
    VAR key: Key)
    : REF ARRAY OF TEXT
    RAISES {BadEnquiry}=
  VAR void: TEXT;
  BEGIN
    IF h.errors = 0 THEN
      IF IsKeyword(h, name, key, void) THEN
        RETURN h.values[key.index];
      ELSE
        RAISE BadEnquiry;
      END;
    ELSE
      <*FATAL Fatal*> BEGIN RAISE Fatal; END;
    END;
  END InternalValue;


PROCEDURE Value(
    h: Handle;
    keyword: Text.T)
    : REF ARRAY OF TEXT
    RAISES {BadEnquiry}=
  VAR
    key: Key;
  BEGIN
    RETURN InternalValue(h, keyword, key);
  END Value;


PROCEDURE Flag(h: Handle; keyword: Text.T): BOOLEAN RAISES {BadEnquiry}=
  VAR
    key: Key;
    value := InternalValue(h, keyword, key);
  BEGIN
    IF key.quota # 0 THEN RAISE BadEnquiry END;
    RETURN value # NIL;
  END Flag;


PROCEDURE Single(h: Handle; keyword: Text.T): Text.T RAISES {BadEnquiry}=
  VAR
    key: Key;
    value := InternalValue(h, keyword, key);
  BEGIN
    IF key.quota # 1 OR NOT key.exact THEN RAISE BadEnquiry END;
    IF value = NIL THEN RETURN NIL ELSE RETURN value[0] END;
  END Single;


<*INLINE*> PROCEDURE KeyName(h: Handle; i: CARDINAL): Text.T RAISES {}=
  BEGIN
    RETURN h.template.keys[i].names.head;
  END KeyName;


PROCEDURE Errors(h: Handle; indent: CARDINAL := 0): Text.T RAISES {}=
  BEGIN
    IF h.errors = 0 THEN
      <*FATAL Fatal*> BEGIN RAISE Fatal; END;
    ELSE

      VAR
        texts := NEW(REF ARRAY OF TEXT, h.errors * 2);
            (* allocates space for all the error messages + padding *)
        pos: CARDINAL := 0;
        padding := Fmt.Pad("", indent);
        fmt: Text.T;

      <*INLINE*> PROCEDURE Add(t: Text.T) RAISES {}=
          BEGIN texts[pos] := t; INC(pos) END Add;
      <*INLINE*> PROCEDURE PaddedAdd(t: Text.T) RAISES {}=
          BEGIN Add(padding); Add(t) END PaddedAdd;

      BEGIN

        IF h.errorList # NIL THEN
          FOR i := 0 TO LAST(h.errorList^) DO
            VAR
              error := h.errorList[i];
            BEGIN
              IF error # Error.None THEN
                CASE error OF
                | Error.RequiredArgMissing =>
                    fmt := "Argument required for key \'%s\'\n";
                | Error.TooFewArgs =>
                    fmt := "Too few arguments for key \'%s\'\n";
                | Error.TooManyArgs =>
                    fmt := "Too many arguments for key \'%s\'\n";
                | Error.KeyAppearsMoreThanOnce =>
                    fmt := "More than one occurrence of key \'%s\'\n";
                | Error.None =>
                    <*ASSERT FALSE*>
                END; (* case *)
                PaddedAdd(Fmt.F(fmt, KeyName(h, i)));
              END;
            END;
          END;
        END;

        VAR
          te: TextList.T := h.leftOver;
        BEGIN
          WHILE te # NIL DO
            IF LooksLikeKeyword(te.head) THEN
              fmt := "Unknown keyword \'%s\'\n";
            ELSE
              fmt := "Unexpected argument \'%s\'\n";
            END;
            PaddedAdd(Fmt.F(fmt, te.head));
            te := te.tail;
          END;
        END;

        RETURN TextExtras.JoinN(texts^);
      END;
    END; (* if *)
  END Errors;


PROCEDURE Bind(
    h: Handle;
    keyword: Text.T;
    v: REF ARRAY OF TEXT;
    override := FALSE)
    RAISES {BadBinding}=
  VAR
    ref: REFANY;
    key: Key;
  BEGIN
    IF h.errors # 0 THEN       
      <*FATAL Fatal*> BEGIN RAISE Fatal; END;
    END;
    IF h.template.table.get(keyword, ref) THEN
      key := NARROW(ref, Key);
      WITH value = h.values[key.index] DO
        IF value # NIL AND NOT override THEN
          (* don't override existing value *)
          RETURN;
        ELSE
          VAR
            ok: BOOLEAN;
          BEGIN
            IF v = NIL THEN
              ok := NOT key.required;
            ELSIF key.exact THEN
              ok := NUMBER(v^) = key.quota;
            ELSE
              ok := NUMBER(v^) <= key.quota;
            END;
            IF ok THEN value := v; RETURN END;
          END;
        END;
      END;
    END;
    RAISE BadBinding;
  END Bind;

VAR
  standard_g: Template;
  args_g: REF T;

PROCEDURE Init()=
  VAR
    total := Params.Count-1;
    new := NEW(REF T, total);
  <* FATAL BadTemplate *>
  BEGIN
    standard_g := NewTemplate(StandardTemplateDescription);
    FOR i := 0 TO total - 1 DO
      new[i] := Params.Get(i+1);
    END;
    args_g := new;
  END Init;

PROCEDURE CommandLine(): REF T RAISES {} =
  BEGIN
    RETURN Copy(args_g^);
  END CommandLine;

PROCEDURE Standard(VAR args: T; VAR help, identify: BOOLEAN) RAISES {}=
  VAR
    h := Decode(standard_g, args, FALSE);
  <* FATAL BadEnquiry *>
  BEGIN
    help := Flag(h, "help");
    identify := Flag(h, "identify");
  END Standard;


BEGIN Init();
END Args.
