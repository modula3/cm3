MODULE %yacc;
%gen
IMPORT %tok;
IMPORT IntIntTbl, IntTextTbl;
IMPORT RTType;
IMPORT Env, Thread, Wr, Fmt, Rd;
FROM Stdio IMPORT stdout;
FROM %tok IMPORT NewPT;
<* FATAL Wr.Failure, Thread.Alerted *>

TYPE
  TypedSymbol = RECORD
    code: INTEGER;
    value: %tok.ParseType;
  END;
CONST
  EOFSymbol = TypedSymbol{code := 0, value := NIL};
  NoToken = TypedSymbol{code := -1, value := NIL};
  NotASymbol = TypedSymbol{code := -1000, value := NIL};

TYPE
  StackElem = RECORD
    state: INTEGER;
    value: TypedSymbol;
  END;
  StackElemArray = REF ARRAY OF StackElem;

  Stack = RECORD
    a: StackElemArray;
    ptr: INTEGER;
  END;

REVEAL
  T = Public BRANDED "%yacc" OBJECT
    lex: %tok.Lexer;
    tokenLookup: IntIntTbl.T := NIL; (* M3 type code -> SymCode *)
    symbols: IntTextTbl.T;           (* SymCode -> name *)
%alloc\
  OVERRIDES
    setLex := SetLex;
    parse := Parse;
    purge := Purge;
%ovr\
  END;

TYPE
  SymCode = BITS %symCodeBits FOR [0..%symCodeLast];
  (* symbol code:  0 .. %symCodePenult
     set default:  %symCodeLast *)

  Action = BITS %actionBits FOR [0..%actionLast];
  (* error:        -1   (not stored in table)
     shift:        1 .. %lastShift0
     accept:       %lastShift1
     reduce:       %lastShift2 .. %lastReduce0
     shift&accept: %lastReduce1
     shift&reduce: %lastReduce2 .. %actionLast  *)

  StateRef = BITS %stateBits FOR [0..%stateLast];
  (* no more:      0
     next state:   1..%stateLast *)

  S = RECORD
    key: SymCode;
    action: Action;
    next: StateRef;
  END;

  R = RECORD
    length: INTEGER;
    returnCode: INTEGER;
    name: TEXT;
  END;

  Y = RECORD
    code: INTEGER;
    name: TEXT;
  END;

CONST
  States = ARRAY [1..%stateLast] OF S {
%States};

  Rules = ARRAY [%lastShift2..%lastReduce0] OF R {
%Rules};

  Symbols = ARRAY [1..%numSym] OF Y {
%Symbols};

VAR
  Debug := Env.Get("%yaccDEBUG") # NIL;

PROCEDURE SetLex(self: T; lex: %tok.Lexer): T =
  BEGIN self.lex := lex; RETURN self; END SetLex;

PROCEDURE Init(self: T) =
  BEGIN (* called on first parse *)
    self.tokenLookup := NEW(IntIntTbl.Default).init(%numSym);
    IF Debug THEN
      self.symbols := NEW(IntTextTbl.Default).init(%numSym);
      FOR i := 1 TO %numSym DO
        EVAL self.symbols.put(Symbols[i].code, Symbols[i].name);
      END;
    END;
  END Init;

PROCEDURE NextToken(self: T): TypedSymbol =
  VAR
    symCode, m3code: INTEGER;
    token: %tok.Token;
    found := FALSE;
  BEGIN
    TRY
      token := self.lex.get();
    EXCEPT
      Rd.EndOfFile => RETURN EOFSymbol;
    END;
    m3code := TYPECODE(token);
    IF NOT self.tokenLookup.get(m3code, symCode) THEN
      REPEAT
        m3code := RTType.Supertype(m3code);
        IF m3code = RTType.NoSuchType THEN
          TYPECASE token OF
          | ConstToken => symCode := -1;
%case\
          ELSE
            <* ASSERT FALSE *>
          END;
          found := TRUE;
        ELSE
          found := self.tokenLookup.get(m3code, symCode);
        END;
      UNTIL found;
      EVAL self.tokenLookup.put(TYPECODE(token), symCode);
    END;
    IF symCode = -1 THEN
      symCode := NARROW(token, ConstToken).val;
    END;
    RETURN TypedSymbol{code := symCode, value := token};
  END NextToken;

PROCEDURE AllocStack(): Stack =
  VAR
    a :=NEW(StackElemArray, 16);
  BEGIN
    a[0] := StackElem{state := 1, value := EOFSymbol};
    RETURN Stack{a := a, ptr := 0};
  END AllocStack;

PROCEDURE Push(VAR stack: Stack; elem: StackElem) =
  VAR
    new: StackElemArray;
  BEGIN
    INC(stack.ptr);
    IF stack.ptr > LAST(stack.a^) THEN
      new := NEW(StackElemArray, NUMBER(stack.a^) * 2);
      SUBARRAY(new^, 0, NUMBER(stack.a^)) := stack.a^;
      stack.a := new;
    END;
    stack.a[stack.ptr] := elem;
  END Push;

PROCEDURE ActionLookup(curState: INTEGER; symbol: TypedSymbol): INTEGER =
  VAR
    cur := curState;
    state: S;
    default := -1;
  BEGIN
    REPEAT
      state := States[cur];
      IF state.key = %symCodeLast THEN
        default := state.action;
      ELSIF state.key = symbol.code THEN
        RETURN state.action;
      END;
      cur := state.next;
    UNTIL cur = 0;
    RETURN default;
  END ActionLookup;

PROCEDURE Parse(self: T; exhaustInput: BOOLEAN := TRUE): StartType =
  VAR
    curState: INTEGER := 1;
    stack := AllocStack();
    action: INTEGER;
    symbol, preservedToken: TypedSymbol;
    skipTokenGets: INTEGER := 0;

  PROCEDURE DebugPrint(message: TEXT) = BEGIN
    IF Debug THEN Wr.PutText(stdout,"%yaccDEBUG: "&message&"\n");
     Wr.Flush(stdout);END;END DebugPrint;
  PROCEDURE DebugSymbol(message: TEXT) = VAR name: TEXT; BEGIN
   IF Debug THEN EVAL self.symbols.get(symbol.code, name);
    DebugPrint(message & " " & name & "(" &
      Fmt.Int(symbol.code) & ")"); END; END DebugSymbol;
  PROCEDURE DebugState(message: TEXT) = BEGIN IF Debug THEN
    DebugPrint(message & " " & Fmt.Int(curState));END;END DebugState;
  PROCEDURE DebugRule(message: TEXT) = BEGIN IF Debug THEN
    DebugPrint(message&" "&Rules[action].name);END;END DebugRule;

  BEGIN
    IF self.tokenLookup = NIL THEN Init(self); END;
    stack.a[0] := StackElem{state := curState, value := NotASymbol};
    DebugState("starting in state");
    LOOP
      IF skipTokenGets = 2 THEN
        skipTokenGets := 1;
        DebugSymbol("scanning reduced symbol");
      ELSIF skipTokenGets = 1 AND preservedToken # NoToken THEN
        skipTokenGets := 0;
        symbol := preservedToken;
        DebugSymbol("re-scanning input token");
      ELSE
        skipTokenGets := 0;
        symbol := NextToken(self);
        preservedToken := symbol;
        DebugSymbol("input token");
      END;
      action := ActionLookup(curState, symbol);
      IF action >= %lastReduce1 THEN
        DebugPrint("shifting anonymously");
        Push(stack, StackElem{state := 0, value := symbol});
        DEC(action, %DECaction);
        IF skipTokenGets = 0 THEN
          preservedToken := NoToken;
        END;
      END;
      IF action = -1 THEN
        DebugPrint("syntax error");
        self.lex.error("%yacc: syntax error");RETURN NIL;
      ELSIF action <= %lastShift0 THEN
        curState := action;
        DebugState("shifting to state");
        Push(stack, StackElem{state := curState, value := symbol});
      ELSIF action = %lastShift1 THEN
        DebugPrint("parsing stopped with singleton start symbol on stack");
        <* ASSERT stack.ptr = 1 *>
        IF exhaustInput AND preservedToken = NoToken THEN
          symbol := NextToken(self);
          DebugPrint("getting token to check that it's an EOF");
        END;
        IF symbol.code # 0 THEN
          IF exhaustInput THEN
            DebugPrint("Error: last token was not EOF");
            self.lex.unget();
            self.lex.error("%yacc: syntax error (parsing stopped before EOF)");
            RETURN NIL;
          END;
          IF preservedToken # NoToken THEN
            self.lex.unget();
            DebugPrint("ungetting last token");
          END;
        END;
        symbol := stack.a[1].value;
        DebugSymbol("returning symbol");
        RETURN symbol.value;
      ELSE
        DebugRule("reducing by rule");
        WITH p=stack.ptr, a=stack.a, v=symbol.value, l=Rules[action].length DO
          CASE action OF
%reduce\
          ELSE
            <* ASSERT FALSE *>
          END;
          FOR i := p - l + 1 TO p DO a[i].value.value.discard(); END;
          DEC(p, l);
          curState := a[p].state;
        END;
        DebugState("popping to state");
        symbol.code := Rules[action].returnCode;
        skipTokenGets := 2;
      END;
    END;
  END Parse; 

PROCEDURE Purge(self: T): INTEGER =
  BEGIN
    RETURN 0%purge;
  END Purge;

(* default methods *)
%defimpl\
BEGIN
END %yacc.
