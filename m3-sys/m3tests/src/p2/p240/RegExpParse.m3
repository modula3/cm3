(* from caltech-parser *)

MODULE RegExpParse;
IMPORT RegExpTok;

TYPE
  TypedSymbol = RECORD
    code: INTEGER;
    value: RegExpTok.ParseType;
  END;

  SymCode = BITS 9 FOR [0..262];
  Action = BITS 6 FOR [0..32];
  StateRef = BITS 5 FOR [0..28];

  S = RECORD
    key: SymCode;
    action: Action;
    next: StateRef;
  END;

CONST
  States = ARRAY [1..28] OF S {
    S{257,3,11}, S{257,8,11}, S{262,11,12}, S{257,9,11}, S{262,13,13},
    S{257,10,11}, S{262,13,14}, S{41,23,15}, S{262,14,16}, S{262,14,17},
    S{40,2,18}, S{124,6,17}, S{257,5,19}, S{257,7,19}, S{124,4,16},
    S{257,7,20}, S{257,5,20}, S{259,30,21}, S{42,27,22}, S{42,27,23},
    S{260,31,24}, S{43,26,25}, S{43,26,26}, S{261,32,0}, S{63,28,27},
    S{63,28,28}, S{258,29,0}, S{258,29,11}};

<*NOWARN*>PROCEDURE ActionLookup(curState: INTEGER; symbol: TypedSymbol): INTEGER =
  VAR
    cur := curState;
    state: S;
    default := -1;
  BEGIN
    REPEAT
      state := States[cur];
      IF state.key = 262 THEN
        default := state.action;
      ELSIF state.key = symbol.code THEN
        RETURN state.action;
      END;
      cur := state.next;
    UNTIL cur = 0;
    RETURN default;
  END ActionLookup;

BEGIN
END RegExpParse.
