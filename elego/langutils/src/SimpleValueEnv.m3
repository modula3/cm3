(*---------------------------------------------------------------------------*)
MODULE SimpleValueEnv;

IMPORT TextList, TextSeq, TextRefTbl, Pickle, TextRd, TextWr, Lex, Scan,
       FloatMode, Fmt, TextTextTbl, ASCII, TextExtras AS TextEx, Text, Rd, Wr,
       FileRd, FileWr, TextConv, OSError, Thread, RdExtras;
IMPORT MsgIF, MsgX, TextUtils, ProcessEnv, TextReadingUtils;

REVEAL
  T = Public BRANDED "SimpleValueEnv.T 0.0" OBJECT
    par   : T;
    env   : TextRefTbl.T;
    mu    : MUTEX;
    msgif : MsgIF.T;
  METHODS
    typeI(name : TEXT; rec := TRUE) : Type := GetTypeInternal;
    textValInternal(name : TEXT; rec := TRUE) : TEXT := TextValInternal;
    substTextValInternal(name : TEXT; rec := TRUE) : TEXT 
      := SubstTextValInternal;
    textValOrNilInternal(name : TEXT; rec := TRUE) : TEXT
      := TextValOrNilInternal;
  OVERRIDES
    init := Init;
    parent := Parent;
    copy := Copy;
    defined := Defined;
    type := GetType;
    intVal := IntVal;
    natVal := NatVal;
    refVal := RefVal;
    textVal := TextVal;
    listVal := ListVal;
    substTextVal := SubstTextVal;
    textValOrNil := TextValOrNil;
    substTextValOrNil := SubstTextValOrNil;
    seqVal := SeqVal;
    setIntVal := SetIntVal;
    setNatVal := SetNatVal;
    setRefVal := SetRefVal;
    setTextVal := SetTextVal;
    setListVal := SetListVal;
    setSeqVal := SetSeqVal;
    delVal := DelVal;
    setFromTextTextTbl := SetFromTextTextTbl;
    toTextTextTbl := ToTextTextTbl;
    toText := ToText;
    keys := Keys;
    keyList := KeyList;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE Init(self : T; parent : T := NIL; msgif : MsgIF.T := NIL) : T =
  BEGIN
    self.mu  := NEW(MUTEX);
    self.par := parent;
    self.env := NEW(TextRefTbl.Default).init();
    self.msgif := msgif;
    RETURN self;
  END Init;

(*---------------------------------------------------------------------------*)
PROCEDURE Parent(self : T) : T =
  BEGIN
    LOCK self.mu DO
      RETURN self.par;
    END;
  END Parent;

(*---------------------------------------------------------------------------*)
PROCEDURE Copy(self : T; env : T; deep := TRUE; rec := TRUE) =
  VAR
    iter : TextRefTbl.Iterator;
    name : TEXT;
    val  : REFANY;

    PROCEDURE CopyOne(env : T) =
      BEGIN
        iter := env.env.iterate();
        WHILE iter.next(name, val) DO
          IF deep THEN
            VAR
              rd : TextRd.T;
              wr : TextWr.T;
            BEGIN
              wr := TextWr.New();
              TRY
                Pickle.Write(wr, val);
                rd := TextRd.New(TextWr.ToText(wr));
                val := Pickle.Read(rd);
              EXCEPT ELSE
                MsgX.Fatal2(self.msgif, "SimpleValueEnv.Copy()",
                            "deep copy of " & name & " failed");
                RETURN;
              END;
            END;
          END;
          EVAL self.env.put(name, val);
        END;
      END CopyOne;

  BEGIN
    LOCK self.mu DO
      LOCK env.mu DO
        CopyOne(env);
        WHILE rec AND env.par # NIL DO
          env := env.par;
          CopyOne(env);
        END;
      END;
    END;
  END Copy;

(*---------------------------------------------------------------------------*)
PROCEDURE Defined(self : T; name : TEXT; rec := TRUE) : BOOLEAN =
  VAR
    val : REFANY;
    def : BOOLEAN;
  BEGIN
    LOCK self.mu DO
      def := self.env.get(name, val);
      IF def THEN
        RETURN TRUE;
      ELSIF rec AND self.par # NIL THEN
        RETURN self.par.defined(name, rec);
      ELSE
        RETURN FALSE;
      END;
    END;
  END Defined;

(*---------------------------------------------------------------------------*)
PROCEDURE GetTypeInternal(self : T; name : TEXT; rec := TRUE) : Type =
  VAR
    val : REFANY;
    def : BOOLEAN;
  BEGIN
    def := self.env.get(name, val);
    IF def THEN
      TYPECASE val OF
        IntObj => 
        IF NARROW(val, IntObj).val >= 0 THEN
          RETURN Type.Nat;
        ELSE
          RETURN Type.Int;
        END;
      | TEXT       => RETURN Type.Text;
      | TextSeq.T  => RETURN Type.Seq;
      | TextList.T => RETURN Type.List;
      ELSE
        RETURN Type.Ref;
      END;
    ELSIF rec AND self.par # NIL THEN
      RETURN self.par.type(name, rec);
    ELSE
      RETURN Type.None;
    END;
  END GetTypeInternal;

(*---------------------------------------------------------------------------*)
PROCEDURE GetType(self : T; name : TEXT; rec := TRUE) : Type =
  BEGIN
    LOCK self.mu DO
      RETURN GetTypeInternal(self, name, rec);
    END;
  END GetType;

(*---------------------------------------------------------------------------*)
PROCEDURE IntVal(self : T; name : TEXT; rec := TRUE) : INTEGER =
  VAR
    val : REFANY;
    def : BOOLEAN;
  BEGIN
    LOCK self.mu DO
      def := self.env.get(name, val);
      IF def THEN
        TYPECASE val OF
          IntObj     => RETURN NARROW(val, IntObj).val;
        | TEXT       => 
          TRY
            RETURN Scan.Int(NARROW(val, TEXT));
          EXCEPT
            Lex.Error,FloatMode.Trap =>
            MsgX.Fatal2(self.msgif, "SimpleValueEnv.IntVal",
                        "cannot convert value to Int: " & name);
            RETURN 0;
          END;
        | TextSeq.T  => RETURN NARROW(val, TextSeq.T).size();
        | TextList.T => RETURN TextList.Length(TextListVal(val));
        ELSE
          MsgX.Fatal2(self.msgif, "SimpleValueEnv.IntVal",
                      "cannot convert Ref to Int: " & name);
          RETURN 0;
        END;
      ELSIF rec AND self.par # NIL THEN
        RETURN self.par.intVal(name, rec);
      ELSE
        MsgX.Fatal2(self.msgif, "SimpleValueEnv.IntVal", <*NOWARN*>
                    "no value associated with " & name);
        RETURN 0;
      END;
    END;
  END IntVal;

(*---------------------------------------------------------------------------*)
PROCEDURE NatVal(self : T; name : TEXT; rec := TRUE) : CARDINAL =
  VAR
    val : REFANY;
    def : BOOLEAN;
  BEGIN
    LOCK self.mu DO
      def := self.env.get(name, val);
      IF def THEN
        TYPECASE val OF
          IntObj => 
          WITH v = NARROW(val, IntObj).val DO
            IF v >= 0 THEN
              RETURN v;
            ELSE
              MsgX.Fatal2(self.msgif, "SimpleValueEnv.NatVal",
                          "cannot convert value to Nat: " & name);
              RETURN 0;
            END;
          END;
        | TEXT       => 
          TRY
            WITH v = Scan.Int(NARROW(val, TEXT)) DO
              IF v >= 0 THEN
                RETURN v;
              ELSE
                MsgX.Fatal2(self.msgif, "SimpleValueEnv.NatVal",
                            "cannot convert value to Nat: " & name);
                RETURN 0;
              END;
            END;
          EXCEPT
            Lex.Error,FloatMode.Trap =>
            MsgX.Fatal2(self.msgif, "SimpleValueEnv.NatVal",
                        "cannot convert value to Nat: " & name);
            RETURN 0;
          END;
        | TextSeq.T  => RETURN NARROW(val, TextSeq.T).size();
        | TextList.T => RETURN TextList.Length(TextListVal(val));
        ELSE
          MsgX.Fatal2(self.msgif, "SimpleValueEnv.NatVal",
                      "cannot convert Ref to Nat: " & name);
          RETURN 0;
        END;
      ELSIF rec AND self.par # NIL THEN
        RETURN self.par.natVal(name, rec);
      ELSE
        MsgX.Fatal2(self.msgif, "SimpleValueEnv.NatVal", <*NOWARN*>
                    "no value associated with " & name);
        RETURN 0;
      END;
    END;
  END NatVal;

(*---------------------------------------------------------------------------*)
PROCEDURE RefVal(self : T; name : TEXT; rec := TRUE) : REFANY =
  VAR
    val : REFANY;
    def : BOOLEAN;
  BEGIN
    LOCK self.mu DO
      def := self.env.get(name, val);
      IF def THEN
        RETURN val;
      ELSIF rec AND self.par # NIL THEN
        RETURN self.par.refVal(name, rec);
      ELSE
        MsgX.Fatal2(self.msgif, "SimpleValueEnv.RefVal", <*NOWARN*>
                    "no value associated with " & name);
        RETURN NIL;
      END;
    END;
  END RefVal;

(*---------------------------------------------------------------------------*)
PROCEDURE TextValInternal(self : T; name : TEXT; rec := TRUE) : TEXT =
  VAR
    val : REFANY;
    def : BOOLEAN;
  BEGIN
    def := self.env.get(name, val);
    IF def THEN
      TYPECASE val OF
        IntObj => 
        WITH v = NARROW(val, IntObj).val DO
          RETURN Fmt.Int(v);
        END;
      | TEXT       => RETURN NARROW(val, TEXT);
      | TextSeq.T  => 
        RETURN TextUtils.TextSeqToText(NARROW(val, TextSeq.T));
      | TextList.T => 
        RETURN TextUtils.TextSeqToText(ListToSeq(TextListVal(val)));
      ELSE
        MsgX.Fatal2(self.msgif, "SimpleValueEnv.TextVal", <*NOWARN*>
                    "cannot convert Ref to Text: " & name);
        RETURN NIL;
      END;
    ELSIF rec AND self.par # NIL THEN
      RETURN self.par.textVal(name, rec);
    ELSE
      MsgX.Fatal2(self.msgif, "SimpleValueEnv.TextVal", <*NOWARN*>
                  "no value associated with " & name);
      RETURN NIL;
    END;
  END TextValInternal;

(*---------------------------------------------------------------------------*)
PROCEDURE TextVal(self : T; name : TEXT; rec := TRUE) : TEXT =
  BEGIN
    LOCK self.mu DO
      RETURN TextValInternal(self, name, rec);
    END;
  END TextVal;

(*---------------------------------------------------------------------------*)
PROCEDURE TextValOrNilInternal(self : T; name : TEXT; rec := TRUE) : TEXT =
  VAR
    val : REFANY;
    def : BOOLEAN;
  BEGIN
    def := self.env.get(name, val);
    IF def THEN
      TYPECASE val OF
        IntObj => 
        WITH v = NARROW(val, IntObj).val DO
          RETURN Fmt.Int(v);
        END;
      | TEXT       => RETURN NARROW(val, TEXT);
      | TextSeq.T  => 
        RETURN TextUtils.TextSeqToText(NARROW(val, TextSeq.T));
      | TextList.T => 
        RETURN TextUtils.TextSeqToText(ListToSeq(TextListVal(val)));
      ELSE
        RETURN NIL;
      END;
    ELSIF rec AND self.par # NIL THEN
      RETURN self.par.textValOrNil(name, rec);
    ELSE
      RETURN NIL;
    END;
  END TextValOrNilInternal;

(*---------------------------------------------------------------------------*)
PROCEDURE TextValOrNil(self : T; name : TEXT; rec := TRUE) : TEXT =
  BEGIN
    LOCK self.mu DO
      RETURN TextValOrNilInternal(self, name, rec);
    END;
  END TextValOrNil;

(*---------------------------------------------------------------------------*)
PROCEDURE SubstEverythingInternal(self : T; t : TEXT) : TEXT =
  VAR res := t;
  BEGIN
    (* obsolete
    IF GetTypeInternal(self, "HOME") = Type.Text THEN
      res := TextUtils.Substitute(res, "{HOME}",
                                  TextValInternal(self, "HOME"));
    END;
    IF GetTypeInternal(self, "USER") = Type.Text THEN
      res := TextUtils.Substitute(res, "{USER}",
                                  TextValInternal(self, "USER"));
    END;
    *)
    res := TextUtils.SubstEnvVars(res, penv);
    (* all environment variables have been substituted in val *)
    TRY
      res := SubstituteVariablesInternal(res, self);
    EXCEPT
      Error(e) => MsgX.Error(self.msgif, "error in variable definition: " & e);
    END;
    (* all internal environement variables have been substituted in val *)
    RETURN res;
  END SubstEverythingInternal;

(*---------------------------------------------------------------------------*)
PROCEDURE SubstEverything(self : T; t : TEXT) : TEXT =
  VAR res := t;
  BEGIN
    (* obsolete
    IF self.type("HOME") = Type.Text THEN
      res := TextUtils.Substitute(res, "{HOME}", self.textVal("HOME"));
    END;
    IF self.type("USER") = Type.Text THEN
      res := TextUtils.Substitute(res, "{USER}", self.textVal("USER"));
    END;
    *)
    res := TextUtils.SubstEnvVars(res, penv);
    (* all environment variables have been substituted in val *)
    TRY
      res := SubstituteVariables(res, self);
    EXCEPT
      Error(e) => MsgX.Error(self.msgif, "error in variable definition: " & e);
    END;
    (* all internal environement variables have been substituted in val *)
    RETURN res;
  END SubstEverything;

(*---------------------------------------------------------------------------*)
PROCEDURE SubstTextVal(self : T; name : TEXT; rec := TRUE) : TEXT =
  VAR
    res : TEXT;
  BEGIN
    res := self.textVal(name, rec);
    RETURN SubstEverything(self, res);
  END SubstTextVal;

(*---------------------------------------------------------------------------*)
PROCEDURE SubstTextValInternal(self : T; name : TEXT; rec := TRUE) : TEXT =
  VAR
    res : TEXT;
  BEGIN
    res := self.textValInternal(name, rec);
    RETURN SubstEverythingInternal(self, res);
  END SubstTextValInternal;

(*---------------------------------------------------------------------------*)
PROCEDURE SubstTextValOrNil(self : T; name : TEXT; rec := TRUE) : TEXT =
  VAR
    res : TEXT;
  BEGIN
    res := self.textValOrNil(name, rec);
    IF res = NIL THEN RETURN NIL END;
    RETURN SubstEverything(self, res);
  END SubstTextValOrNil;

(*---------------------------------------------------------------------------*)
PROCEDURE ListVal(self : T; name : TEXT; rec := TRUE) : TextList.T =
  VAR
    val : REFANY;
    def : BOOLEAN;
  BEGIN
    LOCK self.mu DO
      def := self.env.get(name, val);
      IF def THEN
        TYPECASE val OF
          IntObj => 
          WITH v = Fmt.Int(NARROW(val, IntObj).val) DO
            RETURN TextList.List1(v);
          END;
        | TEXT       => RETURN TextList.List1(NARROW(val, TEXT));
        | TextSeq.T  => RETURN SeqToList(NARROW(val, TextSeq.T));
        | TextList.T => RETURN TextListVal(val);
        ELSE
          MsgX.Fatal2(self.msgif, "SimpleValueEnv.ListVal", <*NOWARN*>
                      "cannot convert Ref to List: " & name);
          RETURN NIL;
        END;
      ELSIF rec AND self.par # NIL THEN
        RETURN self.par.listVal(name, rec);
      ELSE
        MsgX.Fatal2(self.msgif, "SimpleValueEnv.ListVal", <*NOWARN*>
                    "no value associated with " & name);
        RETURN NIL;
      END;
    END;
  END ListVal;

(*---------------------------------------------------------------------------*)
PROCEDURE SeqVal(self : T; name : TEXT; rec := TRUE) : TextSeq.T =
  VAR
    val : REFANY;
    def : BOOLEAN;
  BEGIN
    LOCK self.mu DO
      def := self.env.get(name, val);
      IF def THEN
        TYPECASE val OF
          IntObj => 
          WITH v = Fmt.Int(NARROW(val, IntObj).val) DO
            RETURN TextSeq1(v);
          END;
        | TEXT       => RETURN TextSeq1(NARROW(val, TEXT));
        | TextSeq.T  => RETURN NARROW(val, TextSeq.T);
        | TextList.T => RETURN ListToSeq(TextListVal(val));
        ELSE
          MsgX.Fatal2(self.msgif, "SimpleValueEnv.SeqVal", <*NOWARN*>
                      "cannot convert Ref to Seq: " & name);
          RETURN NIL;
        END;
      ELSIF rec AND self.par # NIL THEN
        RETURN self.par.seqVal(name, rec);
      ELSE
        MsgX.Fatal2(self.msgif, "SimpleValueEnv.SeqVal", <*NOWARN*>
                    "no value associated with " & name);
        RETURN NIL;
      END;
    END
  END SeqVal;

(*---------------------------------------------------------------------------*)
PROCEDURE SetIntVal(self : T; name : TEXT; val : INTEGER) =
  VAR
    v   : REFANY;
    def : BOOLEAN;
  BEGIN
    LOCK self.mu DO
      def := self.env.get(name, v);
      IF def AND self.typeI(name, FALSE) = Type.Int OR 
        self.typeI(name, FALSE) = Type.Nat THEN
        NARROW(v, IntObj).val := val;
      ELSE
        EVAL self.env.put(name, NEW(IntObj).init(val));
      END;
    END;
  END SetIntVal;

(*---------------------------------------------------------------------------*)
PROCEDURE SetNatVal(self : T; name : TEXT; val : CARDINAL) =
  VAR
    v   : REFANY;
    def : BOOLEAN;
  BEGIN
    LOCK self.mu DO
      def := self.env.get(name, v);
      IF def AND self.typeI(name, FALSE) = Type.Int OR 
        self.typeI(name, FALSE) = Type.Nat THEN
        NARROW(v, IntObj).val := val;
      ELSE
        EVAL self.env.put(name, NEW(IntObj).init(val));
      END;
    END;
  END SetNatVal;

(*---------------------------------------------------------------------------*)
PROCEDURE SetRefVal(self : T; name : TEXT; val : REFANY) =
  BEGIN
    LOCK self.mu DO
      EVAL self.env.put(name, val);
    END;
  END SetRefVal;

(*---------------------------------------------------------------------------*)
PROCEDURE SetTextVal(self : T; name : TEXT; val : TEXT) =
  BEGIN
    LOCK self.mu DO
      EVAL self.env.put(name, val);
    END;
  END SetTextVal;

(*---------------------------------------------------------------------------*)
PROCEDURE SetListVal(self : T; name : TEXT; val : TextList.T) =
  BEGIN
    LOCK self.mu DO
      IF val = NIL THEN
        EVAL self.env.put(name, nil);
        (* we must not put NIL into a TextRefTbl.T *)
      ELSE
        EVAL self.env.put(name, val);
      END;
    END;
  END SetListVal;

(*---------------------------------------------------------------------------*)
PROCEDURE SetSeqVal(self : T; name : TEXT; val : TextSeq.T) =
  BEGIN
    LOCK self.mu DO
      EVAL self.env.put(name, val);
    END;
  END SetSeqVal;

(*---------------------------------------------------------------------------*)
PROCEDURE DelVal(self : T; name : TEXT) =
  VAR val : REFANY;
  BEGIN
    LOCK self.mu DO
      EVAL self.env.delete(name, val);
    END;
  END DelVal;

(*---------------------------------------------------------------------------*)
PROCEDURE ToTextTextTbl(self : T; plain := TRUE; rec := TRUE) : TextTextTbl.T =
  VAR
    res   :  TextTextTbl.Default;
    name  :  TEXT;
    value :  TEXT;
    ref   :  REFANY;
    iter  :  TextRefTbl.Iterator;
  BEGIN
    LOCK self.mu DO
      IF rec AND self.par # NIL THEN
        res := self.par.toTextTextTbl(plain, rec);
      ELSE
        res   := NEW(TextTextTbl.Default).init();
      END;
      iter := self.env.iterate();
      WHILE iter.next(name, ref) DO
        value := self.textValOrNilInternal(name, rec);
        IF plain THEN
          value := SubstEverythingInternal(self, value);
        END;
        EVAL res.put(name, value);
      END;
    END;
    RETURN res;
  END ToTextTextTbl;

(*---------------------------------------------------------------------------*)
PROCEDURE SetFromTextTextTbl(self : T; tbl : TextTextTbl.T) : T =
  VAR
    name, value : TEXT;
    iter := tbl.iterate();
  BEGIN
    WHILE iter.next(name, value) DO
      self.setTextVal(name, value);
    END;
    RETURN self;
  END SetFromTextTextTbl;

(*---------------------------------------------------------------------------*)
PROCEDURE IntObjInit(self : IntObj; v : INTEGER) : IntObj =
  BEGIN
    self.val := v;
    RETURN self;
  END IntObjInit;

(*---------------------------------------------------------------------------*)
PROCEDURE TextSeq1(t : TEXT) : TextSeq.T =
  VAR
    res := NEW(TextSeq.T).init(1);
  BEGIN
    res.addhi(t);
    RETURN res;
  END TextSeq1;

(*---------------------------------------------------------------------------*)
PROCEDURE ListToSeq(l : TextList.T) : TextSeq.T =
  VAR 
    res := NEW(TextSeq.T).init(TextList.Length(l));
    act := l;
  BEGIN
    WHILE act # NIL DO
      res.addhi(act.head);
      act := act.tail;
    END;
    RETURN res;
  END ListToSeq;

(*---------------------------------------------------------------------------*)
PROCEDURE SeqToList(s : TextSeq.T) : TextList.T =
  VAR
    res : TextList.T := NIL;
  BEGIN
    FOR i := s.size() - 1 TO 0 DO
      WITH elem = s.get(i) DO
        res := TextList.Cons(elem, res);
      END;
    END;
    RETURN res;
  END SeqToList;

(*---------------------------------------------------------------------------*)
PROCEDURE SubstituteVariables(t : TEXT; parameters : T) : TEXT
  RAISES {Error} =
  VAR
    i : CARDINAL := 0;
    j, k, l, m : CARDINAL;
    pre, suf, name, val : TEXT;
    vchars := ASCII.Set{':', '?', '!'};
    c : CHAR;
    defVar, defConst : BOOLEAN;
    defaultValue, defaultVarName, expr : TEXT;
  BEGIN
    WHILE TextEx.FindChar(t, '{', i) DO
      j := i;
      IF TextEx.FindCharSet(t, vchars, j) AND (j = i + 1) THEN
        (* found {: or {! or {? *)
        c := Text.GetChar(t, j);
        INC(j);
        k := j;
        IF TextEx.FindChar(t, '}', k) THEN
          pre  := Text.Sub(t, 0, i);
          name := Text.Sub(t, j, k - j);
          (* check for default values, either 
             {:varname?varname},
             {:varname:const}, or
             {:varname?varname:const}
          *)
          defaultValue := NIL; l:= 0 ; m := 0;
          defVar := TextEx.FindChar(name, '?', l);
          defConst := TextEx.FindChar(name, ':', m);
          IF defVar AND defConst THEN
            IF l < m THEN
              expr := name;
              name := Text.Sub(expr, 0, l);
              defaultVarName := Text.Sub(expr, l + 1, m - l -1);
              IF parameters # NIL THEN
                defaultValue := parameters.textValOrNil(defaultVarName);
                IF defaultValue = NIL THEN
                  defaultValue := Text.Sub(expr, m + 1);
                ELSE
                  defaultValue := 
                      SubstituteVariables(defaultValue, parameters);
                END;
              ELSE
                defaultValue := Text.Sub(expr, m + 1);
              END;
            ELSE
              RAISE Error("invalid default value syntax: " & expr);
            END;
          ELSIF defVar THEN
            expr := name;
            name := Text.Sub(expr, 0, l);
            defaultVarName := Text.Sub(expr, l + 1);
            IF parameters # NIL THEN
              defaultValue := parameters.textValOrNil(defaultVarName);
              IF defaultValue # NIL THEN
                defaultValue := SubstituteVariables(defaultValue, parameters);
              END;
            ELSE
              defaultValue := NIL;
            END;
          ELSIF defConst THEN
            expr := name;
            name := Text.Sub(expr, 0, m);
            defaultValue := Text.Sub(expr, m + 1);
          END;
          (* If there is a default value, it is now contained in defaultValue, 
             and name is adapted appropriately. *)
          suf  := Text.Sub(t, k + 1, LAST(CARDINAL));
          IF parameters # NIL THEN
            IF (
              parameters.type(name) = Type.Int OR
              parameters.type(name) = Type.Nat OR
              parameters.type(name) = Type.Text OR
              parameters.type(name) = Type.List OR
              parameters.type(name) = Type.Seq) THEN
              val := parameters.substTextVal(name);
              IF c = '!' THEN
                IF val = NIL OR Text.Empty(val) THEN
                  val := defaultValue;
                  IF val = NIL OR Text.Empty(val) THEN
                    RAISE Error("mandatory variable " & name & " is empty");
                  END;
                END;
              END;
              pre := pre & val;
            ELSE (* no value found for `name' *)
              IF defaultValue = NIL THEN
                IF c # '?' THEN
                  RAISE Error("mandatory variable " & name & " undefined");
                END;
              ELSE
                pre := pre & defaultValue;
              END;
            END;
          ELSE
            (* no values at all *)
            IF c # '?' THEN
              RAISE Error("mandatory variable " & name & " undefined");
            END;
          END;
          t := pre & suf;
          i := Text.Length(pre);
        ELSE
          (* no matching '}' found *)
          RAISE Error("syntax error: no matching } in `" & t & "'");
        END;
      ELSE
        (* no valid begin found *)
        INC(i);
      END;
    END;
    RETURN t;
  END SubstituteVariables;

(*---------------------------------------------------------------------------*)
PROCEDURE SubstituteVariablesInternal(t : TEXT; parameters : T) : TEXT
  RAISES {Error} =
  VAR
    i : CARDINAL := 0;
    j, k, l, m : CARDINAL;
    pre, suf, name, val : TEXT;
    vchars := ASCII.Set{':', '?', '!'};
    c : CHAR;
    defVar, defConst : BOOLEAN;
    defaultValue, defaultVarName, expr : TEXT;
  BEGIN
    WHILE TextEx.FindChar(t, '{', i) DO
      j := i;
      IF TextEx.FindCharSet(t, vchars, j) AND (j = i + 1) THEN
        (* found {: or {! or {? *)
        c := Text.GetChar(t, j);
        INC(j);
        k := j;
        IF TextEx.FindChar(t, '}', k) THEN
          pre  := Text.Sub(t, 0, i);
          name := Text.Sub(t, j, k - j);
          (* check for default values, either 
             {:varname?varname},
             {:varname:const}, or
             {:varname?varname:const}
          *)
          defaultValue := NIL; l:= 0 ; m := 0;
          defVar := TextEx.FindChar(name, '?', l);
          defConst := TextEx.FindChar(name, ':', m);
          IF defVar AND defConst THEN
            IF l < m THEN
              expr := name;
              name := Text.Sub(expr, 0, l);
              defaultVarName := Text.Sub(expr, l + 1, m - l -1);
              IF parameters # NIL THEN
                defaultValue := 
                    parameters.textValOrNilInternal(defaultVarName);
                IF defaultValue = NIL THEN
                  defaultValue := Text.Sub(expr, m + 1);
                ELSE
                  defaultValue := 
                      SubstituteVariablesInternal(defaultValue, parameters);
                END;
              ELSE
                defaultValue := Text.Sub(expr, m + 1);
              END;
            ELSE
              RAISE Error("invalid default value syntax: " & expr);
            END;
          ELSIF defVar THEN
            expr := name;
            name := Text.Sub(expr, 0, l);
            defaultVarName := Text.Sub(expr, l + 1);
            IF parameters # NIL THEN
              defaultValue := parameters.textValOrNilInternal(defaultVarName);
              IF defaultValue # NIL THEN
                defaultValue := 
                    SubstituteVariablesInternal(defaultValue, parameters);
              END;
            ELSE
              defaultValue := NIL;
            END;
          ELSIF defConst THEN
            expr := name;
            name := Text.Sub(expr, 0, m);
            defaultValue := Text.Sub(expr, m + 1);
          END;
          (* If there is a default value, it is now contained in defaultValue, 
             and name is adapted appropriately. *)
          suf  := Text.Sub(t, k + 1, LAST(CARDINAL));
          IF parameters # NIL THEN
            IF (
              parameters.typeI(name) = Type.Int OR
              parameters.typeI(name) = Type.Nat OR
              parameters.typeI(name) = Type.Text OR
              parameters.typeI(name) = Type.List OR
              parameters.typeI(name) = Type.Seq) THEN
              val := parameters.substTextValInternal(name);
              IF c = '!' THEN
                IF val = NIL OR Text.Empty(val) THEN
                  val := defaultValue;
                  IF val = NIL OR Text.Empty(val) THEN
                    RAISE Error("mandatory variable " & name & " is empty");
                  END;
                END;
              END;
              pre := pre & val;
            ELSE (* no value found for `name' *)
              IF defaultValue = NIL THEN
                IF c # '?' THEN
                  RAISE Error("mandatory variable " & name & " undefined");
                END;
              ELSE
                pre := pre & defaultValue;
              END;
            END;
          ELSE
            (* no values at all *)
            IF c # '?' THEN
              RAISE Error("mandatory variable " & name & " undefined");
            END;
          END;
          t := pre & suf;
          i := Text.Length(pre);
        ELSE
          (* no matching '}' found *)
          RAISE Error("syntax error: no matching } in `" & t & "'");
        END;
      ELSE
        (* no valid begin found *)
        INC(i);
      END;
    END;
    RETURN t;
  END SubstituteVariablesInternal;

(*---------------------------------------------------------------------------*)
PROCEDURE TextListVal(val : REFANY) : TextList.T =
  BEGIN
    IF val = nil THEN
      RETURN NIL;
    ELSE
      RETURN NARROW(val, TextList.T);
    END;
  END TextListVal;

(*---------------------------------------------------------------------------*)
PROCEDURE ToText(self : T; rec := TRUE; plain := TRUE) : TEXT =
  VAR
    res   := "";
    name  :  TEXT;
    value :  TEXT;
    typet :  TEXT;
    ref   :  REFANY;
    iter  :  TextRefTbl.Iterator;
    t     :  Type;
  BEGIN
    LOCK self.mu DO
      iter := self.env.iterate();
      WHILE iter.next(name, ref) DO
        value := self.textValOrNilInternal(name, FALSE);
        IF plain THEN
          value := SubstEverythingInternal(self, value);
        END;
        t := self.typeI(name);
        CASE t OF
          Type.Nat => typet := "Nat";
        | Type.Int => typet := "Int";
        | Type.Ref => typet := "Ref";
        | Type.Seq => typet := "Seq";
        | Type.Text => typet := "Text";
        | Type.List => typet := "List";
        | Type.None => typet := "None";
        ELSE
          typet := "Unknown";
        END;
        res := res & Fmt.F("%-35s = %-35s\n", name & "[" & typet & "]", value);
      END;
      IF rec AND self.par # NIL THEN
        res := res & "----\n" & self.par.toText(rec, plain);
      END;
    END;
    RETURN res;
  END ToText;

(*---------------------------------------------------------------------------*)
PROCEDURE Keys(self : T; rec := TRUE) : TextSeq.T =
  VAR
    res   := NEW(TextSeq.T).init();
    name  :  TEXT;
    ref   :  REFANY;
    iter  :  TextRefTbl.Iterator;
  BEGIN
    LOCK self.mu DO
      iter := self.env.iterate();
      WHILE iter.next(name, ref) DO
        res.addhi(name);
      END;
      IF rec AND self.par # NIL THEN
        res := TextSeq.Cat(res, self.par.keys(rec));
      END;
    END;
    RETURN res;
  END Keys; 

(*---------------------------------------------------------------------------*)
PROCEDURE KeyList(self : T; rec := TRUE) : TextList.T =
  VAR
    res   : TextList.T := NIL;
    name  : TEXT;
    ref   : REFANY;
    iter  : TextRefTbl.Iterator;
  BEGIN
    LOCK self.mu DO
      iter := self.env.iterate();
      WHILE iter.next(name, ref) DO
        res := TextList.Cons(name, res);
      END;
      IF rec AND self.par # NIL THEN
        res := TextList.AppendD(res, KeyList(self.par, rec));
      END;
    END;
    RETURN res;
  END KeyList;

(*---------------------------------------------------------------------------*)
CONST 
  KeywordChars     = SET OF CHAR {'A'..'Z', 'a'..'z', '0'..'9', '-', '_'};
  KeywordAsciis    = ASCII.AlphaNumerics + ASCII.Set{'-', '_'};
  NonKeywordAsciis = ASCII.All - KeywordAsciis;
  TypeBeginChar    = '[';
  CommentChar      = '#';
  EnvSeparator     = "---end-of-environment---";
  AllChars         = SET OF CHAR{'\000'..'\377'};
  NonNewLineChars  = AllChars - SET OF CHAR{'-'};

(*---------------------------------------------------------------------------*)
PROCEDURE LookAhead(rd : Rd.T) : CHAR RAISES {E} =
  VAR c : CHAR;
  BEGIN
    TRY
      Lex.Skip(rd);
      c := Rd.GetChar(rd);
      Rd.UnGetChar(rd);
    EXCEPT
      Rd.Failure => RAISE E("error reading look-ahead char");
    | Rd.EndOfFile => RAISE E("error: no look-ahead");
    | Thread.Alerted => RAISE E("reading interrupted");
    END;
    RETURN c;
  END LookAhead;

(*---------------------------------------------------------------------------*)
PROCEDURE TextListRepr(l : TextList.T) : TEXT =
  VAR
    one : TEXT;
    res : TEXT := "";
  BEGIN
    IF l = NIL THEN
      RETURN "NIL";
    END;
    WHILE l # NIL DO
      IF Text.Empty(res) THEN
        IF l.head = NIL THEN
          one := TextConv.Encode("", quoted := TRUE);
        ELSE
          one := TextConv.Encode(l.head, quoted := TRUE);
        END;
      ELSE 
        IF l.head = NIL THEN
          one := ", " & TextConv.Encode("", quoted := TRUE);
        ELSE
          one := ", " & TextConv.Encode(l.head, quoted := TRUE);
        END;
      END;
      res := res & one;
      l := l.tail;
    END;
    RETURN res;
  END TextListRepr;

(*---------------------------------------------------------------------------*)
PROCEDURE TextSeqRepr(l : TextSeq.T) : TEXT =
  VAR
    elem : TEXT;
    one  : TEXT;
    res  : TEXT := "";
  BEGIN
    IF l.size() = 0 THEN
      RETURN "EMPTY";
    END;
    FOR i := 0 TO l.size() - 1 DO
      elem := l.get(i);
      IF Text.Empty(res) THEN
        IF elem = NIL THEN
          one := TextConv.Encode("", quoted := TRUE);
        ELSE
          one := TextConv.Encode(elem, quoted := TRUE);
        END;
      ELSE 
        IF elem = NIL THEN
          one := ", " & TextConv.Encode("", quoted := TRUE);
        ELSE
          one := ", " & TextConv.Encode(elem, quoted := TRUE);
        END;
      END;
      res := res & one;
    END;
    RETURN res;
  END TextSeqRepr;

(*---------------------------------------------------------------------------*)
PROCEDURE ReadString(rd : Rd.T) : TEXT 
  RAISES {Rd.Failure, Rd.EndOfFile, Thread.Alerted} =
  VAR
    t : TEXT;
  BEGIN
    t := TextReadingUtils.GetString(rd);
    WHILE NOT Text.Empty(t) AND Text.GetChar(t, Text.Length(t) - 1) = '\\' DO
      t := t & "\"" & RdExtras.GetText(rd, ASCII.Set{}, ASCII.Set{'\"'}, 
                                       FALSE);
    END;
    RETURN t;
  END ReadString;

(*---------------------------------------------------------------------------*)
PROCEDURE ReadTextList(rd : Rd.T) : TextList.T RAISES {E} =
  VAR
    c : CHAR;
    t : TEXT;
    l : TextList.T := NIL;
  BEGIN
    TRY
      c := LookAhead(rd);
      IF c = '\"' THEN
        t := TextConv.Decode(ReadString(rd), quoted := FALSE);
      ELSE
        t := Lex.Scan(rd, KeywordChars);
        IF Text.Equal(t, "NIL") THEN
          RETURN NIL;
        ELSE
          RAISE E("malformed list value");
        END;
      END;
      l := TextList.List1(t);
      c := LookAhead(rd);
      WHILE c = ',' DO
        t := TextConv.Decode(ReadString(rd), quoted := FALSE);
        l := TextList.AppendD(l, TextList.List1(t));
        c := LookAhead(rd);
      END;
    EXCEPT
      Rd.Failure => RAISE E("error reading list value");
    | Rd.EndOfFile => RAISE E("error: incomplete list value");
    | Thread.Alerted => RAISE E("reading interrupted");
    | TextConv.Fail => RAISE E("cannot decode environment: conversion error");
    END;
    RETURN l;
  END ReadTextList;

(*---------------------------------------------------------------------------*)
PROCEDURE ReadTextSeq(rd : Rd.T) : TextSeq.T RAISES {E} =
  VAR
    c :  CHAR;
    t :  TEXT;
    l := NEW(TextSeq.T).init();
  BEGIN
    TRY
      c := LookAhead(rd);
      IF c = '\"' THEN
        t := TextConv.Decode(ReadString(rd), quoted := FALSE);
      ELSE
        t := Lex.Scan(rd, KeywordChars);
        IF Text.Equal(t, "EMPTY") THEN
          RETURN l;
        ELSE
          RAISE E("malformed sequence value");
        END;
      END;
      l.addhi(t);
      c := LookAhead(rd);
      WHILE c = ',' DO
        t := TextConv.Decode(ReadString(rd), quoted := FALSE);
        l.addhi(t);
        c := LookAhead(rd);
      END;
    EXCEPT
      Rd.Failure => RAISE E("error reading sequence value");
    | Rd.EndOfFile => RAISE E("error: incomplete sequence value");
    | Thread.Alerted => RAISE E("reading interrupted");
    | TextConv.Fail => RAISE E("cannot decode environment: conversion error");
    END;
    RETURN l;
  END ReadTextSeq;

(*---------------------------------------------------------------------------*)
PROCEDURE Write(wr : Wr.T; env : T; keys : TextList.T := NIL; rec := TRUE)
  RAISES {E} =
  VAR
    name  :  TEXT;
    ref   :  REFANY;
    type  :  Type;
    val   :  TEXT;
    tt    :  TEXT;
    skip  :  BOOLEAN;
  BEGIN
    IF keys = NIL THEN
      keys := KeyList(env, rec);
    END;
    LOCK env.mu DO
      WHILE keys # NIL DO
        skip := FALSE;
        name := keys.head;
        type := env.typeI(name, rec);
        IF type = Type.Int THEN
          tt := "[int]";
          val := TextConv.Encode(env.textValInternal(name));
        ELSIF type = Type.Nat THEN
          tt := "[nat]";
          val := TextConv.Encode(env.textValInternal(name), quoted := FALSE);
        ELSIF type = Type.Text THEN
          tt := "[text]";
          val := TextConv.Encode(env.textValInternal(name), quoted := TRUE);
        ELSIF type = Type.List THEN
          tt := "[list]";
          IF env.env.get(name, ref) THEN
            val := TextListRepr(NARROW(ref, TextList.T));
          ELSE
            val := "";
          END;
        ELSIF type = Type.Seq THEN
          tt := "[seq]";
          IF env.env.get(name, ref) THEN
            val := TextSeqRepr(NARROW(ref, TextSeq.T));
          ELSE
            val := "";
          END;
        ELSIF type = Type.Ref THEN
          (* tt := TypeBeginChar & "ref" & TypeEndChar; *)
          RAISE E("arbitrary reference types are not yet implemented");
        ELSE
          skip := TRUE;
        END;
        IF NOT skip THEN
          TRY
            Wr.PutText(wr, name & tt & "\t" & val & "\n");
          EXCEPT
            Wr.Failure => RAISE E("write error");
          | Thread.Alerted => RAISE E("interrupted writing environment");
          END;
        END;
        keys := keys.tail;
      END;
    END;
    TRY
      Wr.PutText(wr, EnvSeparator & "\n");
    EXCEPT
      Wr.Failure => RAISE E("write error");
    | Thread.Alerted => RAISE E("interrupted writing environment");
    END;
  END Write;

(*---------------------------------------------------------------------------*)
PROCEDURE Read(rd : Rd.T; VAR env : T) RAISES {E} =
  VAR
    c        : CHAR;
    t, n, vt : TEXT;
    vl       : TextList.T;
    vs       : TextSeq.T;
    vi       : INTEGER;
    vn       : CARDINAL;
  BEGIN
    TRY
      WHILE NOT Rd.EOF(rd) DO
        c := LookAhead(rd);
        IF c = CommentChar THEN
          Lex.Skip(rd, NonNewLineChars);
        ELSE
          n := TextReadingUtils.GetTokenOrString(
                   rd, terminate := NonKeywordAsciis);
          IF Text.Equal(n, EnvSeparator) THEN
            Lex.Skip(rd);
            EXIT;
          END;
          c := LookAhead(rd);
          IF c = TypeBeginChar THEN
            t := TextReadingUtils.GetToken(rd);
          ELSE
            t := "[text]";
          END;
          IF Text.Equal(t, "[text]") THEN
            vt := TextConv.Decode(ReadString(rd), quoted := FALSE);
            env.setTextVal(n, vt);
          ELSIF Text.Equal(t, "[int]") THEN
            vi := Lex.Int(rd);
            env.setIntVal(n, vi);
          ELSIF Text.Equal(t, "[nat]") THEN
            vn := Lex.Int(rd);
            env.setIntVal(n, vn);
          ELSIF Text.Equal(t, "[list]") THEN
            vl := ReadTextList(rd);
            env.setListVal(n, vl);
          ELSIF Text.Equal(t, "[seq]") THEN
            vs := ReadTextSeq(rd);
            env.setSeqVal(n, vs);
          ELSE
            RAISE E("unknown type in environment");
          END;            
        END;
      END;
    EXCEPT
      Lex.Error => RAISE E("lex error reading environment " & n);
    | Rd.Failure => RAISE E("error reading environment");
    | Rd.EndOfFile => RAISE E("error: incomplete environment");
    | TextConv.Fail => RAISE E("cannot decode environment: conversion error "
      & n);
    | Thread.Alerted => RAISE E("reading of environment interrupted");
    | FloatMode.Trap => RAISE E("float mode trap while reading environment " 
      & n);
    END;
  END Read;

(*---------------------------------------------------------------------------*)
PROCEDURE WriteFile(fn : TEXT; env : T; keys : TextList.T := NIL; rec := TRUE)
  RAISES {E} =
  VAR
    wr : Wr.T;
  BEGIN
    TRY
      wr := FileWr.Open(fn);
      Write(wr, env, keys, rec);
      Wr.Close(wr);
    EXCEPT
      Wr.Failure => RAISE E("cannot write environment");
    | OSError.E  => RAISE E("cannot create or open environment file");
    | Thread.Alerted => RAISE E("writing of environment interrupted");
    END;
  END WriteFile;

(*---------------------------------------------------------------------------*)
PROCEDURE ReadFile(fn : TEXT; VAR env : T) RAISES {E} =
  VAR
    rd : FileRd.T;
  BEGIN
    TRY
      rd := FileRd.Open(fn);
      WHILE NOT Rd.EOF(rd) DO
        Read(rd, env);
      END;
      Rd.Close(rd);
    EXCEPT
      Rd.Failure => RAISE E("error reading environment");
    | OSError.E  => RAISE E("cannot open environment file");
    | Thread.Alerted => RAISE E("reading of environment interrupted");
    END;
  END ReadFile;

(*---------------------------------------------------------------------------*)
VAR
  penv := ProcessEnv.Current();
  nil  := TextList.List1("NilList");
BEGIN  
END SimpleValueEnv.
