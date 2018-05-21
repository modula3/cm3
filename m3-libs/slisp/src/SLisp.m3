(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan 30 15:32:24 PST 1995 by kalsow                   *)
(*      modified on Fri Jul 23 15:49:58 PDT 1993 by steveg                   *)
(*      modified on Tue May 18 19:34:09 PDT 1993 by muller                   *)

UNSAFE MODULE SLisp EXPORTS SLisp, SLispClass;

IMPORT Text, Sx, Rd, Wr, RefList, IO, Atom AS Aatom;
IMPORT IntRefTbl, SLispMath, Stdio, Thread, Fmt, TextRd, TextWr,
       RTTypeSRC, Word;

<*FATAL Wr.Failure *>
<*FATAL Thread.Alerted*>
<*FATAL Rd.EndOfFile*>
<*FATAL Sx.PrintError *>

REVEAL 
  T = TPublic BRANDED OBJECT 
         props: RefList.T;
      OVERRIDES
         new := new;
         init := init;
         error := error;

         load := load1;

         defineVar := defineVar;
         defineFun := defineFun;

         checkSymbol := checkSymbol;
         checkList := checkList;
         checkInt := checkInt;
         checkFloat := checkFloat;
         checkString := checkString;

         eval := eval;
           evalSymbol := evalSymbol;
           evalList := evalList;
           evalInt := evalInt;
           evalFloat := evalFloat;
           evalString := evalString;

         sEval := sEval;
         varEval := varEval;
         lookup     := lookup;
        lookupAtom := lookupAtom;
        pushScope  := PushScope;
        popScope   := PopScope;
   END;

PROCEDURE new (<* UNUSED *> self: T): T =
  BEGIN
    RETURN NEW (T).init();
  END new;

PROCEDURE init (self: T): T =
  BEGIN
    self.props := NIL;

    self.underEval := NEW (List);
    self.topFrame := NEW (Frame, procName := "*top*", endScope := TRUE);
    self.frame := self.topFrame; 
    self.depth := 0;

    self.defineFun (NEW (Builtin, name := "abort", apply := Abort, 
                         minArgs := 0, maxArgs := 0));
    self.defineFun (NEW (Builtin, name := "setq", apply := Setq, 
                         minArgs := 2, maxArgs := 2));
    self.defineFun (NEW (Builtin, name := "quote", apply := Quote,
                         minArgs := 1, maxArgs := 1));
    self.defineFun (NEW (Builtin, name := "intern", apply := Intern,
                         minArgs := 1, maxArgs := 1));
    self.defineFun (NEW (Builtin, name := "eval", apply := EvalBI,
                         minArgs := 1, maxArgs := 1));
    self.defineFun (NEW (Builtin, name := "printname", apply := Printname,
                         minArgs := 1, maxArgs := 1));
    self.defineFun (NEW (Builtin, name := "cond", apply := Cond,
                         minArgs := 0, maxArgs := LAST (INTEGER)));
    self.defineFun (NEW (Builtin, name := "if", apply := If,
                         minArgs := 2, maxArgs := LAST (INTEGER)));
    self.defineFun (NEW (Builtin, name := "while", apply := While,
                         minArgs := 1, maxArgs := LAST (INTEGER)));
    self.defineFun (NEW (Builtin, name := "listp", apply := Listp,
                         minArgs := 1, maxArgs := 1));
    self.defineFun (NEW (Builtin, name := "integerp", apply := Integerp,
                         minArgs := 1, maxArgs := 1));
    self.defineFun (NEW (Builtin, name := "floatp", apply := Floatp,
                         minArgs := 1, maxArgs := 1));
    self.defineFun (NEW (Builtin, name := "stringp", apply := Stringp,
                         minArgs := 1, maxArgs := 1));
    self.defineFun (NEW (Builtin, name := "symbolp", apply := Symbolp,
                         minArgs := 1, maxArgs := 1));
    self.defineFun (NEW (Builtin, name := "defun", apply := Defun,
                         minArgs := 2, maxArgs := LAST (INTEGER)));
    self.defineFun (NEW (Builtin, name := "apply", apply := UApply,
                         minArgs := 2, maxArgs := LAST (INTEGER)));
    self.defineFun (NEW (Builtin, name := "defmacro", apply := Defmacro,
                         minArgs := 2, maxArgs := LAST (INTEGER)));
    self.defineFun (NEW (Builtin, name := "progn", apply := Progn,
                         minArgs := 0, maxArgs := LAST (INTEGER)));
    self.defineFun (NEW (Builtin, name := "let", apply := Let,
                         minArgs := 1, maxArgs := LAST (INTEGER)));
    self.defineFun (NEW (Builtin, name := "letstar", apply := LetStar,
                         minArgs := 1, maxArgs := LAST (INTEGER)));
    self.defineFun (NEW (Builtin, name := "backtrace", apply := Backtrace,
                         minArgs := 0, maxArgs := 0));
    self.defineFun (NEW (Builtin, name := "load", apply := Load,
                         minArgs := 1, maxArgs := 1));
    self.defineFun (NEW (Builtin, name := "print", apply := Print,
                         minArgs := 0, maxArgs := LAST (INTEGER)));
    self.defineFun (NEW (Builtin, name := "cons", apply := Cons,
                         minArgs := 2, maxArgs := 2));

    self.defineFun (NEW (Builtin, name := "car", apply := Car,
                         minArgs := 1, maxArgs := 1));
    self.defineFun (NEW (Builtin, name := "cdr", apply := Cdr,
                         minArgs := 1, maxArgs := 1));
    self.defineFun (NEW (Builtin, name := "caar", apply := Caar,
                         minArgs := 1, maxArgs := 1));
    self.defineFun (NEW (Builtin, name := "cadr", apply := Cadr,
                         minArgs := 1, maxArgs := 1));
    self.defineFun (NEW (Builtin, name := "cdar", apply := Cdar,
                         minArgs := 1, maxArgs := 1));
    self.defineFun (NEW (Builtin, name := "cddr", apply := Cddr,
                         minArgs := 1, maxArgs := 1));
    self.defineFun (NEW (Builtin, name := "concat", apply := Concat,
                         minArgs := 0, maxArgs := LAST (INTEGER)));
    self.defineFun (NEW (Builtin, name := "append", apply := Append,
                         minArgs := 0, maxArgs := LAST (INTEGER)));
    self.defineFun (NEW (Builtin, name := "list", apply := LIst,
                         minArgs := 0, maxArgs := LAST (INTEGER)));
    self.defineFun (NEW (Builtin, name := "length", apply := Length,
                         minArgs := 1, maxArgs := 1));
    self.defineFun (NEW (Builtin, name := "get_prop", apply := GetProp,
                         minArgs := 1, maxArgs := 1));
    self.defineFun (NEW (Builtin, name := "set_prop", apply := SetProp,
                         minArgs := 2, maxArgs := 2));

    self.defineVar ("t",      syms.t);
    self.defineVar ("nil",    NIL);
    self.defineVar ("stdin",  Stdio.stdin);
    self.defineVar ("stdout", Stdio.stdout);
    self.defineVar ("stderr", Stdio.stderr);

    SLispMath.Register(self); (* depends on "t" *)

    RETURN self;
  END init;

PROCEDURE error (self: T; msg: Text.T := ""): Sexp RAISES {Error} =
  VAR
    stdin  := self.varEval("stdin");
    stdout := self.varEval("stdout");
    stderr := self.varEval("stderr");
  BEGIN
    Wr.PutText(stderr, "error: ");
    Wr.PutText(stderr, msg);
    Wr.PutText(stderr, "\n");

    IF stdin # NIL THEN
      INC(self.depth);
      self.underEval := RefList.List1(self.underEval);
      self.defineVar("stdout", stderr);
      TRY
        LOOP
          Wr.PutText(stderr, Fmt.Int(self.depth) & "> ");
          Wr.Flush(stderr);
          Write(stderr, self.eval(Read(stdin)));
          Wr.PutText(stderr, "\n");
          Wr.Flush(stderr);
        END;
      EXCEPT
      | Sx.ReadError =>
          Wr.PutText(stderr, "can't parse input\n");
          Wr.Flush(stderr);
      | Error =>
          DEC(self.depth);
          self.underEval := self.underEval.tail;
          self.defineVar("stdout", stdout);
      END;

      RETURN NIL;
    ELSE
      EVAL Backtrace(NIL, self, NIL);
      RAISE Error;
    END;
  END error;

PROCEDURE lookup (self: T; s: Symbol; create: LookupMode): Atom =
  VAR
    at   : Atom;
    frame: Frame;
  BEGIN
    IF self.frame # self.topFrame THEN
      frame := self.frame;
      LOOP
        FOR i := 0 TO frame.size - 1 DO
          IF frame.table[i].symbol = s THEN
            RETURN (frame.table[i].atom);
          END;
        END;
        IF frame.endScope THEN EXIT END;
        frame := frame.next;
      END;
    END;
    FOR i := 0 TO self.topFrame.size - 1 DO
      IF self.topFrame.table[i].symbol = s THEN
        RETURN (self.topFrame.table[i].atom);
      END;
    END;
    CASE create OF
    | LookupMode.CreateLocal =>
        at := NEW(Atom);
        Insert(self.frame, s, at);
        RETURN at;
    | LookupMode.CreateGlobal =>
        at := NEW(Atom);
        Insert(self.topFrame, s, at);
        RETURN at;
    | LookupMode.LookupOnly => RETURN NIL
    END;
  END lookup;

PROCEDURE lookupAtom (self: T; atom: Atom): Symbol =
  VAR frame: Frame;
  BEGIN
    IF self.frame # self.topFrame THEN
      frame := self.frame;
      LOOP
        FOR i := 0 TO frame.size - 1 DO
          IF frame.table[i].atom = atom THEN
            RETURN (frame.table[i].symbol);
          END;
        END;
        IF frame.endScope THEN EXIT END;
        frame := frame.next;
      END;
    END;
    FOR i := 0 TO self.topFrame.size - 1 DO
      IF self.topFrame.table[i].atom = atom THEN
        RETURN (self.topFrame.table[i].symbol);
      END;
    END;
    RETURN NIL;
  END lookupAtom;

PROCEDURE Insert (frame: Frame; symbol: Symbol; atom: Atom) =
  BEGIN
    IF frame.table = NIL OR frame.size = NUMBER (frame.table^) THEN
      VAR newTable := NEW (REF ARRAY OF Binding, 
                           MAX (frame.size * 2, 5)); BEGIN
        IF frame.table # NIL THEN
          SUBARRAY (newTable^, 0, frame.size) := frame.table^; END;
        frame.table := newTable; END; END;
    frame.table [frame.size] := NEW (Binding, symbol := symbol, atom := atom);
    INC (frame.size);
  END Insert;

PROCEDURE defineVar (self: T; name: Text.T; val: Sexp) =
  VAR sym := Aatom.FromText (name);
      at := self.lookup (sym);
  BEGIN
    at.val := val;
  END defineVar;

PROCEDURE defineFun (self: T; cl: Builtin) = 
  VAR sym := Aatom.FromText (cl.name);
      at := self.lookup (sym);
  BEGIN
    at.builtin := cl;
    at.funDefined := TRUE;
  END defineFun;

PROCEDURE eval (self: T; e: Sexp): Sexp RAISES {Error} =
  BEGIN
    self.underEval.head := e;
    self.evalStack := RefList.Cons(e, self.evalStack);
    TRY
      TYPECASE e OF
      | Integer, Float, String => RETURN (e);
      | Symbol (sym) => RETURN self.lookup(sym, LookupMode.CreateLocal).val;
      | List (list) =>
          IF list = NIL OR list.head = NIL THEN
            EVAL self.error("cannot apply");
          END;
          RETURN Apply(self, list.head, list.tail);
      | REF REAL (r) =>
          VAR e := NEW(Float);
          BEGIN
            e^ := FLOAT(r^, REAL);
            RETURN (e);
          END;
      | REF EXTENDED (r) =>
          VAR e := NEW(Float);
          BEGIN
            e^ := FLOAT(r^, REAL);
            RETURN (e);
          END;
      ELSE
        RETURN self.error("wrong type ?");
      END;
    FINALLY
      self.evalStack := self.evalStack.tail;
    END;
  END eval;

PROCEDURE varEval (self: T; name: Text.T): Sexp =
  BEGIN
    RETURN self.lookup (Aatom.FromText (name), LookupMode.CreateLocal).val; 
  END varEval;

PROCEDURE sEval (self: T; s: Text.T): Text.T RAISES {Error} =
  VAR rd := TextRd.New (s); <* FATAL Sx.ReadError *>
  BEGIN
    RETURN SxToText(self.eval (Sx.Read (rd, syntax)));
  END sEval;

PROCEDURE checkSymbol (self: T; e: Sexp): Symbol RAISES {Error} =
  BEGIN
    IF e = NIL OR NOT ISTYPE(e, Symbol) THEN
      RETURN
        self.error(Fmt.F("\"%s\" should be a symbol", SxToText(e)));
    ELSE
      RETURN NARROW(e, Symbol);
    END;
  END checkSymbol;

PROCEDURE evalSymbol (self: T; e: Sexp): Symbol RAISES {Error} = 
  BEGIN
    RETURN self.checkSymbol (self.eval (e));
  END evalSymbol;

PROCEDURE checkList (self: T; e: Sexp): List RAISES {Error} = 
  BEGIN
    IF NOT ISTYPE (e, List) THEN 
       RETURN self.error(Fmt.F("\"%s\" should be a list", SxToText(e)));
    ELSE 
      RETURN NARROW (e, List); END;
  END checkList;

PROCEDURE evalList (self: T; e: Sexp): List RAISES {Error} = 
  BEGIN
    RETURN self.checkList (self.eval (e));
  END evalList;

PROCEDURE checkInt (self: T; e: Sexp): INTEGER RAISES {Error} = 
  BEGIN
    IF e = NIL OR NOT ISTYPE (e, Integer) THEN
       EVAL self.error(Fmt.F("\"%s\" should be an integer", SxToText(e)));
       RETURN 0;
    ELSE 
      RETURN NARROW (e, Integer)^; END;
  END checkInt;

PROCEDURE evalInt (self: T; e: Sexp): INTEGER RAISES {Error} = 
  BEGIN
    RETURN self.checkInt (self.eval (e));
  END evalInt;

PROCEDURE checkFloat (self: T; e: Sexp): REAL RAISES {Error} =
  BEGIN
    IF e = NIL OR NOT ISTYPE(e, Float) THEN
      IF ISTYPE(e, REF REAL) THEN
        RETURN FLOAT(NARROW(e, REF REAL)^, REAL);
      ELSIF ISTYPE(e, REF EXTENDED) THEN
        RETURN FLOAT(NARROW(e, REF EXTENDED)^, REAL);
      ELSE
        EVAL self.error(Fmt.F("\"%s\" should be a float", SxToText(e)));
        RETURN 0.0; (* get rid of warning *)
      END;
    ELSE
      RETURN NARROW(e, Float)^;
    END;
  END checkFloat;

PROCEDURE evalFloat (self: T; e: Sexp): REAL RAISES {Error} = 
  BEGIN
    RETURN self.checkFloat (self.eval (e));
  END evalFloat;

PROCEDURE checkString (self: T; e: Sexp): String RAISES {Error} = 
  BEGIN
    IF e = NIL OR NOT ISTYPE (e, String) THEN
      RETURN self.error(Fmt.F("\"%s\" should be a string", SxToText(e)));
    ELSE 
      RETURN NARROW (e, String); END;
  END checkString;

PROCEDURE evalString (self: T; e: Sexp): String RAISES {Error} = 
  BEGIN
    RETURN self.checkString (self.eval (e));
  END evalString;

(*---------------------------------------------------------------------------*)

PROCEDURE Apply (self: T; fun: Sexp; args: List): Sexp RAISES {Error} =
  VAR
    atom    : Atom;
    newFrame: Frame;
    funSym  : Symbol;
  BEGIN
    funSym := self.checkSymbol(fun);
    atom := self.lookup(funSym, LookupMode.CreateLocal);
    newFrame := NEW(Frame, next := self.frame, size := 0,
                    procName := Aatom.ToText(funSym), endScope := TRUE);

    IF NOT atom.funDefined THEN
      RETURN self.error("undefined: " & Aatom.ToText(self.lookupAtom(atom)));
    ELSIF atom.builtin # NIL THEN
      VAR n := RefList.Length(args);
      BEGIN
        IF n < atom.builtin.minArgs THEN
          RETURN self.error("not enough arguments for: "
                              & Aatom.ToText(self.lookupAtom(atom)));
        ELSIF n > atom.builtin.maxArgs THEN
          RETURN
            self.error(
              "too many arguments for: " & Aatom.ToText(self.lookupAtom(atom)));
        ELSE
          RETURN atom.builtin.apply(self, args);
        END;
      END;
    ELSE
      VAR
        formals           := atom.funFormals;
        formalSym: Symbol;
        actuals           := args;
        body              := atom.funBody;
        res      : Sexp;
        eval              := NOT atom.macro;
      BEGIN

        WHILE formals # NIL DO
          formalSym := self.checkSymbol(formals.head);
          IF formalSym = syms.ampersandRest THEN
            formals := formals.tail;
            formalSym := self.checkSymbol(formals.head);
            VAR
              ll      := Copy(actuals);
              ll_last := ll;
            BEGIN
              IF eval THEN
                WHILE ll_last # NIL DO
                  ll_last.head := self.eval(ll_last.head);
                  ll_last := ll_last.tail;
                END;
              END;
              Insert(newFrame, formalSym, NEW(Atom, val := ll));
            END;
          ELSIF formalSym = syms.ampersandNoEval THEN
            eval := FALSE;
          ELSIF formalSym = syms.ampersandEval THEN
            eval := TRUE;
          ELSE
            IF actuals = NIL THEN
              RETURN self.error(
                       "not enough arguments for call to: " & Aatom.ToText(funSym))
            END;
            IF eval THEN
              Insert(newFrame, formalSym,
                     NEW(Atom, val := self.eval(actuals.head)));
            ELSE
              Insert(newFrame, formalSym, NEW(Atom, val := actuals.head));
            END;
            actuals := actuals.tail;
          END;
          formals := formals.tail;
        END;

        self.frame := newFrame;
        WHILE body # NIL DO
          res := self.eval(body.head);
          body := body.tail;
        END;
        self.frame := self.frame.next;

        IF atom.macro THEN res := self.eval(res); END;
        RETURN res;
      END;
    END;
  END Apply;

(*-------------------------------------------------------------- builtins ---*)

PROCEDURE Abort (<*UNUSED*> self: Builtin; <*UNUSED*> interp: T;
                 <*UNUSED*> args: List): Sexp RAISES {Error} =
  BEGIN
    RAISE Error;
  END Abort;

PROCEDURE Setq (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp RAISES {Error} =
  VAR sym: Symbol; at: Atom;
  BEGIN
    sym := interp.checkSymbol (args.head);
    at := interp.lookup (sym);
    at.val := interp.eval (args.tail.head);
    RETURN at.val;
  END Setq;

PROCEDURE Quote (<*UNUSED*> self: Builtin; <*UNUSED*> interp: T;
                 args: List): Sexp =
  BEGIN
    RETURN args.head;
  END Quote;

PROCEDURE EvalBI (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR e1 := interp.eval(args.head);
  BEGIN
    RETURN interp.eval(e1);
  END EvalBI;

PROCEDURE Intern (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp RAISES {Error} =
  VAR sym: Symbol;
  BEGIN
    sym := Aatom.FromText (interp.evalString (args.head));
    Insert (interp.topFrame, sym, NEW (Atom, val := NIL));
    RETURN sym;
  END Intern;

PROCEDURE Printname (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp RAISES {Error} =
  VAR sym: Symbol;
  BEGIN
    sym := interp.evalSymbol (args.head);
    RETURN Aatom.ToText (sym);
  END Printname;

PROCEDURE Cond (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp RAISES {Error} =
  VAR res, caseVal: Sexp := NIL; condCase: List;
  BEGIN
    WHILE args # NIL DO
      condCase := interp.checkList (args.head);
      caseVal := interp.eval (condCase.head);
      IF caseVal # NIL THEN
        condCase := condCase.tail;
        WHILE condCase # NIL DO
          res := interp.eval (condCase.head);
          condCase := condCase.tail; END;
        RETURN res; END;
      args := args.tail; END;
    RETURN NIL;
  END Cond;

PROCEDURE If (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR
    res  : Sexp;
    forms       := args.tail;
  BEGIN
    IF interp.eval(args.head) # NIL THEN
      RETURN interp.eval(forms.head);
    ELSE
      forms := forms.tail;
      WHILE forms # NIL DO
        res := interp.eval(forms.head);
        forms := forms.tail;
      END;
    END;
    RETURN res;
  END If;

PROCEDURE While (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR
    res  : Sexp;
    cond        := args.head;
    body        := args.tail;
    forms: List;
  BEGIN
    WHILE interp.eval(cond) # NIL DO
      forms := body;
      WHILE forms # NIL DO
        res := interp.eval(forms.head);
        forms := forms.tail;
      END;
    END;
    RETURN res;
  END While;

PROCEDURE Listp (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  BEGIN
    WITH a = interp.eval(args.head) DO
      IF NOT ISTYPE(a, List) THEN RETURN NIL; ELSE RETURN syms.t; END;
    END;
  END Listp;

PROCEDURE Integerp (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  BEGIN
    WITH a = interp.eval(args.head) DO
      IF a = NIL OR NOT ISTYPE(a, Integer) THEN
        RETURN NIL;
      ELSE
        RETURN syms.t;
      END;
    END;
  END Integerp;

PROCEDURE Floatp (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  BEGIN
    WITH a = interp.eval(args.head) DO
      IF a = NIL OR NOT ISTYPE(a, Float) THEN
        RETURN NIL;
      ELSE
        RETURN syms.t;
      END;
    END;
  END Floatp;

PROCEDURE Stringp (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  BEGIN
    WITH a = interp.eval(args.head) DO
      IF a = NIL OR NOT ISTYPE(a, String) THEN
        RETURN NIL;
      ELSE
        RETURN syms.t;
      END;
    END;
  END Stringp;

PROCEDURE Symbolp (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  BEGIN
    WITH a = interp.eval(args.head) DO
      IF a = NIL OR NOT ISTYPE(a, Symbol) THEN
        RETURN NIL;
      ELSE
        RETURN syms.t;
      END;
    END;
  END Symbolp;

PROCEDURE UApply (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp RAISES {Error} =
  VAR a,l: RefList.T := NIL; f: Symbol;
  BEGIN
    f := interp.evalSymbol (args.head);
    args := args.tail;
    WHILE args.tail # NIL DO
      IF l = NIL THEN
        l := RefList.List2 (syms.quote, interp.eval (args.head));
        a := l;
      ELSE
        l.tail := RefList.List2 (syms.quote, interp.eval (args.head));
        l := l.tail; END;
      args := args.tail; END;
    args := interp.eval (args.head);
    WHILE args # NIL DO
      IF l = NIL THEN
        l := RefList.List2 (syms.quote, args.head);
        a := l;
      ELSE
        l.tail := RefList.List2(syms.quote, args.head);
        l := l.tail; END;
      args := args.tail; END;
    RETURN Apply (interp, f, a);
  END UApply;

PROCEDURE Defun (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp RAISES {Error} =
  VAR sym: Symbol; at: Atom;
  BEGIN
    sym := interp.checkSymbol (args.head);
    at := interp.lookup (sym);
    at.funDefined := TRUE;
    at.macro := FALSE;
    at.funFormals := interp.checkList (args.tail.head);
    at.funBody := args.tail.tail;
    RETURN sym;
  END Defun;

PROCEDURE Defmacro (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp RAISES {Error} =
  VAR sym: Symbol; at: Atom;
  BEGIN
    sym := interp.checkSymbol (args.head);
    at := interp.lookup (sym);
    at.funDefined := TRUE;
    at.macro := TRUE;
    at.funFormals := interp.checkList (args.tail.head);
    at.funBody := args.tail.tail;
    RETURN sym;
  END Defmacro;

PROCEDURE Progn (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp RAISES {Error} =
  VAR res: Sexp := NIL;
  BEGIN
    WHILE args # NIL DO
      res := interp.eval (args.head);
      args := args.tail; END;
    RETURN res;
  END Progn;

PROCEDURE LetStar (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  BEGIN
    RETURN Let2(interp, args, TRUE);
  END LetStar;

PROCEDURE Let (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  BEGIN
    RETURN Let2(interp, args);
  END Let;

PROCEDURE Let2 (interp: T; args: List; letStar := FALSE): Sexp
  RAISES {Error} =
  VAR
    newFrame: Frame;
    bindings: List;
    forms           := args.tail;
    res     : Sexp  := NIL;
  BEGIN
    newFrame := NEW(Frame, next := interp.frame, size := 0);
    IF letStar THEN interp.frame := newFrame END;
    bindings := checkList(interp, args.head);
    WHILE bindings # NIL DO
      TYPECASE bindings.head OF
      | NULL =>
          EVAL interp.error("first argument of a binding can not be NIL");
      | List (l) =>
          IF ISTYPE(l.head, Symbol) THEN
            IF l.tail # NIL THEN
              Insert(newFrame, l.head,
                     NEW(Atom, val := interp.eval(l.tail.head)));
            ELSE
              Insert(newFrame, l.head, NEW(Atom, val := NIL));
            END;
          ELSE
            EVAL interp.error("should be a symbol: " & SxToText(l.head));
          END;
      | Symbol (s) => Insert(newFrame, s, NEW(Atom, val := NIL));
      ELSE
        EVAL interp.error("should be a symbol or a list: "
                            & SxToText(bindings.head));
      END;
      bindings := bindings.tail;
    END;
    IF NOT letStar THEN interp.frame := newFrame END;
    WHILE forms # NIL DO
      res := interp.eval(forms.head);
      forms := forms.tail;
    END;
    interp.frame := interp.frame.next;
    RETURN res;
  END Let2;

PROCEDURE Backtrace (<*UNUSED*> self: Builtin; interp: T;
                     <*UNUSED*> args: List): Sexp RAISES {Error} =
  VAR frame := interp.frame; stdout := interp.varEval ("stdout");
  BEGIN
    IF interp.depth > 0 THEN
      Write (stdout, interp.underEval.tail.head);
      Wr.PutText (stdout, ")\n"); END;
    WHILE frame # interp.topFrame DO
      Wr.PutText (stdout, "(" & frame.procName);
      FOR i := 0 TO frame.size - 1 DO
        Wr.PutText (stdout, " ");
        Write (stdout, frame.table [i].atom.val); END;
      Wr.PutText (stdout, ")\n");
      Wr.Flush (stdout);
      frame := frame.next; END;
    RETURN syms.t;
  END Backtrace;

PROCEDURE load1  (interp: T; name: Text.T): Sexp RAISES {Error} =
  VAR from: Rd.T; res: Sexp := NIL;
  BEGIN
    from := IO.OpenRead (name);
    IF from = NIL THEN
      RETURN interp.error(Fmt.F("Could not load file: %s", name))
    END;
    TRY
      LOOP 
        res := interp.eval (Read (from)); END;
    EXCEPT
      | Sx.ReadError => RETURN interp.error(Fmt.F("Sx error loading file: %s", name))
      | Rd.EndOfFile => END;
    RETURN res;
  END load1;

PROCEDURE Load (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp RAISES {Error} =
  BEGIN
    RETURN load1 (interp, interp.evalString (args.head));
  END Load;

PROCEDURE Print (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp RAISES {Error} =
  VAR stdout := interp.varEval ("stdout"); arg := args;
  BEGIN
    WHILE args # NIL DO
      Write (stdout, interp.eval (args.head));
      args := args.tail; END;
    RETURN arg;
  END Print;

PROCEDURE Cons (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp RAISES {Error} =
  BEGIN
    RETURN RefList.Cons (interp.eval     (args.head), 
                      interp.evalList (args.tail.head));
  END Cons;

PROCEDURE Car (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR l := interp.evalList(args.head);
  BEGIN
    IF l = NIL THEN
      RETURN interp.error("\"car\" of empty list")
    ELSE
      RETURN l.head
    END;
  END Car;

PROCEDURE Cdr (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR l := interp.evalList(args.head);
  BEGIN
    IF l = NIL THEN
      RETURN interp.error("\"cdr\" of empty list")
    ELSE
      RETURN l.tail
    END;
  END Cdr;

PROCEDURE Caar (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR l := interp.evalList(args.head);
  BEGIN
    IF l = NIL THEN
      RETURN interp.error("can't take \"caar\" of empty list")
    ELSE
      TYPECASE l.head OF
      | NULL =>
          RETURN interp.error(
                   "can't take \"caar\" of list when first element is nil")
      | List (first) => RETURN first.head
      ELSE
        RETURN
          interp.error(
            "can't take \"caar\" of list when first element isn't a list")
      END;
    END;
  END Caar;

PROCEDURE Cadr (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR l := interp.evalList(args.head);
  BEGIN
    IF l = NIL THEN
      RETURN interp.error("can't take \"cadr\" of empty list")
    ELSIF l.tail = NIL THEN
      RETURN interp.error("can't take \"cadr\" of too short list")
    ELSE
      RETURN l.tail.head
    END;
  END Cadr;

PROCEDURE Cdar (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR l := interp.evalList(args.head);
  BEGIN
    IF l = NIL THEN
      RETURN interp.error("can't take \"cdar\" of empty list")
    ELSE
      TYPECASE l.head OF
      | NULL =>
          RETURN interp.error(
                   "can't take \"cdar\" of list when first element is nil")
      | List (first) => RETURN first.tail
      ELSE
        RETURN
          interp.error(
            "can't take \"cdar\" of list when first element isn't a list")
      END;
    END;
  END Cdar;

PROCEDURE Cddr (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR l := interp.evalList(args.head);
  BEGIN
    IF l = NIL THEN
      RETURN interp.error("can't take \"cddr\" of empty list")
    ELSIF l.tail = NIL THEN
      RETURN interp.error("can't take \"cddr\" of too short list")
    ELSE
      RETURN l.tail.tail
    END;
  END Cddr;

PROCEDURE Concat (<*UNUSED*>self: Builtin; interp: T; args: List): Sexp RAISES {Error} =
  VAR res := "";
  BEGIN
    WHILE args # NIL DO
      res := res & interp.evalString (args.head);
      args := args.tail; END;
    RETURN res;
  END Concat;

PROCEDURE Append (<*UNUSED*>self: Builtin; interp: T; args: List): Sexp RAISES {Error} =
  VAR res, last, l : RefList.T := NIL;
  BEGIN
    WHILE args # NIL DO
      l := interp.evalList (args.head);
      WHILE l # NIL DO
        IF last = NIL THEN
          last := RefList.List1 (l.head);
          res := last;
        ELSE
          last.tail := RefList.List1 (l.head);
          last := last.tail; END;
        l := l.tail; END;
      args := args.tail; END;
    RETURN res;
  END Append;

PROCEDURE LIst (<*UNUSED*>self: Builtin; interp: T; args: List): Sexp RAISES {Error} =
  VAR res, last: RefList.T := NIL;
  BEGIN
    WHILE args # NIL DO
      IF last = NIL THEN
        last := RefList.Cons (interp.eval (args.head), NIL);
        res := last;
      ELSE
        last.tail := RefList.Cons (interp.eval (args.head), NIL);
        last := last.tail; END;
      args := args.tail; END;
    RETURN res;
  END LIst;

PROCEDURE Length (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  BEGIN
    TYPECASE interp.eval(args.head) OF
    | NULL => RETURN Sx.FromInt(0);
    | Symbol, Integer, Float => RETURN Sx.FromInt(1);
    | String (s) => RETURN Sx.FromInt(Text.Length(s));
    | List (l) => RETURN Sx.FromInt(RefList.Length(l));
    ELSE                         <* ASSERT FALSE *>
    END;
  END Length;

PROCEDURE SetProp (<* UNUSED *> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR
    key              := interp.eval(args.head);
    value            := interp.eval(args.tail.head);
    props            := interp.props;
    assoc: RefList.T;
  BEGIN
    WHILE props # NIL DO
      assoc := props.head;
      IF assoc.head = key THEN assoc.tail.head := value; RETURN value END;
      props := props.tail;
    END;
    interp.props := RefList.Cons(RefList.List2(key, value), interp.props);
    RETURN value;
  END SetProp;

PROCEDURE GetProp (<* UNUSED *> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR
    key              := interp.eval(args.head);
    props            := interp.props;
    assoc: RefList.T;
  BEGIN
    WHILE props # NIL DO
      assoc := props.head;
      IF assoc.head = key THEN RETURN assoc.tail.head END;
      props := props.tail;
    END;
    RETURN NIL
  END GetProp;

(*---------------------------------------------------------------- syntax ---*)

VAR
  quoteParser     := NEW (Sx.ReadMacro, read := QuoteParser);

PROCEDURE QuoteParser (<*UNUSED*> self: Sx.ReadMacro;
                       rd: Rd.T; syntax: Sx.Syntax): RefList.T
       RAISES {Sx.ReadError, Thread.Alerted} =
  BEGIN
    RETURN RefList.List1 (RefList.List2 (syms.quote, Sx.Read (rd, syntax)));
  END QuoteParser;




VAR
  backQuoteParser := NEW (Sx.ReadMacro, read := BackQuoteParser);
  backQuoteSyntax := Sx.CopySyntax ();

PROCEDURE ApplyBackQuote (s: Sexp): Sexp = 
  BEGIN
    TYPECASE s OF
      | List (sl) =>
           VAR res := NEW (List); BEGIN
             res.head := syms.append;
             ApplyBackQuoteList (sl, res);
             RETURN res; END;
      | Coma (c) => RETURN c.form; 
      ELSE RETURN RefList.List2 (syms.quote, s); END;
  END ApplyBackQuote;

PROCEDURE ApplyBackQuoteList (l: List; rest: List) =
  BEGIN
    IF l = NIL THEN
      rest.tail := NIL;
    ELSE
      rest.tail := NEW (RefList.T);
      rest := rest.tail;
      TYPECASE l.head OF
        | NULL => rest.head := RefList.List2 (syms.list, NIL);
        | Coma (c) => rest.head := RefList.List2 (syms.list, c.form);
        | ComaAt (c) => rest.head := c.form;
        ELSE 
          rest.head := RefList.List2 (syms.list,
                                     ApplyBackQuote (l.head)); END;

      ApplyBackQuoteList (l.tail, rest); END;
  END ApplyBackQuoteList;

PROCEDURE BackQuoteParser (<*UNUSED*> self: Sx.ReadMacro; 
                           rd: Rd.T; <*UNUSED*>syntax: Sx.Syntax): RefList.T
       RAISES {Sx.ReadError, Thread.Alerted} =
  BEGIN
    RETURN RefList.List1 (ApplyBackQuote (Sx.Read (rd, backQuoteSyntax)));
  END BackQuoteParser;


VAR
  comaParser      := NEW (Sx.ReadMacro, read := ComaParser);

TYPE 
  Coma = BRANDED REF RECORD form: REFANY; END;
  ComaAt = BRANDED REF RECORD form: REFANY; END;

PROCEDURE ComaParser (<*UNUSED*> self: Sx.ReadMacro; 
                      rd: Rd.T; <*UNUSED*> syntax: Sx.Syntax): RefList.T
       RAISES {Sx.ReadError, Thread.Alerted} =
  <* FATAL Rd.Failure *>
  BEGIN
    IF Rd.GetChar (rd) = '@' THEN
      RETURN RefList.List1 (NEW (ComaAt, form := Sx.Read (rd, backQuoteSyntax)));
    ELSE
      Rd.UnGetChar (rd);
      RETURN RefList.List1 (NEW (Coma, form := Sx.Read (rd, backQuoteSyntax))); END;
  END ComaParser;

VAR
  syntax          := Sx.CopySyntax ();
  syms: RECORD
          ampersandRest, ampersandEval, ampersandNoEval,
          list, append, quote, t, nil: Symbol; END;

PROCEDURE InitSyntax () =
  BEGIN
    Sx.SetReadMacro (syntax, '\'', quoteParser);
    Sx.SetReadMacro (syntax, '`', backQuoteParser);

    Sx.SetReadMacro (backQuoteSyntax, ',', comaParser);
    Sx.SetReadMacro (backQuoteSyntax, '\'', quoteParser);

    syms.ampersandRest   := Aatom.FromText ("THE_REST");
    syms.ampersandEval   := Aatom.FromText ("_EVAL");
    syms.ampersandNoEval := Aatom.FromText ("NO_EVAL");
    syms.list            := Aatom.FromText ("list");
    syms.append          := Aatom.FromText ("append");
    syms.quote           := Aatom.FromText ("quote");
    syms.t               := Aatom.FromText ("t");
    syms.nil             := Aatom.FromText ("nil");
  END InitSyntax;

PROCEDURE Read (rd: Reader): Sexp RAISES {Rd.EndOfFile, Sx.ReadError} =
  BEGIN
    RETURN Sx.Read (rd, syntax);
  END Read;

TYPE
  ReadMacro =
    Sx.ReadMacro OBJECT table: IntRefTbl.T OVERRIDES read := ReadList END;

PROCEDURE ReadList (rm: ReadMacro; rd: Rd.T; s: Sx.Syntax): RefList.T
  RAISES {Sx.ReadError, Thread.Alerted} =
  (* Record the starting and ending positions of every list we read, so
     that we can highlight the list if there's an error. *)
  VAR
    start := Rd.Index(rd) - 1;
    form  := Sx.ReadDelimitedList(rd, ')', s);
    end   := Rd.Index(rd);
  BEGIN
    EVAL rm.table.put(start, NEW(Range, start := start, end := end, form := form));
    RETURN RefList.List1(form)
  END ReadList;

PROCEDURE ReadToTable (rd: Reader; table: IntRefTbl.T): Sexp
  RAISES {Rd.EndOfFile, Sx.ReadError} =
  VAR tSyntax := Sx.CopySyntax(syntax);
  BEGIN
    IF table # NIL THEN
      Sx.SetReadMacro (tSyntax, '(', NEW (ReadMacro, table := table));
    END;
    RETURN Sx.Read(rd, tSyntax);
  END ReadToTable;

PROCEDURE Write (wr: Writer; s: Sexp) =
  <* FATAL Sx.PrintError *>
  BEGIN
     SxPrint (wr, s);
  END Write;

PROCEDURE PushScope(interp: T) =
  VAR newFrame := NEW(Frame, next := interp.frame, size := 0);
  BEGIN
    interp.frame := newFrame;
  END PushScope;

PROCEDURE PopScope(interp: T) =
  BEGIN
    interp.frame := interp.frame.next;
  END PopScope;

(*---------------------------------------------------------------------------*)
PROCEDURE Copy (x: RefList.T): RefList.T =
  BEGIN
    IF x = NIL THEN
      RETURN NIL;
    END;
    RETURN RefList.Cons (x.head, Copy (x.tail));
  END Copy;

PROCEDURE SxToText(sx: REFANY): TEXT RAISES {Sx.PrintError} =
  VAR wr: TextWr.T;
  BEGIN
    wr := TextWr.New();
    SxPrint(wr, sx);
    RETURN TextWr.ToText(wr);
  END SxToText;

CONST
  BAR           = '|';
  SQUOTE        = '\'';
  DQUOTE        = '"';
  SLASH         = '\\';
  DIGITS        = SET OF CHAR {'0'.. '9'};
  LETTERS       = SET OF CHAR {'a'.. 'z', 'A'.. 'Z'};
  ALPHANUMERICS = LETTERS + DIGITS;

CONST
  ATOM_CHARS = SET OF
                 CHAR {
                 '!', '#', '$', '%', '&', '*', '+', '-', '.', '/', ':', '<',
                 '=', '>', '?', '@', '[', ']', '^', '_', '{', '}', '~'};
  ID_CHARS = ALPHANUMERICS + SET OF CHAR {'_'};

PROCEDURE SxPrint (wr       : Wr.T;
                   sx       : Sx.T;
                   maxDepth : CARDINAL := LAST(CARDINAL);
                   maxLength: CARDINAL := LAST(CARDINAL)  )
  RAISES {Sx.PrintError} =
  <* FATAL Wr.Failure, Thread.Alerted *>
  CONST
    DEPTH_ELLIPSIS  = "...";
    LENGTH_ELLIPSIS = "...";
  BEGIN
    TYPECASE sx OF
    | NULL => Wr.PutText(wr, "()")
    | REF INTEGER (r) => Wr.PutText(wr, Fmt.Int(r^))
    | REF CHAR (r) =>
        Wr.PutChar(wr, SQUOTE);
        SxPrintChar(wr, r^, SQUOTE);
        Wr.PutChar(wr, SQUOTE)
    | REF REAL (r) =>
        (* Wr.PutText (wr, Fmt.Real (r^, modula := TRUE)) *)
        Wr.PutText(wr, Fmt.Real(r^, Fmt.Style.Auto, literal := TRUE))
    | REF LONGREAL (r) =>
        (* Wr.PutText (wr, Fmt.LongReal (r^, modula := TRUE)) *)
        Wr.PutText(wr, Fmt.LongReal(r^, Fmt.Style.Auto, literal := TRUE))
    | REF EXTENDED (r) =>
        (* Wr.PutText (wr, Fmt.Extended (r^, modula := TRUE)) *)
        Wr.PutText(wr, Fmt.Extended(r^, Fmt.Style.Auto, literal := TRUE))
    | TEXT (t) =>
        Wr.PutChar(wr, DQUOTE);
        FOR i := 0 TO Text.Length(t) - 1 DO
          SxPrintChar(wr, Text.GetChar(t, i), DQUOTE)
        END;
        Wr.PutChar(wr, DQUOTE)
    | Aatom.T (a) =>
        VAR name := Aatom.ToText(a);
        BEGIN
          IF NeedsBars(name) THEN
            Wr.PutChar(wr, BAR);
            FOR i := 0 TO Text.Length(name) - 1 DO
              SxPrintChar(wr, Text.GetChar(name, i), BAR)
            END;
            Wr.PutChar(wr, BAR)
          ELSE
            Wr.PutText(wr, name)
          END
        END
    | RefList.T (list) =>
        IF maxDepth = 0 THEN
          Wr.PutText(wr, DEPTH_ELLIPSIS)
        ELSE
          VAR len := maxLength;
          BEGIN
            Wr.PutChar(wr, '(');
            DEC(maxDepth);
            LOOP
              SxPrint(wr, list.head, maxDepth, maxLength);
              list := list.tail;
              IF list = NIL THEN EXIT END;
              Wr.PutChar(wr, ' ');
              IF len = 0 THEN Wr.PutText(wr, LENGTH_ELLIPSIS); EXIT END;
              DEC(len)
            END;
            Wr.PutChar(wr, ')')
          END
        END
    ELSE
      Wr.PutText(wr, Fmt.F("%s<0x%s>", RTTypeSRC.TypeName(sx),
                           Fmt.Unsigned(LOOPHOLE(sx, Word.T))));
    END
  END SxPrint;

PROCEDURE SxPrintChar (wr: Wr.T; ch: CHAR; delim: CHAR)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF ch = '\n' THEN
      Wr.PutText (wr, "\\n")
    ELSIF ch = '\t' THEN
      Wr.PutText (wr, "\\t")
    ELSIF ch = '\r' THEN
      Wr.PutText (wr, "\\r")
    ELSIF ch = '\f' THEN
      Wr.PutText (wr, "\\f")
    ELSIF ch = SLASH THEN
      Wr.PutText (wr, "\\\\")
    ELSIF ch = delim THEN
      Wr.PutChar (wr, SLASH);
      Wr.PutChar (wr, ch)
    ELSIF ISO_Latin_printing (ch) THEN
      Wr.PutText (wr, Text.FromChar (ch))
    ELSE
      Wr.PutText (wr, Fmt.F ("\\%03s", Fmt.Int (ORD (ch), 8)))
    END
  END SxPrintChar;

PROCEDURE ISO_Latin_printing (ch: CHAR): BOOLEAN =
  BEGIN
    RETURN ' ' <= ch AND ch <= '~' OR '\241' <= ch AND ch <= '\377'
  END ISO_Latin_printing;

PROCEDURE NeedsBars (t: TEXT): BOOLEAN =
  VAR
    len       := Text.Length (t);
    c  : CHAR;
  BEGIN
    IF len = 0 THEN RETURN TRUE END; (* || *)
    c := Text.GetChar (t, 0);
    IF c IN LETTERS THEN
      FOR i := 1 TO len - 1 DO
        c := Text.GetChar (t, i);
        IF NOT c IN ID_CHARS THEN RETURN TRUE END
      END;
      RETURN FALSE
    ELSIF c IN ATOM_CHARS THEN
      FOR i := 1 TO len - 1 DO
        c := Text.GetChar (t, i);
        IF NOT c IN ATOM_CHARS THEN RETURN TRUE END
      END;
      RETURN FALSE
    ELSE
      RETURN TRUE
    END
  END NeedsBars;


(*---------------------------------------------------------------------------*)
      
BEGIN
  InitSyntax ();
END SLisp.
