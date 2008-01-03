(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Apr 29 14:43:32 PDT 1996 by heydon                   *)

MODULE SolveTest EXPORTS Main;

IMPORT RTVal, JunoSolve, JunoValue;
IMPORT Sx, Atom, Rd, Wr, Thread, AtomRefTbl, AtomSeq, RefSeq, RefList, Lex;
FROM Stdio IMPORT stdin, stdout, stderr;

<* FATAL Wr.Failure, Thread.Alerted *>

(* SYNTAX
  
   SolveTest

   DESCRIPTION

   Application to test the Juno run-time solver implemented in the module
   juno-machine/src/JunoSolve.m3. The program reads the description of 0 or
   more systems to solve from standard input, and writes the input system and
   its solution (if any) to standard output.

   Each constraint system is specified by a symbolic expression as defined by
   the "Sx" interface. Each system in the input takes the following form:

|    ((vars
|      	<var-init-1>
|      	...
|      	<var-init-m>)
|     (constraints
|       <constraint-1>
|       ...
|       <constraint-n>))

   where each <var-init-i> has the form:

|    (id [ ~ literal ])

   or

|    (id [ = literal ])

   and each <constraint-j> takes one of the following forms:

|    (= x y)          ;; x = y
|    (cons x y z)     ;; x = (y, z)
|    (plus x y z)     ;; x = y + z
|    (times x y z)    ;; x = y * z
|    (atan x y z)     ;; x = ATAN(y, z)
|    (sin x y)        ;; x = SIN(y)
|    (cos x y)        ;; x = COS(y)
|    (exp x y)        ;; x = EXP(y)
|    (real x)         ;; REAL(x)
|    (text x)         ;; TEXT(x)

   where "x", "y", and "z" are either variables named in one of the
   <var-init-i> lines or literal values as defined below.

   A "literal" must be of the form: 

|    <literal> ::= NIL | "text" | number | (<literal> <literal>)

   A "number" is a "Float" as defined in the Modula-3 "Lex" module.
*)

TYPE
  System = REF RECORD
    names: AtomSeq.T;      (* the sequence of variable names (in order) *)
    vars: AtomRefTbl.T;    (* maps variable names -> JunoSolve.Var's *)
    cons: RefSeq.T;     (* sequence of constraints *)
  END;

EXCEPTION ParseError(TEXT);

VAR
  VarsSym        := Atom.FromText("vars");
  ConstraintsSym := Atom.FromText("constraints");
  NearSym        := Atom.FromText("~");
  EqualSym       := Atom.FromText("=");
  ConsSym        := Atom.FromText("cons");
  PlusSym        := Atom.FromText("plus");
  TimesSym       := Atom.FromText("times");
  AtanSym        := Atom.FromText("atan");
  SinSym         := Atom.FromText("sin");
  CosSym         := Atom.FromText("cos");
  ExpSym         := Atom.FromText("exp");
  RealSym        := Atom.FromText("real");
  TextSym        := Atom.FromText("text");
  NilSym         := Atom.FromText("NIL");

PROCEDURE PrintError(msg: TEXT) =
  BEGIN
    Wr.PutText(stderr, "Fatal error: " & msg & "\n");
    Wr.Flush(stderr)
  END PrintError;

PROCEDURE ParseLabel(sx: Sx.T; nm: Atom.T): RefList.T RAISES {ParseError} =
(* Requires "sx" to be a list whose first element is the atom "nm". Returns
   the tail of the list. Raises "ParseError" if "sx" is not a list or if its
   head is not the atom "nm". *)
  BEGIN
    TYPECASE sx OF RefList.T (l) =>
      TYPECASE l.head OF Atom.T (h) =>
        IF h = nm THEN RETURN l.tail
        ELSE RAISE ParseError("expected atom \"" & Atom.ToText(nm) & "\"")
        END
      ELSE RAISE ParseError("expected atom at head of list") END
    ELSE RAISE ParseError("expected a list") END
  END ParseLabel;

PROCEDURE ParseLiteral(sx: Sx.T): RTVal.T RAISES {ParseError} =
  BEGIN
    TYPECASE sx OF
      TEXT (t) => RETURN RTVal.FromText(t)
    | REF INTEGER (i) => RETURN RTVal.FromInt(i^)
    | REF REAL (r) => RETURN RTVal.FromReal(r^)
    | Atom.T (a) =>
        IF a = NilSym THEN RETURN RTVal.nil
        ELSE
          RAISE ParseError("illegal literal atom \"" & Atom.ToText(a) & "\"")
        END
    | RefList.T (l) =>
        IF RefList.Length(l) = 2 THEN
          VAR car := l.head; cdr := l.tail.head; BEGIN
            RETURN RTVal.FromPair(ParseLiteral(car), ParseLiteral(cdr))
          END
        ELSE RAISE ParseError("pair literal does not contain 2 elements") END
    ELSE RAISE ParseError("illegal literal value") END
  END ParseLiteral;

PROCEDURE ParseVarDecl((*INOUT*) s: System; sx: Sx.T) RAISES {ParseError} =
  BEGIN
    TYPECASE sx OF RefList.T (l) =>
      VAR len := RefList.Length(l); known := FALSE; val: RTVal.T := NIL; BEGIN
        IF len = 1 OR len = 3 THEN
          TYPECASE l.head OF Atom.T (nm) =>
            IF nm = NilSym THEN
              RAISE ParseError("NIL not allowed as a variable name")
            END;
            IF len > 1 THEN
              WITH l2 = l.tail.head DO
                IF l2 = NearSym OR l2 = EqualSym THEN
                  known := (l2 = EqualSym)
                ELSE RAISE ParseError("var decl must contain `=' or `~'") END
              END;
              val := ParseLiteral(l.tail.tail.head);
            END;
            s.names.addhi(nm);
            IF s.vars.put(nm, JunoSolve.New(known, val)) THEN
              RAISE ParseError("variable declared multiple times")
            END
          ELSE RAISE ParseError("var decl does not begin with a name") END
        ELSE RAISE ParseError("var decl has bad length") END
      END
    ELSE RAISE ParseError("var decl is not a list") END
  END ParseVarDecl;

PROCEDURE ReadVars((*INOUT*) s: System; sx: Sx.T) RAISES {ParseError} =
  VAR vars := ParseLabel(sx, VarsSym); BEGIN
    WHILE vars # NIL DO
      ParseVarDecl(s, vars.head);
      vars := vars.tail
    END
  END ReadVars;

PROCEDURE ResolveArg(s: System; sx: Sx.T): JunoSolve.Var
    RAISES {ParseError} =
(* "sx" is either a variable name (which must already be defined in "s") or a
   literal. In the former case, return the associated "JunoSolve.Var" from
   "s.vars". In the latter case, allocate and return a new, fixed var. *)
  VAR ref: REFANY; BEGIN
    TYPECASE sx OF Atom.T (nm) =>
      IF nm # NilSym THEN
        IF s.vars.get(nm, (*OUT*) ref) THEN RETURN ref
        ELSE RAISE ParseError("unknown variable \""
          & Atom.ToText(nm) & "\" in constraint")
        END
      END
    ELSE (*SKIP*)
    END;
    RETURN JunoSolve.New(known := TRUE, val := ParseLiteral(sx))
  END ResolveArg;

PROCEDURE ParseArgs1(s: System; args: RefList.T;
    VAR (*OUT*) arg1: JunoSolve.Var) RAISES {ParseError} =
  BEGIN
    IF RefList.Length(args) = 1 THEN
      arg1 := ResolveArg(s, args.head)
    ELSE RAISE ParseError("incorrect number of arguments to constraint")
    END
  END ParseArgs1;

PROCEDURE ParseArgs2(s: System; args: RefList.T;
    VAR (*OUT*) arg1, arg2: JunoSolve.Var) RAISES {ParseError} =
  BEGIN
    IF RefList.Length(args) = 2 THEN
      arg1 := ResolveArg(s, args.head);
      arg2 := ResolveArg(s, args.tail.head)
    ELSE RAISE ParseError("incorrect number of arguments to constraint")
    END
  END ParseArgs2;

PROCEDURE ParseArgs3(s: System; args: RefList.T;
    VAR (*OUT*) arg1, arg2, arg3: JunoSolve.Var) RAISES {ParseError} =
  BEGIN
    IF RefList.Length(args) = 3 THEN
      arg1 := ResolveArg(s, args.head);
      arg2 := ResolveArg(s, args.tail.head);
      arg3 := ResolveArg(s, args.tail.tail.head)
    ELSE RAISE ParseError("incorrect number of arguments to constraint")
    END
  END ParseArgs3;

PROCEDURE ResolveConstraint(s: System; nm: Atom.T; args: RefList.T):
    JunoSolve.Constraint RAISES {ParseError} =
  VAR arg1, arg2, arg3: JunoSolve.Var; BEGIN
    IF nm = EqualSym THEN
      ParseArgs2(s, args, (*OUT*) arg1, (*OUT*) arg2);
      RETURN JunoSolve.NewEqual(arg1, arg2)
    ELSIF nm = ConsSym THEN
      ParseArgs3(s, args, (*OUT*) arg1, (*OUT*) arg2, (*OUT*) arg3);
      RETURN JunoSolve.NewCons(arg1, arg2, arg3)
    ELSIF nm = PlusSym THEN
      ParseArgs3(s, args, (*OUT*) arg1, (*OUT*) arg2, (*OUT*) arg3);
      RETURN JunoSolve.NewPlus(arg1, arg2, arg3)
    ELSIF nm = TimesSym THEN
      ParseArgs3(s, args, (*OUT*) arg1, (*OUT*) arg2, (*OUT*) arg3);
      RETURN JunoSolve.NewTimes(arg1, arg2, arg3)
    ELSIF nm = AtanSym THEN
      ParseArgs3(s, args, (*OUT*) arg1, (*OUT*) arg2, (*OUT*) arg3);
      RETURN JunoSolve.NewAtan(arg1, arg2, arg3)
    ELSIF nm = SinSym THEN
      ParseArgs2(s, args, (*OUT*) arg1, (*OUT*) arg2);
      RETURN JunoSolve.NewSin(arg1, arg2)
    ELSIF nm = CosSym THEN
      ParseArgs2(s, args, (*OUT*) arg1, (*OUT*) arg2);
      RETURN JunoSolve.NewCos(arg1, arg2)
    ELSIF nm = ExpSym THEN
      ParseArgs2(s, args, (*OUT*) arg1, (*OUT*) arg2);
      RETURN JunoSolve.NewExp(arg1, arg2)
    ELSIF nm = RealSym THEN
      ParseArgs1(s, args, (*OUT*) arg1);
      RETURN JunoSolve.NewReal(arg1)
    ELSIF nm = TextSym THEN
      ParseArgs1(s, args, (*OUT*) arg1);
      RETURN JunoSolve.NewText(arg1)
    ELSE
      RAISE ParseError("unknown constraint type \"" & Atom.ToText(nm) & "\"")
    END
  END ResolveConstraint;

PROCEDURE ParseConstraint((*INOUT*) s: System; sx: Sx.T) RAISES {ParseError} =
  BEGIN
    TYPECASE sx OF RefList.T (l) =>
      TYPECASE l.head OF Atom.T (nm) =>
        s.cons.addhi(ResolveConstraint(s, nm, l.tail))
      ELSE RAISE ParseError("constraint spec does not start with an atom") END
    ELSE RAISE ParseError("constraint spec is not a list") END
  END ParseConstraint;

PROCEDURE ReadConstraints((*INOUT*) s: System; sx: Sx.T) RAISES {ParseError} =
  VAR cons := ParseLabel(sx, ConstraintsSym); BEGIN
    WHILE cons # NIL DO
      ParseConstraint(s, cons.head);
      cons := cons.tail
    END
  END ReadConstraints;

PROCEDURE ReadSystem(sx: Sx.T): System RAISES {ParseError} =
  VAR
    res := NEW(System, names := NEW(AtomSeq.T).init(),
      vars := NEW(AtomRefTbl.Default).init(), cons := NEW(RefSeq.T).init());
  BEGIN
    TYPECASE sx OF RefList.T (l) =>
      IF RefList.Length(l) = 2 THEN
        ReadVars((*INOUT*) res, l.head);
        ReadConstraints((*INOUT*) res, l.tail.head)
      ELSE RAISE ParseError("top-level system is not two lists") END
    ELSE RAISE ParseError("top-level system is not a list") END;
    RETURN res
  END ReadSystem;

PROCEDURE Solve(READONLY s: System) =
(* Invoke the solver and print out the variable values in the
   event of a solution. *)
  VAR cons := NEW(JunoSolve.Constraints, s.cons.size()); BEGIN
    (* copy "s.cons" into "cons" *)
    FOR i := 0 TO NUMBER(cons^) - 1 DO
      cons[i] := s.cons.get(i)
    END;
    IF JunoSolve.P(cons^) THEN
      Wr.PutText(stdout, "Solution found:\n");
      FOR i := 0 TO s.names.size() - 1 DO
        VAR nm := s.names.get(i); ref: REFANY; BEGIN
          Wr.PutText(stdout, "  " & Atom.ToText(nm) & " = ");
          VAR inTbl := s.vars.get(nm, (*OUT*) ref); BEGIN
            <* ASSERT inTbl *> END;
          TYPECASE ref OF <*NOWARN*> JunoSolve.Var (var) =>
            JunoValue.Unparse(stdout, RTVal.ToJV(var.val));
            Wr.PutText(stdout, "\n");
          END;
        END
      END
    ELSE
      Wr.PutText(stdout, "No solution found!\n")
    END;
    Wr.PutText(stdout, "\n");
    Wr.Flush(stdout);
  END Solve;

BEGIN
  TRY
    WHILE NOT Rd.EOF(stdin) DO
      Solve(ReadSystem(Sx.Read(stdin)));
      Lex.Skip(stdin)
    END
  EXCEPT
  | ParseError (msg) => PrintError(msg)
  | Sx.ReadError (msg) => PrintError(msg)
  | Rd.Failure => PrintError("failure reading from standard input")
  | Rd.EndOfFile => PrintError("unexpected end-of-file in input")
  END
END SolveTest.
