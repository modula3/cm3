(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Apr 26 15:48:17 PDT 1996 by heydon                   *)

MODULE NonLinearTest EXPORTS Main;

(* Application to test the Newton-Raphson solver implemented in
   juno-machine/src/NonLinearSolve.m3. This program reads the description of 0
   or more systems to solve from standard input. Comment-lines and blank lines
   in the input are ignored. A comment-line has '#' as its first
   non-whitespace character.

   Each system in the input takes the following form:

|    m n [ g ]
|    <var-init-1>     \                     \
|    ...               > True Variables      \
|    <var-init-j>     /                       \
|    <var-init-1>     \                        > m variables total
|    ...               > Ghost Variables      /
|    <var-init-g>     /                      /
|    <constraint-1>   \                      \
|    ...               > Ghost Constraints    \
|    <constraint-g>   /                        \
|    <constraint-1>   \                         > n constraints total
|    ...               > True Constraints      /
|    <constraint-k>   /                       /

   where j+g = m is the total number of variables, and k+g = n is the total
   number of constraints. Hence, we must have g < m AND g < n.

   The first line contains the number of variables, the number of constraints,
   and the number of ghost variables and constraints, respectively. The number
   of ghost variables/constraints defaults to zero. Each <var-init-i> takes
   the form:

|    var [ ~ value ]

   where "var" is a variable name, and "value" is a real number. Variable
   names must be strings of alphabetic characters only. The first "m"
   variables are the true variables, and the remaining "g" variables are the
   ghost variables. The solver ignores any initial values associated with
   ghost variables, so this program warns if any ghost variables are hinted.

   Each <constraint-i> takes one of the following forms:

|    x = PLUS(y, z)
|    x = MINUS(y, z)
|    x = TIMES(y, z)
|    x = ATAN(y, z)
|    x = SIN(y)
|    x = COS(y)
|    x = EXP(y)

   where "x", "y", and "z" are either real numbers or variables named in one
   of the <var-init-i> lines. The first "g" constraints are the ghost
   constraints, and the remaining "k" constraints are the true constraints.
   The restriction on the ghost constraints is that each "x" must be a ghost
   variable, and each "y" or "z" on the right-hand side of a ghost constraint
   must be either a literal (constant), a true variable, or a ghost variable
   appearing on the left-hand side of a previous ghost constraint.

   For each system in the input, the program prints the input system as a Juno
   command, and then an indication as to whether the non-linear solver could
   find a solution to the system. If it finds a solution, the program prints
   the final value for each of the variables. *)

IMPORT RedundantSolve, NonLinearSolveRep;
FROM JunoValue IMPORT Real;
IMPORT Rd, Wr, Fmt, TextRd, Atom, Lex, FloatMode;
FROM Stdio IMPORT stdin, stdout, stderr;
FROM Thread IMPORT Alerted;

<* FATAL Wr.Failure, Alerted *>

TYPE T = Real;

CONST IdChars = SET OF CHAR { 'a'..'z', 'A'..'Z' };

TYPE
  System = REF RECORD
    gCnt: CARDINAL;
    name: REF ARRAY OF Atom.T;
    value: REF ARRAY OF T;
    con: REF ARRAY OF RedundantSolve.Constraint
  END;
  (* The system has "NUMBER(name^)" variables. The name of the variable with
     index "i" is "name[i]". The first "NUMBER(name^)" entries in "value"
     correspond to the initial values for the variables; the remaining entries
     in "value" are constants. The constraints in "con" must all refer to
     values in the "value" array. *)

EXCEPTION
  BadNum;                  (* raised if a number is malformed *)
  BadVar(Atom.T);          (* raised if undeclared variable is used *)
  BadCon(Atom.T);          (* raised if a bad ghost constraint is declared *)

VAR lineNum := 0;

PROCEDURE Error(msg: TEXT) =
  BEGIN
    Wr.PutText(stderr, "Error, line " & Fmt.Int(lineNum) & ": ");
    Wr.PutText(stderr, msg & "\n");
    Wr.Flush(stderr)
  END Error;

PROCEDURE GetLine(rd: Rd.T): Rd.T RAISES {Rd.EndOfFile} =
(* Return a reader on the first line of "rd" (after skipping whitespace) that
   is not a comment-line or a blank line. *)
  <* FATAL Rd.Failure *>
  VAR res: Rd.T; BEGIN
    LOOP
      res := TextRd.New(Rd.GetLine(rd));
      INC(lineNum);
      Lex.Skip(res);
      IF NOT Rd.EOF(res) THEN
        VAR c := Rd.GetChar(res); BEGIN
          Rd.UnGetChar(res);
          IF c # '#' THEN EXIT END
        END
      END
    END;
    RETURN res
  END GetLine;

EXCEPTION NoMoreInput;

PROCEDURE GetMNG(rd: Rd.T; VAR (*OUT*) m, n, g: CARDINAL)
  RAISES {NoMoreInput, BadNum, Rd.Failure, FloatMode.Trap, Lex.Error} =
  VAR rd2: Rd.T; BEGIN
    TRY rd2 := GetLine(rd) EXCEPT
      Rd.EndOfFile => RAISE NoMoreInput
    END;
    m := Lex.Int(rd2);
    n := Lex.Int(rd2);
    Lex.Skip(rd2);
    IF Rd.EOF(rd2)
      THEN g := 0
      ELSE g := Lex.Int(rd2)
    END;
    IF m < 1 OR n < 1 OR g > MIN(m, n) THEN RAISE BadNum END
  END GetMNG;

PROCEDURE ReadSystem(rd: Rd.T): System
  RAISES {NoMoreInput, Rd.EndOfFile, Rd.Failure, Lex.Error,
          BadNum, BadVar, BadCon} =
  VAR res := NEW(System); m, n, g: CARDINAL; BEGIN
    TRY
      GetMNG(rd, m, n, g);
      res.gCnt := g;
      res.name := NEW(REF ARRAY OF Atom.T, m);
      res.value := NEW(REF ARRAY OF T, m + 2 * n);
      res.con := NEW(REF ARRAY OF RedundantSolve.Constraint, n);
      ReadVars(rd, res);
      ReadConstraints(rd, res);
    EXCEPT
      FloatMode.Trap => RAISE BadNum
    END;
    RETURN res
  END ReadSystem;

PROCEDURE ReadVars(rd: Rd.T; VAR (*INOUT*) sys: System)
    RAISES {Rd.EndOfFile, Rd.Failure, FloatMode.Trap, Lex.Error} =
(* Requires that "sys.gCnt" has been set, and that "sys.name" has been
   allocated. This procedure fills in the "sys.name" array and the values for
   the true variables in the "sys.value" array. *)
  VAR rd2: Rd.T; trueCnt := NUMBER(sys.name^) - sys.gCnt; BEGIN
    FOR i := 0 TO NUMBER(sys.name^) - 1 DO
      rd2 := GetLine(rd);
      sys.name[i] := Atom.FromText(Lex.Scan(rd2, cs := IdChars));
      Lex.Skip(rd2);
      IF i < trueCnt THEN
        (* true variable *)
        Lex.Match(rd2, "~"); Lex.Skip(rd2);
        sys.value[i] := Lex.Real(rd2)
      ELSIF NOT Rd.EOF(rd2) THEN
        Wr.PutText(stderr, "Warning: Ignoring hint for ghost variable \"");
        Wr.PutText(stderr, Atom.ToText(sys.name[i]));
        Wr.PutText(stderr, "\"\n");
        Wr.Flush(stderr)
      END
    END
  END ReadVars;

TYPE Kind = {Id, Real};

PROCEDURE RdToken(rd: Rd.T; VAR (*OUT*) a: Atom.T; VAR (*OUT*) val: REAL): Kind
  RAISES {Rd.EndOfFile, Rd.Failure, FloatMode.Trap, Lex.Error} =
(* Reads either the next identifier or real number from "rd". If the next
   token on the stream is an identifier, sets "a" to that identifier and
   returns "Kind.Id". If the next token on the stream is a real number, sets
   "val" to that number and returns "Kind.Real". *)
  VAR c: CHAR; BEGIN
    Lex.Skip(rd);
    c := Rd.GetChar(rd);
    Rd.UnGetChar(rd);
    IF c IN IdChars THEN
      a := Atom.FromText(Lex.Scan(rd, cs := IdChars));
      RETURN Kind.Id
    ELSE
      val := Lex.Real(rd);
      RETURN Kind.Real
    END
  END RdToken;

VAR (*CONST*)
  Plus, Minus, Times, Atan, Sin, Cos, Exp: Atom.T;

PROCEDURE ReadConstraints(rd: Rd.T; VAR (*INOUT*) sys: System)
  RAISES {Rd.EndOfFile, Rd.Failure, FloatMode.Trap, Lex.Error, BadVar, BadCon}=
(* Requires that "sys.gCnt" has been set, that "sys.name" has been allocated
   and filled with variable names, and that "sys.con" has been allocated. This
   procedure fills in the "sys.con" array with ghost and true constraints, and
   fills in the "sys.value" array with constant values. *)
  VAR next := NUMBER(sys.name^);

  PROCEDURE RdElt(rd: Rd.T): CARDINAL
    RAISES {Rd.EndOfFile, Rd.Failure, FloatMode.Trap, Lex.Error, BadVar} =
  (* Returns the index in "sys.value" for the next token on the input. *)
    VAR nm: Atom.T; val: REAL; res: CARDINAL; BEGIN
      CASE RdToken(rd, nm, val) OF
        Kind.Id =>
          res := 0;
          WHILE res < NUMBER(sys.name^) AND nm # sys.name[res] DO INC(res) END;
          IF res = NUMBER(sys.name^) THEN RAISE BadVar(nm) END
      | Kind.Real =>
          sys.value[next] := val;
          res := next;
          INC(next)
      END;
      RETURN res
    END RdElt;

  VAR
    nm: Atom.T; val: REAL; arg0, argc: CARDINAL; rd2: Rd.T;
    trueCnt := NUMBER(sys.name^) - sys.gCnt;
    gvarDefined := NEW(REF ARRAY OF BOOLEAN, sys.gCnt);

  PROCEDURE CheckGhostConstraint(arg: RedundantSolve.Args; argc: CARDINAL)
      RAISES {BadCon} =
  (* Verify that the arguments of "arg" with "argc" right-hand arguments are a
     valid ghost constraint; if not, raise the exception "BadCon". *)
    BEGIN
      IF arg[0] >= NUMBER(sys.name^) THEN
        RAISE BadCon(Atom.FromText(Fmt.Real(sys.value[arg[0]])))
      END;
      IF arg[0] < trueCnt OR                (* LHS not a ghost variable *)
         gvarDefined[arg[0] - trueCnt] THEN (* ghost var already defined *)
        RAISE BadCon(sys.name[arg[0]])
      END;
      FOR i := 1 TO argc DO
        IF arg[i] >= trueCnt AND arg[i] < NUMBER(sys.name^)
           AND NOT gvarDefined[arg[i] - trueCnt] THEN
          RAISE BadCon(sys.name[arg[0]])
        END
      END
    END CheckGhostConstraint;

  (* ReadConstraints *)
  BEGIN
    FOR i := 0 TO sys.gCnt - 1 DO
      gvarDefined[i] := FALSE
    END;
    FOR i := 0 TO NUMBER(sys.con^) - 1 DO
      rd2 := GetLine(rd);
      arg0 := RdElt(rd2);
      Lex.Skip(rd2); Lex.Match(rd2, "=");
      IF RdToken(rd2, nm, val) # Kind.Id THEN RAISE Lex.Error END;
      IF    nm = Plus  THEN sys.con[i] := RedundantSolve.NewPlus();  argc := 2
      ELSIF nm = Minus THEN sys.con[i] := RedundantSolve.NewMinus(); argc := 2
      ELSIF nm = Times THEN sys.con[i] := RedundantSolve.NewTimes(); argc := 2
      ELSIF nm = Atan  THEN sys.con[i] := RedundantSolve.NewAtan();  argc := 2
      ELSIF nm = Sin   THEN sys.con[i] := RedundantSolve.NewSin();   argc := 1
      ELSIF nm = Cos   THEN sys.con[i] := RedundantSolve.NewCos();   argc := 1
      ELSIF nm = Exp   THEN sys.con[i] := RedundantSolve.NewExp();   argc := 1
      ELSE RAISE Lex.Error
      END;
      WITH arg = sys.con[i].arg DO
        arg[0] := arg0;
        Lex.Skip(rd2); Lex.Match(rd2, "(");
        arg[1] := RdElt(rd2);
        IF argc > 1 THEN
          Lex.Skip(rd2); Lex.Match(rd2, ",");
          arg[2] := RdElt(rd2);
        END;
        Lex.Skip(rd2); Lex.Match(rd2, ")");
        IF i < sys.gCnt THEN
          CheckGhostConstraint(arg, argc);
          gvarDefined[arg[0]-trueCnt] := TRUE
        END
      END
    END
  END ReadConstraints;

PROCEDURE Solve(sys: System) =
(* Solve the constraints "sys.con" for the variable values
   "sys.value[0..NUMBER(sys.name^)-1]". The constraints
   must contain indices in the range "[0..NUMBER(sys.name^)-1]".
   This procedure also has the side-effect of writing a description of the
   input system and solution to standard output. *)
  VAR num := NUMBER(sys.name^); BEGIN
    Wr.PutText(stdout, "\nInput System:\n");
    Wr.PutText(stdout, "  VAR\n");
    WriteVars(sys.name^, sys.value^, num - sys.gCnt);
    Wr.PutText(stdout, "  IN\n");
    WriteConstraints(sys.name^, sys.value^, sys.con^);
    Wr.PutText(stdout, "  -> SKIP\n  END\n");
    Wr.Flush(stdout);
    IF RedundantSolve.P(num - sys.gCnt, num, sys.value^, sys.con^) THEN
      Wr.PutText(stdout, "Solution Found:\n");
      WriteSolution(sys.name^, sys.value^);
    ELSE
      Wr.PutText(stdout, "No Solution Found!\n");
    END;
    Wr.Flush(stdout);
  END Solve;

PROCEDURE WriteVars(
    READONLY nm: ARRAY OF Atom.T;
    READONLY v: ARRAY OF T;
    trueCnt: CARDINAL) =
  VAR n := NUMBER(nm); BEGIN
    FOR i := 0 TO n-1 DO
      Wr.PutText(stdout, "    ");
      Wr.PutText(stdout, Atom.ToText(nm[i]));
      IF i < trueCnt THEN
        Wr.PutText(stdout, " ~ " & Fmt.Real(v[i]))
      END;
      IF i < n - 1 THEN Wr.PutText(stdout, ",") END;
      Wr.PutChar(stdout, '\n')
    END
  END WriteVars;

PROCEDURE WriteConstraints(
    READONLY nm: ARRAY OF Atom.T; 
    READONLY v: ARRAY OF T;
    READONLY c: ARRAY OF RedundantSolve.Constraint) =
  VAR n := NUMBER(nm);			 (* number of variables *)
  PROCEDURE Val(i: CARDINAL): TEXT =
    BEGIN
      IF i < n
        THEN RETURN Atom.ToText(nm[i])
        ELSE RETURN Fmt.Real(v[i])
      END
    END Val;
  VAR last := NUMBER(c) - 1; BEGIN
    FOR i := 0 TO last DO
      VAR arg := c[i].arg; BEGIN
        Wr.PutText(stdout, "    " & Val(arg[0]) & " = ");
        CASE c[i].type OF <* NOWARN *>
        | NonLinearSolveRep.ConType.Plus =>
            Wr.PutText(stdout, Val(arg[1]) & " + " & Val(arg[2]))
        | NonLinearSolveRep.ConType.Minus =>
            Wr.PutText(stdout, Val(arg[1]) & " - " & Val(arg[2]))
        | NonLinearSolveRep.ConType.Times =>
            Wr.PutText(stdout, Val(arg[1]) & " * " & Val(arg[2]))
        | NonLinearSolveRep.ConType.Atan =>
            Wr.PutText(stdout, "ATAN(" & Val(arg[1]) &", "& Val(arg[2]) & ")")
        | NonLinearSolveRep.ConType.Sin =>
            Wr.PutText(stdout, "SIN(" & Val(arg[1]) & ")")
        | NonLinearSolveRep.ConType.Cos =>
            Wr.PutText(stdout, "COS(" & Val(arg[1]) & ")")
        | NonLinearSolveRep.ConType.Exp =>
            Wr.PutText(stdout, "EXP(" & Val(arg[1]) & ")")
        END;
        IF i < last THEN Wr.PutText(stdout, " AND") END;
        Wr.PutChar(stdout, '\n')
      END
    END
  END WriteConstraints;

PROCEDURE WriteSolution(READONLY nm: ARRAY OF Atom.T; READONLY v: ARRAY OF T) =
  BEGIN
    FOR i := 0 TO LAST(nm) DO
      Wr.PutText(stdout, "  ");
      Wr.PutText(stdout, Atom.ToText(nm[i]));
      Wr.PutText(stdout, " = " & Fmt.Real(v[i]) & "\n");
    END;
  END WriteSolution;

BEGIN
  (* initialize atoms used by ReadConstraints *)
  Plus  := Atom.FromText("PLUS");
  Minus := Atom.FromText("MINUS");
  Times := Atom.FromText("TIMES");
  Atan  := Atom.FromText("ATAN");
  Sin   := Atom.FromText("SIN");
  Cos   := Atom.FromText("COS");
  Exp   := Atom.FromText("EXP");
  TRY
    LOOP
      Solve(ReadSystem(stdin))
    END
  EXCEPT
    NoMoreInput =>  (* SKIP *)
  | Rd.EndOfFile => Error("premature end-of-file in input")
  | Rd.Failure =>   Error("reading from standard input")
  | BadNum =>       Error("malformed integer or real in input")
  | Lex.Error =>    Error("malformed input")
  | BadVar (nm) =>  Error("illegal variable \"" & Atom.ToText(nm) & "\"")
  | BadCon (nm) =>  Error("bad ghost constraint for variable \"" &
                      Atom.ToText(nm) & "\"")
  END
END NonLinearTest.
