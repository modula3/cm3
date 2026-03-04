(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

(* ESC/Modula-3 -- Extended Static Checker for Modula-3.

   This is the CLI entry point.  It uses M3ToolFrame to parse Modula-3
   source files with m3tk, then extracts ESC annotations (SPEC/LOOPINV
   pragmas), generates verification conditions, and checks them with
   the Simplify theorem prover.

   Usage: escm3 -interfaces "Fmt" -PathNames "/usr/lib/m3/pkg"
*)

MODULE ESCMain EXPORTS Main;

IMPORT Process, Stdio, Wr, Fmt, Thread;
IMPORT Atom;
IMPORT M3Context, M3Conventions, M3AST_AS, M3CUnit, M3CFETool,
       M3ToolFrame;
IMPORT M3AST_all; (* this cannot be omitted; it defines the particular
                     revelations for all the AST nodes *)
IMPORT ESCPragma, ESCSpec;
IMPORT ESCDriver;
IMPORT RefList;

<* FATAL Wr.Failure, Thread.Alerted *>

TYPE
  ContextClosure = M3Context.Closure OBJECT
    wr: Wr.T;
    unitCount: INTEGER := 0;
    specCount: INTEGER := 0;
  OVERRIDES
    callback := VisitUnit;
  END;

PROCEDURE VisitUnit(cl: ContextClosure;
                    ut: M3CUnit.Type;
                    name: TEXT;
                    cu: M3AST_AS.Compilation_Unit) =
  VAR
    pragmas: RefList.T;
    unitName: Atom.T;
  BEGIN
    (* Get to the actual instantiated tree for generics *)
    cu := M3CUnit.ToGenIns(cu, ut);

    (* Only process primary sources without errors *)
    IF NOT (M3Conventions.PrimarySource IN cu.fe_status) THEN RETURN END;
    IF M3CUnit.Errors * cu.fe_status # M3CUnit.Status{} THEN
      Wr.PutText(cl.wr, "  [skipping " & name & ": parse errors]\n");
      RETURN;
    END;

    INC(cl.unitCount);
    unitName := Atom.FromText(name);

    (* Extract ESC pragmas *)
    pragmas := ESCPragma.Extract(cu, unitName);

    IF pragmas = NIL THEN
      Wr.PutText(cl.wr, "  " & name & ": no ESC annotations found\n");
      RETURN;
    END;

    (* Print extracted pragmas *)
    Wr.PutText(cl.wr, "  " & name & ":\n");
    VAR p := pragmas;
        raw: ESCSpec.RawPragma;
    BEGIN
      WHILE p # NIL DO
        raw := NARROW(p.head, ESCSpec.RawPragma);
        INC(cl.specCount);
        CASE raw.kind OF
        | ESCSpec.PragmaKind.Spec =>
          Wr.PutText(cl.wr, "    SPEC [" & Fmt.Int(raw.line) & ":" &
                     Fmt.Int(raw.col) & "] " & raw.text & "\n");
        | ESCSpec.PragmaKind.LoopInv =>
          Wr.PutText(cl.wr, "    LOOPINV [" & Fmt.Int(raw.line) & ":" &
                     Fmt.Int(raw.col) & "] " & raw.text & "\n");
        | ESCSpec.PragmaKind.PragmaSpec =>
          (* skip *)
        END;
        p := p.tail;
      END;
    END;

    (* Pass to driver for VCG and proving (Phase 2+) *)
    ESCDriver.ProcessUnit(cu, unitName, pragmas, cl.wr);
  END VisitUnit;

PROCEDURE DoRun(<* UNUSED *> w: M3ToolFrame.Worker;
                c: M3Context.T;
                <* UNUSED *> compileResult: INTEGER): INTEGER =
  VAR
    returnCode: INTEGER;
    cl: ContextClosure;
  BEGIN
    returnCode := M3CFETool.CompileInContext(c);
    IF returnCode < 0 THEN
      Wr.PutText(Stdio.stderr, "escm3: compilation failed\n");
      RETURN returnCode;
    END;

    Wr.PutText(Stdio.stdout, "ESC/Modula-3 -- Extended Static Checker\n");
    Wr.PutText(Stdio.stdout, "Scanning for ESC annotations...\n");

    cl := NEW(ContextClosure, wr := Stdio.stdout);
    <* FATAL ANY *>
    BEGIN
      M3Context.Apply(c, cl);
    END;

    Wr.PutText(Stdio.stdout, "\n" & Fmt.Int(cl.unitCount) &
               " unit(s) processed, " &
               Fmt.Int(cl.specCount) & " annotation(s) found.\n");
    RETURN 0;
  END DoRun;

<* FATAL ANY *>
BEGIN
  WITH res = M3ToolFrame.Startup(
               NEW(M3ToolFrame.Worker, work := DoRun),
               compile := FALSE) DO
    Process.Exit(ABS(res));
  END;
END ESCMain.
