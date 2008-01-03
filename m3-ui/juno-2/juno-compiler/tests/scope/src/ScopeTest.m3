(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Apr 21 17:16:28 PDT 1994 by heydon                   *)

(* This program tests the JunoScope interface and module. It simply builds
   some scopes and attempts to perform name lookups on those scopes. It takes
   not input, and generates its output to Stdio.stdout. *)

MODULE ScopeTest EXPORTS Main;

IMPORT JunoScope, JunoAST, JunoValue;
IMPORT Wr, Text, Atom;
FROM Stdio IMPORT stdout;
FROM Thread IMPORT Alerted;

<* FATAL Wr.Failure, Alerted *>

CONST Tab = 2;

PROCEDURE Search(wr: Wr.T; v,s: TEXT; scp: JunoScope.T; local := FALSE) =
  VAR dot := Text.FindChar(v, '.'); ent: JunoScope.Entity; BEGIN
    Wr.PutText(wr, "Searching ");
    IF local THEN Wr.PutText(wr, "locally ") END;
    Wr.PutText(wr, "for \"" & v & "\" in scope \"" & s & "\":\n");
    IF dot = -1 THEN
      ent := JunoScope.Lookup(scp, Atom.FromText(v), local)
    ELSE
      VAR rt := scp; BEGIN
        WHILE JunoScope.Parent(rt) # NIL DO rt := JunoScope.Parent(rt) END;
        ent := JunoScope.Lookup(rt, Atom.FromText(Text.Sub(v, 0, dot)), FALSE);
        IF ent # NIL THEN
          scp := NARROW(ent, JunoScope.Unit).public_scp;
          ent := JunoScope.Lookup(scp,
            Atom.FromText(Text.Sub(v, dot+1, LAST(CARDINAL))), FALSE)
        END
      END
    END;
    IF ent # NIL THEN
      Wr.PutText(wr, "  " & v & ": ");
      JunoScope.PrintEntity(wr, ent, level := LAST(INTEGER), indent := 2*Tab)
    ELSE
      Wr.PutText(wr, "  Not Found\n")
    END;
    Wr.PutChar(wr, '\n');
    Wr.Flush(wr)
  END Search;

VAR
  (* Scopes *)
  top_pub := JunoScope.New(NIL);
  foo_pub := JunoScope.New(top_pub);
  foo_mod := JunoScope.New(top_pub);
  bar_mod := JunoScope.New(top_pub);
  foo_pred := JunoScope.New(foo_mod);
  foo_func := JunoScope.New(foo_mod);
  foo_proc := JunoScope.New(foo_mod);
  foo_proc_1 := JunoScope.New(foo_proc);
  foo_proc_2a := JunoScope.New(foo_proc_1);
  foo_proc_2b := JunoScope.New(foo_proc_1);
  foo_proc_3a := JunoScope.New(foo_proc_2a);
  foo_proc_3b := JunoScope.New(foo_proc_2b);

  (* Entities *)
  foo_ent := NEW(JunoScope.Mod, public_scp := foo_pub, scp := foo_mod);
  bar_ent := NEW(JunoScope.Mod, public_scp := bar_mod, scp := bar_mod);
  pi := NEW(REF JunoValue.Real);
  foo_pi := NEW(JunoScope.Const, index := 0,
    init := NEW(JunoAST.Number, val := pi));
  foo_syntax := NEW(JunoScope.Var, index := 1, init := JunoAST.NilExpr);
  foo_private := NEW(JunoScope.Var, index := 2, init := JunoAST.NilExpr);
  foo_pred_ent := NEW(JunoScope.Pred, formals := foo_pred,
    index := 0, in_cnt := 2);
  foo_func_ent := NEW(JunoScope.Func, formals := foo_func,
    index := 1, in_cnt := 1);
  foo_proc_ent := NEW(JunoScope.Proc, formals := foo_proc,
    index := 2, in_cnt := 1, inout_cnt := 1, out_cnt := 2);
  pred_1 := NEW(JunoScope.Arg, offset := 0, kind := JunoScope.ArgKind.In);
  pred_2 := NEW(JunoScope.Arg, offset := 1, kind := JunoScope.ArgKind.In);
  func_1 := NEW(JunoScope.Arg, offset := 0, kind := JunoScope.ArgKind.Out);
  func_2 := NEW(JunoScope.Arg, offset := 1, kind := JunoScope.ArgKind.In);
  proc_1 := NEW(JunoScope.Arg, offset := 0, kind := JunoScope.ArgKind.Out);
  proc_2 := NEW(JunoScope.Arg, offset := 1, kind := JunoScope.ArgKind.Out);
  proc_3 := NEW(JunoScope.Arg, offset := 2, kind := JunoScope.ArgKind.InOut);
  proc_4 := NEW(JunoScope.Arg, offset := 3, kind := JunoScope.ArgKind.In);
  proc_5 := NEW(JunoScope.Temp, offset := 4);
  proc_6 := NEW(JunoScope.Temp, offset := 5);
  proc_7 := NEW(JunoScope.Temp, offset := 6);
  proc_8 := NEW(JunoScope.Temp, offset := 7);
  proc_9 := NEW(JunoScope.Temp, offset := 8);
  proc_10 := NEW(JunoScope.Temp, offset := 6);
  proc_11 := NEW(JunoScope.Temp, offset := 7);
  proc_12 := NEW(JunoScope.Temp, offset := 8);

BEGIN
  pi^ := 3.14159;
  TRY
    (* Add bindings to top-level scopes *)
    JunoScope.Bind(top_pub, Atom.FromText("Foo"), foo_ent);
    JunoScope.Bind(top_pub, Atom.FromText("Bar"), bar_ent);
    
    (* Add bindings to Foo public scope *)
    JunoScope.Bind(foo_pub, Atom.FromText("Pi"),     foo_pi);
    JunoScope.Bind(foo_pub, Atom.FromText("Syntax"), foo_syntax);
    JunoScope.Bind(foo_pub, Atom.FromText("Pred"),   foo_pred_ent);
    JunoScope.Bind(foo_pub, Atom.FromText("Func"),   foo_func_ent);
    JunoScope.Bind(foo_pub, Atom.FromText("Proc"),   foo_proc_ent);
    
    (* Add bindings to Foo private scope *)
    JunoScope.Bind(foo_mod, Atom.FromText("Pi"),      foo_pi);
    JunoScope.Bind(foo_mod, Atom.FromText("Syntax"),  foo_syntax);
    JunoScope.Bind(foo_mod, Atom.FromText("Private"), foo_private);
    JunoScope.Bind(foo_mod, Atom.FromText("Func"),    foo_func_ent);
    JunoScope.Bind(foo_mod, Atom.FromText("Proc"),    foo_proc_ent);
    
    (* Add bindings to Bar public/private scope *)
    JunoScope.Bind(bar_mod, Atom.FromText("Pi"),      foo_pi);
    
    (* Add bindings to Foo.Pred *)
    JunoScope.Bind(foo_pred, Atom.FromText("a"), pred_1);
    JunoScope.Bind(foo_pred, Atom.FromText("b"), pred_2);
    
    (* Add bindings to Foo.Func *)
    JunoScope.Bind(foo_func, Atom.FromText("y"), func_1);
    JunoScope.Bind(foo_func, Atom.FromText("x"), func_2);
    
    (* Add bindings to Foo.Proc *)
    JunoScope.Bind(foo_proc, Atom.FromText("c"), proc_1);
    JunoScope.Bind(foo_proc, Atom.FromText("d"), proc_2);
    JunoScope.Bind(foo_proc, Atom.FromText("b"), proc_3);
    JunoScope.Bind(foo_proc, Atom.FromText("a"), proc_4);
    
    (* Add bindings to Foo.Proc projection scopes *)
    JunoScope.Bind(foo_proc_1, Atom.FromText("x"), proc_5);
    JunoScope.Bind(foo_proc_1, Atom.FromText("y"), proc_6);
    JunoScope.Bind(foo_proc_2a, Atom.FromText("z"), proc_7);
    JunoScope.Bind(foo_proc_3a, Atom.FromText("w"), proc_8);
    JunoScope.Bind(foo_proc_3a, Atom.FromText("c"), proc_9);
    JunoScope.Bind(foo_proc_2b, Atom.FromText("w"), proc_10);
    JunoScope.Bind(foo_proc_2b, Atom.FromText("v"), proc_11);
    JunoScope.Bind(foo_proc_3b, Atom.FromText("x"), proc_12);
  EXCEPT
    JunoScope.NameClash => <* ASSERT FALSE *>
  END;

  (* Walk down from root scope *)
  Wr.PutText(stdout, "Top-level interface scope:\n");
  JunoScope.Print(stdout, top_pub, LAST(INTEGER), Tab);
  Wr.PutChar(stdout, '\n');
  Wr.Flush(stdout);

  (* Search for various values in various scopes *)
  Search(stdout, "Bar.Pi",      "Foo.Proc", foo_proc);
  Search(stdout, "Foo.Pi",      "Foo",      foo_pub);
  Search(stdout, "Foo.Pi",      "Foo.Func", foo_func);
  Search(stdout, "Foo.Syntax",  "Bar",      bar_mod);
  Search(stdout, "Foo.Private", "Bar",      bar_mod);
  Search(stdout, "Private",     "Foo.Func", foo_func);
  Search(stdout, "Foo.Private", "Foo.Func", foo_func);

  (* Test for NameClash *)
  TRY JunoScope.Bind(top_pub, Atom.FromText("Foo"), foo_private) EXCEPT
    JunoScope.NameClash =>
      Wr.PutText(stdout, "NameClash adding \"Foo\" to top-level scope.\n\n")
  END;

  (* print argument and temp scopes *)
  Wr.PutText(stdout, "Foo.Proc locals Set-1:\n");
  JunoScope.Print(stdout, foo_proc_1, LAST(INTEGER), Tab);
  Wr.PutChar(stdout, '\n'); Wr.Flush(stdout);

  Wr.PutText(stdout, "Foo.Proc locals Set-2a (child of Set-1):\n");
  JunoScope.Print(stdout, foo_proc_2a, LAST(INTEGER), Tab);
  Wr.PutChar(stdout, '\n'); Wr.Flush(stdout);

  Wr.PutText(stdout, "Foo.Proc locals Set-2b (child of Set-1):\n");
  JunoScope.Print(stdout, foo_proc_2b, LAST(INTEGER), Tab);
  Wr.PutChar(stdout, '\n'); Wr.Flush(stdout);

  Wr.PutText(stdout, "Foo.Proc locals Set-3a (child of Set-2a):\n");
  JunoScope.Print(stdout, foo_proc_3a, LAST(INTEGER), Tab);
  Wr.PutChar(stdout, '\n'); Wr.Flush(stdout);

  Wr.PutText(stdout, "Foo.Proc locals Set-3b (child of Set-2b):\n");
  JunoScope.Print(stdout, foo_proc_3b, LAST(INTEGER), Tab);
  Wr.PutChar(stdout, '\n'); Wr.Flush(stdout);

  (* Search for various values in various scopes *)
  Search(stdout, "x", "Set-2a", foo_proc_2a);
  Search(stdout, "w", "Set-2a", foo_proc_2a);
  Search(stdout, "x", "Set-3a", foo_proc_3a);
  Search(stdout, "x", "Set-3b", foo_proc_3b);
  Search(stdout, "x", "Set-3a", foo_proc_3a, local := TRUE);
  Search(stdout, "x", "Set-3b", foo_proc_3b, local := TRUE);
END ScopeTest.
