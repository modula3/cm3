(* Smoke test for the MSIR package.

   Builds two procedures programmatically:

   1. Sum0ToN — exercises block parameters, iadd, icmp, br/cond_br, ret.
   2. MaybeArea — exercises object types, istype, narrow, dispatch.

   Then prints the resulting module. *)

MODULE Main;

IMPORT MSIR, MSIRPrinter, MSIRVerifier, Stdio, Wr;

PROCEDURE BuildSum0ToN(m: MSIR.Module) =
  VAR
    i64   := MSIR.TI(64);
    p     : MSIR.Proc;
    entry, loop, body, exit : MSIR.Block;
    n, zero, one : MSIR.Value;
    i, s, done   : MSIR.Value;
    bi, bs, s2, i2 : MSIR.Value;
    result : MSIR.Value;
  BEGIN
    p := MSIR.NewProc(
        "@Sum0ToN",
        ARRAY OF MSIR.Param {
          MSIR.Param{"n", i64, MSIR.ParamMode.ByValue}
        },
        i64);
    n := MSIR.ProcParam(p, 0);

    entry := MSIR.NewBlock("entry", ARRAY OF MSIR.BlockParam{});
    loop  := MSIR.NewBlock("loop",
        ARRAY OF MSIR.BlockParam {
          MSIR.BlockParam{"i", i64},
          MSIR.BlockParam{"s", i64}
        });
    body  := MSIR.NewBlock("body",
        ARRAY OF MSIR.BlockParam {
          MSIR.BlockParam{"i", i64},
          MSIR.BlockParam{"s", i64}
        });
    exit  := MSIR.NewBlock("exit",
        ARRAY OF MSIR.BlockParam {
          MSIR.BlockParam{"result", i64}
        });

    MSIR.ProcAddBlock(p, entry);
    MSIR.ProcAddBlock(p, loop);
    MSIR.ProcAddBlock(p, body);
    MSIR.ProcAddBlock(p, exit);

    zero := MSIR.ConstInt(i64, 0);
    one  := MSIR.ConstInt(i64, 1);

    MSIR.BuildBr(entry, loop, ARRAY OF MSIR.Value{zero, zero});

    i    := MSIR.BlockParamValue(loop, 0);
    s    := MSIR.BlockParamValue(loop, 1);
    done := MSIR.BuildICmp(loop, "done", MSIR.CmpPred.Sge, i, n);
    MSIR.BuildCondBr(loop, done,
        exit, ARRAY OF MSIR.Value{s},
        body, ARRAY OF MSIR.Value{i, s});

    bi := MSIR.BlockParamValue(body, 0);
    bs := MSIR.BlockParamValue(body, 1);
    s2 := MSIR.BuildIAdd(body, "s2", bs, bi);
    i2 := MSIR.BuildIAdd(body, "i2", bi, one);
    MSIR.BuildBr(body, loop, ARRAY OF MSIR.Value{i2, s2});

    result := MSIR.BlockParamValue(exit, 0);
    MSIR.BuildRet(exit, result);

    MSIR.ModuleAddProc(m, p);
  END BuildSum0ToN;

PROCEDURE BuildMaybeArea(m: MSIR.Module) =
  VAR
    i64    := MSIR.TI(64);

    (* methods: area: proc(gc_ref Animal) -> i64
       (declared with NIL self type — we patch self in TProc params) *)
    animalAreaSig : MSIR.T;
    animal, dog   : MSIR.T;

    p             : MSIR.Proc;
    entry, doDispatch, retZero : MSIR.Block;
    s, isDog, d, r, zero : MSIR.Value;
  BEGIN
    (* Animal.area: proc(gc_ref Animal) -> i64.
       For v0 we just record the signature with a NIL self placeholder;
       real type-checking would require self-reference resolution. *)
    animalAreaSig := MSIR.TProc(
        ARRAY OF MSIR.T { (* self goes here; left empty for simplicity *) },
        i64);

    animal := MSIR.TObject(
        "Animal",
        NIL,
        ARRAY OF MSIR.Field { },
        ARRAY OF MSIR.Method {
          MSIR.Method{"area", animalAreaSig}
        },
        "@Animal_ti");

    dog := MSIR.TObject(
        "Dog",
        animal,                        (* extends Animal — inherits area *)
        ARRAY OF MSIR.Field { },
        ARRAY OF MSIR.Method { },
        "@Dog_ti");

    p := MSIR.NewProc(
        "@MaybeArea",
        ARRAY OF MSIR.Param {
          MSIR.Param{"s", MSIR.TGcRef(animal), MSIR.ParamMode.ByValue}
        },
        i64);
    s := MSIR.ProcParam(p, 0);

    entry      := MSIR.NewBlock("entry",       ARRAY OF MSIR.BlockParam{});
    doDispatch := MSIR.NewBlock("do_dispatch", ARRAY OF MSIR.BlockParam{});
    retZero    := MSIR.NewBlock("ret_zero",    ARRAY OF MSIR.BlockParam{});

    MSIR.ProcAddBlock(p, entry);
    MSIR.ProcAddBlock(p, doDispatch);
    MSIR.ProcAddBlock(p, retZero);

    isDog := MSIR.BuildIstype(entry, "is_dog", s, dog);
    MSIR.BuildCondBr(entry, isDog,
        doDispatch, ARRAY OF MSIR.Value{},
        retZero,    ARRAY OF MSIR.Value{});

    d := MSIR.BuildNarrow(doDispatch, "d", s, dog);
    r := MSIR.BuildDispatch(doDispatch, "r", d, "area",
                            ARRAY OF MSIR.Value{});
    MSIR.BuildRet(doDispatch, r);

    zero := MSIR.ConstInt(i64, 0);
    MSIR.BuildRet(retZero, zero);

    MSIR.ModuleAddProc(m, p);
  END BuildMaybeArea;

PROCEDURE BuildWithLog(m: MSIR.Module) =
  VAR
    i64 := MSIR.TI(64);

    (* External Lookup procedure (declaration only); raises NotFound or Bad. *)
    lookup := MSIR.NewProc(
        "@Lookup",
        ARRAY OF MSIR.Param { MSIR.Param{"key", i64, MSIR.ParamMode.ByValue} },
        i64);

    p     := MSIR.NewProc(
        "@WithLog",
        ARRAY OF MSIR.Param { MSIR.Param{"key", i64, MSIR.ParamMode.ByValue} },
        i64);
    key   := MSIR.ProcParam(p, 0);

    entry := MSIR.NewBlock("entry", ARRAY OF MSIR.BlockParam{});
    body  := MSIR.NewBlock("body",  ARRAY OF MSIR.BlockParam{});
    h1    := MSIR.NewBlock("h1",    ARRAY OF MSIR.BlockParam{});
    h2    := MSIR.NewBlock("h2",
                ARRAY OF MSIR.BlockParam {
                  MSIR.BlockParam{"msg", i64}
                });
    merge := MSIR.NewBlock("merge",
                ARRAY OF MSIR.BlockParam {
                  MSIR.BlockParam{"n", i64}
                });

    cont  := MSIR.NewBlock("cont", ARRAY OF MSIR.BlockParam{});
    lp    := MSIR.NewBlock("lp",   ARRAY OF MSIR.BlockParam{});

    v, n, lp_val, negOne, negTwo : MSIR.Value;
    env     : MSIR.Envelope;
  BEGIN
    MSIR.ProcSetRaises(lookup, ARRAY OF TEXT{"@NotFound", "@Bad"});
    MSIR.ModuleAddProc(m, lookup);

    negOne := MSIR.ConstInt(i64, -1);
    negTwo := MSIR.ConstInt(i64, -2);

    (* entry: br body *)
    MSIR.BuildBr(entry, body, ARRAY OF MSIR.Value{});

    (* body: %v = invoke @Lookup(key), normal->cont, unwind->lp *)
    v := MSIR.BuildInvoke(body, "v", lookup, ARRAY OF MSIR.Value{key}, cont, lp);

    (* cont (normal return): br merge(%v) *)
    MSIR.BuildBr(cont, merge, ARRAY OF MSIR.Value{v});

    (* lp (landing pad): re-raise *)
    lp_val := MSIR.BuildLandingPad(lp, "lp", FALSE);
    MSIR.BuildResume(lp, lp_val);

    (* h1 (NotFound): br merge(-1) *)
    MSIR.BuildBr(h1, merge, ARRAY OF MSIR.Value{negOne});

    (* h2 (Bad(msg)): br merge(-2) *)
    MSIR.BuildBr(h2, merge, ARRAY OF MSIR.Value{negTwo});

    (* merge: ret %n *)
    n := MSIR.BlockParamValue(merge, 0);
    MSIR.BuildRet(merge, n);

    env := MSIR.NewTryExcept(body,
        ARRAY OF MSIR.Handler {
          MSIR.Handler{"@NotFound", h1},
          MSIR.Handler{"@Bad",      h2}
        });

    MSIR.ProcAddBlock(p, entry);
    MSIR.ProcAddEnvelope(p, env);
    MSIR.ProcAddBlock(p, cont);
    MSIR.ProcAddBlock(p, lp);
    MSIR.ProcAddBlock(p, h1);
    MSIR.ProcAddBlock(p, h2);
    MSIR.ProcAddBlock(p, merge);

    MSIR.ModuleAddProc(m, p);
  END BuildWithLog;

PROCEDURE BuildOpenArraySum(m: MSIR.Module) =
  (* PROCEDURE Sum(a: ARRAY OF INTEGER): INTEGER =
       VAR s := 0;
       BEGIN
         FOR i := 0 TO NUMBER(a) - 1 DO s := s + a[i] END;
         RETURN s;
       END Sum; *)
  VAR
    i64    := MSIR.TI(64);
    oa1i64 := MSIR.TOpenArray(1, i64);

    p := MSIR.NewProc(
        "@Sum",
        ARRAY OF MSIR.Param { MSIR.Param{"a", oa1i64, MSIR.ParamMode.ByValue} },
        i64);
    a := MSIR.ProcParam(p, 0);

    entry := MSIR.NewBlock("entry", ARRAY OF MSIR.BlockParam{});
    loop  := MSIR.NewBlock("loop",
        ARRAY OF MSIR.BlockParam {
          MSIR.BlockParam{"i", i64},
          MSIR.BlockParam{"s", i64}
        });
    body  := MSIR.NewBlock("body",
        ARRAY OF MSIR.BlockParam {
          MSIR.BlockParam{"i", i64},
          MSIR.BlockParam{"s", i64}
        });
    exit  := MSIR.NewBlock("exit",
        ARRAY OF MSIR.BlockParam {
          MSIR.BlockParam{"result", i64}
        });

    n, zero, one : MSIR.Value;
    bi, bs, ep, v, s2, i2, lt, i_, s_ : MSIR.Value;
  BEGIN
    MSIR.ProcAddBlock(p, entry);
    MSIR.ProcAddBlock(p, loop);
    MSIR.ProcAddBlock(p, body);
    MSIR.ProcAddBlock(p, exit);

    zero := MSIR.ConstInt(i64, 0);
    one  := MSIR.ConstInt(i64, 1);

    (* entry: %n = openarray.size a, 0; br loop(0, 0) *)
    n := MSIR.BuildOpenArraySize(entry, "n", a, 0);
    MSIR.BuildBr(entry, loop, ARRAY OF MSIR.Value{zero, zero});

    (* loop(%i, %s):
         %lt = icmp slt %i, %n
         cond_br %lt, body(%i, %s), exit(%s) *)
    i_ := MSIR.BlockParamValue(loop, 0);
    s_ := MSIR.BlockParamValue(loop, 1);
    lt := MSIR.BuildICmp(loop, "lt", MSIR.CmpPred.Slt, i_, n);
    MSIR.BuildCondBr(loop, lt,
        body, ARRAY OF MSIR.Value{i_, s_},
        exit, ARRAY OF MSIR.Value{s_});

    (* body(%i, %s):
         subscript_check %i, %n
         %ep = openarray.elem_addr a, %i
         %v  = load i64, %ep
         %s2 = iadd %s, %v
         %i2 = iadd %i, 1
         br loop(%i2, %s2) *)
    bi := MSIR.BlockParamValue(body, 0);
    bs := MSIR.BlockParamValue(body, 1);
    MSIR.BuildSubscriptCheck(body, bi, n);
    ep := MSIR.BuildOpenArrayElemAddr(body, "ep", a, ARRAY OF MSIR.Value{bi});
    v  := MSIR.BuildLoad(body, "v", i64, ep);
    s2 := MSIR.BuildIAdd(body, "s2", bs, v);
    i2 := MSIR.BuildIAdd(body, "i2", bi, one);
    MSIR.BuildBr(body, loop, ARRAY OF MSIR.Value{i2, s2});

    (* exit(%result): ret %result *)
    MSIR.BuildRet(exit, MSIR.BlockParamValue(exit, 0));

    MSIR.ModuleAddProc(m, p);
  END BuildOpenArraySum;

PROCEDURE BuildClamp(m: MSIR.Module) =
  (* PROCEDURE Clamp(v: INTEGER): [0..100] =
       VAR p: [0..100];
       BEGIN
         p := v;          (* range check *)
         RETURN p;
       END Clamp; *)
  VAR
    i64    := MSIR.TI(64);
    pct    := MSIR.TSubrange(i64, 0, 100);

    p := MSIR.NewProc(
        "@Clamp",
        ARRAY OF MSIR.Param { MSIR.Param{"v", i64, MSIR.ParamMode.ByValue} },
        pct);
    v   := MSIR.ProcParam(p, 0);
    entry := MSIR.NewBlock("entry", ARRAY OF MSIR.BlockParam{});

    lo, hi, narrowed : MSIR.Value;
  BEGIN
    MSIR.ProcAddBlock(p, entry);
    lo := MSIR.ConstInt(i64,   0);
    hi := MSIR.ConstInt(i64, 100);
    MSIR.BuildRangeCheck(entry, v, lo, hi);
    narrowed := MSIR.BuildConvert(entry, "p", v, pct);
    MSIR.BuildRet(entry, narrowed);
    MSIR.ModuleAddProc(m, p);
  END BuildClamp;

PROCEDURE BuildSetCombine(m: MSIR.Module) =
  (* PROCEDURE Both(a, b: SET OF [0..6]; d: [0..6]): BOOLEAN =
       BEGIN RETURN d IN (a * b) END Both; *)
  VAR
    i64     := MSIR.TI(64);
    daySet  := MSIR.TSet(i64, 0, 6);

    p := MSIR.NewProc(
        "@Both",
        ARRAY OF MSIR.Param {
          MSIR.Param{"a", daySet, MSIR.ParamMode.ByValue},
          MSIR.Param{"b", daySet, MSIR.ParamMode.ByValue},
          MSIR.Param{"d", i64,    MSIR.ParamMode.ByValue}
        },
        MSIR.TI1());
    a := MSIR.ProcParam(p, 0);
    b := MSIR.ProcParam(p, 1);
    d := MSIR.ProcParam(p, 2);
    entry := MSIR.NewBlock("entry", ARRAY OF MSIR.BlockParam{});

    inter, isMember : MSIR.Value;
  BEGIN
    MSIR.ProcAddBlock(p, entry);
    inter    := MSIR.BuildSetIntersect(entry, "i", a, b);
    isMember := MSIR.BuildSetMember(entry, "r", inter, d);
    MSIR.BuildRet(entry, isMember);
    MSIR.ModuleAddProc(m, p);
  END BuildSetCombine;

PROCEDURE BuildDescribe(m: MSIR.Module) =
  (* PROCEDURE Describe(a: Animal): INTEGER =
       BEGIN
         TYPECASE a OF
         | Puppy   => RETURN 1
         | Dog (d) => RETURN 2
         | Cat     => RETURN 3
         ELSE         RETURN 0
         END;
       END Describe; *)
  VAR
    i64    := MSIR.TI(64);

    animal := MSIR.TObject("Animal", NIL,
                ARRAY OF MSIR.Field{}, ARRAY OF MSIR.Method{},
                "@Animal_ti");
    dog    := MSIR.TObject("Dog", animal,
                ARRAY OF MSIR.Field{}, ARRAY OF MSIR.Method{},
                "@Dog_ti");
    puppy  := MSIR.TObject("Puppy", dog,
                ARRAY OF MSIR.Field{}, ARRAY OF MSIR.Method{},
                "@Puppy_ti");
    cat    := MSIR.TObject("Cat", animal,
                ARRAY OF MSIR.Field{}, ARRAY OF MSIR.Method{},
                "@Cat_ti");

    p := MSIR.NewProc(
        "@Describe",
        ARRAY OF MSIR.Param {
          MSIR.Param{"a", MSIR.TGcRef(animal), MSIR.ParamMode.ByValue}
        },
        i64);
    a := MSIR.ProcParam(p, 0);

    entry    := MSIR.NewBlock("entry", ARRAY OF MSIR.BlockParam{});
    puppyB   := MSIR.NewBlock("puppy", ARRAY OF MSIR.BlockParam{});
    dogB     := MSIR.NewBlock("dog",
                  ARRAY OF MSIR.BlockParam {
                    MSIR.BlockParam{"d", MSIR.TGcRef(dog)}
                  });
    catB     := MSIR.NewBlock("cat", ARRAY OF MSIR.BlockParam{});
    elseB    := MSIR.NewBlock("else_b", ARRAY OF MSIR.BlockParam{});

    one  := MSIR.ConstInt(i64, 1);
    two  := MSIR.ConstInt(i64, 2);
    three := MSIR.ConstInt(i64, 3);
    zero := MSIR.ConstInt(i64, 0);
  BEGIN
    MSIR.ProcAddBlock(p, entry);
    MSIR.ProcAddBlock(p, puppyB);
    MSIR.ProcAddBlock(p, dogB);
    MSIR.ProcAddBlock(p, catB);
    MSIR.ProcAddBlock(p, elseB);

    VAR tc := NEW(REF ARRAY OF MSIR.TypecaseClause, 4); BEGIN
      tc[0] := MSIR.TypecaseClause{FALSE, 1, puppy, puppyB};
      tc[1] := MSIR.TypecaseClause{FALSE, 2, dog,   dogB};
      tc[2] := MSIR.TypecaseClause{FALSE, 3, cat,   catB};
      tc[3] := MSIR.TypecaseClause{TRUE,  0, NIL,   elseB};
      MSIR.BuildTypecase(entry, a, tc);
    END;

    MSIR.BuildRet(puppyB, one);
    MSIR.BuildRet(dogB,   two);
    MSIR.BuildRet(catB,   three);
    MSIR.BuildRet(elseB,  zero);

    MSIR.ModuleAddProc(m, p);
  END BuildDescribe;

PROCEDURE BuildAbort(m: MSIR.Module) =
  (* internal helper: noreturn, C calling convention. *)
  VAR
    p := MSIR.NewProc(
        "@abort_helper",
        ARRAY OF MSIR.Param{},
        MSIR.TVoid());
    entry := MSIR.NewBlock("entry", ARRAY OF MSIR.BlockParam{});
  BEGIN
    MSIR.ProcSetNoReturn(p, TRUE);
    MSIR.ProcSetLinkage(p, MSIR.Linkage.Internal);
    MSIR.ProcSetCallingConvention(p, MSIR.CallingConvention.C);
    MSIR.ProcAddBlock(p, entry);
    MSIR.BuildUnreachable(entry);
    MSIR.ModuleAddProc(m, p);
  END BuildAbort;

PROCEDURE BuildMakeArray(m: MSIR.Module) =
  (* PROCEDURE Make(n: INTEGER): REF ARRAY OF INTEGER =
       VAR r := NEW(REF ARRAY OF INTEGER, n);
       BEGIN RETURN r END Make; *)
  VAR
    i64 := MSIR.TI(64);

    p := MSIR.NewProc(
        "@Make",
        ARRAY OF MSIR.Param { MSIR.Param{"n", i64, MSIR.ParamMode.ByValue} },
        MSIR.TGcRef(MSIR.THeapArray(1, i64)));
    n     := MSIR.ProcParam(p, 0);
    entry := MSIR.NewBlock("entry", ARRAY OF MSIR.BlockParam{});
    r     : MSIR.Value;
  BEGIN
    MSIR.ProcAddBlock(p, entry);
    r := MSIR.BuildOpenArrayNew(entry, "r", i64, ARRAY OF MSIR.Value{n});
    MSIR.BuildRet(entry, r);
    MSIR.ModuleAddProc(m, p);
  END BuildMakeArray;

PROCEDURE BuildCounter(m: MSIR.Module) =
  (* VAR counter: INTEGER := 0;
     PROCEDURE Bump(): INTEGER =
       BEGIN INC(counter); RETURN counter END Bump; *)
  VAR
    i64    := MSIR.TI(64);
    counter := MSIR.NewGlobal("@counter", i64, FALSE);

    p := MSIR.NewProc(
        "@Bump",
        ARRAY OF MSIR.Param{},
        i64);
    entry := MSIR.NewBlock("entry", ARRAY OF MSIR.BlockParam{});
    one  := MSIR.ConstInt(i64, 1);
    cur, sum: MSIR.Value;
  BEGIN
    MSIR.ModuleAddGlobal(m, counter);
    MSIR.ProcAddBlock(p, entry);
    cur := MSIR.BuildLoad(entry, "cur", i64, MSIR.GlobalValue(counter));
    sum := MSIR.BuildIAdd(entry, "sum", cur, one);
    MSIR.BuildStore(entry, sum, MSIR.GlobalValue(counter));
    MSIR.BuildRet(entry, sum);
    MSIR.ModuleAddProc(m, p);
  END BuildCounter;

BEGIN
  VAR m := MSIR.NewModule("Demo");
  BEGIN
    BuildSum0ToN(m);
    BuildMaybeArea(m);
    BuildWithLog(m);
    BuildOpenArraySum(m);
    BuildClamp(m);
    BuildSetCombine(m);
    BuildDescribe(m);
    BuildAbort(m);
    BuildMakeArray(m);
    BuildCounter(m);
    MSIRPrinter.Module(Stdio.stdout, m);

    Wr.PutText(Stdio.stdout, "\n--- verifier ---\n");
    VAR errs := MSIRVerifier.VerifyModule(m);
    BEGIN
      IF errs = NIL THEN
        Wr.PutText(Stdio.stdout, "ok: no errors\n");
      ELSE
        FOR i := 0 TO LAST(errs^) DO
          Wr.PutText(Stdio.stdout, errs[i]);
          Wr.PutText(Stdio.stdout, "\n");
        END;
      END;
    END;
    Wr.Flush(Stdio.stdout);
  END;
END Main.
