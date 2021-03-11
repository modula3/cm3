(* $Id$ *)

(*
  Copyright (c) 2008, 2009, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE Scheme EXPORTS Scheme, SchemeClass;
IMPORT SchemeClass;
IMPORT SchemeInputPort, SchemeEnvironment, SchemePrimitives, SchemePrimitive;
IMPORT SchemeEnvironmentSuper;
IMPORT SchemeBoolean, SchemeSymbol, SchemeMacro;
IMPORT SchemeClosure, SchemeClosureClass, SchemeProcedure, SchemeString;
IMPORT Pathname, Stdio;
IMPORT Wr, TextRd, Thread, Text;
IMPORT AL, FileRd, Rd, OSError, SchemeUtils;
FROM SchemeUtils IMPORT Stringify;
IMPORT SchemePair;
IMPORT Debug, DebugClass;
IMPORT Env, TextReader;

IMPORT SchemeDefsBundle, Bundle;
IMPORT SchemeProfiler;
IMPORT SchemeUnixDeps;
IMPORT SchemeEnvironmentBinding;

TYPE Binding = SchemeEnvironmentBinding.T;

TYPE Pair = SchemePair.T;

<* FATAL Thread.Alerted *>

CONST ProfileProcedures = TRUE;

REVEAL
  T = SchemeClass.Private BRANDED Brand OBJECT
    globalEnvironment : SchemeEnvironment.T;
    interrupter : Interrupter := NIL;
    prims : SchemePrimitive.Definer := NIL;

    mapRTErrors := TRUE;

  METHODS
    readInitialFiles(READONLY files : ARRAY OF Pathname.T) RAISES { E } := ReadInitialFiles;
  OVERRIDES
    init              :=  Init;
    init2             :=  Init2;
    defineInGlobalEnv :=  DefineInGlobalEnv;
    readEvalWriteLoop :=  ReadEvalWriteLoop;
    setInterrupter    :=  SetInterrupter;
    loadFile          :=  LoadFile;
    loadPort          :=  LoadPort;
    loadRd            :=  LoadRd;
    loadText          :=  LoadText;

    eval              :=  Eval;
    evalInGlobalEnv   :=  EvalInGlobalEnv;
    evalList          :=  EvalList2;

    loadEval          :=  LoadEval;
    loadEvalText      :=  LoadEvalText;

    bind              :=  Bind;
    setInGlobalEnv    :=  SetInGlobalEnv;
    setPrimitives     :=  SetPrimitives;
    changeGlobalEnvironment := ChangeGlobalEnvironment;
    getGlobalEnvironment := GetGlobalEnvironment;
    attemptToMapRuntimeErrors := AttemptToMapRuntimeErrors;
    setRTErrorMapping := SetRTErrorMapping;
  END;

PROCEDURE AttemptToMapRuntimeErrors(scm : T) : BOOLEAN = 
  BEGIN RETURN scm.mapRTErrors END AttemptToMapRuntimeErrors;

PROCEDURE SetRTErrorMapping(scm : T; to : BOOLEAN) =
  BEGIN scm.mapRTErrors := to END SetRTErrorMapping;

PROCEDURE GetGlobalEnvironment(t : T) : SchemeEnvironmentSuper.T =
  BEGIN RETURN t.globalEnvironment END GetGlobalEnvironment;

PROCEDURE ChangeGlobalEnvironment(t : T; env : SchemeEnvironmentSuper.T) =
  BEGIN t.globalEnvironment := env END ChangeGlobalEnvironment;

PROCEDURE SetPrimitives(t : T; spd : REFANY) =
  BEGIN t.prims := spd END SetPrimitives;

VAR runInteractionHook : RunInteractionHook := NIL;

PROCEDURE SetInteractionHook(hook : RunInteractionHook) =
  BEGIN runInteractionHook := hook END SetInteractionHook;

PROCEDURE Bind(t : T; var : REFANY; val : Object) =
  BEGIN 
    TYPECASE var OF
      NULL => <*ASSERT FALSE*>
    |
      TEXT => var := SchemeSymbol.Symbol(var)
    |
      SchemeSymbol.T => (* skip *)
    ELSE
      <*ASSERT FALSE*>
    END;

    EVAL t.globalEnvironment.define(var,val) 
  END Bind;

PROCEDURE SetInGlobalEnv(t : T; var : Symbol; val : Object) RAISES { E } =
  BEGIN EVAL t.globalEnvironment.set(var,val) END SetInGlobalEnv;

PROCEDURE Init(t : T; READONLY  files : ARRAY OF Pathname.T;
               env : REFANY(* SchemeEnvironment.Public *)) : T 
  RAISES { E } = 
  BEGIN RETURN t.init2(Stdio.stdin, Stdio.stdout, files, env) END Init;

PROCEDURE Init2(t : T; 
                input : Rd.T;
                output : Wr.T;
                READONLY files : ARRAY OF Pathname.T;
                env : REFANY(* SchemeEnvironment.Public *)) : T
  RAISES { E } =
  BEGIN
    t.input := NEW(SchemeInputPort.T).init(input);
    t.output := output;

    IF env = NIL THEN
      t.globalEnvironment := NEW(SchemeEnvironment.Unsafe).initEmpty();
    ELSE
      t.globalEnvironment := env;
    END;

    t.bind(SearchPathName, path);

    IF t.prims = NIL THEN
      EVAL NEW(SchemePrimitive.DefaultDefiner).installPrimitives(t.globalEnvironment)
    ELSE
      EVAL t.prims.installPrimitives(t.globalEnvironment)
    END;
    t.readInitialFiles(files);
    RETURN t
  END Init2;

PROCEDURE ReadInitialFiles(t : T; READONLY files : ARRAY OF Pathname.T) 
  RAISES { E } =
  BEGIN
    EVAL t.loadRd(NEW(TextRd.T).init(SchemePrimitives.Code));
    FOR i := FIRST(files) TO LAST(files) DO
      IF Debug.GetLevel() >= 20 THEN
        Debug.Out("Scheme.ReadInitialFiles: " & files[i])
      END;
      EVAL t.loadFile(SchemeString.FromText(files[i]))
    END
  END ReadInitialFiles;

PROCEDURE DefineInGlobalEnv(t : T; var, val : Object) =
  BEGIN EVAL t.globalEnvironment.define(var,val) END DefineInGlobalEnv;

PROCEDURE SetInterrupter(t : T; i : Interrupter) =
  BEGIN t.interrupter := i END SetInterrupter;

PROCEDURE ReadEvalWriteLoop(t : T; int : Interrupter) RAISES { Wr.Failure } =
  BEGIN
    t.setInterrupter(int);

    t.bind(SchemeSymbol.Symbol("bang-bang"), NIL);

    LOOP
      Wr.PutText(t.output, ">"); Wr.Flush(t.output);
      TRY
        WITH x = t.input.read() DO
          IF SchemeInputPort.IsEOF(x) THEN RETURN END;
          WITH res = t.evalInGlobalEnv(x) DO
            EVAL SchemeUtils.Write(res, t.output, TRUE);
            EVAL t.globalEnvironment.set(SchemeSymbol.Symbol("bang-bang"),res)
          END
        END
      EXCEPT
        E(e) => Wr.PutText(t.output, "EXCEPTION! " & e & "\n");
        IF EnvDisablesTracebacks THEN
          Wr.PutText(t.output,
                     "(Tracebacks disabled by NOMSCHEMETRACEBACKS.)\n")
        END
      END;
      Wr.PutText(t.output, "\n"); Wr.Flush(t.output)
    END
  END ReadEvalWriteLoop;

PROCEDURE LoadRd(t : T; rd : Rd.T; fn : Pathname.T) : Object RAISES { E } =
  BEGIN RETURN t.loadPort(NEW(SchemeInputPort.T).init(rd, fn)) END LoadRd;

VAR path := SchemeUtils.List1(SchemeString.FromText("."));

CONST SearchPathName = "**scheme-load-path**";

TYPE FileRec = RECORD fn : Pathname.T; rd : Rd.T END;

PROCEDURE FileOpen(t : T; name : Pathname.T) : FileRec
  RAISES { E, OSError.E } =
  (* N.B. routine raises OSError.E(NIL) if path is NIL *)
  TYPE
    XType = { E, OS };
  VAR
    p := t.evalInGlobalEnv(SchemeSymbol.FromText(SearchPathName));
    xArgs : REFANY := NIL;
    xType := XType.OS;
  BEGIN
    IF Text.GetChar(name, 0) = '/' THEN
      TRY
        RETURN FileRec { name,
                         FileRd.Open(name) }
      EXCEPT
        OSError.E(x) => xArgs := x; xType := XType.OS
      END;
    ELSIF Text.GetChar(name, 0) = '~' THEN
      VAR pos := Text.FindChar(name, '/', 1); 
          user, rest : TEXT;
          homeDir : Pathname.T;
      BEGIN
        TRY
          IF pos = -1 THEN pos := LAST(CARDINAL) END;

          IF pos < 2 THEN
            user := SchemeUnixDeps.GetCurrentUser();
            rest := Text.Sub(name, 1)
          ELSE
            user := Text.Sub(name, 1, pos-1);
            rest := Text.Sub(name, pos)
          END;
          homeDir := SchemeUnixDeps.GetHomeDir(user);
        EXCEPT
          SchemeUnixDeps.Error => RAISE E ("error expanding ~ home directory")
        END;

        RETURN FileOpen(t, homeDir & rest)
      END
    ELSE
      WHILE ISTYPE(p,Pair) AND p # NIL DO
        WITH pa = NARROW(p,Pair) DO
          TRY
            WITH path = SchemeString.ToText(pa.first) & "/" & name DO
              RETURN FileRec { path,
                               FileRd.Open(path) }
            END
          EXCEPT
            E(x) => xArgs := x; xType := XType.E
          |
            OSError.E(x) => xArgs := x; xType := XType.OS
          END;
          p := pa.rest
        END
      END
    END;

    CASE xType OF
      XType.E => RAISE E(xArgs)
    |
      XType.OS => RAISE OSError.E(xArgs)
    END
  END FileOpen;

PROCEDURE LoadFile(t : T; fileName : Object) : Object RAISES { E } =
  BEGIN
    WITH name = SchemeUtils.StringifyQ(fileName,FALSE) DO
      TRY
        VAR rec := FileOpen(t, name);
        BEGIN
          TRY
            WITH res = t.loadRd(rec.rd, name) DO
              IF SchemeBoolean.TruthO(res) THEN 
                RETURN SchemeString.FromText(rec.fn) 
              ELSE
                RETURN res
              END
            END
          FINALLY
            Rd.Close(rec.rd) 
          END
        END
      EXCEPT
        OSError.E(err) => 
        VAR bundled : TEXT := Bundle.Get(schemeDefs,name & ".scm"); BEGIN
          IF bundled # NIL THEN
            RETURN t.loadRd(NEW(TextRd.T).init(bundled),
                            "SchemeDefsBundle:" & name)
          ELSE
            RETURN SchemeUtils.Error("can't load " & name & 
                   " : OSError.E : " & AL.Format(err))
          END
        END
        |
          Rd.Failure(err) =>
          RETURN SchemeUtils.Error("trouble loading " & name & 
                 " : Rd.Failure : " & AL.Format(err))
      END
    END
  END LoadFile;

VAR schemeDefs := SchemeDefsBundle.Get();

PROCEDURE LoadEval(t : T; rd : Rd.T) : Object RAISES { E } =
  VAR
    port := NEW(SchemeInputPort.T).init(rd);
    x, res : Object := NIL;
  BEGIN
    LOOP
      x := port.read();

      IF SchemeInputPort.IsEOF(x) THEN 
        EVAL port.close();
        RETURN res 
      END;

      res := t.evalInGlobalEnv(x)
    END
  END LoadEval;

PROCEDURE LoadEvalText(t : T; txt : TEXT) : Object RAISES { E } =
  VAR
    rd := NEW(TextRd.T).init(txt);
  BEGIN
    RETURN t.loadEval(rd)
  END LoadEvalText;

PROCEDURE LoadPort(t : T; in : Object) : Object 
  RAISES { E } =
  BEGIN
    IF in = NIL OR NOT ISTYPE(in, SchemeInputPort.T) THEN
      RAISE E("Not an input port: " & Stringify(in))
    END;

    LOOP
      WITH x = NARROW(in,SchemeInputPort.T).read() DO
        IF SchemeInputPort.IsEOF(x) THEN RETURN SchemeBoolean.True() END;
        EVAL t.evalInGlobalEnv(x)
      END
    END
  END LoadPort;

PROCEDURE LoadText(t : T; txt : TEXT) : Object RAISES { E } = 
  VAR
    rd := NEW(TextRd.T).init(txt);
  BEGIN
    RETURN t.loadRd(rd, "LOCAL")
  END LoadText;

PROCEDURE TruncateText(txt : TEXT; maxLen : CARDINAL) : TEXT =
  CONST 
    Ellipsis     = " ...";
  BEGIN
    WITH l = Text.Length(txt) DO
      IF l > maxLen THEN
        RETURN Text.Sub(txt,0,maxLen) & Ellipsis
      ELSE
        RETURN txt
      END
    END(* WITH *)
  END TruncateText;

PROCEDURE Eval(t : T; x : Object; envP : SchemeEnvironmentSuper.T) : Object 
  RAISES { E } =
  CONST
        Ellipsis     = "\n...";
        MaxTraceback = 4096; (* in bytes of output *)
        MaxPerLine   =  512; (* also in bytes *)
  BEGIN
    IF DoTracebacks AND NOT EnvDisablesTracebacks THEN
      TRY
        RETURN EvalInternal(t, x, envP)
      EXCEPT
        E(txt) =>

        IF Text.Equal(txt, "CallCC123") THEN 
          (* don't intercept call/cc, see SchemePrimitive.m3 *)
          RAISE E(txt) 
        END;

        WITH l = Text.Length(txt) DO
          IF l > MaxTraceback THEN
            IF NOT Text.Equal(Text.Sub(txt, l - Text.Length(Ellipsis)), 
                              Ellipsis) THEN
              RAISE E(txt & Ellipsis)
            END
          ELSE
            RAISE E (txt & "\n(called from) eval " & 
                  TruncateText(Stringify(x),MaxPerLine))
          END
        END(* WITH *);
        RAISE E(txt)
      END
    ELSE
      RETURN EvalInternal(t, x, envP)
    END
  END Eval;

PROCEDURE EvalInternal(t   : T; 
                       x   : Object; 
                       env : SchemeEnvironment.Instance) : Object 
  RAISES { E } =
  TYPE  Macro     = SchemeMacro.T;
        Closure   = SchemeClosure.T;
        Procedure = SchemeProcedure.T;

  CONST First  = SchemeUtils.First;
        Second = SchemeUtils.Second;
        Third  = SchemeUtils.Third;
        Rest   = SchemeUtils.Rest;
        Cons   = SchemeUtils.Cons;
        TruthO = SchemeBoolean.TruthO;

  VAR
    (* this will have to change for processing remote environments *)

    envIsLocallyMade := FALSE;

    DebugLevel := DebugClass.level;

    savedEnv : SchemeEnvironment.Instance := NIL;
  BEGIN
    LOOP
      IF DebugLevel >= 20 THEN Debug.Out("EVAL: " & Stringify(x)) END;


      IF t.interrupter # NIL AND t.interrupter.interrupt() THEN
        RAISE E("Command interrupted")
      END;

      IF x = NIL THEN
        RETURN NIL
      ELSIF ISTYPE(x,Symbol) THEN
        RETURN env.lookup(x)
      ELSIF ISTYPE(x, Binding) THEN
        RETURN NARROW(x, Binding).get()
      ELSIF NOT ISTYPE(x,Pair) THEN 
        RETURN x
      ELSE
        VAR
          fn   := NARROW(x,Pair).first;
          args := NARROW(x,Pair).rest; 
        BEGIN
          IF    fn = SYMquote THEN
            RETURN First(args)
          ELSIF fn = SYMunwindProtect THEN
            WITH protected = First(args),
                 cleanup   = Second(args),
                 error     = Third(args) DO
              x := NIL;
              TRY
                TRY
                  x := t.eval(protected,env)
                FINALLY
                  EVAL t.eval(cleanup,env)
                END
              EXCEPT
                E =>
                EVAL t.eval(error,env)
              END
            END
          ELSIF fn = SYMbegin THEN
            WHILE Rest(args) # NIL DO
              EVAL t.eval(First(args),env);
              args := Rest(args)
            END;
            x := First(args)
          ELSIF fn = SYMdefine THEN
            IF First(args) # NIL AND ISTYPE(First(args), Pair) THEN
              RETURN env.define(First(First(args)),
                                t.eval(Cons(SYMlambda,
                                            Cons(Rest(First(args)), 
                                                 Rest(args))), env))
            ELSE
              RETURN env.define(First(args),
                                t.eval(Second(args), env))
            END
          ELSIF fn = SYMsetB THEN
            RETURN env.set(First(args), t.eval(Second(args), env))
          ELSIF fn = SYMeval THEN
            (* eval doesnt really need to be a special form, but it does
               have to have access to the current environment, so it cant
               be a Primitive either (with the current design of the interpreter) 
             *)
            RETURN t.eval(t.eval(First(args),env),env)
          ELSIF fn = SYMif THEN
            IF TruthO(t.eval(First(args), env)) THEN
              x := Second(args) 
            ELSE
              x := Third(args)
            END
          ELSIF fn = SYMcond THEN
            x := ReduceCond(t, args, env)
          ELSIF fn = SYMlambda THEN
            env.assigned := TRUE;
            RETURN NEW(SchemeClosure.T).init(First(args), 
                                             Rest(args),
                                             env,
                                             TRUE)
          ELSIF fn = SYMmacro THEN
            env.assigned := TRUE;
            RETURN NEW(Macro).init(First(args),
                                   Rest(args),
                                   env)
          ELSIF fn = SYMrunInteraction AND runInteractionHook # NIL THEN
            RETURN runInteractionHook(env, t.eval(First(args),env))
          ELSE
            (* procedure call *)
            fn := t.eval(fn, env);
            

            IF ProfileProcedures AND ISTYPE (fn,Procedure) THEN
              SchemeProfiler.EnterProcedure(fn)
            END;

            TYPECASE fn OF
              NULL => RAISE E("Not a procedure: " & Stringify(fn))
            |
              Macro(m) => 
              x := m.expand(t, x, args)
            |
              Closure(c) => 
              (* can we check here if this is the last thing we
                 eval, in which case we can just re-init the
                 environment?  it won't be re-used, right? *)
              x := c.body; 


              (* this is a teeny tiny tail call optimization.. *)

              (* the following code is a bit tricky and highly
                 optimized.

                 First: we know that the environment can't escape
                 this routine if it is locally manufactured. 
                 (We cannot overwrite passed-in environments, because
                 who knows where else those are used.)  Therefore,
                 if the environment has been allocated locally (on
                 a previous tail call, for instance), we take
                 the liberty to overwrite it instead of allocating
                 a new one.  This is a major performance optimization
                 on systems with slow garbage collectors!

                 Secondly: we see about a 2-4% increase in performance
                 tests by using initEval instead of evalList. 
                 InitEval skips the allocation of the list and instead
                 evaluates the arg list "in place" inside the environment.
                 This optimization is of more dubious value as most
                 of its advantages are already provided by the recycling
                 of cons cells that we do with the "ReturnCons"
                 mechanism. 
              *)
              VAR
                newEnv : SchemeEnvironment.Instance;
              BEGIN
                INC(envsMade);
                
                IF savedEnv # NIL THEN
                  newEnv := savedEnv; savedEnv := NIL
                ELSE
                  newEnv := NEW(SchemeEnvironment.Unsafe)
                END;


                IF envIsLocallyMade AND NOT env.assigned THEN
                  (* we might be able to keep it after we've used it *)
                  savedEnv := env
                END;
                
                (* this is a LOCAL environment.  No need for it to
                   use synchronized methods *)

                env := newEnv.
                          initEval(c.params, args, env, t, c.env);
                envIsLocallyMade := TRUE;

                <* ASSERT c.env.assigned *>

                (* re-check savedEnv, might have been assigned above? *)
                IF savedEnv # NIL AND savedEnv.assigned THEN
                  savedEnv := NIL
                END;
              END

            |
              Procedure(p) =>

              (* more micro-optimizations:
                 
                 apply1 and apply2 unconditionally recycle their
                 list objects internally.

                 at present, this optimization is only provided in
                 SchemePrimitive.m3

                 Yes the code looks messy but at present it can be
                 as much as a 25% performance improvement on machines
                 with slow GC.
              *)

              TYPECASE args OF
                NULL =>
              |
                Pair(pp) =>
                IF pp.rest = NIL THEN
                  RETURN p.apply1(t, EvalInternal(t,pp.first, env))
                ELSIF ISTYPE(pp.rest,Pair) 
                  AND NARROW(pp.rest,Pair).rest = NIL THEN
                  RETURN p.apply2(t, 
                                  EvalInternal(t,pp.first,env),
                                  EvalInternal(t,NARROW(pp.rest,Pair).first, env))
                END
              ELSE END;
              RETURN p.apply(t, t.evalList(args,env))
            ELSE
              RAISE E("Not a procedure: " & Stringify(fn))
            END
          END
        END
      END
    END
  END EvalInternal;

VAR envsMade := 0;

PROCEDURE EvalInGlobalEnv(t : T; x : Object) : Object RAISES { E } =
  BEGIN RETURN t.eval(x, t.globalEnvironment) END EvalInGlobalEnv;

PROCEDURE EvalList2(t : T; list : Object; env : SchemeEnvironmentSuper.T) : Object 
  RAISES { E } =
  CONST Error = SchemeUtils.Error;
  VAR
    res : Pair := NIL;
    ptr : Pair;
  BEGIN
    LOOP
      IF list = NIL THEN
        RETURN res
      ELSIF NOT ISTYPE(list, Pair) THEN
        EVAL Error("Illegal arg list: " & SchemeUtils.StringifyT(list));
        RETURN NIL (*notreached*)
      ELSE
        WITH pair = NARROW(list,Pair) DO
          WITH new = GetCons(t) DO
            new.first := t.eval(pair.first, env);
            new.rest := NIL;

            (* "cons" on the *tail* of the result so we dont have to
               reverse it later *)
            IF res = NIL THEN
              ptr := new;
              res := ptr
            ELSE
              ptr.rest := new;
              ptr := new
            END;
            list := pair.rest
          END
        END
      END
    END
  END EvalList2;

PROCEDURE ReduceCond(t : T; 
                     clauses : Object; env : SchemeEnvironment.T) : Object 
  RAISES { E } =

  CONST First  = SchemeUtils.First;
        Second = SchemeUtils.Second;
        Third  = SchemeUtils.Third;
        Rest   = SchemeUtils.Rest;
        Cons   = SchemeUtils.Cons;
        List2  = SchemeUtils.List2;
        TruthO = SchemeBoolean.TruthO;

  VAR result : Object := NIL;

  BEGIN
    IF Debug.GetLevel() >= 20 THEN
      Debug.Out("Scheme.ReduceCond: clauses=" & Stringify(clauses))
    END;
    LOOP
      IF clauses = NIL THEN RETURN SchemeBoolean.False() END;
      
      WITH clause = First(clauses) DO
        clauses := Rest(clauses);
        IF Debug.GetLevel() >= 20 THEN
          Debug.Out("Scheme.ReduceCond: one clause=" & Stringify(clause) &
                    " First=" & Stringify(First(clause)) & 
                    " Second=" & Stringify(Second(clause)) & 
                    " Third=" & Stringify(Third(clause)) )
        END;
        VAR
          success := FALSE;
        BEGIN
          IF First(clause) = SYMelse THEN
            success := TRUE
          ELSE 
            result := t.eval(First(clause),env);
            (* is this a bug? we overwrite result even if we don't succeed,
               Norvig's SILK does this too... *)
            IF TruthO(result) THEN success := TRUE END; 
          END;

          IF success THEN
            IF Rest(clause) = NIL THEN
              RETURN List2(SYMquote,result,t)
            ELSIF Second(clause) = SYMarrow THEN
              RETURN List2(Third(clause),List2(SYMquote,result),t)
            ELSE 
              RETURN Cons(SYMbegin, Rest(clause),t)
            END
          END
        END
      END
    END
  END ReduceCond;

PROCEDURE GetCons(t : T) : Pair =
  BEGIN
    IF t.freePairs # NIL THEN
      VAR 
        res := t.freePairs;
        next := res.rest;
      BEGIN
        t.freePairs := next;
        RETURN res
      END
    END;
    RETURN NEW(Pair)
  END GetCons;

PROCEDURE ReturnCons(t : T; cons : Pair) = 
  BEGIN
    IF cons = NIL THEN RETURN END;

    VAR p : Pair := cons; BEGIN
      WHILE p.rest # NIL DO
        p.first := SYMrip;
        p := p.rest
      END;

      p.first := SYMrip;

      p.rest := t.freePairs;
      t.freePairs := cons
    END
  END ReturnCons;

PROCEDURE SymbolCheck(x : Object) : SchemeSymbol.T RAISES { E } =
  BEGIN
    IF x # NIL AND ISTYPE(x,SchemeSymbol.T) THEN RETURN x 
    ELSE  RAISE E("expected a symbol, got: " & 
                  SchemeUtils.StringifyT(x))
    END
  END SymbolCheck;

PROCEDURE VectorCheck(t : Object) : Vector RAISES { E } = 
  BEGIN
    IF t = NIL OR NOT ISTYPE(t, Vector) THEN
      RAISE E("expected a vector : " & SchemeUtils.Stringify(t))
    END;
    RETURN t
  END VectorCheck;

VAR
  SYMquote := SchemeSymbol.Symbol("quote");
  SYMbegin := SchemeSymbol.Symbol("begin");
  SYMdefine := SchemeSymbol.Symbol("define");
  SYMrunInteraction := SchemeSymbol.Symbol("run-interaction");
  SYMsetB := SchemeSymbol.Symbol("set!");
  SYMif := SchemeSymbol.Symbol("if");
  SYMcond := SchemeSymbol.Symbol("cond");
  SYMeval := SchemeSymbol.Symbol("eval");
  SYMlambda := SchemeSymbol.Symbol("lambda");
  SYMmacro := SchemeSymbol.Symbol("macro");
  SYMelse := SchemeSymbol.Symbol("else");
  SYMarrow := SchemeSymbol.Symbol("=>");
  SYMrip := SchemeSymbol.Symbol("####r.i.p.-dead-cons-cell####");
  SYMunwindProtect := SchemeSymbol.Symbol("unwind-protect");

(* the following for testing Bindings in Closure building! *)

PROCEDURE IsSpecialForm(s : Symbol) : BOOLEAN =
  BEGIN
    RETURN 
      s =  SYMquote OR 
      s =  SYMbegin OR 
      s =  SYMdefine OR 
      s =  SYMrunInteraction OR 
      s =  SYMsetB OR 
      s =  SYMif OR 
      s =  SYMcond OR 
      s =  SYMeval OR 
      s =  SYMlambda OR 
      s =  SYMmacro OR 
      s =  SYMelse OR 
      s =  SYMarrow OR 
      s =  SYMrip OR 
      s =  SYMunwindProtect 
  END IsSpecialForm;

BEGIN 
  EnvDisablesTracebacks := Env.Get("NOMSCHEMETRACEBACKS") # NIL;

  WITH sPath = Env.Get("MSCHEMEPATH") DO
    IF sPath # NIL THEN
      path := NIL;
      VAR
        reader := NEW(TextReader.T).init(sPath);
        p : TEXT;
      BEGIN
        WHILE reader.next(":",p) DO
          path := NEW(Pair, first := SchemeString.FromText(p),
                      rest := path)
        END;
        path := SchemeUtils.Reverse(path)
      END
    END
  END
END Scheme.



