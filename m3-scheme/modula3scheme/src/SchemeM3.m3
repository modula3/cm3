(* $Id$ *)

MODULE SchemeM3;

IMPORT Scheme;
FROM Scheme IMPORT E, Object, SymbolCheck;

IMPORT SchemeProcedure, SchemePrimitive;
IMPORT SchemeJailBreak, SchemeM3TableOps;
IMPORT XTime AS Time, Date, RTCollector, Pathname;
IMPORT OSError;
IMPORT SchemeSymbol;
FROM SchemeUtils IMPORT First, Second, Third, Stringify, Error;
IMPORT SchemeString, SchemeLongReal;
FROM SchemeLongReal IMPORT FromO;
FROM SchemeBoolean IMPORT True, Truth;
IMPORT SchemeBoolean;
IMPORT Fmt;
IMPORT TextRefSchemeAutoTbl;
IMPORT Stdio, Wr, Debug, AL, FileWr, Thread;
IMPORT IP, Process;
IMPORT SchemeEnvironment;
IMPORT TZ, SchemeUtils;
IMPORT NetObj;
IMPORT FS;
IMPORT SchemeProcedureStubs;
IMPORT RTType, RTBrand;
IMPORT FileRd, Rd;
IMPORT SchemeAtRun;
IMPORT SchemeCommandRunner;

<* FATAL Thread.Alerted *>

REVEAL
  T = Public BRANDED Brand OBJECT
    jailBreak : SchemeJailBreak.T := NIL;
    m3TableOps : SchemeM3TableOps.T := NIL;
  OVERRIDES
    init := Init;
    setTableOps       :=  SetTableOps;
  END;

PROCEDURE SetTableOps(t : T; to : SchemeM3TableOps.T) =
  BEGIN t.m3TableOps := to END SetTableOps;

PROCEDURE Init(t : T; 
               READONLY arr : ARRAY OF Pathname.T; 
               env : REFANY) : Scheme.T 
  RAISES { E } =
  BEGIN
    t.setPrimitives(prims); (* load in my special primitives *)
    RETURN Scheme.T.init(t,arr,env);
  END Init;

(**********************************************************************)

PROCEDURE HaveGlobalNameApply(<*UNUSED*>p : SchemeProcedure.T; 
                              interp : Scheme.T; 
                              args : Object) : Object 
  RAISES { E } =
  VAR
    e : SchemeEnvironment.T;
  BEGIN
    WITH sym = SymbolCheck(First(args)),
         ee = interp.getGlobalEnvironment() DO
      IF ISTYPE(ee, SchemeEnvironment.T) THEN
        e := ee
      ELSE
        RAISE E("Unknown error: global environment of wrong type?")
      END;

      TRY
        EVAL e.lookup(sym);
        RETURN SchemeBoolean.True()
      EXCEPT
        E => RETURN SchemeBoolean.False()
      END
    END
  END HaveGlobalNameApply;

PROCEDURE DefineNewGlobal(<*UNUSED*>p : SchemeProcedure.T; 
                          interp : Scheme.T; 
                          args : Object) : Object 
  RAISES { E } =
  VAR
    e : SchemeEnvironment.T;
  BEGIN
    WITH sym = SymbolCheck(First(args)),
         ee = interp.getGlobalEnvironment() DO
      IF ISTYPE(ee, SchemeEnvironment.T) THEN
        e := ee
      ELSE
        RAISE E("Unknown error: global environment of wrong type?")
      END;

      RETURN e.define(sym, Second(args))
    END
  END DefineNewGlobal;
    
PROCEDURE JailBreakApply(<*UNUSED*>p : SchemeProcedure.T; 
                         interp : Scheme.T; 
                         args : Object) : Object RAISES { E } =
  BEGIN
    WITH i = NARROW(interp, T) DO
      IF i.jailBreak = NIL THEN
        RETURN Error("No jailbreak defined")
      ELSE
        RETURN i.jailBreak.apply(args)
      END
    END
  END JailBreakApply;

PROCEDURE Modula3OpApply(<*UNUSED*>p : SchemeProcedure.T; interp : Scheme.T; args : Object) : Object RAISES { E } =
  BEGIN
    WITH i = NARROW(interp, T) DO
      IF i.m3TableOps = NIL THEN
        RETURN Error("No table ops defined")
      ELSE
        RETURN i.m3TableOps.apply(args)
      END
    END
  END Modula3OpApply;

PROCEDURE FmtRealApply(<*UNUSED*>p : SchemeProcedure.T; <*UNUSED*>interp : Scheme.T; args : Object) : Object RAISES { E } =
  BEGIN
    WITH x = First(args), y = Second(args), z = Third(args) DO
      VAR
        style : Fmt.Style;
      BEGIN
        IF    SchemeSymbol.SymEq(y, "auto") THEN
          style := Fmt.Style.Auto
        ELSIF SchemeSymbol.SymEq(y, "fix") THEN
          style := Fmt.Style.Fix
        ELSIF SchemeSymbol.SymEq(y, "sci") THEN
          style := Fmt.Style.Sci
        ELSE
          RETURN Error("Unknown formatting style " & Stringify(y))
        END;
        
        RETURN SchemeString.FromText(Fmt.LongReal(FromO(x),
                                                  style,
                                                  TRUNC(FromO(z))))
      END
    END
  END FmtRealApply;

PROCEDURE GCApply(<*UNUSED*>p : SchemeProcedure.T; <*UNUSED*>interp : Scheme.T; <*UNUSED*>args : Object) : Object =
  BEGIN RTCollector.Collect(); RETURN True() END GCApply;

PROCEDURE StringifyApply(<*UNUSED*>p : SchemeProcedure.T; <*UNUSED*>interp : Scheme.T; args : Object) : Object RAISES { E } =
  BEGIN 
    WITH x = First(args) DO
      RETURN SchemeString.FromText(Stringify(x)) 
    END
  END StringifyApply;

PROCEDURE TimeToStringApply(<*UNUSED*>p : SchemeProcedure.T; 
                            <*UNUSED*>interp : Scheme.T; 
                                      args : Object) : Object RAISES { E } =
    CONST
      Months = ARRAY Date.Month OF TEXT {
      "JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
      "JUL", "AUG", "SEP", "OCT", "NOV", "DEC" 
      };
    VAR
      x      := First(args);
      tzName := Second(args);
      t      := SchemeLongReal.FromO(x);
      d      : Date.T;
    BEGIN
      IF t > FLOAT(LAST(CARDINAL),Time.T) OR t < 0.0d0 THEN
        RETURN Error("Time out of range " & Fmt.LongReal(t))
      END;

      IF tzName = NIL THEN
        d := Date.FromTime(t)
      ELSE
        TRY
          WITH tz = NEW(TZ.T).init(SchemeString.ToText(tzName)) DO
            d := tz.localtime(t)
          END
        EXCEPT
          OSError.E(err) =>
          RAISE E("Couldn't handle TZ \"" & SchemeString.ToText(tzName) & 
                "\": OSError.E: " & AL.Format(err))
        END
      END;

      RETURN SchemeString.FromText(Fmt.F("%04s-%3s-%02s ",
                                         Fmt.Int(d.year),
                                         Months[d.month],
                                         Fmt.Int(d.day)) &
                                         Fmt.F("%02s:%02s:%02s.%03s",
                                               Fmt.Int(d.hour),
                                               Fmt.Int(d.minute),
                                               Fmt.Int(d.second),
                                               Fmt.Int(TRUNC((t - FLOAT(TRUNC(t),Time.T))*1000.0d0))))


  END TimeToStringApply;

PROCEDURE TimeToListApply(<*UNUSED*>p : SchemeProcedure.T; 
                            <*UNUSED*>interp : Scheme.T; 
                                      args : Object) : Object RAISES { E } =
  CONST 
    LR = SchemeLongReal.FromLR;
    I  = SchemeLongReal.FromI;
  VAR
    x      := First(args);
    tzName := Second(args);
    t      := SchemeLongReal.FromO(x);
    d      : Date.T;
  BEGIN
    IF t > FLOAT(LAST(CARDINAL),Time.T) OR t < 0.0d0 THEN
      RETURN Error("Time out of range " & Fmt.LongReal(t))
    END;
    
    IF tzName = NIL THEN
      d := Date.FromTime(t)
    ELSE
      TRY
        WITH tz = NEW(TZ.T).init(SchemeString.ToText(tzName)) DO
          d := tz.localtime(t)
        END
      EXCEPT
        OSError.E(err) =>
        RAISE E("Couldn't handle TZ \"" & SchemeString.ToText(tzName) & 
              "\": OSError.E: " & AL.Format(err))
      END
    END;
    
    WITH subsecs = t - FLOAT(TRUNC(t),Time.T) DO
      RETURN SchemeUtils.MakeList(
                 ARRAY OF Object { I(d.year),
                                   I(ORD(d.month)+1),
                                   I(d.day),
                                   I(ORD(d.weekDay)),
                                   I(d.hour),
                                   I(d.minute),
                                   LR(FLOAT(d.second,LONGREAL)+subsecs) })
    END

  END TimeToListApply;

PROCEDURE TimenowApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                       <*UNUSED*>args : Object) : Object =
  BEGIN
    RETURN SchemeLongReal.FromLR(Time.Now())
  END TimenowApply;

PROCEDURE NetObjExportGlobalEnvApply(<*UNUSED*>p : SchemeProcedure.T; 
                                     interp      : Scheme.T; 
                                     args        : Object) : Object 
  RAISES { E } =
  BEGIN
    WITH nam = First(args) DO
      TRY
        NetObj.Export(SchemeString.ToText(nam),
                      interp.getGlobalEnvironment());
        RETURN nam
      EXCEPT
        NetObj.Error(err) => RAISE E("NetObjExportGlobalEnv: caught NetObj.Error : " & AL.Format(err))
      END
    END
  END NetObjExportGlobalEnvApply;

PROCEDURE StdioStderrApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                       <*UNUSED*>args : Object) : Object =
  BEGIN
    RETURN Stdio.stderr
  END StdioStderrApply;

PROCEDURE DebugAddstreamApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  BEGIN
    WITH x = First(args) DO
      IF x = NIL OR NOT ISTYPE(x, Wr.T) THEN
        RETURN Error ("Not a Wr.T: " & Stringify(x))
      END;
      Debug.AddStream(x);
      RETURN x
    END
  END DebugAddstreamApply;

PROCEDURE DebugSetLevelApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  BEGIN
    WITH x = TRUNC(SchemeLongReal.FromO(First(args))) DO
      Debug.SetLevel(x);
      RETURN First(args)
    END
  END DebugSetLevelApply;

PROCEDURE DebugRemstreamApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  BEGIN
    WITH x = First(args) DO
      IF x = NIL OR NOT ISTYPE(x, Wr.T) THEN
        RETURN Error ("Not a Wr.T: " & Stringify(x))
      END;
      Debug.RemStream(x);
      RETURN x
    END
  END DebugRemstreamApply;

PROCEDURE FileWrOpenApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  VAR
    fn : Pathname.T;
    append := FALSE;
  BEGIN
    IF ISTYPE(First(args),SchemeSymbol.T) THEN
      WITH sym = First(args) DO
        IF sym = SchemeSymbol.FromText("append") THEN
          append := TRUE
        END
      END;
      fn := SchemeString.ToText(Second(args))
    ELSE
      fn := SchemeString.ToText(First(args))
    END;

    TRY
      IF append THEN
        RETURN FileWr.OpenAppend(fn)
      ELSE
        RETURN FileWr.Open(fn)
      END
    EXCEPT
      OSError.E(err) => RETURN Error("FileWrOpenApply : " & AL.Format(err))
    END
  END FileWrOpenApply;

PROCEDURE FileRmApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  BEGIN
    WITH fn = SchemeString.ToText(First(args)) DO
      TRY
        FS.DeleteFile(fn)
      EXCEPT
        OSError.E(err) => RETURN Error("FileRmApply : " & AL.Format(err))
      END
    END;
    RETURN SchemeBoolean.True()
  END FileRmApply;

PROCEDURE FileMvApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  BEGIN
    WITH fn1 = SchemeString.ToText(First(args)), 
         fn2 = SchemeString.ToText(Second(args)) DO
      TRY
        FS.Rename(fn1,fn2)
      EXCEPT
        OSError.E(err) => RETURN Error("FileMvApply : " & AL.Format(err))
      END
    END;
    RETURN SchemeBoolean.True()
  END FileMvApply;

PROCEDURE FileCmpApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  PROCEDURE Close() =
    BEGIN
      IF rd1 # NIL THEN TRY Rd.Close(rd1) EXCEPT ELSE END END;
      IF rd2 # NIL THEN TRY Rd.Close(rd2) EXCEPT ELSE END END;
    END Close;

  VAR
    rd1, rd2 : Rd.T := NIL;
  BEGIN
    WITH fn1 = SchemeString.ToText(First(args)), 
         fn2 = SchemeString.ToText(Second(args)) DO
      TRY
        CONST BufSiz = 1024;
        VAR
          buff1, buff2 : ARRAY [0..BufSiz-1] OF CHAR;
          l1, l2 : CARDINAL;
        BEGIN
          rd1 := FileRd.Open(fn1);
          rd2 := FileRd.Open(fn2);
          REPEAT 
            l1 := Rd.GetSub(rd1,buff1);
            l2 := Rd.GetSub(rd2,buff2);
            IF l1 # l2 OR SUBARRAY(buff1,0,l1) # SUBARRAY(buff2,0,l2) THEN 
              Close(); RETURN SchemeBoolean.False() 
            END
          UNTIL l1 # BufSiz;
          Close(); RETURN SchemeBoolean.True()
        END
      EXCEPT
        
        OSError.E(err) => Close(); RETURN Error("FileMvApply : OSError.E : " & AL.Format(err))
      |
        Rd.Failure(err) => Close(); RETURN Error("FileMvApply : Rd.Failure : " & AL.Format(err))

      END
    END
  END FileCmpApply;

PROCEDURE DebugSetEnvApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  BEGIN
    WITH x = SchemeString.ToText(First(args)) DO
      TRY
        Debug.SetEnv(x); RETURN True()
      EXCEPT
        (*
        OSError.E(err) => RETURN Error("DebugSetEnvApply : " & AL.Format(err))
        *)
      END
    END
  END DebugSetEnvApply;

PROCEDURE DebugClearEnvApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  BEGIN
    WITH x = SchemeString.ToText(First(args)) DO
      TRY
        Debug.ClearEnv(x); RETURN True()
      EXCEPT
        (*
          OSError.E(err) => RETURN Error("DebugClearEnvApply : " & AL.Format(err))
          *)
      END
    END
  END DebugClearEnvApply;

PROCEDURE DebugHaveEnvApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  BEGIN
    WITH x = SchemeString.ToText(First(args)) DO
      TRY
        RETURN Truth(Debug.HaveEnv(x))
      EXCEPT
        (*
        OSError.E(err) => RETURN Error("DebugHaveEnvApply : " & AL.Format(err))
        *)
      END
    END
  END DebugHaveEnvApply;

PROCEDURE WrCloseApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  BEGIN
    WITH x = (First(args)) DO
      TRY
        IF x = NIL OR NOT ISTYPE(x, Wr.T) THEN
          RETURN Error("WrCloseApply : type error : " & Stringify(x))
        END;
        Wr.Close(x);
        RETURN True()
      EXCEPT
        Wr.Failure(err) => RETURN Error("WrCloseApply : Wr.Failure : " & AL.Format(err))
      END
    END
  END WrCloseApply;

(* this is sort-of thread safe *)
VAR myHostName : SchemeSymbol.T := NIL;

PROCEDURE FmtAddr(ip : IP.Address) : TEXT =
  VAR 
    res := "";
  BEGIN
    FOR i := FIRST(ip.a) TO LAST(ip.a) DO
      res := res & Fmt.Int(ip.a[i]);
      IF i # LAST(ip.a) THEN res := res & "." END
    END;
    RETURN res
  END FmtAddr;

PROCEDURE HostnameApply(<*UNUSED*>p : SchemeProcedure.T; 
                        <*UNUSED*>interp : Scheme.T; 
                        <*UNUSED*>args : Object) : Object RAISES { E } =
  BEGIN
    IF myHostName # NIL THEN RETURN myHostName END;

    TRY
      WITH hostAddr = IP.GetHostAddr(),
           hostCanon= IP.GetCanonicalByAddr(hostAddr) DO
        IF hostCanon = NIL THEN
          RAISE E ("No host name mapping for address " & FmtAddr(hostAddr))
        END;
        myHostName := SchemeSymbol.Symbol(hostCanon)
      END;
      RETURN myHostName
    EXCEPT
      IP.Error(ec) =>
      RETURN Error("HostnameApply : IP.Error : " & AL.Format(ec))
    END
  END HostnameApply;

(* this is sort-of thread safe *)
VAR myUnixPID : SchemeLongReal.T := NIL;

PROCEDURE GetUnixPIDApply(<*UNUSED*>p : SchemeProcedure.T; 
                        <*UNUSED*>interp : Scheme.T; 
                        <*UNUSED*>args : Object) : Object =
  BEGIN
    IF myUnixPID # NIL THEN RETURN myUnixPID END;

    myUnixPID := SchemeLongReal.FromI(Process.GetMyID());
    
    RETURN myUnixPID
  END GetUnixPIDApply;

(**********************************************************************)

PROCEDURE RTBrandGetNameApply(<*UNUSED*>p : SchemeProcedure.T; 
                        <*UNUSED*>interp : Scheme.T; 
                        args : Object) : Object RAISES { E } =
  BEGIN
    TRY
      RETURN SchemeString.FromText(RTBrand.GetName(SchemeLongReal.Card(First(args))))
    EXCEPT 
      RTBrand.NotBranded => RAISE E ("RTBrand.NotBranded")
    END
  END RTBrandGetNameApply;

PROCEDURE TypecodeApply(<*UNUSED*>p : SchemeProcedure.T; 
                        <*UNUSED*>interp : Scheme.T; 
                        args : Object) : Object =
  BEGIN
    RETURN SchemeLongReal.FromI(TYPECODE(First(args)))
  END TypecodeApply;

PROCEDURE MaxTypecodeApply(<*UNUSED*>p : SchemeProcedure.T; 
                           <*UNUSED*>interp : Scheme.T; 
                           <*UNUSED*>args : Object) : Object =
  BEGIN
    RETURN SchemeLongReal.FromI(RTType.MaxTypecode())
  END MaxTypecodeApply;

PROCEDURE SupertypeApply(<*UNUSED*>p : SchemeProcedure.T; 
                         <*UNUSED*>interp : Scheme.T; 
                         args : Object) : Object RAISES { E } =
  VAR
    tc := SchemeLongReal.Int(First(args));
  BEGIN
    IF tc < FIRST(RTType.Typecode) OR 
       tc > LAST(RTType.Typecode) OR
       tc > RTType.MaxTypecode() THEN
      RAISE E("SchemeM3.SupertypeApply: typecode out of range: " & Stringify(First(args)))
    END;
    RETURN SchemeLongReal.FromI(RTType.Supertype(tc))
  END SupertypeApply;

PROCEDURE IsSubtypeApply(<*UNUSED*>p : SchemeProcedure.T; 
                         <*UNUSED*>interp : Scheme.T; 
                         args : Object) : Object RAISES { E } =
  VAR
    tca := SchemeLongReal.Int(First(args));
    tcb := SchemeLongReal.Int(Second(args));
  BEGIN
    IF tca < FIRST(RTType.Typecode) OR 
       tca > LAST(RTType.Typecode) OR
       tca > RTType.MaxTypecode() THEN
      RAISE E("SchemeM3.SupertypeApply: typecode out of range: " & Stringify(First(args)))
    END;
    IF tcb < FIRST(RTType.Typecode) OR 
       tcb > LAST(RTType.Typecode) OR
       tcb > RTType.MaxTypecode() THEN
      RAISE E("SchemeM3.SupertypeApply: typecode out of range: " & Stringify(Second(args)))
    END;
    RETURN SchemeBoolean.Truth(RTType.IsSubtype(tca,tcb))
  END IsSubtypeApply;

PROCEDURE GetNDimensionsApply(<*UNUSED*>p : SchemeProcedure.T; 
                              <*UNUSED*>interp : Scheme.T; 
                              args : Object) : Object RAISES { E } =
  VAR
    tc := SchemeLongReal.Int(First(args));
  BEGIN
    IF tc < FIRST(RTType.Typecode) OR 
       tc > LAST(RTType.Typecode) OR
       tc > RTType.MaxTypecode() THEN
      RAISE E("SchemeM3.GetNDimensionsApply: typecode out of range: " & Stringify(First(args)))
    END;
    RETURN SchemeLongReal.FromI(RTType.GetNDimensions(tc))
  END GetNDimensionsApply;

PROCEDURE FSStatusModificationTimeApply(<*UNUSED*>p : SchemeProcedure.T; 
                              <*UNUSED*>interp : Scheme.T; 
                              args : Object) : Object RAISES { E } =
  BEGIN
    TRY
      WITH status = FS.Status(SchemeString.ToText(First(args))) DO
        RETURN SchemeLongReal.FromLR(status.modificationTime)
      END
    EXCEPT
      OSError.E(al) => RAISE E("FSStatusModificationTimeApply: OSError.E: " & AL.Format(al))
    END
  END FSStatusModificationTimeApply;

(**********************************************************************)

PROCEDURE ExtendWithM3(prims : SchemePrimitive.ExtDefiner)  : SchemePrimitive.ExtDefiner =
  BEGIN 
    prims.addPrim("jailbreak", NEW(SchemeProcedure.T, 
                                   apply := JailBreakApply), 
                  1, 1);
    
    prims.addPrim("stringify", NEW(SchemeProcedure.T, 
                                   apply := StringifyApply), 
                  1, 1);

    prims.addPrim("fmtreal", NEW(SchemeProcedure.T,
                                 apply := FmtRealApply), 
                  3, 3);

    prims.addPrim("gc", NEW(SchemeProcedure.T, 
                            apply := GCApply),
                  0, 0);

    prims.addPrim("timenow", NEW(SchemeProcedure.T, 
                                 apply := TimenowApply), 
                  0, 0);

    prims.addPrim("time->string", NEW(SchemeProcedure.T, 
                                      apply := TimeToStringApply), 
                  1, 2);

    prims.addPrim("time->list", NEW(SchemeProcedure.T, 
                                      apply := TimeToListApply), 
                  1, 2);

    prims.addPrim("modula-3-op", NEW(SchemeProcedure.T, 
                                     apply := Modula3OpApply), 
                  2, 3);

    prims.addPrim("stdio-stderr", NEW(SchemeProcedure.T,
                                      apply := StdioStderrApply), 
                  0, 0);

    prims.addPrim("debug-addstream", NEW(SchemeProcedure.T,
                                      apply := DebugAddstreamApply), 
                  1, 1);

    prims.addPrim("debug-setlevel", NEW(SchemeProcedure.T,
                                      apply := DebugSetLevelApply), 
                  1, 1);

    prims.addPrim("debug-remstream", NEW(SchemeProcedure.T,
                                      apply := DebugRemstreamApply), 
                  1, 1);

    prims.addPrim("debug-setenv", NEW(SchemeProcedure.T,
                                      apply := DebugSetEnvApply),
                  1, 1);

    prims.addPrim("debug-clearenv", NEW(SchemeProcedure.T,
                                      apply := DebugClearEnvApply),
                  1, 1);

    prims.addPrim("debug-haveenv", NEW(SchemeProcedure.T,
                                      apply := DebugHaveEnvApply),
                  1, 1);

    prims.addPrim("filewr-open", NEW(SchemeProcedure.T,
                                      apply := FileWrOpenApply), 
                  1, 2);

    prims.addPrim("remove-file", NEW(SchemeProcedure.T,
                                     apply := FileRmApply),
                  1, 1);

    prims.addPrim("fs-rename", NEW(SchemeProcedure.T,
                                     apply := FileMvApply),
                  2, 2);

    prims.addPrim("cmp-files", NEW(SchemeProcedure.T,
                                     apply := FileCmpApply),
                  2, 2);

    prims.addPrim("wr-close", NEW(SchemeProcedure.T,
                                      apply := WrCloseApply), 
                  1, 1);

    prims.addPrim("hostname", NEW(SchemeProcedure.T, 
                                 apply := HostnameApply), 
                  0, 0);

    prims.addPrim("getunixpid", NEW(SchemeProcedure.T, 
                                    apply := GetUnixPIDApply), 
                  0, 0);

    prims.addPrim("global-exists?", NEW(SchemeProcedure.T, 
                                        apply := HaveGlobalNameApply), 
                  1, 1);

    prims.addPrim("define-global-symbol", NEW(SchemeProcedure.T, 
                                              apply := DefineNewGlobal), 
                  2, 2);

    (* RTType things *)

    prims.addPrim("rttype-typecode", NEW(SchemeProcedure.T,
                                         apply := TypecodeApply),
                  1, 1);

    prims.addPrim("rttype-maxtypecode", NEW(SchemeProcedure.T,
                                            apply := MaxTypecodeApply),
                  0, 0);
    
    prims.addPrim("rttype-supertype", NEW(SchemeProcedure.T,
                                          apply := SupertypeApply),
                  1, 1);

    prims.addPrim("rttype-issubtype?", NEW(SchemeProcedure.T,
                                          apply := IsSubtypeApply),
                  2, 2);

    prims.addPrim("rttype-ndimensions", NEW(SchemeProcedure.T,
                                            apply := GetNDimensionsApply),
                  1, 1);

    prims.addPrim("rtbrand-getname", NEW(SchemeProcedure.T, apply := RTBrandGetNameApply),
                  1, 1);

    prims.addPrim("fs-status-modificationtime",NEW(SchemeProcedure.T,
                                            apply := FSStatusModificationTimeApply),
                  1, 1);


    (****************************************)

    prims.addPrim("netobj-export-global-environment",
                  NEW(SchemeProcedure.T, 
                      apply := NetObjExportGlobalEnvApply), 
                  1, 1);

    RETURN prims
  END ExtendWithM3;

PROCEDURE GetPrims() : SchemePrimitive.ExtDefiner = 
  BEGIN RETURN prims END GetPrims;

VAR 
  prims := NEW(SchemePrimitive.ExtDefiner).init();
BEGIN 
  TextRefSchemeAutoTbl.Register(); (* vide module initialization order,
                                      Green Book *)
  prims := ExtendWithM3(prims);
  SchemeEnvironment.ExtendWithIntrospectionPrimitives(prims)  ;
  prims := SchemeProcedureStubs.Extend(prims);
  prims := SchemeAtRun.Extend(prims);
  prims := SchemeCommandRunner.Extend(NEW(SchemeCommandRunner.TextOutputParser),prims)
END SchemeM3.
