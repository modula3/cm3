(* $Id$ *)

MODULE SchemeProfiler;
IMPORT SchemeProcedure;
FROM SchemeUnixDeps IMPORT RUSAGE_SELF, struct_rusage, GetErrno, struct_timeval;
IMPORT Fmt, Debug;
IMPORT LongrealPQ, TextRefTbl;
IMPORT XTime AS Time;
IMPORT TextIntTbl;
IMPORT SchemeProfilerSysDep;

PROCEDURE EnterProcedure(p : SchemeProcedure.T) =
  BEGIN
    IF enabled = TRUE THEN
      LOCK mu DO
        VAR
          rusage_retval := SchemeProfilerSysDep.getrusage(RUSAGE_SELF, rusage);
          (* RUSAGE_CHILDREN too? *)
        BEGIN
          IF rusage_retval < 0 THEN
            Debug.Warning("SchemeProfiler.EnterProcedure: getrusage failed, disabling profiling, errno=" & 
              Fmt.Int(GetErrno()));
            enabled := FALSE
          ELSE
            (* do calcs *)
            WITH time = Time.Now(),
                 name = p.format() DO
              IF first THEN
                (* first time, do nothing *)
                first := FALSE
              ELSE
                (* not first time, record measurements *)
                WITH rec = GetStats(oname) DO
                  rec.name := oname;
                  rec.wall := rec.wall + (otime-time);
                  rec.cpu := rec.cpu + Cpu(rusage)-Cpu(orusage);
                  IF ooname = NIL THEN ooname := "--spontaneous--" END;
                  VAR calls := 0; BEGIN
                    EVAL rec.callsFrom.get(ooname, calls);
                    INC(calls);
                    EVAL rec.callsFrom.put(ooname, calls)
                  END;
                  PutStats(rec)
                END
              END;
              orusage := rusage;
              otime := time;
              ooname := oname;
              oname := name;
            END
          END
        END
      END
    END
  END EnterProcedure;

PROCEDURE PutStats(s : REF Stats) = BEGIN EVAL tab.put(s.name,s) END PutStats;

PROCEDURE GetStats(n : TEXT) : REF Stats = 
  VAR r : REFANY;
  BEGIN
    IF tab.get(n,r) THEN 
      RETURN r
    ELSE
      RETURN NEW(REF Stats,
                 callsFrom := NEW(TextIntTbl.Default).init(),
                 name := n,
                 wall := 0.0d0,
                 cpu := 0.0d0)
    END
  END GetStats;

PROCEDURE FromTimeval(READONLY tv : struct_timeval) : Time.T =
  BEGIN
    RETURN FLOAT(tv.tv_sec,Time.T) + 1.0d-6 * FLOAT(tv.tv_usec,Time.T)
  END FromTimeval;

PROCEDURE Cpu(READONLY ru : struct_rusage) : Time.T =
  BEGIN RETURN FromTimeval(ru.ru_utime) + FromTimeval(ru.ru_stime) END Cpu;

VAR mu := NEW(MUTEX);
    enabled := FALSE;
    rusage, orusage : struct_rusage;
    tab := NEW(TextRefTbl.Default).init();
    first := TRUE;
    otime : Time.T;
    oname, ooname : TEXT := NIL;

PROCEDURE Enable() = 
  BEGIN LOCK mu DO enabled := TRUE; first := TRUE END END Enable;

PROCEDURE Disable() = BEGIN LOCK mu DO enabled := FALSE END END Disable;

PROCEDURE TopN(n : CARDINAL; by : By) : REF ARRAY OF Stats =

  PROCEDURE Metric(READONLY s : Stats) : Time.T =
    BEGIN
      CASE by OF
        By.Wallclock => RETURN s.wall
      |
        By.CPU => RETURN s.cpu
      END
    END Metric;

  <*FATAL LongrealPQ.Empty*>
  TYPE
    Elt = LongrealPQ.Elt OBJECT s : REF Stats END;
  VAR
    res : REF ARRAY OF Stats;
  BEGIN 
    LOCK mu DO
      res := NEW(REF ARRAY OF Stats, MIN(n, tab.size()));
      WITH pq = NEW(LongrealPQ.Default).init(),
           iter = tab.iterate() DO
        VAR
          n : TEXT;
          s : REFANY;
        BEGIN
          WHILE iter.next(n,s) DO
            WITH metric = Metric(NARROW(s,REF Stats)^) DO
              pq.insert(NEW(Elt, 
                            priority := -metric,
                            s := s))
            END
          END
        END;

        FOR i := FIRST(res^) TO LAST(res^) DO
          res[i] := NARROW(pq.deleteMin(),Elt).s^
        END
      END
    END;
    RETURN NIL 
  END TopN;

BEGIN END SchemeProfiler.
