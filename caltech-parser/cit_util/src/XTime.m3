(* $Id$ *)

MODULE XTime;
IMPORT Time;
IMPORT TextReader, Lex, FloatMode, IP, RTParams, Scan;
IMPORT Fmt;
IMPORT Wr, Stdio, Env, Thread;
IMPORT TCP, AL, Process;
IMPORT Rd, ConnRW;
(* were not allowed to import Debug, since Debug imports us *)

PROCEDURE Now() : T =
  BEGIN
    <*ASSERT initialized*>
    IF active THEN
      RETURN FakeNow()
    ELSE
      RETURN Time.Now()
    END
  END Now;

(**********************************************************************)

PROCEDURE CurrentOffset(now : T) : T =
  (* mu must be LOCKed *)
  BEGIN
    IF    now > breaks[End.End].start THEN
      RETURN breaks[End.End].offset
    ELSE
      WITH astart = breaks[End.Begin].start,
           astop  = breaks[End.End].start,
           ooffset = breaks[End.Begin].offset,
           noffset = breaks[End.End].offset DO
        
        <* ASSERT now >= astart *>
        RETURN (now - astart)/(astop - astart) * (noffset-ooffset) + 
               ooffset
      END
    END
  END CurrentOffset;

PROCEDURE FakeNow() : T =
  BEGIN
    LOCK mu DO
      WITH now = Time.Now() DO
        RETURN now + CurrentOffset(now)
      END
    END
  END FakeNow;

TYPE 
  Break = RECORD
    offset : T;
    start  : T;
  END;

  End = { Begin, End };

VAR
  active := FALSE;
  breaks    : ARRAY End OF Break;
  mu     := NEW(MUTEX);

PROCEDURE SetOffset(to : T) =
  BEGIN
    LOCK mu DO
      breaks[End.End].offset := to;
      breaks[End.End].start  := FIRST(T);
      active := TRUE;
    END
  END SetOffset;

PROCEDURE AdjustOffset(to : T; absRate := 0.1d0; maxDelta := 30.0d0) 
  RAISES { CantAdjust } =
  BEGIN
    LOCK mu DO
      WITH now         = Time.Now(),
           curOff      = CurrentOffset(now),
           adjInterval = ABS(to-curOff)/absRate DO

        (*
        DebugOut("XTime.AdjustOffset: now=" & Fmt.LongReal(now) &
                                    " cur=" & Fmt.LongReal(curOff) &
                                    "  to=" & Fmt.LongReal(to) &
                                    " dlt=" & Fmt.LongReal(to-curOff) &
                                    " ivl=" & Fmt.LongReal(adjInterval));
        *)

        IF NOT initialized THEN
          breaks[End.End] := Break { to, now }
        ELSIF NOT adjInterval <= maxDelta THEN 
          (* 
             careful about overflow / NaN here-- reason for NOT <= rather than
             >.

             also careful that we must allow the very FIRST initialization to
             be large (initialized) 
          *)

          RAISE CantAdjust
        ELSE
          breaks[End.Begin] :=  Break { curOff, now };
          breaks[End.End]   :=  Break { to    , now + adjInterval }
        END
      END;
      active := TRUE
    END
  END AdjustOffset;

PROCEDURE SetXTime(to : T; adjust : BOOLEAN; absRate, maxDelta : LONGREAL) 
  RAISES { CantAdjust } =
  VAR
    newOff : LONGREAL;
    fakeNow := Now();
    realNow := Time.Now();
  BEGIN
    LOCK mu DO
      WITH 
           curOff = CurrentOffset(realNow),
           delta  = to-fakeNow DO
        newOff := curOff + delta
      END
    END;
    IF adjust THEN
      AdjustOffset(newOff, absRate, maxDelta)
    ELSE
      SetOffset(newOff)
    END
  END SetXTime;

(**********************************************************************)

VAR dLevelT := Env.Get("DEBUGLEVEL");
    dLevel := 0;

PROCEDURE DebugOut(txt : TEXT) =
  BEGIN
    IF dLevel >= 10 THEN
      TRY
        Wr.PutText(Stdio.stderr, txt & "\n");
        Wr.Flush(Stdio.stderr)
      EXCEPT
        Thread.Alerted, Wr.Failure => (* just debug out, skip *)
      END
    END
  END DebugOut;

TYPE 
  Closure = Thread.SizedClosure OBJECT
    rd : Rd.T;
    mu : MUTEX;
    c  : Thread.Condition;
    initialized := FALSE;
  OVERRIDES
    apply := ClApply;
  END;

PROCEDURE ClApply(cl : Closure) : REFANY =
  <*FATAL Thread.Alerted, Wr.Failure*>
  VAR
    line : TEXT;
  BEGIN
    TRY
      LOOP
        (* 
           it would be better to send along Time.Now() on the remote
           system (the real time) and compare it to what we expect the
           real time to be on the local system (initializing this on
           startup, tracking it after).  Remember that we are dealing
           with two deltas here: delta in Time.Now() remote vs. local,
           as well as delta between XTime.Now() remote vs. local.

           For now, the following tricky hack is supposed to reject
           stale time updates setting on the network link, e.g., if we
           just returned from suspension...

        *)

        line := Rd.GetLine(cl.rd);

        WHILE Rd.CharsReady(cl.rd) > 0 DO line := Rd.GetLine(cl.rd) END;

        (* set the time ! *)
        WITH now = Time.Now(),
             tgt = Scan.LongReal(line) DO
          TRY
            AdjustOffset(tgt - now)
          EXCEPT
            CantAdjust =>
            (* this can happen during debugging! *)
            Wr.PutText(Stdio.stderr, 
                       "WARNING: XTime.ClApply: can't adjust time by " & 
                       Fmt.LongReal(tgt - now) & " seconds\n")
          END
        END;
          
        IF NOT cl.initialized THEN
          LOCK cl.mu DO
            cl.initialized := TRUE;
            Thread.Broadcast(cl.c)
          END
        END
      END
    EXCEPT
      Rd.EndOfFile, Rd.Failure => 
      Wr.PutText(Stdio.stderr, 
          "Warning: lost connection to XTime source, clock now free-running");
      Wr.Flush(Stdio.stderr);
      RETURN NIL 
    |
      Lex.Error, FloatMode.Trap =>
      Process.Crash("Error: XTime source data mis-formatted");
      <*ASSERT FALSE*>
    END
  END ClApply;

<*FATAL TextReader.NoMore, Lex.Error, FloatMode.Trap*>
VAR
  initialized := FALSE;
BEGIN
  Grain := Time.Grain;

  IF dLevelT # NIL THEN dLevel := Scan.Int(dLevelT) END;

  (* search for an arg of type @M3xtime=<host>:<port> *)
  WITH xtimespec = RTParams.Value("xtime") DO
    IF xtimespec # NIL THEN
      WITH sReader = NEW(TextReader.T).init(xtimespec),
           host = sReader.nextE(":"),
           port = Scan.Int(sReader.nextE("")) DO
        DebugOut("Connecting to XTime source at "& 
          host & ", port " & Fmt.Int(port));
        
        <*FATAL Thread.Alerted*>
        VAR
          ip : IP.Address;
          success := FALSE;
          conn : TCP.T;
        BEGIN
          TRY
            success := IP.GetHostByName(host, ip)
          EXCEPT
            IP.Error => (* skip *)
          END;

          IF NOT success THEN
            Process.Crash("Couldn't connect to XTime source at \"" & 
              host & "\": host name lookup failure")
          END;

          WITH ep = IP.Endpoint { ip, port } DO
            TRY
              conn := TCP.Connect(ep);
              
              WITH cl = NEW(Closure,
                            stackSize := 4096,
                            rd := ConnRW.NewRd(conn), 
                            mu := NEW(MUTEX), 
                            c  := NEW(Thread.Condition)) DO
                EVAL Thread.Fork(cl);

                DebugOut("Awaiting XTime initialization via TCP...");
                LOCK cl.mu DO
                  WHILE NOT cl.initialized DO
                    Thread.Wait(cl.mu, cl.c)
                  END
                END
              END
            EXCEPT
              IP.Error(e) =>
              Process.Crash("Couldn't connect to XTime source at \"" & 
                host & "\": TCP connection failure : " & AL.Format(e))
            END
          END
        END
      END
    END
  END;

  initialized := TRUE
END XTime.
