(* Copyright 1992 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* File: SortSoundsView.m3                                   *)
(* Last modified on Thu Jun 20 17:24:22 PDT 1996 by heydon   *)
(*      modified on Mon Jan  9 13:40:46 PST 1995 by najork   *)
(*      modified on Mon Mar  8 09:31:14 PST 1993 by mhb      *)
(*      modified on Fri Nov 20 15:48:00 PST 1992 by sclafani *)
(*      modified on Fri Apr 19 19:14:51 PDT 1991 by johnh    *)

MODULE SortSoundsView;

IMPORT FormsVBT, Lex, Midi, Rd, Scan, Thread, VBT, Wr, FloatMode;
IMPORT Sort, SortAlgClass, SortViewClass, View, Zeus, ZeusClass, ZeusPanel;

FROM Midi IMPORT Channel;

<* FATAL Midi.Failure, Wr.Failure, Thread.Alerted *>
<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>

TYPE
  T = SortViewClass.T BRANDED OBJECT
        player: Midi.T;
        fv    : FormsVBT.T;
        channels: ARRAY Channel OF
                    RECORD
                      volume      : INTEGER;
                      timbre      : INTEGER;
                      active      : BOOLEAN;
                      lastNote    : Midi.Note;
                      lastVelocity: Midi.Velocity;
                    END;
      OVERRIDES
        init       := ViewInit;
        startrun   := Startrun;
        endrun     := Endrun;
        delete     := Delete;
        snapshot   := Snapshot;
        restore    := Restore;
        reactivity := Reactivity;
        oeInit     := Init;
        oeSetVal   := SetVal;
        oeSwapElts := SwapElts;
        discard    := Discarded;
      END;


(* * * * * Init Routines * * * * *)

PROCEDURE InitMidi (view: T) =
  VAR server := FormsVBT.GetText(view.fv, "audioServer");
  BEGIN
    view.player := NIL;
    TRY
      view.player := Midi.Open(server)
    EXCEPT
      Midi.Failure =>
        ZeusPanel.ReportError("cannot open " & server);
    END;
  END InitMidi;

(* * * * * Zeus Routines * * * * *)

PROCEDURE New (): View.T =
  BEGIN
    TRY
      WITH view = NEW (T) DO
        view.fv := ZeusPanel.NewForm ("noise.fv");
        FormsVBT.AttachProc (view.fv, "mute", MuteP, view);
        RETURN view.init (view.fv);
      END;
    EXCEPT FormsVBT.Error (err) =>
      ZeusPanel.ReportError (err);
    END;
    RETURN NIL;
  END New;

PROCEDURE ViewInit (view: T; ch: VBT.T): View.T =
  BEGIN
    FOR channel := FIRST (Channel) TO LAST (Channel) DO
      WITH c = view.channels [channel] DO
        c.timbre := -1;
        c.volume := -1;
        c.active := FALSE;
        c.lastNote := 0;
        c.lastVelocity := 0;
      END;
    END;
    RETURN SortViewClass.T.init (view, ch);
  END ViewInit;

PROCEDURE Startrun (view: T) =
  BEGIN
    SortViewClass.T.startrun (view);
    InitMidi (view);
    (* Don't erase FV control panel *)
  END Startrun;

PROCEDURE Endrun (view: T) =
  BEGIN
    SilenceView (view);
    SortViewClass.T.endrun (view);
  END Endrun;

PROCEDURE Snapshot (view: T; wr: Wr.T) RAISES {ZeusClass.Error} =
  BEGIN
    Wr.PutText(wr, "(");
    view.fv.snapshot(wr);
    SortViewClass.T.snapshot(view, wr);
    Wr.PutText(wr, ")");
  END Snapshot;

PROCEDURE Restore (view: T; rd: Rd.T) RAISES {ZeusClass.Error} =
  BEGIN
    IF rd = NIL THEN
      SortViewClass.T.restore(view, rd);
    ELSE
      TRY
        Lex.Skip(rd);
        Lex.Match(rd, "(");
        view.fv.restore(rd);
        SortViewClass.T.restore(view, rd);
        Lex.Skip(rd);
        Lex.Match(rd, ")");
      EXCEPT
        Lex.Error, Rd.Failure, FormsVBT.Mismatch,
            FormsVBT.Error =>
          RAISE ZeusClass.Error("SortSoundsView.Restore error");
      END;
    END;
  END Restore;

PROCEDURE Reactivity (<* UNUSED *> t: T; <* UNUSED *> on: BOOLEAN) =
  BEGIN
  END Reactivity;

PROCEDURE Delete (view: T) =
  BEGIN
    SilenceView (view);
    SortViewClass.T.delete (view);
  END Delete;

PROCEDURE Discarded (view: T) =
  BEGIN
    SilenceView (view);
    SortViewClass.T.discard (view);
  END Discarded;


(* * * * * Control Panel Callbacks * * * * *)

PROCEDURE MuteP (<*UNUSED*> fv   : FormsVBT.T;
                 <*UNUSED*> event: TEXT;
                            arg  : REFANY;
                 <*UNUSED*> time : VBT.TimeStamp) =
  BEGIN
    SilenceView (NARROW (arg, T));
  END MuteP;

(* * * * * Event Processing Routines * * * * *)

PROCEDURE Init (view: T; N: CARDINAL; passes: CARDINAL) =
  BEGIN
    SortViewClass.T.oeInit (view, N, passes);
  END Init;

PROCEDURE SetVal (view: T; i: CARDINAL; key: Sort.Key) =
  BEGIN
    SortViewClass.T.oeSetVal (view, i, key);
    Silence (view, "exchange1");
    Silence (view, "exchange2");
    MakeSound (view, "set", i);
  END SetVal;

PROCEDURE SwapElts (view: T; i, j: CARDINAL) =
  BEGIN
    SortViewClass.T.oeSwapElts (view, i, j);
    Silence (view, "set");
    MakeSound (view, "exchange1", i);
    MakeSound (view, "exchange2", j);
  END SwapElts;


(* * * * * Generate Sound * * * * *)

PROCEDURE Note (zcl: SortAlgClass.T; k: INTEGER): INTEGER =
  BEGIN
    IF zcl.N > 88 THEN
      RETURN 32 + (64 * zcl.a [k]) DIV zcl.N;
    ELSE
      RETURN 16_40 - zcl.N DIV 2 + zcl.a [k];
    END;
  END Note;

PROCEDURE MakeSound (view: T; event: TEXT; val: INTEGER) =
  BEGIN
    Play (view, event, Note (Zeus.Resolve (view).alg, val));
  END MakeSound;

PROCEDURE SilenceView (view: T) =
  BEGIN
    IF view.player = NIL THEN RETURN END;
    FOR ch := FIRST (Channel) TO LAST (Channel) DO
      WITH c = view.channels [ch] DO
        IF c.active THEN
          Midi.EndNote (view.player, ch, c.lastNote, c.lastVelocity);
          c.active := FALSE;
        END;
      END;
    END;
  END SilenceView;

PROCEDURE Silence (view: T; event: TEXT) =
  VAR channel: Channel;
  BEGIN
    IF view.player = NIL THEN RETURN END;
    channel := GetInteger (view.fv, event & "Channel");
    WITH c = view.channels [channel] DO
      IF c.active THEN
        Midi.EndNote (view.player, channel, c.lastNote, c.lastVelocity);
        c.active := FALSE;
      END;
    END;
  END Silence;

PROCEDURE Play (view: T; event: TEXT; note: INTEGER) =
  VAR
    channel             : Channel;
    newTimbre, newVolume: Midi.Value;
    newVelocity         : Midi.Velocity;
  BEGIN
    IF view.player = NIL THEN RETURN END;
    channel := GetInteger (view.fv, event & "Channel");
    WITH c = view.channels [channel] DO
      IF c.active THEN
        Midi.EndNote (view.player, channel, c.lastNote, c.lastVelocity);
        c.active := FALSE;
      END;
      IF FormsVBT.GetBoolean (view.fv, event) THEN
        newVolume := FormsVBT.GetInteger (view.fv, event & "Volume");
        IF c.volume # newVolume THEN
          Midi.Volume (view.player, channel, newVolume);
          c.volume := newVolume;
        END;
        newTimbre := FormsVBT.GetInteger (view.fv, event & "Timbre");
        IF c.timbre # newTimbre THEN
          Midi.Timbre (view.player, channel, newTimbre);
          c.timbre := newTimbre;
        END;
        newVelocity := 16_60;
        Midi.BeginNote (view.player, channel, note, newVelocity);
        c.active := TRUE;
        c.lastNote := note;
        c.lastVelocity := newVelocity;
      END;
    END;
  END Play;

PROCEDURE GetInteger (fv: FormsVBT.T; name: TEXT): INTEGER =
  <* FATAL Lex.Error, FloatMode.Trap *>
  BEGIN
    RETURN Scan.Int (FormsVBT.GetText (fv, name));
  END GetInteger;


(* * * * * Global Initialization * * * * *)

BEGIN
  ZeusPanel.RegisterView (New, "Sound Effects", "Sort");
END SortSoundsView.
