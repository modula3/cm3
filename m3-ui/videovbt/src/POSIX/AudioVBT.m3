(* Copyright (C) 1989, 1990, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Wed Jun 29 00:06:49 PDT 1994 by msm      *)
(*      modified on Mon Oct 25 17:28:45 PDT 1993 by sfreeman *)
<* PRAGMA LL *>

MODULE AudioVBT;

IMPORT AtomList, Filter, Jva, JVAudio, JVConverter, OSError, RdUtils, Rect,
       Thread, VBT, VBTClass;

REVEAL
  T = Public BRANDED OBJECT
        jva                : JVAudio.T := NIL;
        ignoreMapping, mute: BOOLEAN;
      OVERRIDES
        init     := Init;
        reshape  := Reshape;
        rescreen := Rescreen;
        misc     := Misc;
        discard  := Discard;
      END;

PROCEDURE Init (t            : T;
                ch           : VBT.T;
                source       : TEXT;
                mute                      := FALSE;
                ignoreMapping             := FALSE;
                volume       : Jva.Volume := 0      ): T
  RAISES {OSError.E, Thread.Alerted} =
  BEGIN
    t.jva := JVAudio.New(source);
    t.ignoreMapping := ignoreMapping;
    t.mute := mute;
    (* we know we're unmapped at the moment *)
    t.jva.setMute(mute AND NOT ignoreMapping);
    t.jva.setVolume(volume);
    RETURN Filter.T.init(t, ch);
  END Init;

PROCEDURE Reshape (t: T; READONLY cd: VBT.ReshapeRec) =
  BEGIN
    IF NOT Rect.Congruent(cd.prev, cd.new) THEN
      TRY
        DoMute(t, "AudioVBT: Reshape");
      EXCEPT
      | Thread.Alerted =>          (*skip *)
      END
    END;
    Filter.T.reshape(t, cd);
  END Reshape;

PROCEDURE Rescreen (t: T; READONLY cd: VBT.RescreenRec) =
  BEGIN
    TRY
      DoMute(t, "AudioVBT: Rescreen");
    EXCEPT
    | Thread.Alerted =>          (*skip *)
    END;
    Filter.T.rescreen(t, cd);
  END Rescreen;

PROCEDURE Misc (t: T; READONLY cd: VBT.MiscRec) =
  BEGIN
    IF cd.type = VBT.Deleted OR cd.type = VBT.Disconnected THEN
      LOCK t DO IF t.jva # NIL THEN t.jva.close(); END; END;
    END;
    Filter.T.misc(t, cd);
  END Misc;

PROCEDURE Discard (t: T) =
  BEGIN
    LOCK t DO IF t.jva # NIL THEN t.jva.close(); END; END;
    Filter.T.discard(t);
  END Discard;

(* -- external procedures -- *)

PROCEDURE SetMute (t: T; mute: BOOLEAN) RAISES {Thread.Alerted} =
  BEGIN
    IF mute = t.mute THEN RETURN; END;
    LOCK t DO 
      t.mute := mute; 
      DoMute(t, "AudioVBT: SetMute"); 
    END;
  END SetMute;

PROCEDURE SetIgnoreMapping (t: T; ignore: BOOLEAN)
  RAISES {Thread.Alerted} =
  BEGIN
    LOCK t DO
      t.ignoreMapping := ignore;
      DoMute(t, "AudioVBT: SetIgnoreMapping");
    END;
  END SetIgnoreMapping;

PROCEDURE SetVolume (t: T; volume: Jva.Volume) RAISES {Thread.Alerted} =
  BEGIN
    TRY
      LOCK t DO t.jva.setVolume(volume); END;
    EXCEPT
    | OSError.E (e) => Report(e, "AudioVBT: SetVolume");
    END;
  END SetVolume;

(* -- internal procedures -- *)

(* check the state of the T and set muting accordingly *) <* LL >= t *>

PROCEDURE DoMute (t: T; msg: TEXT) RAISES {Thread.Alerted} =
  VAR
    mute := t.mute OR ((Rect.IsEmpty(t.domain) OR t.st = NIL)
                         AND NOT t.ignoreMapping);
  BEGIN
    TRY t.jva.setMute(mute); EXCEPT | OSError.E (e) => Report(e, msg); END;
  END DoMute;

PROCEDURE Report (e: AtomList.T; msg: TEXT) =
  BEGIN
    VAR etext := "";
    BEGIN
      IF e # NIL AND e.head # NIL THEN
        etext := RdUtils.FailureText(e);
      END;
      JVConverter.ReportError(msg & etext);
    END;
  END Report;

BEGIN
END AudioVBT.
