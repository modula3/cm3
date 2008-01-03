(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Tue Nov  3 13:05:10 PST 1992 by sclafani *)

MODULE Midi;

IMPORT Midic, MidiLineServer, MidiPrivate, Text;

TYPE
  Byte = BITS 8 FOR [0 .. 255];
  Buffer = ARRAY [0 .. 3] OF Byte;

(*
REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        beginNote       := BeginNote;
        endNote         := EndNote;
        endAllNotes     := EndAllNotes;
        close           := Close;
        timbre          := Timbre;
        volume          := Volume;
        modulation      := Modulation;
        breath          := Breath;
        foot            := Foot;
        pitchBend       := PitchBend;
        sustain         := Sustain;
        sostenuto       := Sostenuto;
        keyPressure     := KeyPressure;
        channelPressure := ChannelPressure;
        dataEntry       := DataEntry;
        controlChange   := ControlChange;
      END;
*)

PROCEDURE Open (name: Text.T): T RAISES {Failure} =
  BEGIN
    IF Text.Equal ("/dev/", Text.Sub (name, 0, 5)) THEN
      RETURN NEW (Midic.T).init (name);
    ELSE
      RETURN NEW (MidiLineServer.T).init (name);
    END;
  END Open;


(***** Basic operations *****)

PROCEDURE BeginNote (t       : T;
                     channel : Channel;
                     note    : Note;
                     velocity: Velocity  := Default) RAISES {Failure} =
  VAR buffer: Buffer;
  BEGIN
    buffer [0] := 16_90 + channel;
    buffer [1] := note;         (* 0x40 := c on D-5 *)
    buffer [2] := velocity;
    t.play (buffer, 0, 3);
  END BeginNote;

PROCEDURE EndNote (t       : T;
                   channel : Channel;
                   note    : Note;
                   velocity: Velocity  := Default) RAISES {Failure} =
  VAR buffer: Buffer;
  BEGIN
    buffer [0] := 16_80 + channel;
    buffer [1] := note;
    buffer [2] := velocity;
    t.play (buffer, 0, 3);
  END EndNote;

PROCEDURE EndAllNotes (t: T; channel: Channel) RAISES {Failure} =
  VAR buffer: Buffer;
  BEGIN
    buffer [0] := 16_b0 + channel;
    buffer [1] := 16_7b;
    buffer [2] := 16_00;
    t.play (buffer, 0, 3);
  END EndAllNotes;

PROCEDURE Close (t: T) RAISES {Failure} =
  BEGIN
    t.close ();
  END Close;


(***** Setting channel attributes *****)

PROCEDURE Timbre (t: T; channel: Channel; timbre: Value := Default)
  RAISES {Failure} =
  VAR buffer: Buffer;
  BEGIN
    buffer [0] := 16_c0 + channel;
    buffer [1] := timbre;
    t.play (buffer, 0, 2);
  END Timbre;

PROCEDURE Volume (t: T; channel: Channel; volume: Value) RAISES {Failure} =
  VAR buffer: Buffer;
  BEGIN
    buffer [0] := 16_b0 + channel;
    buffer [1] := 16_07;        (* Main volume control change *)
    buffer [2] := volume;
    t.play (buffer, 0, 3);
  END Volume;

PROCEDURE Modulation (t: T; channel: Channel; modulation: Value := Default)
  RAISES {Failure} =
  VAR buffer: Buffer;
  BEGIN
    buffer [0] := 16_b0 + channel;
    buffer [1] := 16_01;        (* MOD wheel control change *)
    buffer [2] := modulation;
    t.play (buffer, 0, 3);
  END Modulation;

PROCEDURE Breath (t: T; channel: Channel; breath: Value := Default)
  RAISES {Failure} =
  VAR buffer: Buffer;
  BEGIN
    buffer [0] := 16_b0 + channel;
    buffer [1] := 16_02;        (* Breath control change *)
    buffer [2] := breath;
    t.play (buffer, 0, 3);
  END Breath;

PROCEDURE Foot (t: T; channel: Channel; foot: Value := Default)
  RAISES {Failure} =
  VAR buffer: Buffer;
  BEGIN
    buffer [0] := 16_b0 + channel;
    buffer [1] := 16_04;        (* Foot pedal control change *)
    buffer [2] := foot;
    t.play (buffer, 0, 3);
  END Foot;


(***** Special Effects *****)

PROCEDURE PitchBend (t: T; channel: Channel; bend: Value := Default)
  RAISES {Failure} =
  VAR buffer: Buffer;
  BEGIN
    buffer [0] := 16_e0 + channel;
    buffer [1] := 16_00;        (* LSB for pitch bend: not used now. *)
    buffer [2] := bend;         (* MSB for pitch bend. *)
    t.play (buffer, 0, 3);
  END PitchBend;

PROCEDURE Sustain (t: T; channel: Channel; on: BOOLEAN) RAISES {Failure} =
  VAR
    value : Value;
    buffer: Buffer;
  BEGIN
    IF on THEN value := LAST (Value); ELSE value := FIRST (Value); END;
    buffer [0] := 16_b0 + channel;
    buffer [1] := 16_40;        (* Sustain pedal control change *)
    buffer [2] := value;
    t.play (buffer, 0, 3);
  END Sustain;

PROCEDURE Sostenuto (t: T; channel: Channel; on: BOOLEAN)
  RAISES {Failure} =
  VAR
    value : Value;
    buffer: Buffer;
  BEGIN
    IF on THEN value := LAST (Value); ELSE value := FIRST (Value); END;
    buffer [0] := 16_b0 + channel;
    buffer [1] := 16_42;        (* Sostenuto pedal control change *)
    buffer [2] := value;
    t.play (buffer, 0, 3);
  END Sostenuto;

PROCEDURE KeyPressure (t      : T;
                       channel: Channel;
                       note   : Note;
                       touch  : Value     := Default) RAISES {Failure} =
  VAR buffer: Buffer;
  BEGIN
    buffer [0] := 16_a0 + channel;
    buffer [1] := note;
    buffer [2] := touch;
    t.play (buffer, 0, 3);
  END KeyPressure;

PROCEDURE ChannelPressure (t: T; channel: Channel; touch: Value := Default)
  RAISES {Failure} =
  VAR buffer: Buffer;
  BEGIN
    buffer [0] := 16_a0 + channel;
    buffer [1] := touch;
    t.play (buffer, 0, 2);
  END ChannelPressure;


(***** Miscellany *****)

PROCEDURE DataEntry (t: T; channel: Channel; data: Value)
  RAISES {Failure} =
  VAR buffer: Buffer;
  BEGIN
    buffer [0] := 16_b0 + channel;
    buffer [1] := 16_06;        (* Data entry control change *)
    buffer [2] := data;
    t.play (buffer, 0, 3);
  END DataEntry;

PROCEDURE ControlChange (t      : T;
                         channel: Channel;
                         control: Value;
                         value  : Value    ) RAISES {Failure} =
  VAR buffer: Buffer;
  BEGIN
    buffer [0] := 16_b0 + channel;
    buffer [1] := control;
    buffer [2] := value;
    t.play (buffer, 0, 3);
  END ControlChange;

BEGIN
END Midi.
