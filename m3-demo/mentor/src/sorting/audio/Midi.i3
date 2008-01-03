(* Copyright 1991 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Adapted from Michael Good's midic.h version X00-07 *)
(* Last modified on Tue Sep 15 16:56:12 PDT 1992 by sclafani *)

INTERFACE Midi;

IMPORT Text;

TYPE
  Channel = [0 .. 15];
  Value = [0 .. 127];

  Note = Value;                 (* Key number. *)
  Velocity = Value;

CONST Default = 16_40;          (* no velocity, no pitch bend, etc. *)

EXCEPTION Failure(Text.T);

TYPE T <: ROOT;

(***** Basic operations *****)

PROCEDURE Open (name: Text.T): T RAISES {Failure};
(* Opens a connection to the MIDI interface device, which may be attached
   to a serial port (such as "/dev/tty00") or to a LineServer with an
   Ethernet address.  Assumes the device has already been initialized. *)

PROCEDURE BeginNote (t: T; ch: Channel; n: Note; v: Velocity := Default)
  RAISES {Failure};
(* Start playing the note for key /n/ on channel /ch/ with velocity /v/. *)

PROCEDURE EndNote (t: T; ch: Channel; n: Note; v: Velocity := Default)
  RAISES {Failure};
(* Stop playing the note for key /n/ on channel /ch/ with velocity /v/. *)

PROCEDURE EndAllNotes (t: T; ch: Channel) RAISES {Failure};
(* Turns off all notes on channel /ch/. *)

PROCEDURE Close (t: T) RAISES {Failure};
(* Closes MIDI connection without turning all notes off. *)


(***** Setting channel attributes *****)

PROCEDURE Timbre (t: T; ch: Channel; timbre: Value := Default)
  RAISES {Failure};
(* Chooses timbre (aka MIDI program change). *)

PROCEDURE Volume (t: T; ch: Channel; volume: Value := Default)
  RAISES {Failure};
(* Chooses volume (aka MIDI program change). *)

PROCEDURE Modulation (t: T; ch: Channel; modulation: Value := Default)
  RAISES {Failure};
(* Changes modulation of the specified channel. *)

PROCEDURE Breath (t: T; ch: Channel; breath: Value := Default)
  RAISES {Failure};
(* Changes breath of the specified channel. *)

PROCEDURE Foot (t: T; ch: Channel; foot: Value := Default)
  RAISES {Failure};
(* Changes foot pedal control value of the specified channel. *)


(***** Special Effects *****)

PROCEDURE PitchBend (t: T; ch: Channel; bend: Value := Default)
  RAISES {Failure};
(* Bend the pitch of the note on channel /ch/.  Default value is no bend;
   maximum/minimum values for /bend/ raise or lower a semitone. *)

PROCEDURE Sustain (t: T; ch: Channel; on: BOOLEAN) RAISES {Failure};
(* Turns sustain pedal controller on or off. *)

PROCEDURE Sostenuto (t: T; ch: Channel; on: BOOLEAN) RAISES {Failure};
(* Turns sostenuto pedal controller on or off. *)

PROCEDURE KeyPressure (t: T; ch: Channel; n: Note; touch: Value := Default)
  RAISES {Failure};
(* Change MIDI polyphonic key pressure. *)

PROCEDURE ChannelPressure (t: T; ch: Channel; touch: Value := Default)
  RAISES {Failure};
(* Change MIDI channel pressure. *)


(***** Miscellany *****)

PROCEDURE DataEntry (t: T; ch: Channel; data: Value) RAISES {Failure};
(* Changes data entry control of the specified channel. *)

PROCEDURE ControlChange (t: T; ch: Channel; control: Value; value: Value)
  RAISES {Failure};
(* Perform arbitrary MIDI control changes.  Most control changes are
   device-specific.  Volume, modulation, and all-notes-off are some that
   are standardized across most MIDI devices. *)

END Midi.
