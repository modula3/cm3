(* Copyright 1991 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Sep 16 16:01:47 PDT 1992 by sclafani *)

INTERFACE MidiPrivate;

IMPORT Midi;
FROM Midi IMPORT Failure;

REVEAL
  Midi.T = BRANDED OBJECT
           METHODS
             init (name: TEXT): Midi.T RAISES {Failure};
             play (READONLY buffer: ARRAY OF BITS 8 FOR [0 .. 255];
                            start : CARDINAL                         := 0;
                   length: CARDINAL := LAST (CARDINAL)) RAISES {Failure};
             close () RAISES {Failure};
           END;

END MidiPrivate.
