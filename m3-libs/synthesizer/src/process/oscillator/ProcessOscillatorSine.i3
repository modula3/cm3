INTERFACE ProcessOscillatorSine;
(* author: thielema *)

IMPORT Signal;


PROCEDURE Do (length: CARDINAL;  (* number of samples of the generated
                                    sound *)
              freq: LONGREAL;    (* frequency of oscillation as ratio of
                                    the sample rate, that is freq=0.01 at
                                    sample rate 44100 Hz means 441 Hz
                                    oscillation frequency; alternative
                                    interpretation: number of waves per
                                    sample, or: reciprocal of the period in
                                    samples; Should be at most 0.5D0 in
                                    order to avoid aliasing. *)
              phase := 0.0D0; (* beginning phase of the wave; e.g.  0.25D0
                                 means a sine starting at 90 degree (i.e.
                                 with value 1, that is a cosine) *) ):
  Signal.RefArray;


END ProcessOscillatorSine.
