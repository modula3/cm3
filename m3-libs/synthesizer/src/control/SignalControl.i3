INTERFACE SignalControl;
(* author: thielema *)

IMPORT Signal;

(* A normal signal should have values between -1 and 1.  A control signal
   contains values for controlling some parameters of processes such as
   frequencies of oscillators, volumes, filter parameters and so on which
   are normally in very different ranges, e.g.  frequencies from 1.0D-6 to
   1.0D-1. *)

(* These types can't be distinguished from the ones in Signal, but they
   should be used for documentation purposes. *)
TYPE
  Array = ARRAY OF LONGREAL;
  RefArray = REF Array;

(* Type synonyme for all control signals and processes that turn an
   unqualified signal to a control signal. *)
TYPE T = Signal.T;

(* We can't use a special superclass for this signal class since this would
   exclude postprocessing like Split, Concat, Cut which are very reasonable
   for control signals.  TYPE T = Signal.T BRANDED OBJECT END; *)

END SignalControl.
