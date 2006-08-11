
GENERIC INTERFACE ScaledSignal(R, C, S);

CONST Brand = C.Brand & "ScaledSignal";

TYPE
  T = RECORD
        signal: S.T;             (* real valued signal *)
        resolution: R.T;         (* resolution, that is the width of a peak
                                    represented by a value *)
      END;

(* We could make ScaledSignal a subclass of Signal.  But with the current
   type definition it is easy to attach and detach the signal from the
   object.  Many functions on signals would not work properly on this
   subclass because they return pure signals rather than specialised
   signals. *)

PROCEDURE First (READONLY x: T; ): R.T;

PROCEDURE Last (READONLY x: T; ): R.T;

(* Number would not make sense here.  Would an appropriate Length function
   return Last(x)-First(x) or x.signal.getNumber()*x.resolution? *)

END ScaledSignal.
