
GENERIC INTERFACE SignalRep(V, Signal, SignalFmtLex);

REVEAL Signal.T <: T;

TYPE
  T = SignalFmtLex.T OBJECT
        data : V.T;
        first: Signal.IndexType;
      END;


END SignalRep.
