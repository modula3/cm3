
GENERIC INTERFACE SignalRep(V,Signal,SignalFmtLex);

REVEAL
  Signal.T <: TPrivate;

TYPE
  TPrivate = SignalFmtLex.T OBJECT
	data  : V.T;
	first : Signal.IndexType;
  END;


END SignalRep.
