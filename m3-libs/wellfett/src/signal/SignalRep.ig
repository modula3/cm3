
GENERIC INTERFACE SignalRep(R,Signal,SignalFmtLex);

REVEAL
  Signal.T <: TPrivate;

TYPE
  TPrivate = SignalFmtLex.T OBJECT
	data  : REF ARRAY OF R.T;
	first : Signal.IndexType;
  END;


END SignalRep.
