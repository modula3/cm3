
GENERIC INTERFACE SignalRep(R,Signal);

REVEAL
  Signal.T <: TPrivate;

TYPE
  TPrivate = Signal.TPublic OBJECT
	data  : REF ARRAY OF R.T;
	first : Signal.IndexType;
  END;


END SignalRep.
