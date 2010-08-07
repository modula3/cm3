(*--------------------------------------------------------------------------*)
INTERFACE ProgramVersion;

TYPE
  T = OBJECT
  METHODS
    initFromRd(rd : Rd.T) : T;
    program() : TEXT;
    shortText() : TEXT;
    longText() : TEXT;
    version() : Version.T;
    serialNo() : TEXT;
    brand() : TEXT;
    date() : TEXT;
  END;

VAR
  Start     := "[@@-";
  Name      := "ComPact";
  Sep       := "-";
  Version   := "1.1";
  Sep       := "-";
  Qualifier := "FREE";
  Sep       := "-";
  SerialNo  := "0000000000000000";
  Sep       := "-";
  Brand     := "0000000000000000";
  Sep       := "-";
  Branded   := "07.08.2010";
  Sep       := "-";
  End       := "-@@]";

  (* version of this program, constructed at module initialization *)
  my        : T; (* CONST *) 

END ProgramVersion.
