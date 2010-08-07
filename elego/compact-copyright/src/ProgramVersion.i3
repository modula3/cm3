(*--------------------------------------------------------------------------*)
INTERFACE ProgramVersion;

IMPORT Rd;
IMPORT Version AS V;

TYPE
  T <: Public;

  Public = OBJECT
  METHODS
    initFromRd(rd : Rd.T; name := NIL) : T;
    found() : BOOLEAN;
    branded() : BOOLEAN;
    corrupt() : BOOLEAN;
    program() : TEXT;
    shortText() : TEXT;
    longText() : TEXT;
    version() : V.T;
    serialNo() : TEXT;
    brand() : TEXT;
    date() : TEXT;
  END;

VAR
  Start     := "[@@-";
  Name      := "ComPact";
  Sep1      := "-";
  Version   := "1.1";
  Sep2      := "-";
  Qualifier := "BETA";
  Sep3      := "-";
  SerialNo  := "0000000000000000";
  Sep4      := "-";
  Brand     := "0000000000000000";
  Sep5      := "-";
  Branded   := "01.01.1999";
  End       := "-@@]";

  (* version of this program, constructed at module initialization *)
  My        : T; (* CONST *) 

END ProgramVersion.
