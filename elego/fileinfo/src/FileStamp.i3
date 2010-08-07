(*--------------------------------------------------------------------------*)
INTERFACE FileStamp;

IMPORT Time;
IMPORT APN AS APN;

TYPE
  T = OBJECT
    name  : APN.T;
    mtime : Time.T;
  METHODS
    init(p : APN.T; mt : Time.T) : T := Init;
    toText() : TEXT := ToText;
  END;

(*--------------------------------------------------------------------------*)
PROCEDURE New(p : APN.T; mt : Time.T) : T;

(*--------------------------------------------------------------------------*)
PROCEDURE Init(self : T; p : APN.T; mt : Time.T) : T;

(*--------------------------------------------------------------------------*)
PROCEDURE ToText(self : T) : TEXT;

END FileStamp.
