INTERFACE ConnectRdWr;

IMPORT Rd, Wr, Thread;

VAR
  Debug : CARDINAL := 0;

TYPE T <: Public;
TYPE Public = Thread.Closure OBJECT
METHODS
  init(rd : Rd.T; wr : Wr.T; name : TEXT; killpid := 0) : T;
END;

END ConnectRdWr.
