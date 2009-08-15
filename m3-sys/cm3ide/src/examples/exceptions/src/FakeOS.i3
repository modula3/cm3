
INTERFACE FakeOS;

EXCEPTION Error (TEXT);

PROCEDURE Copy(source, destination: TEXT) RAISES {Error};
  (* Copy contents of "source" file into "destination". *)

END FakeOS. 

