
INTERFACE FakeOS;

(* As a user-defined interface, "FakeOS" provides access to the
   "FakeOS" module. "FakeOS" acts as a gateway; the only calls
   available to clients are the ones declared in this interface.
   In particular, the procedure "Copy" is exported via this
   interface. The body of this procedure will be in the 
   implementation of this interface, "FakeOS.m3". *) 

PROCEDURE Copy(source, destination: TEXT);
  (* Copy a file named "source" to a file named "destination". *)

END FakeOS. 

