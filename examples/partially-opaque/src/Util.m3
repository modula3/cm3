
MODULE Util;
IMPORT Person;

(* Define the procedure "Describe", which is exported via interface
   "Person". "Describe" is visible to all clients of the
   "Person" interface. Since method "fullname()" is also exported,
   clients of "Person" interface can override the functionality of 
   "fullname()" to affect how "Describe" works. *)

PROCEDURE Describe(p: T; desc: TEXT; wr: Wr.T := NIL) =
BEGIN
  IO.Put (p.fullname() & " is " & desc & ".\n", wr);
END Describe;

END Util.

