(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE OWr EXPORTS OWr;

IMPORT Text, TextF;

PROCEDURE PutText (self: T; t: Text.T) RAISES {Closed} =
  BEGIN 
    self.putString (SUBARRAY (t^, 0, Text.Length (t) - 1));
  END PutText;

BEGIN
END OWr.
