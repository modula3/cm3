INTERFACE Msg;

PROCEDURE M(a, b, c, d, e, f : TEXT := NIL); (* message *)
PROCEDURE D(a, b, c, d, e, f : TEXT := NIL); (* debug message *)
PROCEDURE V(a, b, c, d, e, f : TEXT := NIL); (* verbose message *)
PROCEDURE F(a, b, c, d, e, f : TEXT := NIL); (* fatal message, abort *)

VAR
  debug := FALSE;
  verbose := FALSE;
END Msg.

