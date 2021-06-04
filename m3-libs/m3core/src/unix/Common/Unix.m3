UNSAFE MODULE Unix;
IMPORT UnixC;
FROM Ctypes IMPORT int;

(* This is unfortunate but structural type equivalence get in the way of
 * passing the fixed size array directly to C.
 *)
PROCEDURE pipe (VAR files: ARRAY [0..1] OF int): int =
BEGIN
  RETURN UnixC.pipe (ADR (files [0]));
END pipe;

BEGIN
  Assertions();
END Unix.
