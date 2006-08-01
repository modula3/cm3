(* Copyright according to COPYRIGHT-CMASS. *)
(* FIXME: copied from FreeBSD3 target. Probably needs to be changed. *)

INTERFACE Csignal;

FROM Ctypes IMPORT int;

TYPE
  Handler = PROCEDURE (s: int);

<*EXTERNAL*>
PROCEDURE signal (sig: int; func: Handler): Handler;

END Csignal.
