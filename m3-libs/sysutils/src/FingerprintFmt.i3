(*--------------------------------------------------------------------------*)
INTERFACE FingerprintFmt;

IMPORT Fingerprint;

(*--------------------------------------------------------------------------*)
PROCEDURE Hex(fp : Fingerprint.T) : TEXT;
  (* Convert the byte sequence of `fp' into a hexadecimal representation. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Scan(t : TEXT; VAR fp : Fingerprint.T) : BOOLEAN;
  (* Interprets `t' as hexadecimal representation of a fingerprint; `fp'
     is the result. Returns FALSE if the conversion fails, in which
     case `fp' is undefined; TRUE else. *)
     
END FingerprintFmt.
