(* Copyright 2020,2021 Eric Sessoms / MIT License *)

INTERFACE Crypto_Hmac;
(* One-way compression with key *)

IMPORT
  Crypto_Hash AS Hash;


TYPE
  T <: REFANY;


PROCEDURE New(hash: Hash.T; key: TEXT): T;
  (* Allocate new HMAC initialized with `key` *)


PROCEDURE Reset(hmac: T; key: TEXT := NIL);
  (* Reinitialize to compute new authentication code

Can provide new `key` or key := NIL reuses previous key. *)


PROCEDURE UpdateString(hmac: T; READONLY data: ARRAY OF CHAR);
  (* Update HMAC with provided data *)


PROCEDURE Update(hmac: T; data: TEXT);
  (* Update HMAC with provided data *)


PROCEDURE OutputSize(hmac: T): CARDINAL;
  (* Returns the output size of the underlying hash *)


PROCEDURE DigestString(hmac: T; VAR out: ARRAY OF CHAR);
  (* Read finalized authentication code

`out` must have size determined by the underlying hash.

This procedure destroys the state of the hmac, such that it must be
reinitialized to to compute another code.  *)


PROCEDURE Digest(hmac: T; VAR out: TEXT);
  (* Read finalized authentication code

Destructive, see warning above for `DigestSttring`. *)


PROCEDURE HexDigest(hmac: T; VAR out: TEXT);
  (* Return digest formatted as lowercase hexadecimal string

See warning above for `DigestString`. *)


END Crypto_Hmac.
