(* Copyright 2020,2021 Eric Sessoms / MIT License *)

INTERFACE Crypto_Sha2;
(* Secure Hash Algorithm 2 *)

IMPORT
  Crypto_Hash AS Hash;

PROCEDURE New_Sha224(): Hash.T;
PROCEDURE New_Sha256(): Hash.T;

END Crypto_Sha2.
