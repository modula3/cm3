(* Copyright 2020,2021 Eric Sessoms / MIT License *)

INTERFACE Crypto_Sha1;
(* Secure Hash Algorithm 1 *)

IMPORT
  Crypto_Hash AS Hash;

PROCEDURE New(): Hash.T;

END Crypto_Sha1.
