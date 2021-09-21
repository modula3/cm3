(* Copyright 2020,2021 Eric Sessoms / MIT License *)

INTERFACE Crypto_Md5;
(* Message Digest 5 *)

IMPORT
  Crypto_Hash AS Hash;

PROCEDURE New(): Hash.T;

END Crypto_Md5.
