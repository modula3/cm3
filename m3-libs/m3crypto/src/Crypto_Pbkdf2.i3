(* Copyright 2020,2021 Eric Sessoms / MIT License *)

INTERFACE Crypto_Pbkdf2;
(* Password-Based Key-Derivation Function #2

Iteratively hashes a salted password, making it expensive to guess. *)

IMPORT
  Crypto_Hash AS Hash;


PROCEDURE DerivedKey(
    hash: Hash.T;
    password, salt: TEXT;
    iterations, keyLength: CARDINAL): TEXT;
  (* Iteratively hash salted password

The password should be normalized in a manner appropriate to the
application, and encoded as UTF-8 bytes.

The salt should be a cryptographically random vector of bytes of the
same length as the hash. *)


END Crypto_Pbkdf2.
