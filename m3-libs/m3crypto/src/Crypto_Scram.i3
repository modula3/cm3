(* Copyright 2020,2021 Eric Sessoms / MIT License *)

INTERFACE Crypto_Scram;
(* Salted Challenge-Response Authentication Mechanism

N.B., This module does not implement the SCRAM *protocol*, which of
course varies by use-case, it only provides the required cryptographic
functions.

SCRAM supports a unique combination of features not usually available
in other authentication mechanisms, namely:
1. the user's password is stored encrypted,
2. the user's unencrypted password is never sent over the wire, and
3. the user is able to verify the server's identity.

Most systems offer only either (1) or (2).

A typical authentication exchange is as follows.
1. The client generates a signature using the "stored key".
2. The client transmits the signature XORed with the "client key".
3. The server, having generated the same signature, uses XOR to read
   the transmitted client key.
4. The server hashes the client key to verify against the stored key.

The server authenticates to the client in a similar fashion, but using
the "server key" instead of the client key. *)

IMPORT
  Crypto_Hash AS Hash;


PROCEDURE SaltedPassword(
    hash: Hash.T; password, salt: TEXT; iterations: CARDINAL): TEXT;
  (* Generate initial key from user's password and random salt

The password should be normalized in a manner appropriate to the
application, and encoded as UTF-8 bytes.

The salt should be a cryptographically random vector of bytes of the
same length as the hash.

Neither the server nor the client need to retain the salted password.
It is only used to generate other derived keys.  *)


PROCEDURE ClientKey(hash: Hash.T; saltedPassword: TEXT): TEXT;
  (* Generate key used to authenticate user

The user's client will need to retain the client key.  The server does
not store this key.  *)


PROCEDURE ServerKey(hash: Hash.T; saltedPassword: TEXT): TEXT;
  (* Generate key used to authenticate server

Both the server and the user's client will need to retain the server
key. *)


PROCEDURE StoredKey(hash: Hash.T; clientKey: TEXT): TEXT;
  (* Generate hash of the client key

This hash is used to validate the client key.

The server will need to retain this key.  The client can reproduce it
at will from the client key. *)


END Crypto_Scram.
