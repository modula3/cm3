(* Copyright 2020,2021 Eric Sessoms / MIT License *)

MODULE Crypto_Scram;

IMPORT
  Crypto_Hash   AS Hash,
  Crypto_Hmac   AS Hmac,
  Crypto_Pbkdf2 AS Pbkdf2;


PROCEDURE SaltedPassword(
    hash: Hash.T; password, salt: TEXT; iterations: CARDINAL): TEXT =
  VAR
    hashLength := Hash.OutputSize(hash);
  BEGIN
    RETURN Pbkdf2.DerivedKey(hash, password, salt, iterations, hashLength)
  END SaltedPassword;


PROCEDURE ClientKey(hash: Hash.T; saltedPassword: TEXT): TEXT =
  VAR
    digest: TEXT;
    hmac := Hmac.New(hash, saltedPassword);
  BEGIN
    Hmac.Update(hmac, "Client Key");
    Hmac.Digest(hmac, digest);
    RETURN digest
  END ClientKey;


PROCEDURE ServerKey(hash: Hash.T; saltedPassword: TEXT): TEXT =
  VAR
    digest: TEXT;
    hmac := Hmac.New(hash, saltedPassword);
  BEGIN
    Hmac.Update(hmac, "Server Key");
    Hmac.Digest(hmac, digest);
    RETURN digest
  END ServerKey;


PROCEDURE StoredKey(hash: Hash.T; clientKey: TEXT): TEXT =
  VAR
    digest: TEXT;
  BEGIN
    Hash.Reset(hash);
    Hash.Update(hash, clientKey);
    Hash.Digest(hash, digest);
    RETURN digest
  END StoredKey;


BEGIN
  (* SKIP *)
END Crypto_Scram.
