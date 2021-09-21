(* Copyright 2020,2021 Eric Sessoms / MIT License *)

MODULE Crypto_Hmac;

IMPORT
  Crypto_Hash AS Hash,
  Text;

FROM Word IMPORT Xor;


REVEAL
  T = BRANDED OBJECT
    hash: Hash.T;
    key: REF ARRAY OF CHAR
  END;


PROCEDURE InitKey(hmac: T; key: TEXT) =
  BEGIN
    (* Ensure key fits into one block. *)
    IF Text.Length(key) > Hash.BlockSize(hmac.hash) THEN
      Hash.Reset(hmac.hash);
      Hash.Update(hmac.hash, key);
      Hash.Digest(hmac.hash, key)
    END;
    <* ASSERT Text.Length(key) <= Hash.BlockSize(hmac.hash) *>

    (* Make local copy of key, padded with zeros. *)
    hmac.key := NEW(REF ARRAY OF CHAR, Hash.BlockSize(hmac.hash));
    FOR i := FIRST(hmac.key^) TO LAST(hmac.key^) DO
      hmac.key[i] := '\000'
    END;
    Text.SetChars(hmac.key^, key)
  END InitKey;


PROCEDURE Reset(hmac: T; key: TEXT := NIL) =
  VAR
    innerKey: ARRAY [0..63] OF CHAR;
  BEGIN
    <* ASSERT NUMBER(innerKey) >= Hash.BlockSize(hmac.hash) *>

    IF key # NIL THEN
      InitKey(hmac, key)
    END;

    (* Prepare inner key. *)
    FOR i := FIRST(hmac.key^) TO LAST (hmac.key^) DO
      innerKey[i] := VAL(Xor(ORD(hmac.key[i]), 16_36), CHAR)
    END;

    (* Initialize inner hash. *)
    Hash.Reset(hmac.hash);
    Hash.UpdateString(hmac.hash, SUBARRAY(innerKey, 0, NUMBER(hmac.key^)));
  END Reset;


PROCEDURE New(hash: Hash.T; key: TEXT): T =
  VAR
    hmac := NEW(T, hash := hash);
  BEGIN
    Reset(hmac, key);
    RETURN hmac
  END New;


PROCEDURE UpdateString(hmac: T; READONLY data: ARRAY OF CHAR) =
  BEGIN
    Hash.UpdateString(hmac.hash, data)
  END UpdateString;


PROCEDURE Update(hmac: T; data: TEXT) =
  BEGIN
    Hash.Update(hmac.hash, data)
  END Update;


PROCEDURE OutputSize(hmac: T): CARDINAL =
  BEGIN
    RETURN Hash.OutputSize(hmac.hash)
  END OutputSize;


PROCEDURE Finish(hmac: T) =
  VAR
    hashLen: CARDINAL;
    innerDigest, outerKey: ARRAY [0..63] OF CHAR;
  BEGIN
    hashLen := OutputSize(hmac);

    (* Finalize inner hash. *)
    Hash.DigestString(hmac.hash, SUBARRAY(innerDigest, 0, hashLen));

    (* Prepare outer key. *)
    FOR i := FIRST(hmac.key^) TO LAST (hmac.key^) DO
      outerKey[i] := VAL(Xor(ORD(hmac.key[i]), 16_5C), CHAR)
    END;

    (* Compute outer hash. *)
    Hash.Reset(hmac.hash);
    Hash.UpdateString(hmac.hash, SUBARRAY(outerKey, 0, NUMBER(hmac.key^)));
    Hash.UpdateString(hmac.hash, SUBARRAY(innerDigest, 0, hashLen))
  END Finish;


PROCEDURE DigestString(hmac: T; VAR out: ARRAY OF CHAR) =
  BEGIN
    Finish(hmac);
    Hash.DigestString(hmac.hash, out)
  END DigestString;


PROCEDURE Digest(hmac: T; VAR out: TEXT) =
  BEGIN
    Finish(hmac);
    Hash.Digest(hmac.hash, out)
  END Digest;


PROCEDURE HexDigest(hmac: T; VAR out: TEXT) =
  BEGIN
    Digest(hmac, out);
    out := Hash.ToHexString(out)
  END HexDigest;


BEGIN
  (* SKIP *)
END Crypto_Hmac.
