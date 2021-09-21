(* Copyright 2020,2021 Eric Sessoms / MIT License *)

MODULE Crypto_Pbkdf2;

IMPORT
  Crypto_Hash AS Hash,
  Crypto_Hmac AS Hmac,
  Text;

FROM Crypto_HashImpl IMPORT PutWord32be;
FROM Word IMPORT Xor;


PROCEDURE DerivedKey(
    hash: Hash.T;
    password, salt: TEXT;
    iterations, keyLength: CARDINAL): TEXT =

  PROCEDURE Block(hmac: Hmac.T; block: CARDINAL; VAR derived: ARRAY OF CHAR) =
    (* Compute one block of derived key *)
    VAR
      buffer, digest: ARRAY [0..31] OF CHAR;
      hashLen: CARDINAL;
    BEGIN
      hashLen := Hash.OutputSize(hash);
      <* ASSERT NUMBER(derived) >= hashLen *>

      (* First round, take HMAC of salt and block number. *)
      PutWord32be(buffer, 0, block);
      Hmac.Reset(hmac);
      Hmac.Update(hmac, salt);
      Hmac.UpdateString(hmac, SUBARRAY(buffer, 0, 4));
      Hmac.DigestString(hmac, SUBARRAY(digest, 0, hashLen));

      (* Copy first-round HMAC for input into second-round. *)
      SUBARRAY(buffer, 0, hashLen) := SUBARRAY(digest, 0, hashLen);

      (* Effectively, XOR with zeros in destination. *)
      SUBARRAY(derived, 0, hashLen) := SUBARRAY(digest, 0, hashLen);

      (* Subsequent rounds, take HMAC of previous round. *)
      FOR iter := 2 TO iterations DO
        Hmac.Reset(hmac);
        Hmac.UpdateString(hmac, SUBARRAY(buffer, 0, hashLen));
        Hmac.DigestString(hmac, SUBARRAY(digest, 0, hashLen));

        (* Save HMAC for input into next-round. *)
        SUBARRAY(buffer, 0, hashLen) := SUBARRAY(digest, 0, hashLen);

        (* XOR into destination. *)
        FOR i := 0 TO hashLen - 1 DO
          derived[i] := VAL(Xor(ORD(derived[i]), ORD(buffer[i])), CHAR)
        END
      END
    END Block;

  VAR
    block, derivedLen, hashLen: CARDINAL;
    derived: REF ARRAY OF CHAR;
    hmac: Hmac.T;
  BEGIN
    hashLen := Hash.OutputSize(hash);
    derivedLen := ((keyLength + hashLen - 1) DIV hashLen) * hashLen;
    derived := NEW(REF ARRAY OF CHAR, derivedLen);

    hmac := Hmac.New(hash, password);

    (* Each block adds `hashLen` bytes to the derived key. *)
    derivedLen := 0;
    block := 1;
    WHILE keyLength > derivedLen DO
      Block(hmac, block, SUBARRAY(derived^, derivedLen, hashLen));
      INC(derivedLen, hashLen);
      INC(block)
    END;

    RETURN Text.FromChars(SUBARRAY(derived^, 0, keyLength))
  END DerivedKey;


BEGIN
  (* SKIP *)
END Crypto_Pbkdf2.
