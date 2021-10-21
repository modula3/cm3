(* Copyright 2020,2021 Eric Sessoms / MIT License *)

MODULE Crypto_Sha1;

IMPORT
  Crypto_Hash     AS Hash,
  Crypto_HashImpl AS Impl;

FROM Impl IMPORT GetWord32be, LeftRotate32, Pad, PutWord32be;
FROM Word IMPORT And, Extract, Not, Or, Xor;


TYPE
  T = Impl.T BRANDED OBJECT
    h0, h1, h2, h3, h4: INTEGER
  OVERRIDES
    outputSize := OutputSize;
    reset := Reset;
    chunk := Chunk;
    digestString := DigestString
  END;


PROCEDURE OutputSize(<* UNUSED *> hash: T): CARDINAL =
  BEGIN
    RETURN 20
  END OutputSize;


PROCEDURE Reset(hash: T) =
  BEGIN
    hash.used := 0;
    hash.msglen := 0;
    hash.h0 := 16_67452301;
    hash.h1 := 16_EFCDAB89;
    hash.h2 := 16_98BADCFE;
    hash.h3 := 16_10325476;
    hash.h4 := 16_C3D2E1F0
  END Reset;


PROCEDURE New(): Hash.T =
  VAR
    hash := NEW(T);
  BEGIN
    Reset (hash);
    RETURN hash
  END New;


PROCEDURE Chunk(hash: T) =
  VAR
    a, b, c, d, e, f, k, t: INTEGER;
    w: ARRAY [0..79] OF INTEGER;
  BEGIN
    a := hash.h0;
    b := hash.h1;
    c := hash.h2;
    d := hash.h3;
    e := hash.h4;

    FOR i := FIRST(w) TO 15 DO
      w[i] := GetWord32be(hash.buffer, 4*i)
    END;

    FOR i := 16 TO LAST(w) DO
      w[i] := LeftRotate32(Xor(w[i-3], Xor(w[i-8], Xor(w[i-14], w[i-16]))), 1)
    END;

    FOR i := FIRST(w) TO LAST(w) DO
      IF    i <= 19 THEN
        f := Or(And(b, c), And(Not(b), d));
        k := 16_5A827999
      ELSIF i <= 39 THEN
        f := Xor(b, Xor(c, d));
        k := 16_6ED9EBA1
      ELSIF i <= 59 THEN
        f := Or(And(b, c), Or(And(b, d), And(c, d)));
        k := 16_8F1BBCDC
      ELSIF i <= 79 THEN
        f := Xor(b, Xor(c, d));
        k := 16_CA62C1D6
      ELSE
        <* ASSERT FALSE *>
      END;

      t := LeftRotate32(a, 5) + f + e + k + w[i];
      e := d;
      d := c;
      c := LeftRotate32(b, 30);
      b := a;
      a := t
    END;

    INC(hash.h0, a);
    INC(hash.h1, b);
    INC(hash.h2, c);
    INC(hash.h3, d);
    INC(hash.h4, e)
END Chunk;


PROCEDURE DigestString(hash: T; VAR out: ARRAY OF CHAR) =
  BEGIN
    (* Original message length. *)
    INC(hash.msglen, 8 * hash.used);

    Pad(hash);

    (* Append message length in bits. *)
    FOR i := 0 TO 7 DO
      (* big-endian *)
      hash.buffer[hash.used + i] := VAL(Extract(hash.msglen, 8*(7-i), 8), CHAR)
    END;
    Chunk(hash);

    PutWord32be(out,  0, hash.h0);
    PutWord32be(out,  4, hash.h1);
    PutWord32be(out,  8, hash.h2);
    PutWord32be(out, 12, hash.h3);
    PutWord32be(out, 16, hash.h4)
  END DigestString;


BEGIN
  (* SKIP *)
END Crypto_Sha1.
