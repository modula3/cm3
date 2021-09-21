(* Copyright 2020,2021 Eric Sessoms / MIT License *)

MODULE Crypto_Sha2;

IMPORT
  Crypto_Hash     AS Hash,
  Crypto_HashImpl AS Impl;

FROM Impl IMPORT GetWord32be, Pad, PutWord32be;
FROM Word IMPORT And, Extract, LeftShift, Not, Or, RightShift, Xor;


TYPE
  Sha256 = Impl.T BRANDED OBJECT
    h0, h1, h2, h3, h4, h5, h6, h7: INTEGER
  OVERRIDES
    outputSize := OutputSize256;
    reset := Reset256;
    chunk := Chunk;
    digestString := DigestString256
  END;

  Sha224 = Sha256 BRANDED OBJECT OVERRIDES
    outputSize := OutputSize224;
    reset := Reset224;
    digestString := DigestString224
  END;


CONST
  k = ARRAY [0..63] OF INTEGER
  { 16_428A2F98, 16_71374491, 16_B5C0FBCF, 16_E9B5DBA5,
    16_3956C25B, 16_59F111F1, 16_923F82A4, 16_AB1C5ED5,
    16_D807AA98, 16_12835B01, 16_243185BE, 16_550C7DC3,
    16_72BE5D74, 16_80DEB1FE, 16_9BDC06A7, 16_C19BF174,
    16_E49B69C1, 16_EFBE4786, 16_0FC19DC6, 16_240CA1CC,
    16_2DE92C6F, 16_4A7484AA, 16_5CB0A9DC, 16_76F988DA,
    16_983E5152, 16_A831C66D, 16_B00327C8, 16_BF597FC7,
    16_C6E00BF3, 16_D5A79147, 16_06CA6351, 16_14292967,
    16_27B70A85, 16_2E1B2138, 16_4D2C6DFC, 16_53380D13,
    16_650A7354, 16_766A0ABB, 16_81C2C92E, 16_92722C85,
    16_A2BFE8A1, 16_A81A664B, 16_C24B8B70, 16_C76C51A3,
    16_D192E819, 16_D6990624, 16_F40E3585, 16_106AA070,
    16_19A4C116, 16_1E376C08, 16_2748774C, 16_34B0BCB5,
    16_391C0CB3, 16_4ED8AA4A, 16_5B9CCA4F, 16_682E6FF3,
    16_748F82EE, 16_78A5636F, 16_84C87814, 16_8CC70208,
    16_90BEFFFA, 16_A4506CEB, 16_BEF9A3F7, 16_C67178F2
  };


PROCEDURE OutputSize256(<* UNUSED *> hash: Sha256): CARDINAL =
  BEGIN
    RETURN 32
  END OutputSize256;


PROCEDURE OutputSize224(<* UNUSED *> hash: Sha224): CARDINAL =
  BEGIN
    RETURN 28
  END OutputSize224;


PROCEDURE Reset256(hash: Sha256) =
  BEGIN
    hash.used := 0;
    hash.msglen := 0;
    hash.h0 := 16_6A09E667;
    hash.h1 := 16_BB67AE85;
    hash.h2 := 16_3C6EF372;
    hash.h3 := 16_A54FF53A;
    hash.h4 := 16_510E527F;
    hash.h5 := 16_9B05688C;
    hash.h6 := 16_1F83D9AB;
    hash.h7 := 16_5BE0CD19
  END Reset256;


PROCEDURE New_Sha256(): Hash.T =
  VAR
    hash := NEW(Sha256);
  BEGIN
    Reset256(hash);
    RETURN hash
  END New_Sha256;


PROCEDURE Reset224(hash: Sha224) =
  BEGIN
    hash.used := 0;
    hash.msglen := 0;
    hash.h0 := 16_C1059ED8;
    hash.h1 := 16_367CD507;
    hash.h2 := 16_3070DD17;
    hash.h3 := 16_F70E5939;
    hash.h4 := 16_FFC00B31;
    hash.h5 := 16_68581511;
    hash.h6 := 16_64F98FA7;
    hash.h7 := 16_BEFA4FA4
  END Reset224;


PROCEDURE New_Sha224(): Hash.T =
  VAR
    hash := NEW(Sha224);
  BEGIN
    Reset224(hash);
    RETURN hash
  END New_Sha224;


PROCEDURE Ror(word, bits: INTEGER): INTEGER =
  BEGIN
    word := And(word, 16_FFFFFFFF);
    RETURN Or(LeftShift(word, 32 - bits), RightShift(word, bits))
  END Ror;


PROCEDURE Chunk(hash: Sha256) =
  VAR
    a, b, c, d, e, f, g, h: INTEGER;
    ch, mj, s0, s1, t1, t2: INTEGER;
    w: ARRAY [0..63] OF INTEGER;
  BEGIN
    a := hash.h0;
    b := hash.h1;
    c := hash.h2;
    d := hash.h3;
    e := hash.h4;
    f := hash.h5;
    g := hash.h6;
    h := hash.h7;

    FOR i := FIRST(w) TO 15 DO
      w[i] := GetWord32be(hash.buffer, 4*i)
    END;

    FOR i := 16 TO LAST(w) DO
      s0 := Xor(Ror(w[i-15],  7), Xor(Ror(w[i-15], 18), RightShift(w[i-15],  3)));
      s1 := Xor(Ror(w[i- 2], 17), Xor(Ror(w[i- 2], 19), RightShift(w[i- 2], 10)));
      w[i] := And(w[i-16] + s0 + w[i-7] + s1, 16_FFFFFFFF)
    END;

    FOR i := FIRST(w) TO LAST(w) DO
      s1 := Xor(Ror(e, 6), Xor(Ror(e, 11), Ror(e, 25)));
      ch := Xor(And(e, f), And(Not(e), g));
      t1 := h + s1 + ch + k[i] + w[i];
      s0 := Xor(Ror(a, 2), Xor(Ror(a, 13), Ror(a, 22)));
      mj := Xor(And(a, b), Xor(And(a, c), And(b, c)));
      t2 := s0 + mj;

      h := g;
      g := f;
      f := e;
      e := d + t1;
      d := c;
      c := b;
      b := a;
      a := t1 + t2
    END;

    INC(hash.h0, a);
    INC(hash.h1, b);
    INC(hash.h2, c);
    INC(hash.h3, d);
    INC(hash.h4, e);
    INC(hash.h5, f);
    INC(hash.h6, g);
    INC(hash.h7, h)
  END Chunk;


PROCEDURE DigestString256(hash: Sha256; VAR out: ARRAY OF CHAR) =
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
    PutWord32be(out, 16, hash.h4);
    PutWord32be(out, 20, hash.h5);
    PutWord32be(out, 24, hash.h6);
    PutWord32be(out, 28, hash.h7)
  END DigestString256;


PROCEDURE DigestString224(hash: Sha224; VAR out: ARRAY OF CHAR) =
  VAR
    fullOutput: ARRAY [0..31] OF CHAR;
  BEGIN
    DigestString256(hash, fullOutput);
    SUBARRAY(out, 0, 28) := SUBARRAY(fullOutput, 0, 28)
  END DigestString224;


BEGIN
  (* SKIP *)
END Crypto_Sha2.
