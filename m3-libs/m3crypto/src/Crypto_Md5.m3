(* Copyright 2020,2021 Eric Sessoms / MIT License *)

MODULE Crypto_Md5;

IMPORT
  Crypto_Hash     AS Hash,
  Crypto_HashImpl AS Impl;

FROM Impl IMPORT GetWord32le, LeftRotate32, Pad, PutWord32le;
FROM Word IMPORT And, Extract, Not, Or, Xor;


TYPE
  T = Impl.T BRANDED OBJECT
    a0, b0, c0, d0: INTEGER
  OVERRIDES
    outputSize := OutputSize;
    reset := Reset;
    chunk := Chunk;
    digestString := DigestString
  END;


TYPE
  Round = [0..63];


CONST
  (* K[i] := FLOOR(232 * ABS(Math.sin(i + 1))) *)
  K = ARRAY Round OF INTEGER
  { 16_D76AA478, 16_E8C7B756, 16_242070DB, 16_C1BDCEEE,
    16_F57C0FAF, 16_4787C62A, 16_A8304613, 16_FD469501,
    16_698098D8, 16_8B44F7AF, 16_FFFF5BB1, 16_895CD7BE,
    16_6B901122, 16_FD987193, 16_A679438E, 16_49B40821,
    16_F61E2562, 16_C040B340, 16_265E5A51, 16_E9B6C7AA,
    16_D62F105D, 16_02441453, 16_D8A1E681, 16_E7D3FBC8,
    16_21E1CDE6, 16_C33707D6, 16_F4D50D87, 16_455A14ED,
    16_A9E3E905, 16_FCEFA3F8, 16_676F02D9, 16_8D2A4C8A,
    16_FFFA3942, 16_8771F681, 16_6D9D6122, 16_FDE5380C,
    16_A4BEEA44, 16_4BDECFA9, 16_F6BB4B60, 16_BEBFBC70,
    16_289B7EC6, 16_EAA127FA, 16_D4EF3085, 16_04881D05,
    16_D9D4D039, 16_E6DB99E5, 16_1FA27CF8, 16_C4AC5665,
    16_F4292244, 16_432AFF97, 16_AB9423A7, 16_FC93A039,
    16_655B59C3, 16_8F0CCC92, 16_FFEFF47D, 16_85845DD1,
    16_6FA87E4F, 16_FE2CE6E0, 16_A3014314, 16_4E0811A1,
    16_F7537E82, 16_BD3AF235, 16_2AD7D2BB, 16_EB86D391
  };

  (* Shifts *)
  s = ARRAY Round OF CARDINAL
  { 7, 12, 17, 22,
    7, 12, 17, 22,
    7, 12, 17, 22,
    7, 12, 17, 22,
    5,  9, 14, 20,
    5,  9, 14, 20,
    5,  9, 14, 20,
    5,  9, 14, 20,
    4, 11, 16, 23,
    4, 11, 16, 23,
    4, 11, 16, 23,
    4, 11, 16, 23,
    6, 10, 15, 21,
    6, 10, 15, 21,
    6, 10, 15, 21,
    6, 10, 15, 21
  };


PROCEDURE OutputSize(<* UNUSED *> hash: T): CARDINAL =
  BEGIN
    RETURN 16
  END OutputSize;


PROCEDURE Reset(hash: T) =
  BEGIN
    hash.used := 0;
    hash.msglen := 0;
    hash.a0 := 16_67452301;
    hash.b0 := 16_EFCDAB89;
    hash.c0 := 16_98BADCFE;
    hash.d0 := 16_10325476
  END Reset;


PROCEDURE New(): Hash.T =
  VAR
    hash := NEW(T);
  BEGIN
    Reset(hash);
    RETURN hash
  END New;


PROCEDURE Chunk(hash: T) =
  VAR
    A, B, C, D, F: INTEGER;
    M: ARRAY [0..15] OF INTEGER;
    g: INTEGER;
  BEGIN
    A := hash.a0;
    B := hash.b0;
    C := hash.c0;
    D := hash.d0;

    FOR i := FIRST(M) TO LAST(M) DO
      M[i] := GetWord32le(hash.buffer, 4*i)
    END;

    FOR i := FIRST(Round) TO LAST(Round) DO
      IF    i <= 15 THEN
        F := Or(And(B, C), And(Not(B), D));
        g := i
      ELSIF i <= 31 THEN
        F := Or(And(D, B), And(Not(D), C));
        g := (5*i + 1) MOD 16
      ELSIF i <= 47 THEN
        F := Xor(Xor(B, C), D);
        g := (3*i + 5) MOD 16
      ELSIF i <= 63 THEN
        F := Xor(C, Or(B, Not(D)));
        g :=  7*i      MOD 16
      ELSE
        <* ASSERT FALSE *>
      END;
      F := F + A + K[i] + M[g];
      A := D;
      D := C;
      C := B;
      B := B + LeftRotate32(F, s[i])
    END;

    INC(hash.a0, A);
    INC(hash.b0, B);
    INC(hash.c0, C);
    INC(hash.d0, D)
  END Chunk;


PROCEDURE DigestString(hash: T; VAR out: ARRAY OF CHAR) =
  BEGIN
    (* Original message length. *)
    INC(hash.msglen, 8 * hash.used);

    Pad(hash);

    (* Append message length in bits. *)
    FOR i := 0 TO 7 DO
      hash.buffer[hash.used + i] := VAL(Extract(hash.msglen, 8*i, 8), CHAR)
    END;
    Chunk(hash);

    PutWord32le(out,  0, hash.a0);
    PutWord32le(out,  4, hash.b0);
    PutWord32le(out,  8, hash.c0);
    PutWord32le(out, 12, hash.d0)
  END DigestString;


BEGIN
  (* SKIP *)
END Crypto_Md5.
