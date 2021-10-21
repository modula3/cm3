(* Copyright 2020,2021 Eric Sessoms / MIT License *)

MODULE Crypto_Hash;

IMPORT
  Rd,
  Text,
  TextRd,
  TextWr,
  Wr,

  (* These hash interfaces are included here for the convenience of
  the user, who will not need to import the individual hash modules.
  There is no initialization dependency between `Crypto_Hash` and any
  of the derived modules. *)
  Crypto_Md5  AS Md5,
  Crypto_Sha1 AS Sha1,
  Crypto_Sha2 AS Sha2;


PROCEDURE New(algorithm: Algorithm): T =
  VAR
    hash: T;
  BEGIN
    CASE algorithm OF
    | Algorithm.MD5    => hash := Md5.New()
    | Algorithm.SHA1   => hash := Sha1.New()
    | Algorithm.SHA224 => hash := Sha2.New_Sha224()
    | Algorithm.SHA256 => hash := Sha2.New_Sha256()
    END;
    RETURN hash
  END New;


PROCEDURE BlockSize(hash: T): CARDINAL =
  BEGIN
    RETURN hash.blockSize()
  END BlockSize;


PROCEDURE OutputSize(hash: T): CARDINAL =
  BEGIN
    RETURN hash.outputSize()
  END OutputSize;


PROCEDURE Reset(hash: T) =
  BEGIN
    hash.reset()
  END Reset;


PROCEDURE UpdateString(hash: T; READONLY data: ARRAY OF CHAR) =
  BEGIN
    hash.updateString(data)
  END UpdateString;


PROCEDURE Update(hash: T; data: TEXT) =
  VAR
    chars: ARRAY [0..4095] OF CHAR;
    numChars, start: CARDINAL;
  BEGIN
    start := 0;
    WHILE start # Text.Length(data) DO
      Text.SetChars(chars, data, start);
      numChars := MIN(NUMBER(chars), Text.Length(data) - start);
      UpdateString(hash, SUBARRAY(chars, 0, numChars));
      INC(start, numChars)
    END
  END Update;


PROCEDURE DigestString(hash: T; VAR out: ARRAY OF CHAR) =
  BEGIN
    <* ASSERT NUMBER(out) >= OutputSize(hash) *>
    hash.digestString(out)
  END DigestString;


PROCEDURE Digest(hash: T; VAR out: TEXT) =
  VAR
    digest: ARRAY [0..31] OF CHAR;
  BEGIN
    DigestString(hash, digest);
    out := Text.FromChars(SUBARRAY(digest, 0, OutputSize(hash)))
  END Digest;


PROCEDURE ToHexString(data: TEXT): TEXT =
  <* FATAL ANY *>
  CONST
    digits = "0123456789abcdef";
  VAR
    byte: INTEGER;
    result: TEXT;
    rd := TextRd.New(data);
    wr := TextWr.New();
  BEGIN
    WHILE NOT Rd.EOF(rd) DO
      byte := ORD(Rd.GetChar(rd));
      Wr.PutChar(wr, Text.GetChar(digits, byte DIV 16));
      Wr.PutChar(wr, Text.GetChar(digits, byte MOD 16))
    END;

    result := TextWr.ToText(wr);

    Rd.Close (rd);
    Wr.Close (wr);

    RETURN result
  END ToHexString;


PROCEDURE HexDigest(hash: T; VAR out: TEXT) =
  VAR
    digest: TEXT;
  BEGIN
    Digest(hash, digest);
    out := ToHexString(digest)
  END HexDigest;


BEGIN
  (* SKIP *)
END Crypto_Hash.
