(* Copyright 2020,2021 Eric Sessoms / MIT License *)

MODULE Crypto_HashImpl;

FROM Word IMPORT And, Extract, Insert, LeftShift, Or, RightShift;


PROCEDURE BlockSize(<* UNUSED *> hash: T): CARDINAL =
  BEGIN
    RETURN 64
  END BlockSize;


PROCEDURE UpdateString(hash: T; READONLY data: ARRAY OF CHAR) =
  VAR
    from, len: CARDINAL;
  BEGIN
    from := 0;
    WHILE from # NUMBER(data) DO
      len := MIN(NUMBER(hash.buffer) - hash.used, NUMBER(data) - from);
      SUBARRAY(hash.buffer, hash.used, len) := SUBARRAY(data, from, len);
      INC(hash.used, len);
      INC(from, len);

      IF hash.used = NUMBER(hash.buffer) THEN
        hash.chunk();
        INC(hash.msglen, 8 * hash.used);
        hash.used := 0
      END
    END
  END UpdateString;


PROCEDURE GetWord32be(
    READONLY buffer: ARRAY OF CHAR; start: CARDINAL): INTEGER =
  VAR
    result := 0;
  BEGIN
    FOR i := 0 TO 3 DO
      result := Or(LeftShift(result, 8), ORD(buffer[start+i]))
    END;
    RETURN result
  END GetWord32be;


PROCEDURE GetWord32le(
    READONLY buffer: ARRAY OF CHAR; start: CARDINAL): INTEGER =
  VAR
    result := 0;
  BEGIN
    FOR i := 0 TO 3 DO
      result := Insert(result, ORD(buffer[start+i]), 8*i, 8)
    END;
    RETURN result
  END GetWord32le;


PROCEDURE LeftRotate32(word: INTEGER; bits: CARDINAL): INTEGER =
  BEGIN
    word := And(word, 16_FFFFFFFF);
  RETURN
    Or(LeftShift(word, bits), RightShift(word, 32 - bits))
  END LeftRotate32;


PROCEDURE Pad(hash: T) =
  BEGIN
    (* Add single 1 bit. *)
    hash.buffer[hash.used] := '\200';
    INC(hash.used);

    (* Pad *)
    IF hash.used + 8 > NUMBER(hash.buffer) THEN
      WHILE hash.used # NUMBER(hash.buffer) DO
        hash.buffer[hash.used] := '\000';
        INC(hash.used)
      END;
      hash.chunk();
      hash.used := 0
    END;

    WHILE hash.used + 8 # NUMBER(hash.buffer) DO
      hash.buffer[hash.used] := '\000';
      INC(hash.used)
    END;
  END Pad;


PROCEDURE PutWord32be(
    VAR buffer: ARRAY OF CHAR; start: CARDINAL; value: INTEGER) =
  BEGIN
    FOR i := 0 TO 3 DO
      buffer[start + i] := VAL(Extract(value, 8*(3-i), 8), CHAR)
    END
  END PutWord32be;


PROCEDURE PutWord32le(
    VAR buffer: ARRAY OF CHAR; start: CARDINAL; value: INTEGER) =
  BEGIN
    FOR i := 0 TO 3 DO
      buffer[start + i] := VAL(Extract(value, 8*i, 8), CHAR)
    END
  END PutWord32le;


BEGIN
  (* SKIP *)
END Crypto_HashImpl.
