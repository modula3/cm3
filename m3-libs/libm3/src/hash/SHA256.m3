(* Copyright (C) 2021 Peter McKinna.  All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

UNSAFE MODULE SHA256;

IMPORT Text, Fmt, Rd, Ctypes;
FROM Word IMPORT Plus, And, Or, Xor, Not, LeftShift, RightShift;
FROM Swap IMPORT Swap8, Int64On32, Endian, endian;

<*FATAL ANY*>

CONST
  Mask32   = 16_FFFFFFFF;
  BlockLen = 64;
  WordLen  = BlockLen DIV 8;

  (*Initialize array of round constants:
    (first 32 bits of the fractional parts of the cube roots
    of the first 64 primes 2..311)*)
  K = ARRAY [0 .. 63] OF
        INTEGER{
        16_428a2f98, 16_71374491, 16_b5c0fbcf, 16_e9b5dba5, 16_3956c25b,
        16_59f111f1, 16_923f82a4, 16_ab1c5ed5, 16_d807aa98, 16_12835b01,
        16_243185be, 16_550c7dc3, 16_72be5d74, 16_80deb1fe, 16_9bdc06a7,
        16_c19bf174, 16_e49b69c1, 16_efbe4786, 16_0fc19dc6, 16_240ca1cc,
        16_2de92c6f, 16_4a7484aa, 16_5cb0a9dc, 16_76f988da, 16_983e5152,
        16_a831c66d, 16_b00327c8, 16_bf597fc7, 16_c6e00bf3, 16_d5a79147,
        16_06ca6351, 16_14292967, 16_27b70a85, 16_2e1b2138, 16_4d2c6dfc,
        16_53380d13, 16_650a7354, 16_766a0abb, 16_81c2c92e, 16_92722c85,
        16_a2bfe8a1, 16_a81a664b, 16_c24b8b70, 16_c76c51a3, 16_d192e819,
        16_d6990624, 16_f40e3585, 16_106aa070, 16_19a4c116, 16_1e376c08,
        16_2748774c, 16_34b0bcb5, 16_391c0cb3, 16_4ed8aa4a, 16_5b9cca4f,
        16_682e6ff3, 16_748f82ee, 16_78a5636f, 16_84c87814, 16_8cc70208,
        16_90befffa, 16_a4506ceb, 16_bef9a3f7, 16_c67178f2};

TYPE
  Uint32 = Ctypes.unsigned_int;
  Byte = [0 .. 16_FF];
  Int32Bytes = ARRAY[0..3] OF Byte;
  StateType = ARRAY [0 .. WordLen - 1] OF Uint32;
  BufArr = ARRAY [0 .. BlockLen - 1] OF Byte;
  HashArr = ARRAY [0 .. BlockLen - 1] OF Uint32;
  IntArr = ARRAY[0 .. WordLen - 1] OF Byte;
  BufRef = REF ARRAY OF Byte;

  Control = RECORD
              data   : BufArr;
              dataLen: INTEGER;
              bitLen : INTEGER;
              state  : StateType;
            END;
  RefCtl = REF Control;

PROCEDURE ToUInt(b : Int32Bytes) : Uint32 =
  VAR
    t : Uint32;
  BEGIN
    (* produce a big-endian form 32 bit word *)
    t := Or (Or (                b[3],
                      LeftShift (b[2], 8)),
                  Or (LeftShift (b[1], 16),
                      LeftShift (b[0], 24)));
    RETURN t;
  END ToUInt;

PROCEDURE LengthBlock(ctrl : RefCtl) : IntArr =
  VAR
    len64 : Int64On32;
    buf : IntArr;
  BEGIN
    len64 := LOOPHOLE(VAL(ctrl.bitLen,LONGINT),Int64On32);
    IF endian = Endian.Little THEN
      len64 := Swap8(len64);
    END;
    buf := LOOPHOLE(len64,IntArr);
    RETURN buf;
  END LengthBlock;

PROCEDURE Add (x, y: Uint32): Uint32 =
  BEGIN
    RETURN And(Plus(x,y),Mask32);
  END Add;

<*UNUSED*>PROCEDURE Rol32 (w, s: Uint32): Uint32 =
  BEGIN
    RETURN And(Or(LeftShift(w, s), RightShift(w, 32 - s)), Mask32);
  END Rol32;

PROCEDURE Ror32 (w, s: Uint32): Uint32 =
  BEGIN
    RETURN And(Or(RightShift(w, s), LeftShift(w, 32 - s)), Mask32);
    (*RETURN Rol32(w, 32 - s); could use this instead *)
  END Ror32;

PROCEDURE S0 (x: Uint32): Uint32 =
  BEGIN
    RETURN And(Xor(Xor(Ror32(x, 7), Ror32(x, 18)), RightShift(x, 3)),Mask32);
  END S0;

PROCEDURE S1 (x: Uint32): Uint32 =
  BEGIN
    RETURN And(Xor(Xor(Ror32(x, 17), Ror32(x, 19)), RightShift(x, 10)),Mask32);
  END S1;

PROCEDURE EP1 (x: Uint32): Uint32 =
  BEGIN
    RETURN Xor(Xor(Ror32(x, 6), Ror32(x, 11)), Ror32(x, 25));
  END EP1;

PROCEDURE EP0 (x: Uint32): Uint32 =
  BEGIN
    RETURN Xor(Xor(Ror32(x, 2), Ror32(x, 13)), Ror32(x, 22));
  END EP0;

PROCEDURE MAJ (x, y, z: Uint32): Uint32 =
  BEGIN
    RETURN
      Xor(And(x, y), Xor(And(x, z), And(y, z)));
  END MAJ;

PROCEDURE CH (x, y, z: Uint32): Uint32 =
  BEGIN
    RETURN Xor(And(x, y), And(Not(x), z));
  END CH;

PROCEDURE Init (): RefCtl =
  VAR ctx := NEW(RefCtl);
  BEGIN
    (*Initialize hash values:
      (first 32 bits of the fractional parts of the square roots
      of the first 8 primes 2..19)*)
    ctx.state :=
      StateType{16_6a09e667, 16_bb67ae85, 16_3c6ef372, 16_a54ff53a,
                16_510e527f, 16_9b05688c, 16_1f83d9ab, 16_5be0cd19};
    ctx.dataLen := 0;
    ctx.bitLen := 0;
    RETURN ctx;
  END Init;

PROCEDURE Transform (ctx: RefCtl; READONLY data: BufArr) =
  VAR
    a, b, c, d, e, f, g, h, t1, t2: Uint32;
    m                             : HashArr := HashArr{0, ..};
  BEGIN
    FOR i := 0 TO 15 DO
      m[i] := ToUInt(SUBARRAY(data,i*4,4));
    END;

    (*Extend the first 16 words into the remaining 48 words w[16..63]
      of the message schedule array:*)
    FOR i := 16 TO BlockLen - 1 DO
      m[i] := Add(Add(Add(S1(m[i - 2]), m[i - 7]), S0(m[i - 15])),  m[i - 16]);
    END;

    (*Initialize working variables to the current hash value:*)
    a := ctx.state[0];
    b := ctx.state[1];
    c := ctx.state[2];
    d := ctx.state[3];
    e := ctx.state[4];
    f := ctx.state[5];
    g := ctx.state[6];
    h := ctx.state[7];

    (*Compression function main loop:*)
    FOR i := 0 TO BlockLen - 1 DO
      t1 := Add(Add(Add(Add(h, EP1(e)), CH(e, f, g)), K[i]), m[i]);
      t2 := Add(EP0(a), MAJ(a, b, c));
      h := g;
      g := f;
      f := e;
      e := Add(d, t1);
      d := c;
      c := b;
      b := a;
      a := Add(t1, t2);
    END;

    (*Add the compressed chunk to the current hash value:*)
    ctx.state[0] := Add(ctx.state[0], a);
    ctx.state[1] := Add(ctx.state[1], b);
    ctx.state[2] := Add(ctx.state[2], c);
    ctx.state[3] := Add(ctx.state[3], d);
    ctx.state[4] := Add(ctx.state[4], e);
    ctx.state[5] := Add(ctx.state[5], f);
    ctx.state[6] := Add(ctx.state[6], g);
    ctx.state[7] := Add(ctx.state[7], h);
  END Transform;

PROCEDURE Update (ctx: RefCtl; bufLen: CARDINAL; data: BufRef) =
  BEGIN
    FOR i := 0 TO bufLen - 1 DO
      ctx.data[ctx.dataLen] := data[i];
      INC(ctx.dataLen);
      IF ctx.dataLen = 64 THEN
        Transform(ctx, ctx.data);
        INC(ctx.bitLen, 512);
        ctx.dataLen := 0;
      END;
    END;
  END Update;

PROCEDURE Final (ctx: RefCtl): StateType =
  VAR
    len : INTEGER;
    hash: StateType;
  BEGIN
    len := ctx.dataLen;
    (* Pad whatever data is left in the buffer.*)
    ctx.data[len] := 16_80;
    INC(len);
    IF ctx.dataLen < 56 THEN
      WHILE len < 56 DO ctx.data[len] := 0; INC(len); END;
    ELSE
      WHILE len < BlockLen DO ctx.data[len] := 0; INC(len); END;
      Transform(ctx, ctx.data);
      FOR i := 0 TO 55 DO ctx.data[i] := 0; END;
    END;

    (* Append to the padding the total message's length in bits and
       transform. *)
    INC(ctx.bitLen, ctx.dataLen * 8);
    SUBARRAY(ctx.data,56,8) := LengthBlock(ctx);
    Transform(ctx, ctx.data);

    (*Produce the final hash value (big-endian):*)
    FOR i := 0 TO 7 DO hash[i] := ctx.state[i]; END;

    RETURN hash;
  END Final;

PROCEDURE TxtToBlock (txt: TEXT): BufRef =
  VAR
    arr: REF ARRAY OF CHAR;
    len                    := Text.Length(txt);
    b  : BufRef;
  BEGIN
    arr := NEW(REF ARRAY OF CHAR, len);
    Text.SetChars(arr^, txt);
    b := LOOPHOLE(arr, BufRef);
    RETURN b;
  END TxtToBlock;

PROCEDURE Sha256Text (txt: TEXT): StateType =
  VAR
    ctrl: RefCtl;
    b   : BufRef;
  BEGIN
    b := TxtToBlock(txt);
    ctrl := Init();
    Update(ctrl, NUMBER(b^), b);
    RETURN Final(ctrl);
  END Sha256Text;

PROCEDURE Sha256File (rd : Rd.T) : StateType =
  VAR
    ctrl : RefCtl;
    b : BufRef;
    count : CARDINAL;
    buf := NEW(REF ARRAY OF CHAR,BlockLen);
  BEGIN
    ctrl := Init();
    REPEAT
      count := Rd.GetSub(rd,buf^);
      IF count > 0 THEN
        b := LOOPHOLE(buf,BufRef);
        Update(ctrl, count, b);
      END;
    UNTIL count < BlockLen;
    RETURN Final(ctrl);
  END Sha256File;

PROCEDURE Construct (digest: StateType): TEXT =
  VAR result: TEXT := "";
  BEGIN
    FOR i := 0 TO WordLen - 1 DO
      result := result & Fmt.Pad(Fmt.Unsigned(digest[i], 16), 2, '0');
    END;
    RETURN result;
  END Construct;

PROCEDURE FromText (txt: TEXT): TEXT =
  BEGIN
    RETURN Construct(Sha256Text(txt));
  END FromText;

PROCEDURE FromFile(rd : Rd.T) : TEXT =
  BEGIN
    RETURN Construct(Sha256File(rd));
  END FromFile;

BEGIN
END SHA256.
