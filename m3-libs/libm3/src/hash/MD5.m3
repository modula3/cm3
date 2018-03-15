(* Copyright (C) 2017 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

UNSAFE MODULE MD5;

IMPORT Word,Text,Rd,Ctypes,Fmt;
FROM Swap IMPORT Swap4,Swap8,Int64On32;

<*FATAL ANY*>

CONST
  BlockLen = 64;
  WordLen = BlockLen DIV 4;
  Mask32 = 16_FFFFFFFF;

TYPE
  Uint32 = Ctypes.unsigned_int;
  Byte = [0..16_FF];
  
  Int32Bytes = ARRAY[0..3] OF Byte;
  ByteBuf = ARRAY[0..BlockLen-1] OF Byte;
  BlockBuf = ARRAY[0..WordLen-1] OF Uint32;
  RefBlock = REF ARRAY OF Byte;
  Digest = ARRAY[0..WordLen-1] OF Byte; (* result type *)

  MD5Control = RECORD
    bufLen : CARDINAL; (* current length of buffer *)
    length : CARDINAL; (* length of stream *)
    A,B,C,D : Uint32;
    buffer  : ByteBuf;
  END;
  RefMD5Control = REF MD5Control;

  Endian = {Big, Little};

VAR 
  endian : Endian;
  
PROCEDURE GetEndian() : Endian =
  VAR  
    ref := NEW(UNTRACED REF INTEGER);
  BEGIN
    ref^ := 1;
    IF 1 = LOOPHOLE(ref, UNTRACED REF [0..255])^ THEN
      RETURN Endian.Little;
    ELSE
      RETURN Endian.Big;
    END;
  END GetEndian;
  
PROCEDURE Swap(n : INTEGER) : INTEGER =
  BEGIN
    IF endian = Endian.Big THEN
      RETURN Swap4(n);
    ELSE
      RETURN n;
    END;
  END Swap;
  
(* 32 bit left rotate *)
PROCEDURE LeftRotate32(w,s : INTEGER) : INTEGER =
  BEGIN
    RETURN Word.And(Word.Or(Word.LeftShift(w,s), Word.RightShift(w,32-s)),Mask32);
  END LeftRotate32;
  
PROCEDURE ToInt(b : Int32Bytes) : Uint32 =
  VAR
    t : Uint32;
  BEGIN
    t := Word.Or (Word.Or (                b[0],
                           Word.LeftShift (b[1], 8)),
                  Word.Or (Word.LeftShift (b[2], 16),
                           Word.LeftShift (b[3], 24)));
    t := Word.And(t,Mask32);
    RETURN t;
  END ToInt;
  
PROCEDURE Init() : RefMD5Control =
  VAR
    ctrl := NEW(RefMD5Control);
  BEGIN
    ctrl.A := 16_67452301;
    ctrl.B := 16_efcdab89;
    ctrl.C := 16_98badcfe;
    ctrl.D := 16_10325476;
    ctrl.bufLen := 0;
    ctrl.length := 0;
    RETURN ctrl;
  END Init;

PROCEDURE DigestControl(ctrl : RefMD5Control) : Digest =
  TYPE
    ResBuf = ARRAY[0..3] OF Uint32;
  VAR
    resBuf : ResBuf;
    digest : Digest;
  BEGIN
    resBuf[0] := Swap(ctrl.A);
    resBuf[1] := Swap(ctrl.B);
    resBuf[2] := Swap(ctrl.C);
    resBuf[3] := Swap(ctrl.D);
    digest := LOOPHOLE(resBuf,Digest);
    RETURN digest;
  END DigestControl;

PROCEDURE R1(VAR a: Uint32; b,c,d,x: Uint32; s: Byte; ac: Uint32) =
  (* F(b,c,d) = (b and c) or ((not b) and d) *)
  BEGIN
    a := Word.And(a + Word.Or(Word.And(b,c), Word.And(Word.Not(b), d)) + x + ac, Mask32);
    a := LeftRotate32(a,s);
    a := Word.And(b + a,Mask32);
  END R1;

PROCEDURE R2(VAR a: Uint32; b,c,d,x: Uint32; s: Byte; ac: Uint32) =
  (* G(b,c,d) = (b and d) or (c and (not d)) *)
  BEGIN
    a := Word.And(a + Word.Or(Word.And(b,d), Word.And(Word.Not(d), c)) + x + ac, Mask32);
    a := LeftRotate32(a,s);
    a := Word.And(b + a,Mask32);
  END R2;

PROCEDURE R3(VAR a: Uint32; b,c,d,x: Uint32; s: Byte; ac: Uint32) =
  (* H(b,c,d) = b xor c xor d; *)
  BEGIN
    a := Word.And(a + Word.Xor(b, Word.Xor(c,d)) + x + ac, Mask32);
    a := LeftRotate32(a,s);
    a := Word.And(b + a,Mask32);
  END R3;

PROCEDURE R4(VAR a: Uint32; b,c,d,x: Uint32; s: Byte; ac: Uint32) =
  (* I(b,c,d) = c xor (b or (not d)); *)
  BEGIN
    a := Word.And(a + Word.Xor(c, Word.Or(b,Word.Not(d))) + x + ac, Mask32);
    a := LeftRotate32(a,s);
    a := Word.And(b + a,Mask32);
  END R4;

PROCEDURE Transform(ctrl : RefMD5Control) =
  VAR
    a, b, c, d : Uint32;
    block : BlockBuf; 
  BEGIN
    FOR i := 0 TO WordLen - 1 DO
      block[i] := ToInt(SUBARRAY(ctrl.buffer,i*4,4));
    END;
    
    a := ctrl.A;
    b := ctrl.B;
    c := ctrl.C;
    d := ctrl.D;

    (* Round 1 *)
    R1(a,b,c,d,block[0] , 7,16_d76aa478); 
    R1(d,a,b,c,block[1] ,12,16_e8c7b756); 
    R1(c,d,a,b,block[2] ,17,16_242070db); 
    R1(b,c,d,a,block[3] ,22,16_c1bdceee);
    R1(a,b,c,d,block[4] , 7,16_f57c0faf); 
    R1(d,a,b,c,block[5] ,12,16_4787c62a); 
    R1(c,d,a,b,block[6] ,17,16_a8304613); 
    R1(b,c,d,a,block[7] ,22,16_fd469501);
    R1(a,b,c,d,block[8] , 7,16_698098d8); 
    R1(d,a,b,c,block[9] ,12,16_8b44f7af); 
    R1(c,d,a,b,block[10],17,16_ffff5bb1); 
    R1(b,c,d,a,block[11],22,16_895cd7be);
    R1(a,b,c,d,block[12], 7,16_6b901122); 
    R1(d,a,b,c,block[13],12,16_fd987193); 
    R1(c,d,a,b,block[14],17,16_a679438e); 
    R1(b,c,d,a,block[15],22,16_49b40821);

    (* Round 2 *)
    R2(a,b,c,d,block[1] , 5,16_f61e2562); 
    R2(d,a,b,c,block[6] , 9,16_c040b340); 
    R2(c,d,a,b,block[11],14,16_265e5a51); 
    R2(b,c,d,a,block[0] ,20,16_e9b6c7aa);
    R2(a,b,c,d,block[5] , 5,16_d62f105d); 
    R2(d,a,b,c,block[10], 9,16_02441453); 
    R2(c,d,a,b,block[15],14,16_d8a1e681); 
    R2(b,c,d,a,block[4] ,20,16_e7d3fbc8);
    R2(a,b,c,d,block[9] , 5,16_21e1cde6); 
    R2(d,a,b,c,block[14], 9,16_c33707d6); 
    R2(c,d,a,b,block[3] ,14,16_f4d50d87); 
    R2(b,c,d,a,block[8] ,20,16_455a14ed);
    R2(a,b,c,d,block[13], 5,16_a9e3e905); 
    R2(d,a,b,c,block[2] , 9,16_fcefa3f8); 
    R2(c,d,a,b,block[7] ,14,16_676f02d9); 
    R2(b,c,d,a,block[12],20,16_8d2a4c8a);

    (* Round 3 *)
    R3(a,b,c,d,block[5] , 4,16_fffa3942); 
    R3(d,a,b,c,block[8] ,11,16_8771f681); 
    R3(c,d,a,b,block[11],16,16_6d9d6122); 
    R3(b,c,d,a,block[14],23,16_fde5380c);
    R3(a,b,c,d,block[1] , 4,16_a4beea44); 
    R3(d,a,b,c,block[4] ,11,16_4bdecfa9); 
    R3(c,d,a,b,block[7] ,16,16_f6bb4b60); 
    R3(b,c,d,a,block[10],23,16_bebfbc70);
    R3(a,b,c,d,block[13], 4,16_289b7ec6); 
    R3(d,a,b,c,block[0] ,11,16_eaa127fa); 
    R3(c,d,a,b,block[3] ,16,16_d4ef3085); 
    R3(b,c,d,a,block[6] ,23,16_04881d05);
    R3(a,b,c,d,block[9] , 4,16_d9d4d039); 
    R3(d,a,b,c,block[12],11,16_e6db99e5); 
    R3(c,d,a,b,block[15],16,16_1fa27cf8); 
    R3(b,c,d,a,block[2] ,23,16_c4ac5665);

    (* Round 4 *)
    R4(a,b,c,d,block[0] , 6,16_f4292244); 
    R4(d,a,b,c,block[7] ,10,16_432aff97); 
    R4(c,d,a,b,block[14],15,16_ab9423a7); 
    R4(b,c,d,a,block[5] ,21,16_fc93a039);
    R4(a,b,c,d,block[12], 6,16_655b59c3); 
    R4(d,a,b,c,block[3] ,10,16_8f0ccc92); 
    R4(c,d,a,b,block[10],15,16_ffeff47d); 
    R4(b,c,d,a,block[1] ,21,16_85845dd1);
    R4(a,b,c,d,block[8] , 6,16_6fa87e4f); 
    R4(d,a,b,c,block[15],10,16_fe2ce6e0); 
    R4(c,d,a,b,block[6] ,15,16_a3014314); 
    R4(b,c,d,a,block[13],21,16_4e0811a1);
    R4(a,b,c,d,block[4] , 6,16_f7537e82); 
    R4(d,a,b,c,block[11],10,16_bd3af235); 
    R4(c,d,a,b,block[2] ,15,16_2ad7d2bb); 
    R4(b,c,d,a,block[9] ,21,16_eb86d391);

    ctrl.A := Word.And(ctrl.A + a,Mask32);
    ctrl.B := Word.And(ctrl.B + b,Mask32);
    ctrl.C := Word.And(ctrl.C + c,Mask32);
    ctrl.D := Word.And(ctrl.D + d,Mask32);

    INC(ctrl.length,BlockLen);
  END Transform;
  
PROCEDURE Update(ctrl: RefMD5Control; bufLen : CARDINAL; buf : RefBlock) =
  VAR 
    num,index : CARDINAL := 0;
  BEGIN
    (* 1. Transform existing data *)
    IF ctrl.bufLen > 0 THEN
      (* Fill buffer to BlockLen bytes if possible *)
      num := BlockLen - ctrl.bufLen;
      IF num > bufLen THEN
        num := bufLen;
      END;

      SUBARRAY(ctrl.buffer,ctrl.bufLen,num) := SUBARRAY(buf^,index,num);
      INC(index,num);      
      INC(ctrl.bufLen, num);

      (* If buffer contains BlockLen bytes, transform it*)
      IF ctrl.bufLen = BlockLen THEN
        Transform(ctrl);
        ctrl.bufLen := 0;
      END;
    END;
    
    (* 2. Transform BlockLen- byte blocks of buf *)
    num := bufLen - num;
    WHILE num >= BlockLen DO
      SUBARRAY(ctrl.buffer,0,BlockLen) := SUBARRAY(buf^,index,BlockLen);
      Transform(ctrl);
      INC(index,BlockLen);
      DEC(num, BlockLen);
    END;

    (* 3. If there's a block smaller than BlockLen bytes left, add it to buffer *)
    IF num > 0 THEN
      ctrl.bufLen := num;
      SUBARRAY(ctrl.buffer,0,num) := SUBARRAY(buf^,index,num);      
    END;
  END Update;

PROCEDURE LengthBlock(ctrl : RefMD5Control) : RefBlock =
  VAR
    length : LONGINT;
    len64 : Int64On32;
    buf : RefBlock;
  BEGIN
    buf := NEW(RefBlock,8);
    length := VAL(ctrl.length + ctrl.bufLen,LONGINT) * 8L;
    IF endian = Endian.Big THEN
      len64 := LOOPHOLE(length,Int64On32);
      len64 := Swap8(len64);
      length := LOOPHOLE(len64,LONGINT);
    END;
    buf^ := LOOPHOLE(length,ARRAY[0..7] OF Byte);
    RETURN buf;
  END LengthBlock;
  
PROCEDURE Final(ctrl : RefMD5Control) : Digest =
  VAR
    pads : CARDINAL;
    padArr,lenArr : RefBlock;
    digest : Digest;
  BEGIN
    padArr := NEW(RefBlock,BlockLen);
    padArr[0] := 16_80;

    (* 1. Compute 64 bit length of the stream in bits *)
    lenArr := LengthBlock(ctrl);
    
    (* 2. Append padding bits *)
    IF ctrl.bufLen >= 56 THEN
      pads := 120 - ctrl.bufLen
    ELSE
      pads := 56 - ctrl.bufLen;
    END;
    
    Update(ctrl, pads, padArr);

    (* 3. Append length of the stream *)
    Update(ctrl, 8, lenArr);

    (* 4. State to digest *)
    digest := DigestControl(ctrl);
    RETURN digest;
  END Final;

PROCEDURE TxtToBlock(txt : TEXT) : RefBlock =
  VAR
    arr : REF ARRAY OF CHAR;
    len := Text.Length(txt);
    b : RefBlock;  
  BEGIN
    arr := NEW(REF ARRAY OF CHAR,len);
    Text.SetChars(arr^,txt);
    b := LOOPHOLE(arr,RefBlock);
    RETURN b;
  END TxtToBlock;

PROCEDURE Construct(digest : Digest) : TEXT =
  VAR result : TEXT := "";
  BEGIN
    FOR i := 0 TO WordLen - 1 DO
      result := result & Fmt.Pad(Fmt.Int(digest[i],16),2,'0');
    END;
    RETURN result;
  END Construct;
  
PROCEDURE MDText(txt : TEXT) : Digest =
  VAR
    ctrl : RefMD5Control;
    b : RefBlock;
  BEGIN
    b := TxtToBlock(txt);
    ctrl := Init();
    Update(ctrl, NUMBER(b^), b);
    RETURN Final(ctrl);
  END MDText;
  
PROCEDURE MDFile(rd : Rd.T) : Digest =
  VAR
    ctrl : RefMD5Control;
    b : RefBlock;
    count : CARDINAL;
    buf := NEW(REF ARRAY OF CHAR,BlockLen);
  BEGIN
    ctrl := Init();
    REPEAT
      count := Rd.GetSub(rd,buf^);
      IF count > 0 THEN
        b := LOOPHOLE(buf,RefBlock);
        Update(ctrl, count, b);
      END;
    UNTIL count < BlockLen;
    RETURN Final(ctrl);
  END MDFile;

PROCEDURE FromText(txt : TEXT) : TEXT =
  BEGIN
    RETURN Construct(MDText(txt));
  END FromText;
  
PROCEDURE FromFile(rd : Rd.T) : TEXT =
  BEGIN
    RETURN Construct(MDFile(rd));
  END FromFile;
  
(* debug ctrl *)
(*
PROCEDURE DC(a,b,c,d : Uint32) =
  BEGIN
    IO.Put("dd " & Fmt.Int(Swap(a),16) & " " &
                   Fmt.Int(Swap(b),16) & " " &
                   Fmt.Int(Swap(c),16) & " " &
                   Fmt.Int(Swap(d),16) & "\n");
  END DC;
*)

BEGIN
  endian := GetEndian();
END MD5.