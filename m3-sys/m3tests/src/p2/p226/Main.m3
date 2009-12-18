UNSAFE MODULE Main;
FROM Cstdint IMPORT uint8_t, uint16_t, uint32_t;
IMPORT RTIO;

VAR
    a8: uint8_t := 1;
    b8: uint8_t := 2;
    c8: uint8_t := 3;
    d8: uint8_t := 4;
    a16: uint16_t := 10;
    b16: uint16_t := 20;
    c16: uint16_t := 30;
    d16: uint16_t := 40;
    a32: uint32_t := 100;
    b32: uint32_t := 200;
    c32: uint32_t := 300;
    d32: uint32_t := 400;
 (* a64: uint64_t := 1000;
    b64: uint64_t := 2000;
    c64: uint64_t := 3000;
    d64: uint64_t := 4000; *)
    ap := LOOPHOLE(16_1000, ADDRESS);
    bp := LOOPHOLE(16_2000, ADDRESS);
    cp := LOOPHOLE(16_3000, ADDRESS);
    dp := LOOPHOLE(16_4000, ADDRESS);
    b:BOOLEAN;

PROCEDURE PrintA(a:TEXT; b:ADDRESS)=
BEGIN
  RTIO.PutText(a);
  RTIO.PutText(":");
  RTIO.PutAddr(b);
  RTIO.PutText("\n");
  RTIO.Flush();
END PrintA;

PROCEDURE PrintI(a:TEXT; b:INTEGER)=
BEGIN
  RTIO.PutText(a);
  RTIO.PutText(":");
  RTIO.PutInt(b);
  RTIO.PutText("\n");
  RTIO.Flush();
END PrintI;

PROCEDURE PrintB(a:TEXT; b:BOOLEAN)=
BEGIN
  RTIO.PutText(a);
  RTIO.PutText(":");
  RTIO.PutText(ARRAY [FALSE..TRUE] OF TEXT{"FALSE", "TRUE"}[b]);
  RTIO.PutText("\n");
  RTIO.Flush();
END PrintB;

PROCEDURE PrintS(a:TEXT)=
BEGIN
  RTIO.PutText(a);
END PrintS;

BEGIN

(* not portable, just for debugging
  PrintS("addresses for debugging\n\n");
  PrintA("b", ADR(b));
  PrintA("a8", ADR(a8));
  PrintA("b8", ADR(b8));
  PrintA("c8", ADR(c8));
  PrintA("d8", ADR(d8));
  PrintA("a16", ADR(a16));
  PrintA("b16", ADR(b16));
  PrintA("c16", ADR(c16));
  PrintA("d16", ADR(d16));
  PrintA("a32", ADR(a32));
  PrintA("b32", ADR(b32));
  PrintA("c32", ADR(c32));
  PrintA("d32", ADR(d32));
  PrintA("a64", ADR(a64));
  PrintA("b64", ADR(b64));
  PrintA("c64", ADR(c64));
  PrintA("d64", ADR(d64));
  PrintA("ap", ADR(ap));
  PrintA("bp", ADR(bp));
  PrintA("cp", ADR(cp));
  PrintA("dp", ADR(dp));
  PrintS("\n");
  PrintS("\n");
*)
  PrintS("initial values\n\n");

  PrintB("b", b);
  PrintI("a8", a8);
  PrintI("b8", b8);
  PrintI("c8", c8);
  PrintI("d8", d8);
  PrintI("a16", a16);
  PrintI("b16", b16);
  PrintI("c16", c16);
  PrintI("d16", d16);
  PrintI("a32", a32);
  PrintI("b32", b32);
  PrintI("c32", c32);
  PrintI("d32", d32);
(*PrintI("a64", a64);
  PrintI("b64", b64);
  PrintI("c64", c64);
  PrintI("d64", d64); *)
  PrintA("ap", ap);
  PrintA("bp", bp);
  PrintA("cp", cp);
  PrintA("dp", dp);
  PrintS("\n");
  PrintS("\n");

  PrintS("\nshould print false\n\n");

(* 8 bit parameters not yet supported by integrated backend *)

(* d8 := CAS(a8, b8, c8);*)
  (*b := CASP(a8, b8, c8); *)
  PrintB("b", b);
  PrintI("d8", d8);

  PrintS("\n");
  d16 := CAS(a16, b16, c16);
    b := CASP(a16, b16, c16);
  PrintB("b", b);
  PrintI("d16", d16);

  PrintS("\n");
  d32 := CAS(a32, b32, b32);
    b := CASP(a32, b32, c32);
  PrintB("b", b);
  PrintI("d32", d32);

(*PrintS("\n");
  d64 := CAS(a64, b64, b64);
    b := CASP(a64, b64, c64);
  PrintB("b", b);
  PrintI("d64", d64);*)

  PrintS("\n");
   dp := CAS(ap, bp, cp);
    b := CASP(ap, bp, cp);
  PrintB("b", b);
  PrintA("dp", dp);


  PrintS("\n");
  PrintS("\n");

  (* generate new values *)

  INC(a8, 100);
  INC(b8, 100);
  INC(c8, 100);
  INC(d8, 100);
  INC(a16);
  INC(b16);
  INC(c16);
  INC(d16);
  INC(a32);
  INC(b32);
  INC(c32);
  INC(d32);
(*INC(a64);
  INC(b64);
  INC(c64);
  INC(d64); *)
  INC(ap);
  INC(bp);
  INC(cp);
  INC(dp);

  PrintS("\nshould print true\n\n");

(*d8 := CAS(a8, a8, c8);
    b := CASP(a8, a8, c8);*)
  PrintB("b", b);
  PrintI("d8", d8);

  d16 := CAS(a16, a16, c16);
    b := CASP(a16, a16, c16);
  PrintB("b", b);
  PrintI("d16", d16);

  PrintS("\n");
  d32 := CAS(a32, a32, b32);
    b := CASP(a32, a32, c32);
  PrintB("b", b);
  PrintI("d32", d32);

(*PrintS("\n");
  d64 := CAS(a64, a64, b64);
    b := CASP(a64, a64, c64);
  PrintB("b", b);
  PrintI("d64", d64);*)

  PrintS("\n");
   dp := CAS(ap, ap, cp);
    b := CASP(ap, ap, cp);
  PrintB("b", b);
  PrintA("dp", dp);




END Main.
