MODULE Main;

IMPORT IO, IP;

CONST
  null = IP.Address{a := ARRAY OF BITS 8 FOR [0..255] {0,0,0,0}};
VAR
  addr: IP.Address;
  one := IP.Address{a := ARRAY OF BITS 8 FOR [0..255] {1,1,1,1}};
BEGIN
  addr := IP.Address{a := ARRAY OF BITS 8 FOR [0..255] {87,250,125,7}};
  addr := null;
  addr := one;
  IO.Put("OK\n");
END Main.
