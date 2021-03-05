UNSAFE MODULE IPInternal;
IMPORT IP, M3toC;

TYPE
  Endpoint4 = IP.Endpoint4;
  Endpoint16 = IP.Endpoint16;

PROCEDURE CopyStoT(s: char_star; VAR text: TEXT) =
BEGIN
  text := NIL;
  IF s # NIL THEN
    text := M3toC.CopyStoT(s);
  END;
END CopyStoT;

PROCEDURE NewEndpoint4(VAR endpoint: EP; port: int; VAR address: Address4) =
BEGIN
    endpoint := NEW(Endpoint4, adr := address, port := port);
END NewEndpoint4;

PROCEDURE NewEndpoint6(VAR endpoint: EP; port: int; VAR address: Address16) =
BEGIN
    endpoint := NEW(Endpoint16, adr := address, port := port);
END NewEndpoint6;

BEGIN
END IPInternal.
