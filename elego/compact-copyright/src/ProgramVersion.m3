(*--------------------------------------------------------------------------*)
MODULE ProgramVersion;

IMPORT Rd, Text;
IMPORT Version AS V;

REVEAL
  T = Public BRANDED "ProgramVersion 0.0" OBJECT
    name  : TEXT;
    ver   : V.T;
    qual  : TEXT;
    num   : TEXT;
    br    : TEXT;
    d     : TEXT;
    okay  : BOOLEAN;
  METHODS
  OVERRIDES
    initFromRd := InitFromRd;
    found := Found;
    branded := IsBranded;
    corrupt := Corrupt;
    program := Program;
    shortText := ShortText;
    longText := LongText;
    version := GetVersion;
    serialNo := GetSerialNo;
    brand := GetBrand;
    date := Date;
  END;

(*--------------------------------------------------------------------------*)
PROCEDURE InitFromRd(self : T; rd : Rd.T; name := NIL) : T =
  BEGIN
    self.okay := FALSE;
    self.name := "unknown program";
    self.ver := NEW(V.T).fromText("x.x.x");
    self.num := "unknown serial number";
    self.br := "unknown brand";
    self.d := "unknown date";
    self.qual := "unqualified";
    (* FIXME *)
    RETURN self;
  END InitFromRd;

(*--------------------------------------------------------------------------*)
PROCEDURE Found(self : T) : BOOLEAN =
  BEGIN
    RETURN self.okay;
  END Found; 

(*--------------------------------------------------------------------------*)
PROCEDURE IsBranded(self : T) : BOOLEAN =
  BEGIN
    RETURN NOT Text.Equal(self.num, "0000000000000000") AND
           NOT Text.Equal(self.br, "0000000000000000");
  END IsBranded; 

(*--------------------------------------------------------------------------*)
PROCEDURE Corrupt(self : T) : BOOLEAN =
  BEGIN
    RETURN self.okay; (* FIXME *)
  END Corrupt; 

(*--------------------------------------------------------------------------*)
PROCEDURE Program(self : T) : TEXT =
  BEGIN
    IF self.okay THEN
      RETURN self.name;
    ELSE
      RETURN "unknown program";
    END;
  END Program;

(*--------------------------------------------------------------------------*)
CONST b = " ";

(*--------------------------------------------------------------------------*)
PROCEDURE ShortText(self : T) : TEXT =
  BEGIN
    RETURN self.program() & b & self.ver.toText() & b & self.qual;
  END ShortText;

(*--------------------------------------------------------------------------*)
PROCEDURE LongText(self : T) : TEXT =
  BEGIN
    RETURN self.shortText() & b & self.num & b & self.br & b & 
           self.d;
  END LongText;

(*--------------------------------------------------------------------------*)
PROCEDURE GetVersion(self : T) : V.T =
  BEGIN
    RETURN self.ver;
  END GetVersion;

(*--------------------------------------------------------------------------*)
PROCEDURE GetSerialNo(self : T) : TEXT =
  BEGIN
    IF self.okay THEN
      RETURN self.num;
    ELSE
      RETURN "unknown serial number";
    END;
  END GetSerialNo;

(*--------------------------------------------------------------------------*)
PROCEDURE GetBrand(self : T) : TEXT =
  BEGIN
    IF self.okay THEN
      RETURN self.br;
    ELSE
      RETURN "unknown brand";
    END;
  END GetBrand;

(*--------------------------------------------------------------------------*)
PROCEDURE Date(self : T) : TEXT =
  BEGIN
    RETURN self.d;
  END Date;

BEGIN
  My := NEW(T);
  My.name := Name;
  My.ver := NEW(V.T).fromText(Version);
  My.qual := Qualifier;
  My.num := SerialNo;
  My.br := Brand;
  My.d := Branded;
  My.okay := TRUE;
END ProgramVersion.
