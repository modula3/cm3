(*--------------------------------------------------------------------------*)
MODULE FileStamp;

IMPORT Time, FmtTime;
IMPORT APN AS APN;

(*--------------------------------------------------------------------------*)
PROCEDURE New(p : APN.T; mt : Time.T) : T =
  BEGIN
    RETURN NEW(T).init(p, mt);
  END New;

(*--------------------------------------------------------------------------*)
PROCEDURE Init(self : T; p : APN.T; mt : Time.T) : T =
  BEGIN
    self.name := p;
    self.mtime := mt;
    RETURN self;
  END Init;

(*--------------------------------------------------------------------------*)
PROCEDURE ToText(self : T) : TEXT =
  BEGIN
    RETURN self.name.denotation() & "[" & FmtTime.Long(self.mtime) & "]";
  END ToText;

BEGIN
END FileStamp.
