MODULE Main;

IMPORT IO, Fmt, RT0, RTType;

TYPE
  BRef   = BRANDED "p300 ref" REF INTEGER;
  BObj   = BRANDED "p300 object" OBJECT x: INTEGER END;
  BArr   = BRANDED "p300 array" REF ARRAY OF CHAR;
  Plain  = REF INTEGER;
  PObj   = OBJECT y: INTEGER END;

PROCEDURE PutBrand (tag: TEXT;  tc: CARDINAL) =
  VAR def := RTType.Get (tc);  b: RT0.BrandPtr;  txt := "";
  BEGIN
    b := def.brand_ptr;
    IF b = NIL THEN
      IO.Put (tag & ": no brand\n");
    ELSE
      FOR i := 0 TO b.length - 1 DO txt := txt & Fmt.Char (b.chars[i]) END;
      IO.Put (tag & ": \"" & txt & "\" (" & Fmt.Int (b.length) & ")\n");
    END;
  END PutBrand;

VAR
  r := NEW (BRef);
  o := NEW (BObj);
  a := NEW (BArr, 3);
  p := NEW (Plain);
  q := NEW (PObj);

BEGIN
  PutBrand ("BRef", TYPECODE (r));
  PutBrand ("BObj", TYPECODE (o));
  PutBrand ("BArr", TYPECODE (a));
  PutBrand ("Plain", TYPECODE (p));
  PutBrand ("PObj", TYPECODE (q));
END Main.
