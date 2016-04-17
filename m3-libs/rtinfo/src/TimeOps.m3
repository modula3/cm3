MODULE TimeOps;

IMPORT
  Fmt,
  Text;

IMPORT
  TextOps;

PROCEDURE ToSeconds(t: T): INTEGER =
  BEGIN
    IF t.plus THEN
      RETURN +(t.hour*3600+t.minute*60+t.second);
    ELSE
      RETURN -(t.hour*3600+t.minute*60+t.second);
    END;
  END ToSeconds;

PROCEDURE FromSeconds(s: INTEGER): T =
  VAR
    t: T := NEW(T);
  BEGIN
    IF s < 0 THEN
      t.plus:=FALSE;
      s:=ABS(s);
    END;
    
    t.second:=s MOD 60;
    t.minute:=(s DIV 60) MOD 60;
    t.hour:=s DIV 3600;

    RETURN t;
  END FromSeconds;

PROCEDURE Add(t1, t2: T): T =
  BEGIN
    RETURN FromSeconds(ToSeconds(t1) + ToSeconds(t2));
  END Add;

PROCEDURE Sub(t1, t2: T): T =
  BEGIN
    RETURN FromSeconds(ToSeconds(t1) - ToSeconds(t2));
  END Sub;

PROCEDURE ToText(t: T): TEXT =
  BEGIN
    IF t.plus THEN
      RETURN Fmt.F("%s:%02s:%02s", Fmt.Int(t.hour), Fmt.Int(t.minute), Fmt.Int(t.second));
    ELSE
      RETURN Fmt.F("- %s:%02s:%02s", Fmt.Int(t.hour), Fmt.Int(t.minute), Fmt.Int(t.second));
    END;
  END ToText;

PROCEDURE FromText(text: TEXT): T =
  VAR
    t: T := NEW(T);
  BEGIN
    text:=TextOps.RemoveSpaces(text);
    IF Text.GetChar(text, 0) = '-' THEN
      t.plus:=FALSE;
      text:=TextOps.RemoveSpaces(Text.Sub(text, 1));
    END;

    t.hour:=TextOps.ToInt(TextOps.ItemS(text, ":", 0));
    t.minute:=TextOps.ToInt(TextOps.ItemS(text, ":", 1));
    t.second:=TextOps.ToInt(TextOps.ItemS(text, ":", 2));

    RETURN FromSeconds(ToSeconds(t));
  END FromText;

BEGIN
END TimeOps.
