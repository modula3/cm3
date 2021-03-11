(* $Id$ *)
MODULE MagTransform;
IMPORT Scan;
IMPORT Lex, FloatMode, TextReader;
IMPORT MagPoint, MagRect;
IMPORT Debug;
FROM Fmt IMPORT Int, F;

PROCEDURE Parse(from : TEXT) : T RAISES { ParseError } =
  VAR 
    rd := NEW(TextReader.T).init(from); 
    res : T; 
  BEGIN
    TRY
      res.a := Scan.Int(rd.nextE(" "));
      res.b := Scan.Int(rd.nextE(" "));
      res.c := Scan.Int(rd.nextE(" "));
      res.d := Scan.Int(rd.nextE(" "));
      res.e := Scan.Int(rd.nextE(" "));
      res.f := Scan.Int(rd.nextE(" "))
    EXCEPT
      Lex.Error, FloatMode.Trap, TextReader.NoMore => RAISE ParseError
    END;
    IF NOT rd.isEmpty() THEN RAISE ParseError END;
    RETURN res
  END Parse;

PROCEDURE Point(READONLY a : MagPoint.T; READONLY by : T) : MagPoint.T =
  BEGIN
    CheckDet(by);
    RETURN MagPoint.T { by.a * a.x + by.b * a.y + by.c,
                        by.d * a.x + by.e * a.y + by.f }
  END Point;

PROCEDURE Rect(READONLY a : MagRect.T; READONLY by : T) : MagRect.T =
  BEGIN
    RETURN MagRect.FromPoints(Point(a.ll, by), Point(a.ur, by))
  END Rect;

PROCEDURE Compose(READONLY sup, sub : T) : T =
  VAR
    res : T;
  BEGIN
    CheckDet(sup); CheckDet(sub);
    
    res.a := sub.a * sup.a + sub.d * sup.b;
    res.b := sub.b * sup.a + sub.e * sup.b;
    res.c := sub.c * sup.a + sub.f * sup.b + sup.c;

    res.d := sub.a * sup.d + sub.d * sup.e;
    res.e := sub.b * sup.d + sub.e * sup.e;
    res.f := sub.c * sup.d + sub.f * sup.e + sup.f;

    CheckDet(res);

    RETURN res
  END Compose;

(* compute determinant, should always be 1 or -1 *)
PROCEDURE Det(READONLY a : T) : INTEGER =
  BEGIN RETURN a.a * a.e - a.b * a.d END Det;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE CheckDet(READONLY a : T) =
  BEGIN 
    IF Det(a) # 1 AND Det(a) # -1 THEN
      Debug.Error("Non-unit determinant of Magic transform. (T="&Format(a)&")")
    END
  END CheckDet;

PROCEDURE Format(READONLY a : T) : TEXT =
  BEGIN
    RETURN F("%s %s %s %s %s ", 
             Int(a.a), Int(a.b), Int(a.c), Int(a.d), Int(a.e)) & Int(a.f)
  END Format;    

PROCEDURE Inverse(READONLY a : T) : T =
  VAR
    res : T;
    det := a.a * a.e - a.b * a.d;
    invShift := Unitary;
  BEGIN
    invShift.c := -a.c;
    invShift.f := -a.f;
    <* ASSERT ABS(det) = 1 *>
    res.a := a.e*det;
    res.b := -a.b*det;
    res.d := -a.d*det;
    res.e := a.a*det;

    res := Compose(res,invShift);

    <* ASSERT Compose(res,a) = Compose(a,res) *>
    <* ASSERT Compose(res,a) = Unitary *>

    RETURN res
  END Inverse;

BEGIN END MagTransform.
