(* $Id$ *)

MODULE MagRect;
IMPORT MagPoint;
IMPORT Word;
IMPORT Lex, FloatMode;
IMPORT TextReader;
IMPORT Scan;
IMPORT Fmt;
IMPORT Wr, Thread;
IMPORT MagDir;
FROM Fmt IMPORT Int;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN 
    RETURN MagPoint.Equal(a.ll,b.ll) AND MagPoint.Equal(a.ur,b.ur) 
  END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN RETURN Word.Times(MagPoint.Hash(a.ll),MagPoint.Hash(a.ur)) END Hash;

PROCEDURE Union(READONLY a, b : T) : T =
  BEGIN RETURN T { ll := MagPoint.Min(a.ll,b.ll), 
                   ur := MagPoint.Max(a.ur,b.ur) } END Union;

PROCEDURE IsProper(a : T) : BOOLEAN =
  BEGIN 
    (* a MagRect is proper if ll and ur are properly ordered OR
       ll and ur are the same (zero-size rect) *)
    RETURN MagPoint.Order(a.ll,a.ur) = -1 OR MagPoint.Equal(a.ll,a.ur) 
  END IsProper;

PROCEDURE Overlap(READONLY a, b : T) : BOOLEAN =
  VAR dummy : T; BEGIN RETURN Intersection(a,b,dummy) END Overlap;

PROCEDURE Intersection(READONLY a, b : T; VAR intersection : T) : BOOLEAN =
  VAR res := T { ll := MagPoint.Max(a.ll,b.ll), 
                    ur := MagPoint.Min(a.ur,b.ur) }; BEGIN 
    IF IsProper(res) THEN intersection := res; RETURN TRUE
    ELSE RETURN FALSE
    END
  END Intersection;

PROCEDURE IntersectionOrAbort(READONLY a, b : T) : T =
  VAR
    c :T;
    doIntersect := Intersection(a,b,c);
  BEGIN
    <* ASSERT doIntersect *>
    RETURN c
  END IntersectionOrAbort;

PROCEDURE ParseFromReader(rd : TextReader.T; VAR res : T) RAISES { ParseError } =
  BEGIN
    TRY
      res.ll.x := Scan.Int(rd.nextE(" "));
      res.ll.y := Scan.Int(rd.nextE(" "));
      res.ur.x := Scan.Int(rd.nextE(" "));
      res.ur.y := Scan.Int(rd.nextE(" "));
    EXCEPT
      Lex.Error, FloatMode.Trap, TextReader.NoMore => RAISE ParseError
    END
  END ParseFromReader;

PROCEDURE Parse(coords : TEXT) : T RAISES { ParseError } =
  VAR
    rd := NEW(TextReader.T).init(coords);
    res : T;
  BEGIN
    ParseFromReader(rd, res);
    IF NOT rd.isEmpty() THEN RAISE ParseError END;
    RETURN res
  END Parse;

PROCEDURE Shift(READONLY a : T; by : MagPoint.T) : T =
  BEGIN
    RETURN T { ll := MagPoint.T { a.ll.x - by.x, a.ll.y - by.y },
               ur := MagPoint.T { a.ur.x - by.x, a.ur.y - by.y } }
  END Shift;

PROCEDURE FromPoints(READONLY a, b : MagPoint.T) : T = 
  BEGIN
    RETURN T { MagPoint.T { MIN(a.x, b.x), MIN(a.y, b.y) },
               MagPoint.T { MAX(a.x, b.x), MAX(a.y, b.y) } }
  END FromPoints;

PROCEDURE Format(READONLY a : T) : TEXT =
  BEGIN 
    RETURN "{ " & MagPoint.Format(a.ll) & " , " & MagPoint.Format(a.ur) & " }" 
  END Format;

PROCEDURE FormatForMagic(READONLY a : T) : TEXT =
  BEGIN
    RETURN Fmt.F("%s %s %s %s", 
             Int(a.ll.x), Int(a.ll.y), Int(a.ur.x), Int(a.ur.y))
  END FormatForMagic;

PROCEDURE WriteToWrForMagic(wr : Wr.T; a : T) RAISES { Wr.Failure, 
                                                       Thread.Alerted } =
  BEGIN
    Wr.PutText(wr, Fmt.F("%s %s %s %s", 
             Int(a.ll.x), Int(a.ll.y), Int(a.ur.x), Int(a.ur.y)))
  END WriteToWrForMagic;

PROCEDURE Distance(READONLY a, b : T) : CARDINAL =
  BEGIN
    (* if they overlap, then the distance is zero *)
    IF Overlap(a, b) THEN RETURN 0 END;

    (* if they overlap in x, then the distance is the distance in y *)
    IF a.ll.x <= b.ur.x AND b.ll.x <= a.ur.x THEN
      RETURN MIN(ABS(a.ll.y - b.ur.y), ABS(b.ll.y - a.ur.y))
    END;

    (* if they overlap in y, then the distance is the distance in x *)
    IF a.ll.y <= b.ur.y AND b.ll.y <= a.ur.y THEN
      RETURN MIN(ABS(a.ll.x - b.ur.x), ABS(b.ll.x - a.ur.x))
    END;

    (* else, it is the minimum corner-to-corner distance *)
    (* I think this is right... *)
    RETURN MIN(ABS(a.ll.y - b.ur.y), ABS(b.ll.y - a.ur.y)) +
           MIN(ABS(a.ll.x - b.ur.x), ABS(b.ll.x - a.ur.x))

  END Distance;

PROCEDURE Clip(READONLY a, to : T; VAR res : T) : BOOLEAN =
  BEGIN
    <* ASSERT IsProper(to) *>
    IF NOT Overlap(a,to) THEN RETURN FALSE END;
    res := a;
    IF a.ll.x < to.ll.x THEN res.ll.x := to.ll.x END;
    IF a.ll.y < to.ll.y THEN res.ll.y := to.ll.y END;

    IF a.ur.x > to.ur.x THEN res.ur.x := to.ur.x END;
    IF a.ur.y > to.ur.y THEN res.ur.y := to.ur.y END;
    <* ASSERT IsProper(res) *>
    RETURN TRUE
  END Clip;

PROCEDURE Bloat(READONLY a : T; dir : MagDir.T; by : CARDINAL) : T =
  VAR
    ll := a.ll;
    ur := a.ur;
    step := MagPoint.DirDelta[dir];
  BEGIN
    IF step.x + step.y > 0 THEN
      ur.x := ur.x + step.x * by;
      ur.y := ur.y + step.y * by
    ELSE
      ll.x := ll.x + step.x * by;
      ll.y := ll.y + step.y * by
    END;
    RETURN T { ll, ur }
  END Bloat;

PROCEDURE Bloats(READONLY a : T; dirs : SET OF MagDir.T; by : CARDINAL) : T =
  VAR
    res := a;
  BEGIN
    FOR d := FIRST(MagDir.T) TO LAST(MagDir.T) DO
      IF d IN dirs THEN res := Bloat(res, d, by) END
    END;
    RETURN res
  END Bloats;

BEGIN END MagRect.












