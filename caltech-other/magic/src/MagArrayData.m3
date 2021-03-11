(* $Id$ *)
MODULE MagArrayData EXPORTS MagArrayData, MagArrayDataClass;
IMPORT MagRect, MagTransform, TransformList;
IMPORT TextReader;
IMPORT Scan;
IMPORT FloatMode, Lex;
IMPORT Fmt;
IMPORT MagArrayElemTransform AS ElemTransform;

REVEAL
  T = BRANDED Brand REF RECORD
    xlo, xhi, xsep, ylo, yhi, ysep : INTEGER;
  END;

(* step for arrays, -1 if indices running backwards *)
(* Thanks to Andrew Lines for finding this bug. *)
TYPE By = [ -1 .. 1 ];

PROCEDURE Xby(READONLY a : T) : By =
  BEGIN
    IF a.xlo > a.xhi THEN RETURN -1 ELSE RETURN 1 END
  END Xby;

PROCEDURE Yby(READONLY a : T) : By =
  BEGIN
    IF a.ylo > a.yhi THEN RETURN -1 ELSE RETURN 1 END
  END Yby;

PROCEDURE Parse(data : TEXT) : T RAISES { ParseError } =
  VAR
    res := NEW(T);
    reader := NEW(TextReader.T).init(data);
  BEGIN
    TRY
      res.xlo := Scan.Int(reader.nextE(" ", skipNulls := TRUE));
      res.xhi := Scan.Int(reader.nextE(" ", skipNulls := TRUE));
      res.xsep:= Scan.Int(reader.nextE(" ", skipNulls := TRUE));

      res.ylo := Scan.Int(reader.nextE(" ", skipNulls := TRUE));
      res.yhi := Scan.Int(reader.nextE(" ", skipNulls := TRUE));
      res.ysep:= Scan.Int(reader.nextE(" ", skipNulls := TRUE))
    EXCEPT
      TextReader.NoMore, FloatMode.Trap, Lex.Error => RAISE ParseError
    END;
    IF NOT reader.isEmpty() THEN RAISE ParseError END;
    RETURN res
  END Parse;

PROCEDURE ComputeArrayBBox(READONLY box : MagRect.T; 
                           READONLY transform : MagTransform.T;
                           READONLY arrayData : T) : MagRect.T =
  VAR
    res : MagRect.T ;
  BEGIN

    (* this routine should be better documented!!!! *)

    IF arrayData.xsep >= 0 THEN
      res.ll.x := box.ll.x;
      res.ur.x := box.ur.x + 
                      arrayData.xsep * Xby(arrayData) * (arrayData.xhi - arrayData.xlo);
    ELSE
      res.ll.x := box.ur.x;
      res.ur.x := box.ll.x + 
                      arrayData.xsep * Xby(arrayData) * (arrayData.xhi - arrayData.xlo);
    END;

    IF arrayData.ysep >= 0 THEN
      res.ll.y := box.ll.y;
      res.ur.y := box.ur.y + 
                      arrayData.ysep * Yby(arrayData) * (arrayData.yhi - arrayData.ylo);
    ELSE
      res.ll.y := box.ur.y;
      res.ur.y := box.ll.y + 
                      arrayData.ysep * Yby(arrayData) * (arrayData.yhi - arrayData.ylo);
    END;

    RETURN MagTransform.Rect(res, transform)
  END ComputeArrayBBox;

PROCEDURE FormatForMag(READONLY t : T) : TEXT =
  BEGIN
    RETURN "array " & 
           Fmt.Int(t.xlo) & " " &
           Fmt.Int(t.xhi) & " " &
           Fmt.Int(t.xsep) & " " &
           Fmt.Int(t.ylo) & " " &
           Fmt.Int(t.yhi) & " " &
           Fmt.Int(t.ysep) & " "
  END FormatForMag;

PROCEDURE Format(READONLY t : T) : TEXT = 
  VAR
    res : TEXT := "";
  BEGIN
    res := Fmt.F("x: %s, %s, sep = %s, by = %s; ", 
                 Fmt.Int(t.xlo), Fmt.Int(t.xhi), Fmt.Int(t.xsep), Fmt.Int(Xby(t))) &
           Fmt.F("y: %s, %s, sep = %s, by = %s", 
                 Fmt.Int(t.ylo), Fmt.Int(t.yhi), Fmt.Int(t.ysep), Fmt.Int(Yby(t)));
    RETURN res
  END Format;

(* need to carry thru array indices here... *)
PROCEDURE ToTransformList(READONLY transform : MagTransform.T;
                          READONLY a : T) : TransformList.T =
  VAR
    res : TransformList.T := NIL;
    xArray := NOT (a.xlo = a.xhi);
    yArray := NOT (a.ylo = a.yhi);
  BEGIN
    FOR i := a.xlo TO a.xhi BY Xby(a) DO
      FOR j := a.ylo TO a.yhi BY Yby(a) DO
        VAR 
          xoff := (i - a.xlo) * a.xsep * Xby(a);
          yoff := (j - a.ylo) * a.ysep * Yby(a);
          subXform := MagTransform.Unitary;
        BEGIN
          subXform.c := xoff;
          subXform.f := yoff;
          res := TransformList.Cons(
                     ElemTransform.T { MagTransform.Compose(transform, subXform), 
                                       xArray, yArray, i, j },
                                    res)
        END
      END
    END;
    RETURN res
  END ToTransformList;

PROCEDURE ToTransformIterator(READONLY transform : MagTransform.T;
                              READONLY a : T) : MagTransform.Iterator =
  BEGIN
    RETURN NEW(ArrayTransformIterator, 
                      superTransform := transform,
                      xby            := Xby(a),
                      yby            := Yby(a),
                      a              := a,
                      xArray         := NOT (a.xlo = a.xhi),
                      yArray         := NOT (a.ylo = a.yhi),
                      i              := a.xlo,
                      j              := a.ylo)
  END ToTransformIterator;

TYPE 
  ArrayTransformIterator = MagTransform.Iterator OBJECT
    superTransform : MagTransform.T;
    xby, yby : By;
    i, j : INTEGER;
    a : T;
    xArray, yArray : BOOLEAN;
  OVERRIDES 
    next := ATINext
  END;

PROCEDURE ATINext(self : ArrayTransformIterator; 
                  VAR result : MagTransform.T) : BOOLEAN =
  BEGIN
    IF self.j = self.a.yhi + self.yby THEN
      INC(self.i, self.xby);
      self.j := self.a.ylo
    END;
    IF self.i = self.a.xhi + self.xby THEN RETURN FALSE END;
    VAR 
      xoff := (self.i - self.a.xlo) * self.a.xsep * self.xby;
      yoff := (self.j - self.a.ylo) * self.a.ysep * self.yby;
      subXform := MagTransform.Unitary;
    BEGIN
      subXform.c := xoff;
      subXform.f := yoff;
      result :=  MagTransform.Compose(self.superTransform, subXform)
    END;
    INC(self.j, self.yby);
    RETURN TRUE
  END ATINext;

PROCEDURE FormatIndex(READONLY t : ElemTransform.T) : TEXT =
  VAR
    res := "[";
  BEGIN
    <* ASSERT t.xArray OR t.yArray *>  (* is this right? *)
    IF NOT t.xArray THEN 
      res := res & Fmt.Int(t.yIdx)
    ELSIF NOT t.yArray THEN
      res := res & Fmt.Int(t.xIdx)
    ELSE
      res := res & Fmt.Int(t.yIdx) & "," & Fmt.Int(t.xIdx)
    END;
      
    RETURN res & "]"
  END FormatIndex;

BEGIN END MagArrayData.


