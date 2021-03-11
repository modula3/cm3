(* $Id$ *)

MODULE ToRefany;
IMPORT ToRefanyClass;
IMPORT ToRefanyTbl;
IMPORT Word;
IMPORT Fmt, Debug, RTType;
IMPORT RT0;

VAR 
  tbl := NEW(ToRefanyTbl.Default).init();

PROCEDURE AddType(type : ToRefanyClass.T) = 
  VAR
    x : BOOLEAN;
  BEGIN
    x := tbl.put(type.typecode, type);
    <* ASSERT NOT x *>
  END AddType;

PROCEDURE Hash(a : T) : Word.T =
  BEGIN RETURN FindTypeCode(TYPECODE(a)).hash(a) END Hash;

PROCEDURE Equal(a, b : T) : BOOLEAN  =
  VAR
    lcm : RT0.Typecode;
  BEGIN 
    IF NOT FindLCM(TYPECODE(a),TYPECODE(b),lcm) THEN RETURN FALSE END;

    RETURN FindTypeCode(lcm).equal(a,b) 
  END Equal;

PROCEDURE FindTypeCode(typecode : RT0.Typecode) : ToRefanyClass.T =
  VAR
    type : ToRefanyClass.T;
  BEGIN
    WHILE NOT tbl.get(typecode,type) DO
      typecode := RTType.Supertype(typecode);
      IF typecode = RTType.NoSuchType THEN
        Debug.Error("No ToRefany type defined for TC=" & Fmt.Int(typecode));
        <* ASSERT FALSE *>
      END
    END;
    RETURN type
  END FindTypeCode;

PROCEDURE FindLCM(aArg, b : RT0.Typecode; 
                     VAR lcm : RT0.Typecode) : BOOLEAN =
  VAR
    a := aArg;
  BEGIN
    WHILE a # RTType.NoSuchType DO
      <* ASSERT RTType.IsSubtype(aArg,a) *>
      IF RTType.IsSubtype(b,a) THEN lcm := a; RETURN TRUE END;
      a := RTType.Supertype(a)
    END;
    RETURN FALSE
  END FindLCM;

BEGIN END ToRefany.
