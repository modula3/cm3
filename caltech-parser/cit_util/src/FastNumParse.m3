MODULE FastNumParse;
IMPORT Debug;
IMPORT Fmt;
IMPORT Math;

PROCEDURE Int     (READONLY buff : ARRAY OF CHAR; 
                   VAR      p    : CARDINAL; 
                   VAR      z    : INTEGER) : BOOLEAN =
  VAR
    neg := FALSE;
    s := p;
    success := FALSE;
  BEGIN
    IF p = NUMBER(buff) THEN RETURN FALSE END;
    IF NOT buff[p] IN SET OF CHAR { '-', '0' .. '9' } THEN RETURN FALSE END;
    IF buff[p] = '-' THEN neg := TRUE; INC(p) END;
    IF p = NUMBER(buff) THEN p := s; RETURN FALSE END;
    WHILE p < NUMBER(buff) AND buff[p] IN SET OF CHAR { '0' .. '9' } DO 
      IF success = FALSE THEN z := 0 END;
      success := TRUE;
      z := z * 10 + ORD(buff[p]) - ORD('0');
      INC(p)
    END;
    (* p = NUMBER(buff) OR NOT buff[p] IN SET OF CHAR ... *)
    IF neg THEN z := -z END;
    RETURN  success
  END Int;


PROCEDURE LongReal(READONLY buff : ARRAY OF CHAR; 
                   VAR      p    : CARDINAL; 
                   VAR      z    : LONGREAL) : BOOLEAN =
  VAR  
    mi := 0;
  BEGIN
    IF NOT Int(buff,p,mi) THEN RETURN FALSE END;

    (* read integer part of mantissa into mi 
       from here on down we have read SOME number *)

    z := FLOAT(mi, LONGREAL); 
    IF p = NUMBER(buff) THEN RETURN TRUE END;

    IF buff[p] =       '.'      THEN
      (* read <int>(.) *)
      INC(p);
      VAR
        mf := 0.0d0;
        fl := 0;
      BEGIN
        WHILE p < NUMBER(buff)                      AND 
              buff[p] IN SET OF CHAR { '0' .. '9' } AND 
              fl < 20 DO 
          mf := mf * 10.0d0 + FLOAT(ORD(buff[p]) - ORD('0'), LONGREAL);
          INC(p);
          INC(fl)
        END;
        (* read <int>.<max 20 digits> *)
        WHILE p < NUMBER(buff)                      AND 
              buff[p] IN SET OF CHAR { '0' .. '9' }     DO
          INC(p)
        END;
        (* read <int>.<digits> *)
        mf := mf * Exp[-fl];
        z := z + mf
      END
    END;

    IF p = NUMBER(buff) THEN RETURN TRUE END;

    IF buff[p] IN SET OF CHAR { 'e', 'E' } THEN
      (* read <mixed-decimal>([eE]) *)
      VAR s := p;
          exp := 0;
      BEGIN
        INC(p);
        IF p = NUMBER(buff) OR NOT Int(buff, p, exp) THEN 
          p := s;
          RETURN TRUE
        END;
        (* read <mixed-decimal>[eE]<int> )*)
        z := z * Exp[exp];
        RETURN TRUE
      END
    ELSE
      (* read <mixed-decimal> *)
      RETURN TRUE
    END
  END LongReal;

CONST
  MaxExp = 400;
VAR
  Exp : ARRAY [-MaxExp..MaxExp] OF LONGREAL;
BEGIN 
  FOR i := FIRST(Exp) TO LAST(Exp) DO
    Exp[i] := Math.pow(10.0d0,FLOAT(i,LONGREAL))
  END;
END FastNumParse.
