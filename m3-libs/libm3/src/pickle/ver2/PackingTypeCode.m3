(* 
 * For more information on this program, contact Blair MacIntyre          
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 500 W 120th St, Room 450, New York, NY, 10027.                         
 *                                                                        
 * Copyright (C) Blair MacIntyre 1995, Columbia University 1995           
 * 
 *)

MODULE PackingTypeCode;

IMPORT Word;

PROCEDURE Equal(a, b: T): BOOLEAN =
  BEGIN
    RETURN a.from = b.from AND a.to = b.to AND a.tc = b.tc;
  END Equal;

PROCEDURE Hash(a: T): Word.T =
  BEGIN
    RETURN Word.Xor(Word.Xor(a.to, a.from), a.tc);
  END Hash;

PROCEDURE Compare(a, b: T): [-1..1] =
  BEGIN
    IF a.from > b.from THEN
      RETURN 1;
    ELSIF a.from < b.from THEN
      RETURN -1;
    ELSIF a.to > b.to THEN
      RETURN 1;
    ELSIF a.to < b.to THEN
      RETURN -1;
    ELSIF a.tc > b.tc THEN
      RETURN 1;
    ELSIF a.tc < b.tc THEN
      RETURN -1;
    END;
    RETURN 0;
  END Compare;

BEGIN
END PackingTypeCode.
