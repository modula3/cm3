(*
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 *
 * This file is released under the same conditions as Pickle.m3. See COPYRIGHT.
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
