(* Copyright (C) 1994 Digital Equipment Corporation.           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Aug 23 09:36:08 PDT 1994 by kalsow     *)

MODULE Test2 EXPORTS Main;

IMPORT Random, SortedIntIntTbl, Params, Scan;
<*FATAL ANY*>

CONST
  LoadFactor = 0.3;

VAR
  size     := Scan.Int (Params.Get (1));
  nTests   := Scan.Int (Params.Get (2));
  tbl      := NEW (SortedIntIntTbl.Default).init ();
  array    := NEW (REF ARRAY OF INTEGER, size);
  rnd      := NEW (Random.Default).init ();
  array_sz := 0;
  next_val := 1;
  
PROCEDURE DoIt () =
  VAR test, key, val, start: INTEGER;  iter: SortedIntIntTbl.Iterator;
  BEGIN
    (* preload the table *)
    FOR i := 0 TO ROUND (LoadFactor * FLOAT (size))-1 DO
      key := NewKey ();
      val := next_val;  INC (next_val);
      EVAL tbl.put (key, val);
      array [key] := val;
      INC (array_sz);
    END;
    <*ASSERT tbl.size() = array_sz *>

    (* run the tests *)
    FOR i := 0 TO nTests-1 DO
      test := rnd.integer (0, 99);
      CASE test OF

      | 00..09 => (* put existing *)
          IF (array_sz > 0) THEN
            key := ExistingKey ();
            val := next_val;  INC (next_val);
            <*ASSERT tbl.put (key, val)*>
            array[key] := val;
          END;

      | 10..29 => (* get existing *)
          IF (array_sz > 0) THEN
            key := ExistingKey ();
            <*ASSERT tbl.get (key, val)*>
            <*ASSERT array [key] = val*>
          END;

      | 30..49 => (* delete existing *)
          IF (array_sz > 0) THEN
            key := ExistingKey ();
            <*ASSERT tbl.delete (key, val)*>
            <*ASSERT array [key] = val*>
            array[key] := 0;
            DEC (array_sz);
            <*ASSERT tbl.size() = array_sz *>
          END;

      | 50..69 => (* put new *)
          IF (array_sz < size) THEN
            key := NewKey ();
            val := next_val;  INC (next_val);
            <*ASSERT NOT tbl.put (key, val)*>
            array[key] := val;
            INC (array_sz);
            <*ASSERT tbl.size() = array_sz *>
          END;
          
      | 70..79 => (* get missing *)
          IF (array_sz < size) THEN
            key := NewKey ();
            <*ASSERT NOT tbl.get (key, val)*>
          END;

      | 80..89 => (* delete missing *)
          IF (array_sz < size) THEN
            key := NewKey ();
            <*ASSERT NOT tbl.delete (key, val)*>
          END;

      | 90..94 => (* iterate up *)
          start := rnd.integer (0, size-1);
          iter  := tbl.iterateOrdered (up := TRUE);
          iter.seek (start);
          FOR i := start TO size-1 DO
            IF array [i] # 0 THEN
              <* ASSERT iter.next (key, val) *>
              <* ASSERT i = key *>
              <* ASSERT array [i] = val *>
            END;
          END;
          <*ASSERT NOT iter.next (key, val)*>

      | 95..99 => (* iterate down *)
          start := rnd.integer (0, size-1);
          iter  := tbl.iterateOrdered (up := FALSE);
          iter.seek (start);
          FOR i := start TO 0 BY -1 DO
            IF array [i] # 0 THEN
              <* ASSERT iter.next (key, val) *>
              <* ASSERT i = key *>
              <* ASSERT array [i] = val *>
            END;
          END;
          <*ASSERT NOT iter.next (key, val)*>

      ELSE <*ASSERT FALSE*>
      END;
    END;
  END DoIt;

PROCEDURE NewKey (): INTEGER =
  VAR key := rnd.integer (0, size-1);
  BEGIN
    WHILE (array [key] # 0) DO
      INC (key);
      IF (key >= size) THEN key := 0; END;
    END;
    RETURN key;
  END NewKey;

PROCEDURE ExistingKey (): INTEGER =
  VAR key := rnd.integer (0, size-1);
  BEGIN
    WHILE (array [key] = 0) DO
      INC (key);
      IF (key >= size) THEN key := 0; END;
    END;
    RETURN key;
  END ExistingKey;

BEGIN
  DoIt ();
END Test2.
