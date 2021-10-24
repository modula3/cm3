MODULE DataRecs;

IMPORT BTree, IO, Fmt, Random, Text, Date,Time;

VAR
  rnd: Random.T;

  DescArr := ARRAY [0..4] OF TEXT {"Flour","Sauce", "Jam", "Noodles", "Rice"};
  SupplyArr := ARRAY [0..5] OF TEXT {"Acme","Browns", "Juice Company", "BHP", "Shiptons", "Smiths"};
  StateArr := ARRAY [0..6] OF TEXT {"NY","C", "AR", "WA", "NM", "FL", "NCA"};

<* FATAL BTree.Error *>

PROCEDURE Key (key: TEXT): BTree.KeyType =
  VAR k := BTree.KeyType{'\000', ..};
  BEGIN
    Text.SetChars(k, key);
    RETURN k;
  END Key;

PROCEDURE AddRecTest (primaryKey,secondaryKey: TEXT) =
  VAR
    dr   : RefDat;
    ofs  : INTEGER;
    found: BOOLEAN;
  BEGIN
    dr := NEW(RefDat);

    Text.SetChars(dr.itemId, primaryKey); 
    Text.SetChars(dr.desc, DescArr[rnd.integer(0,4)]); 
    Text.SetChars(dr.barcode, secondaryKey);
    Text.SetChars(dr.supplier, SupplyArr[rnd.integer(0,5)]);
    dr.manufDate := Time.Now();
    dr.usebyDate := Time.Now();
    dr.weight := 34.56E2;
    dr.plantId := 458;

    found := fbr.findKey(Key(primaryKey), 1, ofs);
    IF found THEN
      IO.Put(" \nAddRec - key exists not adding \n");
      RETURN;
    END;

    (* add the record first and obtain the offset *)
    ofs := fbr.addRec(dr);

    (* add both keys for the record *)
    EVAL fbr.insertKey(Key(primaryKey), ofs, 1);
    EVAL fbr.insertKey(Key(secondaryKey), ofs, 2);
    fbr.print(1);
  END AddRecTest;

PROCEDURE DelRecTest (key: TEXT) =
  VAR
    ofs  : INTEGER;
    found: BOOLEAN;
    skey : BTree.KeyType;
    dr   : RefDat;
  BEGIN
    IO.Put("Del Rec - key " & key & "\n ");

    found := fbr.findKey(Key(key), 1, ofs);
    IF NOT found THEN
      IO.Put(" \nDelRec - key does not exist : not deleting \n");
      RETURN;
    END;

    dr := NEW(RefDat);
    fbr.getRec(ofs,dr);
    (* get secondary key so we can delete it *)
    skey := ToKey(dr.barcode);
    (* delete the data record *)
    fbr.delRec(ofs);
    (* delete both keys *)
    EVAL fbr.deleteKey(Key(key), 1);
    EVAL fbr.deleteKey(skey, 2);

    fbr.print(1);

  END DelRecTest;

PROCEDURE FindRecTest(key : TEXT; keyNum : INTEGER) =
VAR
  dr : RefDat;
  ofs : INTEGER;
  found : BOOLEAN;
BEGIN
  (* search for the rec give the key *)
  found := fbr.findKey(Key(key),keyNum,ofs);
  IF found THEN
    IO.Put("Found KEY data offset = ");
    IO.PutInt(ofs);
    IO.Put("\n");

    dr := NEW(RefDat);
    (* get the record *)
    fbr.getRec(ofs,dr);
    (* use it *)
  ELSE
    IO.Put("key not found\n");
  END;
END FindRecTest;

PROCEDURE UpdateRecTest (key: TEXT; keyNum: INTEGER) =
  VAR
    dr   : RefDat;
    ofs  : INTEGER;
    found: BOOLEAN;
  BEGIN
    (* now search for the rec give the key *)
    found := fbr.findKey(Key(key), keyNum, ofs);
    IF found THEN
      IO.Put("Found KEY" & key & " data offset = " & Fmt.Int(ofs) & "\n");

      dr := NEW(RefDat);
      fbr.getRec(ofs, dr);
      IO.Put("item number " & Text.FromChars(dr.itemId) & "\n");
      IO.Put("desc " & Text.FromChars(dr.desc) & "\n");
      (* update a field *)
      dr.usebyDate := Time.Now();
      (* save the record *)
      fbr.putRec(ofs, dr);
    ELSE
      IO.Put("key " & key & " not found\n");
    END;
  END UpdateRecTest;

PROCEDURE RandChar(lo,hi : INTEGER) : CHAR =
  VAR ch : CHAR;
  BEGIN
    ch := VAL(rnd.integer(lo,hi),CHAR);
    RETURN ch;
  END RandChar;

PROCEDURE RandIt(VAR val : ARRAY OF CHAR; lo,hi : INTEGER) =
  BEGIN
    FOR i := FIRST(val) TO LAST(val) DO
      val[i] := RandChar(lo,hi);
    END;
  END RandIt;

PROCEDURE RandomRec() : RefDat =
  VAR
    dr : RefDat;
    t : Time.T;
    d,d1 : Date.T;
    x : INTEGER;
  BEGIN
    dr := NEW(RefDat);
    RandIt(dr.itemId,48,57); 
    Text.SetChars(dr.desc, DescArr[rnd.integer(0,4)]); 
    RandIt(dr.barcode,48,57); 
    Text.SetChars(dr.supplier, SupplyArr[rnd.integer(0,5)]);
    Text.SetChars(dr.plantState, StateArr[rnd.integer(0,6)]);
    dr.plantId := rnd.integer(500,1000);
    dr.weight := rnd.real(0.03,10.0);
    RandIt(dr.recycleCat,48,57); 
    t := Time.Now();
    d := Date.FromTime(t);
    d1 := d;
    x := rnd.integer(1,25);
    DEC(d.year,x);
    t := Date.ToTime(d);
    dr.manufDate := t;
    d := d1;
    x := rnd.integer(1,5);
    INC(d.year,x);
    dr.usebyDate := t;
    RETURN dr;
  END RandomRec;

PROCEDURE SimpleRecs () =
VAR
    iter : BTree.Iterator;
    dr : RefDat;
  BEGIN
    AddRecTest("sam", "198726");
    AddRecTest("jim", "2232875");
    AddRecTest("ralph", "33443728");
    AddRecTest("emma", "9980556");
    AddRecTest("sally", "33240098");
    AddRecTest("june", "55361039");
    AddRecTest("moira", "55289443");
    AddRecTest("beth", "88736236");
    UpdateRecTest("sally", 1);
    UpdateRecTest("sam", 1);
    DelRecTest("june" );
    DelRecTest("jim" );
    DelRecTest("beth");
    DelRecTest("sam");
    (* not in tree *)
    DelRecTest("william");

    AddRecTest("alan", "66777665");
    AddRecTest("geoff", "221129908");
    AddRecTest("ashley", "55456266");
    dr := NEW(RefDat);
    iter := fbr.iterate();
    WHILE iter.next(dr) DO
     IO.Put("iter " & Text.FromChars(dr.itemId) & "\n");
    END; 
    fbr.print(1);
  END SimpleRecs;

PROCEDURE ToKey(READONLY val : ARRAY OF CHAR) : BTree.KeyType =
  VAR k := BTree.KeyType{'\000', ..};
  BEGIN
    FOR i := FIRST(val) TO LAST(val) DO
      k[i] := val[i];
    END;
    RETURN k;
  END ToKey;

PROCEDURE RandomRecs () =
  VAR
    dr : RefDat;
    found : BOOLEAN;
    key : IdType;
    ofs : INTEGER;
    pkey,skey : BTree.KeyType;
    iter : BTree.Iterator;
  BEGIN
    FOR j := 0 TO 10000 DO
      dr := RandomRec();
      pkey := ToKey(dr.itemId);
      skey := ToKey(dr.barcode);

      found := fbr.findKey(pkey,1,ofs);
      IF NOT found THEN
        ofs := fbr.addRec(dr);
        EVAL fbr.insertKey(pkey, ofs, 1);
        EVAL fbr.insertKey(skey, ofs, 2);
      END;
    END;

    rnd := NEW(Random.Default).init(fixed := TRUE);
    FOR j := 0 TO 1000  BY 2 DO
      RandIt(key,48,57); 
      pkey := ToKey(key);
      found := fbr.findKey(pkey,1,ofs);
      IF found THEN
        dr := NEW(RefDat);
        fbr.getRec(ofs, dr);
        skey := ToKey(dr.barcode);
        fbr.delRec(ofs);
        EVAL fbr.deleteKey(pkey, 1);
        EVAL fbr.deleteKey(skey, 2);
      ELSE
        IO.Put(Text.FromChars(dr.itemId) & " DELETED already\n");
      END;
    END;

    iter := fbr.iterate();
    WHILE iter.next(dr) DO
     IO.Put("iter " & Text.FromChars(dr.itemId) & "\n");
    END; 
  END RandomRecs;

BEGIN
  rnd := NEW(Random.Default).init(fixed := TRUE);
END DataRecs.
