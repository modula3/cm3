UNSAFE MODULE SimpleKeys;

IMPORT BTree,IO,Fmt,Random,Text;
IMPORT DataRecs;

TYPE
  DatRec = DataRecs.DatRec;

VAR
  rnd : Random.T;
  r : INTEGER;

  tins := ARRAY [1..126] OF TEXT {
"artichoke", "aubergine", "asparagus", "legumes", "alfalfa-sprouts",
"azuki-beans", "bean-sprouts", "black-beans", "black-eyed peas", "borlotti-bean",
"broad-beans", "chickpeas", "green-beans", "kidney-beans", "lentils",
"lima-beans", "butter-bean", "mung-beans", "navy-beans", "peanuts",
"pinto-beans", "runner-beans", "split-peas", "soy-beans", "peas",
"mangetout", "broccoflower", "broccoli", "brussels-sprouts", "cabbage",
"kohlrabi", "Savoy-cabbage", "red-cabbage", "cauliflower", "celery",
"endive", "fiddleheads", "frisee", "fennel", "greens", "bok-choy", "chard",
"collard-greens", "kale", "mustard-greens", "herbs", "anise", "basil",
"caraway", "coriander", "chamomile", "daikon", "dill", "fennel", "lavender",
"cymbopogon", "marjoram", "oregano", "parsley", "rosemary", "thyme", "lettuce",
"arugula", "mushrooms", "nettles", "okra", "onions", "chives", "garlic",
"leek", "onion", "shallot", "scallion", "peppers", "bell-pepper", "chili-pepper",
"jalapeño", "habanero", "paprika", "tabasco-pepper", "cayenne-pepper", "radicchio",
"rhubarb", "beetroot", "mangel-wurzel", "carrot", "celeriac", "corms",
"eddoe", "konjac", "taro", "water-chestnut", "ginger", "parsnip", "rutabaga",
"radish", "wasabi", "horseradish", "white radish", "tubers", "jicama",
"jerusalem artichoke", "potato", "sweet-potato", "yam", "turnip", "salsify",
"skirret", "sweetcorn", "topinambur", "squashes", "acorn-squash",
"bitter-melon", "butternut-squash", "banana-squash", "courgette", "cucumber",
"delicata", "gem-squash", "hubbard squash", "marrow", "spaghetti-squash",
"spinach", "tat-soi", "tomato", "watercress"};

  nf := ARRAY [1..5] OF TEXT { "XXXX", "YYYY", "ZZZ", "QQQ", "RRR"};

<*FATAL BTree.Error*>

PROCEDURE Key(key : TEXT) : BTree.KeyType =
  VAR
    k := BTree.KeyType{'\000',..};
  BEGIN
    Text.SetChars(k,key);
    RETURN k;
  END Key;

PROCEDURE Comp(a,b : BTree.KeyType) : BOOLEAN =
  VAR ret : [-1..1];
  BEGIN
    ret := Text.Compare(Text.FromChars(a), Text.FromChars(b));
    RETURN ret <= 0;
  END Comp;

PROCEDURE CompKeys() =
  VAR key : TEXT;
   r : INTEGER;
   c := 100;
  BEGIN
    IO.Put("Comp Keys\n");
    FOR i := FIRST(tins) TO LAST(tins) DO
      rnd := NEW(Random.Default).init(fixed := TRUE);
      FOR j := 1 TO c DO
        r := rnd.integer(1,c);
        key := Fmt.Pad(Fmt.Int(r),4);
        IF Comp(Key(tins[i]),Key(key)) THEN
          IO.Put(" key is true " & tins[i] & "\n");
        END;
      END;
    END;
  END CompKeys;

PROCEDURE InsertKeys() =
  VAR key : TEXT;
  BEGIN
    IO.Put("Insert keys\n");
    FOR i := FIRST(tins) TO LAST(tins) DO
      key := tins[i];
      EVAL fbr.insertKey(Key(key),0,1);
    END;
    fbr.print(1);
  END InsertKeys;

PROCEDURE SearchKeys() =
  VAR
    dr : INTEGER;
    key : TEXT;
    found : BOOLEAN;
  BEGIN
    (* test simple all keys found *)
    IO.Put("Search for all keys\n");
    FOR i := FIRST(tins) TO LAST(tins) DO
      key := tins[i];
      found := fbr.findKey(Key(key),1,dr);
      IF NOT found THEN
        IO.Put(key & " not found dr " & Fmt.Int(dr) & "\n");
      END;
    END;
  END SearchKeys;

PROCEDURE SearchOtherKeys() =
  VAR
    dr : INTEGER;
    key : TEXT;
    found : BOOLEAN;
  BEGIN
    (* test simple all keys found *)
    IO.Put("SearchOtherKeys for all keys\n");
    FOR i := FIRST(tins) TO LAST(tins) DO
      key := tins[i] & Fmt.Int(i);
      found := fbr.findKey(Key(key),2,dr);
      IF NOT found THEN
        IO.Put(key & " not found\n");
      END;
      key := tins[i] & Fmt.Int(i+10);
      found := fbr.findKey(Key(key),3,dr);
      IF NOT found THEN
        IO.Put(key & " not found\n");
      END;
    END;
  END SearchOtherKeys;

PROCEDURE DeleteKeys() =
  VAR key : TEXT;
  BEGIN
    IO.Put("Delete all keys\n");
    FOR i := FIRST(tins) TO LAST(tins) DO
      key := tins[i];
      EVAL fbr.deleteKey(Key(key),1);
    END;
    fbr.print(1);
  END DeleteKeys;

PROCEDURE InsertOtherKeys() =
  VAR key : TEXT;
  BEGIN
    IO.Put("Insert alternate keys\n");
    FOR i := FIRST(tins) TO LAST(tins) DO
      (* concat index for secondary keys *)
      key := tins[i] & Fmt.Int(i);
      EVAL fbr.insertKey(Key(key),0,2);
      key := tins[i] & Fmt.Int(i+10);
      EVAL fbr.insertKey(Key(key),0,3);
    END;
    fbr.print(1);
    fbr.print(2);
    fbr.print(3);
  END InsertOtherKeys;

PROCEDURE InsertKeysBackward() =
  VAR key : TEXT;
  BEGIN
    IO.Put("Insert keys backward\n");
    FOR i := LAST(tins) TO FIRST(tins) BY -1 DO
      key := tins[i];
      EVAL fbr.insertKey(Key(key),0,1);
    END;
    fbr.print(1);

    FOR i := LAST(tins) TO FIRST(tins) BY -1 DO
      key := tins[i];
      EVAL fbr.deleteKey(Key(key),1);
    END;
    fbr.print(1);
  END InsertKeysBackward;

PROCEDURE DeleteKeysBackward() =
  VAR key : TEXT;
  BEGIN
    IO.Put("Delete keys backward\n");
    FOR i := LAST(tins) TO FIRST(tins) BY -1 DO
      key := tins[i];
      EVAL fbr.deleteKey(Key(key),1);
    END;
    fbr.print(1);
  END DeleteKeysBackward;

PROCEDURE NotFoundKeys() =
  VAR
    dr : INTEGER;
    key : TEXT;
    found : BOOLEAN;
  BEGIN
    (* test not found keys *)
    IO.Put("Search for all not found keys\n");
    FOR i := FIRST(nf) TO LAST(nf) DO
      key := nf[i];
      found := fbr.findKey(Key(key),1,dr);
      IF found THEN
        IO.Put(key & " key was found when it should not\n");
      END;
    END;
  END NotFoundKeys;

PROCEDURE RandomInsertKeys() =
  VAR key : TEXT;
      c := 100;
      r : INTEGER;
  BEGIN
    IO.Put("Random insert keys\n");
    rnd := NEW(Random.Default).init(fixed := TRUE);
    FOR i := 1 TO c DO
      r := rnd.integer(1,c);
      key := Fmt.Pad(Fmt.Int(r),5);
      EVAL fbr.insertKey(Key(key),0,1);
      key := Fmt.Int(r+10);
      EVAL fbr.insertKey(Key(key),0,2);
      key := Fmt.Int(r+100);
      EVAL fbr.insertKey(Key(key),0,3);
    END;
    fbr.print(1);
    fbr.print(2);
    fbr.print(3);
  END RandomInsertKeys;

PROCEDURE RandomSearchKeys() =
  VAR 
    dr : INTEGER;
    c : INTEGER := 100;
    found : BOOLEAN;
    key : TEXT;
  BEGIN
    (* large key search most not found *)
    IO.Put("Random search keys\n");
    FOR i := 1 TO c DO
      r := rnd.integer(1,c);
      key := Fmt.Pad(Fmt.Int(r),4);
      found := fbr.findKey(Key(key),1,dr);
      IF NOT found THEN
        IO.Put("not found " & key & "\n");
      ELSE
        IO.Put("found key = " & key & "\n");
      END;
    END;
  END RandomSearchKeys;

PROCEDURE RandomDeleteKeys() =
  VAR key : TEXT;
      c := 100;
      r : INTEGER;
  BEGIN
    IO.Put("Random delete keys\n");
    rnd := NEW(Random.Default).init(fixed := TRUE);
    FOR i := 1 TO c DO
      r := rnd.integer(1,c);
      key := Fmt.Pad(Fmt.Int(r),4);
      EVAL fbr.deleteKey(Key(key),1);
    END;
    fbr.print(1);
  END RandomDeleteKeys;

PROCEDURE CheckFileTest() =
  VAR fname := "Temp";
  BEGIN
    IF fbr # NIL THEN fbr.close(); END;
    TRY
      fbr := NEW(BTree.T).open(fname);
    EXCEPT
    | BTree.Error => IO.Put("Ok file does not exist\n");
    END;
    fbr := NEW(BTree.T).create(fname,numKeys := 1,blockLen := BYTESIZE(DatRec));
    fbr.close();
    fbr := NEW(BTree.T).create(fname,numKeys := 2,blockLen := BYTESIZE(DatRec));
    fbr.close();
    TRY
      fbr := NEW(BTree.T).open(fname);
      fbr.close();
    EXCEPT
    | BTree.Error => IO.Put("Error file should exist\n");
    END;
  END CheckFileTest;

BEGIN
  CompKeys();
  rnd := NEW(Random.Default).init(fixed := TRUE);
END SimpleKeys.
