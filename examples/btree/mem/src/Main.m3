MODULE Main;

IMPORT BTree,IO,Fmt,Random;

VAR
  root,p: BTree.Page;
  r,k : INTEGER;
  found : BOOLEAN;
  rnd : Random.T;

  ins := ARRAY [1..25] OF INTEGER {
    20, 40, 10, 30, 15, 35, 7, 26, 18, 22, 5, 42,
    13, 46, 27, 8, 32, 38, 23, 45, 25, 48, 49, 50, 19};

BEGIN

  IO.Put("start");

  rnd := NEW(Random.Default).init(fixed := TRUE);
  root := NIL;

  (* insert static keys *)
  FOR i := FIRST(ins) TO LAST(ins) DO
    IO.Put("\n\nadd key " & Fmt.Int(ins[i]) & "\n\n");
    BTree.InsertKey(root,ins[i]);
    BTree.ShowTree(root,0);
    BTree.ValidTree(root);
  END;

  (* add dupl key which should fail *)
  IO.Put("insert duplicate key "); IO.PutInt(ins[9]); IO.Put("\n");
  BTree.InsertKey(root,ins[9]);
  BTree.ShowTree(root,0);

  (* search for all keys *)
  FOR i := FIRST(ins) TO LAST(ins) DO
    found := BTree.Search(root,ins[i],p,k);
    IF found THEN
      IO.Put("found key "); IO.PutInt(ins[i]);
      IO.Put(" page "); IO.PutInt(p.pageNum);
      IO.Put(" k "); IO.PutInt(k); IO.Put("\n");
    ELSE 
      IO.Put("key not found "); IO.PutInt(ins[i]); IO.Put(" ");
     END;
  END;

  (* seach for non existant key *)
  found := BTree.Search(root,999,p,k);
  IO.Put("key 999 ");
  IF found THEN IO.Put("found\n"); ELSE IO.Put("not found\n"); END;

  (* delete a non existant key *)
  IO.Put("delete non existant key "); IO.PutInt(999); IO.Put("\n");
  BTree.DeleteKey(root,999);

  (* delete all keys backwards *)
  FOR i := LAST(ins) TO FIRST(ins) BY -1 DO
    IO.Put("delete key " & Fmt.Int(ins[i]) & "\n");
    BTree.DeleteKey(root,ins[i]);
  END;

  IO.Put("show tree should be empty\n");
  BTree.ShowTree(root,0);

  (* add 500 random integers *)
  FOR i := 1 TO 500  DO
    r := rnd.integer(0,10000);
    IO.Put("\nadd key " & Fmt.Int(r) & "\n");
    BTree.InsertKey(root,r);
    BTree.ValidTree(root);
  END;
  BTree.ShowTree(root,0);

END Main.
