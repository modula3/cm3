MODULE Main;

IMPORT Json,IO,Scan,Text;
(* IMPORT Rd,FS;*)

<*FATAL ANY *>

PROCEDURE IterAll(node : Json.T) = 
  VAR
    iter : Json.Iterator;
    n2 : Json.T;
    name : TEXT;
  BEGIN
    iter := node.iterate();
    WHILE iter.next(name,n2) DO
      IO.Put(name & " kind " & Json.NK[n2.kind()] & "\n");
      IF n2.kind() = Json.NodeKind.nkObject OR 
         n2.kind() = Json.NodeKind.nkArray THEN
        IterAll(n2);
      END;
    END;
  END IterAll;

PROCEDURE FindText(jp : Json.T; txt : TEXT) : Json.T =
  VAR node : Json.T;
  BEGIN
    node := jp.find(txt);
    IF node = NIL THEN
      IO.Put("Not found - " & txt & "\n");
    ELSE
      IO.Put("Found - name- " & node.name() & " kind- " & Json.NK[node.kind()] & " value- " & node.value() & "\n");
      (*
      CASE node.kind() OF
      | Json.NodeKind.nkText => IO.Put("text");
      | Json.NodeKind.nkInt => IO.Put("int");
      | Json.NodeKind.nkBool => IO.Put("float");
      | Json.NodeKind.nkFloat => IO.Put("float");
      | Json.NodeKind.nkNull => IO.Put("float");
      | Json.NodeKind.nkRoot => IO.Put("root");
      | Json.NodeKind.nkObject => IO.Put("obj");
      | Json.NodeKind.nkArray => IO.Put("array");
      END; 
      *)
    END;
    RETURN node;
  END FindText;

PROCEDURE TestParser() =
VAR
  buf,arr,obj,name : TEXT;
  node,n2,n3 : Json.T;
  iter : Json.Iterator;
  pval : REAL;
BEGIN

  buf := "\"name\"";
  node := Json.ParseBuf(buf);
  <*ASSERT node # NIL *>
  IO.Put(node.format() & "\n");

  buf := "{ \"fred\" : \"myval\" }";
  node := Json.ParseBuf(buf);
  IO.Put(node.format() & "\n");

  obj := "{ \"arr\" : [1,3,5,true,null,2.3e4] }";
  node := Json.ParseBuf(obj);
  <*ASSERT node.kind() = Json.NodeKind.nkObject *>

  arr := "[1,3,5,true,null,2.3e4]";
  node := Json.ParseBuf(arr);
  <*ASSERT node.kind() = Json.NodeKind.nkArray *>

  obj := "{ \"myobj\" : { \"n1\\uABCD\" : \"n1val\"}}";
  node := Json.ParseBuf(obj);
  <*ASSERT node.kind() = Json.NodeKind.nkObject *>

  obj := "{ \"arr\" : [1,3,5,\"txt\",true,null,2.3e4] }";
  node := Json.ParseBuf(obj);
  n3 := node.find("/arr");
  <*ASSERT n3 # NIL *>
  <*ASSERT n3.kind() = Json.NodeKind.nkArray *>

  (* add an empty array *)
  EVAL node.addArr("emptyarr");
  IO.Put(node.format() & "\n");

  (* add the n3 array which duplicates the array *)
  EVAL node.addArr("myarr",n3);
  IO.Put(node.format() & "\n");

  obj := "{ \"myobj\" : { \"n1\\uABCD\" : \"n1val\"}}";
  n3 := Json.ParseBuf(obj);
  IO.Put(n3.format() & "\n");
  n3 := n3.find("/myobj");

  (* replace arr with myobj *)
  node.updateArrObj("myarr",n3);
  IO.Put(node.format() & "\n");

  obj := "{\"name1\" : \"val1\" , \"name2\" : \"val2\"}";
  node := Json.ParseBuf(obj);
  n2 := FindText(node,"name2");
  <*ASSERT n2 # NIL *>

  (* simple node additions *)
  EVAL node.addText("email","fred@gmail.com");
  EVAL node.addInt("number",1234567);
  EVAL node.addFloat("float",-2356.323D3);
  EVAL node.addBool("istrue",TRUE);
  EVAL node.addBool("isfalse",FALSE);
  EVAL node.addNull("isnull");
  IO.Put(node.format() & "\n");

  (* find and convert *)
  n2 := node.find("/float");
  pval := Scan.Real(n2.value());
  IO.Put("found number "); IO.PutReal(pval); IO.Put("\n");

  (* simple node updates *)
  node.updateText("email","bob@hotmail.com");
  node.updateInt("number",98765);
  node.updateFloat("float",1.23d1);
  node.updateBool("istrue",FALSE);
  node.updateBool("isfalse",TRUE);
  IO.Put(node.format() & "\n");

  (* simple delete n2 contains deleted node *)
  n2 := node.delete("isfalse"); 
  IO.Put(node.format() & "\n");

  (* overwrite number as it already exists *)
  n2 := node.addInt("number",3334);
  n2 := n2.addInt("number",3334); (* error returns nil n2 is not an object*)
  <*ASSERT n2 = NIL *>
  IO.Put(node.format() & "\n");

  (* tests on json file *)
  node := Json.ParseFile("json.txt");
  IO.Put(node.format() & "\n");

  n2 := FindText(node,"/"); (* return object *)
  n2 := FindText(node,"/billTo"); (* return object *)
  n2 := FindText(n2,"zip");
  EVAL node.addText("email","fred@gmail.com");
  EVAL node.addInt("number",3334);
  n2 := n2.addInt("number2",3334); (* error returns nil *)
  <*ASSERT n2 = NIL *>
  n2 := node.addFloat("float",-2356.323D302);
  n2 := node.addNull("mynull");
  node.updateText("city","Paris");

  n2 := FindText(node,"/billTo/surcharge");
  n2 := FindText(node,"/addr");
  n2 := FindText(node,"/name");
  n2 := FindText(node,"/price");
  pval := Scan.Real(n2.value());

  n2 := FindText(node,"/cars"); (* second elt of cars *)
  <*ASSERT n2 # NIL AND n2.kind() = Json.NodeKind.nkArray *>
  n2 := FindText(node,"/cars/1"); (* second elt of cars *)
  <*ASSERT n2 # NIL AND n2.kind() = Json.NodeKind.nkText *>
  n2 := FindText(node,"/billTo/name");
  <*ASSERT n2 # NIL AND n2.kind() = Json.NodeKind.nkText *>
  n2 := FindText(node,"/billTo/testnotfound"); (* returns nil *)
  <*ASSERT n2 = NIL  *>
  n2 := FindText(node,"/billTo/names/first");
  <*ASSERT n2 # NIL AND n2.kind() = Json.NodeKind.nkText *>

  n2 := FindText(node,"/billTo");
  <*ASSERT n2 # NIL AND n2.kind() = Json.NodeKind.nkObject *>
  n3 := n2.delete("state"); (* single node *)
  <*ASSERT n3 # NIL AND n3.kind() = Json.NodeKind.nkText *>
  n3 := n2.delete("names"); (* struct node *)
  <*ASSERT n3 # NIL AND n3.kind() = Json.NodeKind.nkObject *>

  (* delete 3rd item of array cars which must be a string rep of index*)
  n2 := FindText(node,"/cars");
  <*ASSERT n2 # NIL AND n2.kind() = Json.NodeKind.nkArray *>
  n3 := n2.delete("2"); (* 3rd element *)

  IO.Put(node.format() & "\n");
  n2 := FindText(node,"/");
  <*ASSERT n2 # NIL AND n2.kind() = Json.NodeKind.nkObject AND Text.Equal(n2.name(),"root") *>

  n2 := node.find("/shipTo");
  IF n2 # NIL THEN
    EVAL n2.delete("name");
  END;
  IO.Put(node.format() & "\n");

  node := node.root();
  (* delete shipto object node from main tree and add to billto *)
  n2 := node.delete("shipTo");
  n3 := node.find("/billTo");
  EVAL n3.addObj("ignoreme",n2);
  IO.Put(node.format() & "\n");

  (* nodes within nodes *)
  node := node.find("/nest");
  node := node.addObj("firstobject");
  node := node.addObj("newobject");
  node := node.addObj("newobject");
  n2 := node.addInt("secondnumber",3334);

  node := node.root().find("/billTo");
  n2 := node.addArr("NewArr");
  n2 := n2.addText("noname","FIRST");
  
  IO.Put(node.root().format() & "\n");

  n2 := node.root().find("/cars");
  iter := n2.iterate();
  WHILE iter.next(name,n3) DO
    IO.Put(name & " kind " & Json.NK[n3.kind()] & "\n");
  END;

  IterAll(node.root());
END TestParser;

(*
PROCEDURE TestSuite() =
  VAR
    jsonParser : Json.T;
    iter : FS.Iterator;
    f,txt : TEXT;
    res : BOOLEAN;
    rd : Rd.T;
  BEGIN
(*
  iterate over a directory and parse files.
  Used in the Json test directory.
*)
    jsonParser := NEW(Json.T);
    iter := FS.Iterate(".");
    res :=iter.next(f);
    WHILE res DO
      IO.Put(f & "\n");
      rd := IO.OpenRead(f);
      jsonParser := Json.ParseFile(f);
      txt := jsonParser.format();
      IO.Put(txt);
      res := iter.next(f);
    END;
  END TestSuite;
*)

BEGIN

  TestParser();

END Main.

