MODULE Main;

(*
  need to update this test suit and need some requrements
  1 a test procedure should test a single component? and be verified
  ie compared against expected results.
  2 each test should have the input created as a text so the first set
    of tests just show that parsing works.
  3 to compare the expected with the input do we just compare texts?
    well that wont be possible with the update set of tests

*)

IMPORT Json,IO,Text,Fmt,Wr,Rd,FS,Pickle2;

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
    END;
    RETURN node;
  END FindText;

PROCEDURE P1() =
  VAR
    buf : TEXT;
    node : Json.T;
  BEGIN
    (* parsing - simplest *)
    buf := "\"name\"";
    node := Json.ParseBuf(buf);
    <*ASSERT node # NIL *>
    <*ASSERT node.kind() = Json.NodeKind.nkText *>
    <*ASSERT node.size() = 0 *>
    <*ASSERT Text.Equal(node.name(),"root")  *>
    <*ASSERT Text.Equal(node.value(),"name")  *>
    IO.Put(node.format() & "\n");
  END P1;

PROCEDURE P2() =
  VAR
    buf : TEXT;
    node,n1 : Json.T;
  BEGIN
    (* parsing - simple object with a text *)
    buf := "{ \"name\" : \"john\" }";
    node := Json.ParseBuf(buf);
    <*ASSERT node # NIL *>
    <*ASSERT node.kind() = Json.NodeKind.nkObject *>
    <*ASSERT node.size() = 1 *>
    <*ASSERT Text.Equal(node.name(),"root")  *>
    <*ASSERT Text.Equal(node.value(),"_Obj_")  *>
    n1 := node.find("name");
    <*ASSERT n1 # NIL *>
    <*ASSERT n1.kind() = Json.NodeKind.nkText *>
    <*ASSERT n1.size() = 0 *>
    <*ASSERT Text.Equal(n1.name(),"name")  *>
    <*ASSERT Text.Equal(n1.value(),"john")  *>
    IO.Put(node.format() & "\n");
  END P2;

PROCEDURE P3() =
  VAR
    buf : TEXT;
    node,n1 : Json.T;
  BEGIN
    (* parsing - simple array *)
    buf := "[1,2,\"name\",true,null,2.3e4]";
    node := Json.ParseBuf(buf);
    <*ASSERT node # NIL *>
    <*ASSERT node.kind() = Json.NodeKind.nkArray *>
    <*ASSERT node.size() = 6 *>
    <*ASSERT Text.Equal(node.name(),"root")  *>
    <*ASSERT Text.Equal(node.value(),"_Arr_")  *>
    n1 := node.find("0");
    <*ASSERT n1 # NIL *>
    <*ASSERT n1.kind() = Json.NodeKind.nkInt *>
    <*ASSERT n1.size() = 0 *>
    <*ASSERT Text.Equal(n1.name(),"0")  *>
    <*ASSERT Text.Equal(n1.value(),"1")  *>
    n1 := node.find("2");
    <*ASSERT n1 # NIL *>
    <*ASSERT n1.kind() = Json.NodeKind.nkText *>
    <*ASSERT n1.size() = 0 *>
    <*ASSERT Text.Equal(n1.name(),"2")  *>
    <*ASSERT Text.Equal(n1.value(),"name")  *>
    n1 := node.find("3");
    <*ASSERT n1 # NIL *>
    <*ASSERT n1.kind() = Json.NodeKind.nkBool *>
    <*ASSERT n1.size() = 0 *>
    <*ASSERT Text.Equal(n1.name(),"3")  *>
    <*ASSERT Text.Equal(n1.value(),"true")  *>
    n1 := node.find("4");
    <*ASSERT n1 # NIL *>
    <*ASSERT n1.kind() = Json.NodeKind.nkNull *>
    <*ASSERT n1.size() = 0 *>
    <*ASSERT Text.Equal(n1.name(),"4")  *>
    <*ASSERT Text.Equal(n1.value(),"null")  *>
    n1 := node.find("5");
    <*ASSERT n1 # NIL *>
    <*ASSERT n1.kind() = Json.NodeKind.nkFloat *>
    <*ASSERT n1.size() = 0 *>
    <*ASSERT Text.Equal(n1.name(),"5")  *>
    <*ASSERT Text.Equal(n1.value(),"2.3e4")  *>
    IO.Put(node.format() & "\n");
  END P3;

PROCEDURE P4() =
  VAR
    buf : TEXT;
    node,n1,n2 : Json.T;
  BEGIN
    (* parsing - object in object *)
    buf := "{ \"someobj\" : { \"ABCD\" : \"value\"}}";
    node := Json.ParseBuf(buf);
    <*ASSERT node # NIL *>
    <*ASSERT node.kind() = Json.NodeKind.nkObject *>
    <*ASSERT node.size() = 1 *>
    <*ASSERT Text.Equal(node.name(),"root")  *>
    <*ASSERT Text.Equal(node.value(),"_Obj_")  *>
    n1 := node.find("someobj");
    <*ASSERT n1 # NIL *>
    <*ASSERT n1.kind() = Json.NodeKind.nkObject *>
    <*ASSERT n1.size() = 1 *>
    <*ASSERT Text.Equal(n1.name(),"someobj")  *>
    <*ASSERT Text.Equal(n1.value(),"_Obj_")  *>
    n2 := n1.find("ABCD");
    <*ASSERT n2 # NIL *>
    <*ASSERT n2.kind() = Json.NodeKind.nkText *>
    <*ASSERT n2.size() = 0 *>
    <*ASSERT Text.Equal(n2.name(),"ABCD")  *>
    <*ASSERT Text.Equal(n2.value(),"value")  *>
    IO.Put(node.format() & "\n");
  END P4;

PROCEDURE P5() =
  VAR
    buf : TEXT;
    node,n1,n2 : Json.T;
  BEGIN
    (* parsing - array in object *)
    buf := "{ \"arr\" : [1,5,2.3e4] }";
    node := Json.ParseBuf(buf);
    <*ASSERT node # NIL *>
    <*ASSERT node.kind() = Json.NodeKind.nkObject *>
    <*ASSERT node.size() = 1 *>
    <*ASSERT Text.Equal(node.name(),"root")  *>
    <*ASSERT Text.Equal(node.value(),"_Obj_")  *>
    n1 := node.find("arr");
    <*ASSERT n1 # NIL *>
    <*ASSERT n1.kind() = Json.NodeKind.nkArray *>
    <*ASSERT n1.size() = 3 *>
    <*ASSERT Text.Equal(n1.name(),"arr")  *>
    <*ASSERT Text.Equal(n1.value(),"_Arr_")  *>
    n2 := node.find("arr/0");
    <*ASSERT n2 # NIL *>
    <*ASSERT n2.kind() = Json.NodeKind.nkInt *>
    <*ASSERT n2.size() = 0 *>
    <*ASSERT Text.Equal(n2.name(),"0")  *>
    <*ASSERT Text.Equal(n2.value(),"1")  *>
    n2 := n1.find("0");
    <*ASSERT n2 # NIL *>
    <*ASSERT n2.kind() = Json.NodeKind.nkInt *>
    <*ASSERT n2.size() = 0 *>
    <*ASSERT Text.Equal(n2.name(),"0")  *>
    <*ASSERT Text.Equal(n2.value(),"1")  *>
    IO.Put(node.format() & "\n");
  END P5;

PROCEDURE P6() =
  VAR
    buf : TEXT;
    node,n1 : Json.T;
  BEGIN
    (* array in array *)
    buf := "[1,2,\"name\",true,[5,6,7],null,2.3e4]";
    node := Json.ParseBuf(buf);
    <*ASSERT node # NIL *>
    <*ASSERT node.kind() = Json.NodeKind.nkArray *>
    <*ASSERT node.size() = 7 *>
    <*ASSERT Text.Equal(node.name(),"root")  *>
    <*ASSERT Text.Equal(node.value(),"_Arr_")  *>
    n1 := node.find("4");
    <*ASSERT n1 # NIL *>
    <*ASSERT n1.kind() = Json.NodeKind.nkArray *>
    <*ASSERT n1.size() = 3 *>
    <*ASSERT Text.Equal(n1.name(),"4")  *>
    <*ASSERT Text.Equal(n1.value(),"_Arr_")  *>
    n1 := node.find("4/2");
    <*ASSERT n1 # NIL *>
    <*ASSERT n1.kind() = Json.NodeKind.nkInt *>
    <*ASSERT n1.size() = 0 *>
    <*ASSERT Text.Equal(n1.name(),"2")  *>
    <*ASSERT Text.Equal(n1.value(),"7")  *>
  END P6;

PROCEDURE P7() =
  VAR
    buf : TEXT;
    node,n1 : Json.T;
  BEGIN
    (* find stuff *)
    buf := "{\"name\" : \"X\", \"A\" : {\"B\" : 55}, \"arr\" : [1,5,2.3e4] }";
    node := Json.ParseBuf(buf);
    <*ASSERT node # NIL *>
    <*ASSERT node.kind() = Json.NodeKind.nkObject *>
    <*ASSERT node.size() = 3 *>
    n1 := node.find("/");
    <*ASSERT n1 # NIL *>
    <*ASSERT n1.kind() = Json.NodeKind.nkObject *>
    <*ASSERT Text.Equal(n1.name(),"root")  *>
    n1 := node.find("/name");
    <*ASSERT n1 # NIL *>
    <*ASSERT n1.kind() = Json.NodeKind.nkText *>
    <*ASSERT Text.Equal(n1.name(),"name")  *>
    n1 := node.find("name");
    <*ASSERT n1 # NIL *>
    <*ASSERT n1.kind() = Json.NodeKind.nkText *>
    <*ASSERT Text.Equal(n1.name(),"name")  *>
    n1 := node.find("A/B");
    <*ASSERT n1 # NIL *>
    <*ASSERT n1.kind() = Json.NodeKind.nkInt *>
    <*ASSERT Text.Equal(n1.name(),"B")  *>
    n1 := node.find("arr/1");
    <*ASSERT n1 # NIL *>
    <*ASSERT n1.kind() = Json.NodeKind.nkInt *>
    <*ASSERT Text.Equal(n1.name(),"1")  *>
    n1 := node.find("arr/9");
    <*ASSERT n1 = NIL *>
  END P7;

PROCEDURE P8() =
  VAR
    buf,txt : TEXT;
    node,n1 : Json.T;
    int : INTEGER;
    bool : BOOLEAN;
    float : LONGREAL;
  BEGIN
    (* get things and test conversions *)
    buf := "[1,2,\"name\",true,[5,6,7],null,2.3e4]";
    node := Json.ParseBuf(buf);
    <*ASSERT node # NIL *>
    <*ASSERT node.kind() = Json.NodeKind.nkArray *>
    n1 := node.find("1");
    <*ASSERT n1 # NIL *>
    int := n1.getInt();
    <*ASSERT int = 2 *>
    n1 := node.find("2");
    <*ASSERT n1 # NIL *>
    txt := n1.value();
    <*ASSERT Text.Equal(txt,"name") *>
    n1 := node.find("3");
    <*ASSERT n1 # NIL *>
    bool := n1.getBool();
    <*ASSERT bool = TRUE *>
    n1 := node.find("4");
    <*ASSERT n1 # NIL *>
    <*ASSERT n1.kind() = Json.NodeKind.nkArray *>
    n1 := node.find("6");
    <*ASSERT n1 # NIL *>
    float := n1.getFloat();
    <*ASSERT float = 2.3D4 *>
    n1 := node.find("2");
    <*ASSERT n1 # NIL *>
    TRY
      int := n1.getInt();
      <*ASSERT FALSE *>
    EXCEPT
    | Json.E => 
    END;
  END P8;

PROCEDURE P9() =
  VAR
    buf : TEXT;
    node,n1 : Json.T;
  BEGIN
    (* simple node additions *)
    buf := "{}";
    node := Json.ParseBuf(buf);
    <*ASSERT node # NIL *>
    <*ASSERT node.kind() = Json.NodeKind.nkObject *>
    n1 := node.addText("email","fred@gmail.com");
    <*ASSERT Text.Equal(n1.value(),"fred@gmail.com") *>
    n1 := node.addInt("number",1234567);
    <*ASSERT Text.Equal(n1.value(),"1234567") *>
    n1 := node.addFloat("float",-2356.323D3);
    <*ASSERT Text.Equal(n1.value(),"-2.356323e6") *>
    n1 := node.addBool("istrue",TRUE);
    <*ASSERT Text.Equal(n1.value(),"true") *>
    <*ASSERT n1.getBool() = TRUE *>
    n1 := node.addBool("isfalse",FALSE);
    <*ASSERT Text.Equal(n1.value(),"false") *>
    <*ASSERT n1.getBool() = FALSE *>
    n1 := node.addNull("isnull");
    <*ASSERT Text.Equal(n1.value(),"null") *>
    (* adding with the same name replaces the original *)
    EVAL node.addInt("number",9876);
    n1 := node.find("number");
    <*ASSERT Text.Equal(n1.value(),"9876") *>
    IO.Put(node.format() & "\n");
  END P9;

PROCEDURE P10() =
  VAR
    buf : TEXT;
    node,n1,n2 : Json.T;
  BEGIN
    (* array additions *)
    buf := "[1,[],2,\"name\",true,[5,6,7],null,2.3e4]";
    node := Json.ParseBuf(buf);
    <*ASSERT node # NIL *>
    <*ASSERT node.kind() = Json.NodeKind.nkArray *>
    n1 := node.addText("","test");
    <*ASSERT n1 # NIL AND Text.Equal(n1.value(),"test") AND node.size() = 9 *>
    n2 := node.find("5");
    <*ASSERT n2.kind() = Json.NodeKind.nkArray *>
    n1 := n2.addInt("",9999);
    <*ASSERT n1 # NIL AND n1.getInt() = 9999 AND n2.size() = 4 *>
    IO.Put(node.format() & "\n");
    (* array in object *)
    buf := "{ \"arr\" :[1,2] }";
    node := Json.ParseBuf(buf);
    (* this replaces the existing arr with new type text *)
    n1 := node.addText("arr","newtext");
    <*ASSERT n1.kind() = Json.NodeKind.nkText *>
    IO.Put(node.format() & "\n");
  END P10;

PROCEDURE P11() =
  VAR
    buf : TEXT;
    node : Json.T;
  BEGIN
    (* simple node updates *)
    buf := "{\"email\" : \"X\", \"number\" : 23, \"float\" : 2.3e4, \"istrue\" : true, \"isfalse\" : false, \"isnull\" : null }";
    node := Json.ParseBuf(buf);
    <*ASSERT node # NIL *>
    node.updateText("email","bob@hotmail.com");
    node.updateInt("number",98765);
    node.updateFloat("float",1.23d1);
    node.updateBool("istrue",TRUE);
    node.updateBool("isfalse",FALSE);
    IO.Put(node.format() & "\n");
  END P11;

PROCEDURE P12() =
  VAR
    buf : TEXT;
    node,n1,n2 : Json.T;
  BEGIN
    (* update array *)
    buf := "[1,2,\"name\",true,[5,6,7],null,2.3e4]";
    node := Json.ParseBuf(buf);
    <*ASSERT node # NIL *>
    <*ASSERT node.kind() = Json.NodeKind.nkArray *>
    buf := "{\"fred\" : \"sam\"}";
    n1 := Json.ParseBuf(buf);
    (* update an array element with an object *)
    node.updateArrObj("6",n1);
    node.updateArrObj("4/2",n1);
    (* update an array element with an array *)
    buf := "[true,false,null]";
    n2 := Json.ParseBuf(buf);
    <*ASSERT n2.kind() = Json.NodeKind.nkArray *>
    node.updateArrObj("1",n2);
    n1 := node.find("1");
    <*ASSERT n1.kind() = Json.NodeKind.nkArray *>
    (* update an array element with a text *)
    node.updateText("2","12 main st");
    (* update an array element with an int *)
    node.updateInt("4/1",777);
    IO.Put(node.format() & "\n");
  END P12;

PROCEDURE P13() =
  VAR
    buf : TEXT;
    node,n1 : Json.T;
  BEGIN
    (* update object *)
    buf := "{\"A\" : \"X\", \"B\" : {\"C\" : 55}, \"D\" : 11, \"arr\" : [1,5,2.3e4] }";
    node := Json.ParseBuf(buf);
    <*ASSERT node # NIL *>
    <*ASSERT node.kind() = Json.NodeKind.nkObject *>
    buf := "{\"fred\" : \"sam\"}";
    n1 := Json.ParseBuf(buf);
    (* update an array element with an object *)
    node.updateArrObj("D",n1);
    node.updateArrObj("B/C",n1);
    (* update an array element with an array *)
    IO.Put(node.format() & "\n");
  END P13;

PROCEDURE P14() =
  VAR
    buf : TEXT;
    node,n1 : Json.T;
  BEGIN
    (* deletes *)
    buf := "{\"name\" : \"X\", \"A\" : {\"B\" : 55}, \"arr\" : [1,5,2.3e4] }";
    node := Json.ParseBuf(buf);
    <*ASSERT node # NIL *>
    <*ASSERT node.kind() = Json.NodeKind.nkObject *>
    n1 := node.delete("name");
    <*ASSERT n1 # NIL AND Text.Equal(n1.value(),"X") AND node.size() = 2 *>
    n1 := node.delete("A/B");
    <*ASSERT n1 # NIL AND Text.Equal(n1.value(),"55")
        AND node.size() = 2 AND n1.size() = 0 *>
    (* delete second elt of arr *)
    n1 := node.delete("arr/1");
    <*ASSERT n1 # NIL AND Text.Equal(n1.value(),"5")
        AND node.size() = 2 AND n1.size() = 0 *>
    (* delete object A *)
    n1 := node.delete("A");
    <*ASSERT n1 # NIL AND node.size() = 1 AND n1.size() = 0 *>
    IO.Put(node.format() & "\n");
  END P14;

PROCEDURE P15() =
  VAR
    buf : TEXT;
    node,n1,n2 : Json.T;
  BEGIN
    (* misc *)
    buf := "{\"name\" : \"X\", \"A\" : {\"B\" : 55}, \"arr\" : [1,5,2.3e4] }";
    node := Json.ParseBuf(buf);
    <*ASSERT node # NIL *>
    <*ASSERT node.kind() = Json.NodeKind.nkObject *>
    (* copy a node *)
    n1 := node.copy();
    <*ASSERT n1 # NIL AND node.size() = n1.size() *>
    n2 := n1.root();
    (* delete entire tree *)
    n2.clear();
    <*ASSERT n2 # NIL AND n2.size() = 0 *>
    IO.Put(n2.rawText() & "\n");
  END P15;

PROCEDURE P16() =
  VAR
    buf,name : TEXT;
    node,n1 : Json.T;
    iter : Json.Iterator;
  BEGIN
    (* iterator test *)
    buf := "{\"name\" : \"X\", \"A\" : {\"B\" : 55}, \"arr\" : [1,5,2.3e4] }";
    node := Json.ParseBuf(buf);
    iter := node.iterate();
    WHILE iter.next(name,n1) DO
      IO.Put("name:" & name & " kind " & Json.NK[n1.kind()] & "\n");
    END;

    IterAll(node.root());
  END P16;

PROCEDURE P17() =
  VAR
    rd : Rd.T;
    wrt : Wr.T;
    buf : TEXT;
    node,n1 : Json.T;
  BEGIN
    (* pickle test *)
    buf := "{\"name\" : \"X\", \"A\" : {\"B\" : 55}, \"arr\" : [1,5,2.3e4] }";
    node := Json.ParseBuf(buf);
    wrt := IO.OpenWrite("json.pkl");
    Pickle2.Write(wrt,node);
    Wr.Close(wrt);
    rd := IO.OpenRead("json.pkl");
    n1 := NARROW(Pickle2.Read(rd), Json.T);
    Rd.Close(rd);
    IO.Put(n1.root().format() & "\n");
  END P17;

PROCEDURE P19() =
  VAR
    node,n2,n3 : Json.T;
  BEGIN
    (* tests on json file - produces a lot of output *)
    node := Json.ParseFile("json.txt");
    IO.Put("root size " & Fmt.Int(node.size()) & "\n");
    n2 := FindText(node,"/billTo"); (* return object *)
    IO.Put("billto size " & Fmt.Int(n2.size()) & "\n");
    n3 := FindText(node,"nothing");
    IO.Put("nothing size " & Fmt.Int(n3.size()) & "\n");
    (* dont need / before first search term *)
    n3 := FindText(n2,"address");
    IO.Put("address size " & Fmt.Int(n3.size()) & "\n");
    n3 := FindText(n2,"/address");
    n3 := FindText(n2,"names/second");
    n2 := FindText(node,"nest");
    n3 := FindText(n2,"0");
    (* array of array same as [0,0] *)
    n3 := FindText(n2,"0/0");
    (* find all elts of array *)
    IO.Put("size " & Fmt.Int(n2.size()) & "\n");
    FOR i := 0 TO n2.size() - 1 DO
      n3 := FindText(n2,Fmt.Int(i));
    END;

    IO.Put(node.format() & "\n");
    n3 := FindText(node,"/billTo");

    (* this replaces the object replaceme in node with billTo but
       the new node is named billTo *)
    node.updateArrObj("replaceme",n3);

    IO.Put(node.rawText() & "\n");

    n2 := FindText(node,"/params/contentChanges/0/text"); (* return object *)
    <*ASSERT n2 # NIL *>
    IO.Put(n2.value());

    n2 := FindText(node,"/"); (* return object *)
    n2 := FindText(node,"/billTo"); (* return object *)
    n2 := FindText(n2,"zip");

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
  END P19;

PROCEDURE P20() =
  VAR
    jsonParser : Json.T;
    iter : FS.Iterator;
    f,txt : TEXT;
    res : BOOLEAN;
    rd : Rd.T;
  BEGIN
(*
  Iterate over a directory and parse files.
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
  END P20;

BEGIN
  P1();
  P2();
  P3();
  P4();
  P5();
  P6();
  P7();
  P8();
  P9();
  P10();
  P11();
  P12();
  P13();
  P14();
  P15();
  P16();
  P17();
(* 
  P19();
  P20();
*)
END Main.

