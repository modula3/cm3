MODULE Main;

IMPORT Json,IO,Rd,FS;
IMPORT Scanner AS SK;

VAR
  jsonParser : Json.T;
  iter : FS.Iterator;
  f,txt : TEXT;
  res : BOOLEAN;
  rd : Rd.T;

PROCEDURE TestParser() =
VAR
  buf : TEXT;
  chars : SK.Buf;
  jsonParser : Json.T;
  node,n2 : Json.T;
BEGIN
  chars := NEW(SK.Buf,100);
  buf := "{ \"fred\" : \"myval\" }";
(*
  Text.SetChars(chars^,buf);
  s := NEW(SK.Default);

  t := s.initFromBuf(chars);
*)
(*
  rd := IO.OpenRead("json.txt");
  t := s.initFromRd(rd);
*)

  jsonParser := Json.ParseBuf(buf);
  IO.Put(jsonParser.format());

  jsonParser := Json.ParseFile("json.txt");
  IO.Put(jsonParser.format());

  node := jsonParser.find("/billTo");
  n2 := node.addText("email","fred@gmail.com");
  n2 := node.addInt("number",3334);
  n2 := node.addFloat("float",-2356.323D302);
  node.updateText("city","Paris");


  node := jsonParser.find("/name");
  node := jsonParser.find("/price");
  node := jsonParser.find("/cars");
  node := jsonParser.find("/billTo/name");
  node := jsonParser.find("/billTo/testme"); (* returns object *)
  node := jsonParser.find("/billTo/names/first");

  node := jsonParser.find("/billTo");
  n2 := node.delete("names");

  (* delete 2nd item of array cars which must be a string rep of index*)
  node := jsonParser.find("/cars");
  n2 := node.delete("2");

(* fix me should return root
  node := jsonParser.find("/");
*)
  node := jsonParser.find("/shipTo");
  IF node # NIL THEN
    n2 := node.delete("name");
  END;
  node := jsonParser.root();
  n2 := node.delete("shipTo");
  node := jsonParser.find("/billTo");
  n2 := node.addObj("ignoreme",n2);

  node := jsonParser.find("/nest");
  node := node.addObj("newobject");

  IO.Put(jsonParser.format());

END TestParser;

PROCEDURE TestSuite() =
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

BEGIN

  TestParser();

END Main.

