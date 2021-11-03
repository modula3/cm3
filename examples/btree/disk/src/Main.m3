MODULE Main;

IMPORT BTree;
IMPORT SimpleKeys,DataRecs;

VAR
  fname : TEXT := "testfile";
  fbr : BTree.T;

<*FATAL BTree.Error*>

BEGIN

  fbr := NEW(BTree.T).create(fname,numKeys := 3,blockLen := BYTESIZE(DataRecs.DatRec));
(*
  fbr := NEW(BTree.T).open(fname);
*)
  SimpleKeys.CheckFileTest();

  SimpleKeys.fbr := fbr;

  SimpleKeys.InsertKeys();
  (* insert again but this time found so not inserted *)
  SimpleKeys.InsertKeys();
  SimpleKeys.SearchKeys();
  (* search for keys which should not be found *)
  SimpleKeys.NotFoundKeys();
  SimpleKeys.DeleteKeys();
  SimpleKeys.InsertKeysBackward();
  SimpleKeys.DeleteKeysBackward();
  SimpleKeys.InsertOtherKeys();
  SimpleKeys.SearchOtherKeys();
  SimpleKeys.RandomInsertKeys();
  SimpleKeys.RandomSearchKeys();
  SimpleKeys.RandomDeleteKeys();

  SimpleKeys.fbr.printInfo();

  DataRecs.fbr := fbr;
  DataRecs.SimpleRecs();
  DataRecs.RandomRecs();

  IF fbr # NIL THEN fbr.close(); END;

END Main.
