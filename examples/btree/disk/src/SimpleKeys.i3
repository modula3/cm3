INTERFACE SimpleKeys;

IMPORT BTree;

VAR
  fbr : BTree.T;
  fname : TEXT := "testfile";

PROCEDURE InsertKeys();
PROCEDURE SearchKeys();
PROCEDURE DeleteKeys();
PROCEDURE InsertOtherKeys();
PROCEDURE SearchOtherKeys();
PROCEDURE NotFoundKeys();
PROCEDURE InsertKeysBackward();
PROCEDURE DeleteKeysBackward();
PROCEDURE RandomInsertKeys();
PROCEDURE RandomSearchKeys();
PROCEDURE RandomDeleteKeys();
PROCEDURE CheckFileTest();

END SimpleKeys.
