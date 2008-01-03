(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Apr  7 14:40:00 PDT 1994 by kalsow                   *)

INTERFACE TextDB;

IMPORT TextList, RefList;

TYPE
  T <: T_; T_ = OBJECT
  METHODS
    init (): T;
    all_relations (): RefList.T;
    get_relation (nm: TEXT): Relation;
    create_relation (nm: TEXT): Relation;
    load (path: TEXT);
    dump (path: TEXT);
  END;

TYPE
  Relation <: R_; R_ = OBJECT
  METHODS
    name     (): TEXT;
    getValue (key: TEXT): TextList.T;
    getKey   (value: TEXT): TextList.T;
    insert   (key, value: TEXT);
    delete   (key, value: TEXT);
    deleteValue (value: TEXT);
  END;

END TextDB.
