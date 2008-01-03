(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

MODULE CallbackX;

IMPORT View, Item;

REVEAL T = Public BRANDED OBJECT
    v: View.T;
  OVERRIDES
    init := Init;
    itemsCreated := ItemsCreated;
    itemsModified := ItemsModified;
    itemsDeleted := ItemsDeleted;
  END;

PROCEDURE Init (cb: T; v: View.T): T =
  BEGIN
    cb.v := v;
    RETURN cb;
  END Init;

PROCEDURE ItemsCreated (cb: T; its: Item.TArray) =
  BEGIN 
    View.ItemsCreated (cb.v, its);
  END ItemsCreated;

PROCEDURE ItemsModified (cb: T; its: Item.TArray; additive: BOOLEAN) =
  BEGIN
    View.ItemsModified (cb.v, its, additive);
  END ItemsModified;

PROCEDURE ItemsDeleted (cb: T; ids: Item.IDArray) =
  BEGIN
    View.ItemsDeleted (cb.v, ids);
  END ItemsDeleted;

BEGIN
END CallbackX.
