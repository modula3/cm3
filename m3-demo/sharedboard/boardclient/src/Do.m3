(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

MODULE Do;

IMPORT Win, Item, View;

REVEAL T = Public BRANDED OBJECT
       wn: Win.T;
       records: ARRAY [0..Num-1] OF Rec;
       lo := 0;
       hi := 0;
       empty := TRUE;
     OVERRIDES
       init := Init;
       createItems := CreateItems;
       deleteItems := DeleteItems;
       undo := Undo;
     END;
     
TYPE Rec = RECORD code: Code; its: Item.TArray END;
     Code = {Created, Deleted};

CONST Num = 100;

(* "records" stores the information from last "Num" calls to
   create or delete items. It is used as a circular buffer.
   "lo" points to the oldest valid record in "records". 
   "hi" points to the newest entry in records + 1, modulo "Num".
   "empty" is true when there is no valid record in "records".
*)

PROCEDURE Init (do: T; wn: Win.T) : T =
  BEGIN
    do.wn := wn;
    RETURN do;
  END Init; 

PROCEDURE CreateItems (do: T; its: Item.TArray) =
  BEGIN
    View.CreateItems (do.wn, its);
    do.records [do.hi] := Rec {Code.Created, its};
    IF do.hi = do.lo THEN
      IF do.empty THEN
        do.empty := FALSE;
      ELSE 
        do.lo := (do.lo + 1) MOD Num;
      END;
    END;
    do.hi := (do.hi + 1) MOD Num;
  END CreateItems;

PROCEDURE DeleteItems (do: T; its: Item.TArray) =
  VAR ids := NEW (Item.IDArray, NUMBER (its^));
  BEGIN
    FOR i := FIRST (its^ ) TO LAST (its^) DO
      ids[i] := its[i].id;
    END;
    View.DeleteItems (do.wn, ids);
    do.records [do.hi] := Rec {Code.Deleted, its};
    IF do.hi = do.lo THEN
      IF do.empty THEN
        do.empty := FALSE;
      ELSE 
        do.lo := (do.lo + 1) MOD Num;
      END;
    END;
    do.hi := (do.hi + 1) MOD Num;
  END DeleteItems; 

PROCEDURE Undo (do: T) RAISES {NoInfo} =
  BEGIN
    IF do.empty THEN RAISE NoInfo END;
    do.hi := (do.hi - 1) MOD Num;
    IF do.hi = do.lo THEN do.empty := TRUE END;
    CASE (do.records[do.hi].code) OF
    | Code.Created => 
      VAR its := do.records[do.hi].its;
          ids := NEW (Item.IDArray, NUMBER (its^));
      BEGIN
        FOR i := FIRST (its^ ) TO LAST (its^) DO
          ids[i] := its[i].id;
        END;
        View.DeleteItems (do.wn, ids);
      END;
    | Code.Deleted => View.CreateItems (do.wn, do.records[do.hi].its);
    END;
  END Undo;

BEGIN
END Do.
