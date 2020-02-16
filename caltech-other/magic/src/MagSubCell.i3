(* $Id$ *)
INTERFACE MagSubCell;
IMPORT MagCell, MagTransform;
IMPORT MagArrayData, MagRect;

TYPE
  T = RECORD
    c : MagCell.T;
    transform : MagTransform.T;
    useId : TEXT;
    array : REF MagArrayData.T := NIL;
    box : MagRect.T;
  END;

CONST Brand = "SubCell";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;
 
END MagSubCell.
