(* $Id$ *)

MODULE MagCellFlatten;
IMPORT MagCell, MagCellInstance;
IMPORT TextCellInstanceTbl;
IMPORT MagTransform;
IMPORT Debug;

PROCEDURE AddSubCell(subCell : MagCell.T; 
                     id : TEXT; transform : MagTransform.T; 
                     parentPath : TEXT;
                     args : REFANY) =
  VAR 
    tbl : TextCellInstanceTbl.T := args; (* NARROW the arg *)
    parentInstance, instance : MagCellInstance.T;
    x : BOOLEAN;
  BEGIN
    IF parentPath = NIL THEN
      parentInstance := NIL
    ELSE
      x := tbl.get(parentPath, parentInstance);
      <* ASSERT x *>
    END;

    instance := NEW(MagCellInstance.T).init(subCell, id, transform, 
                                            parentInstance);

    Debug.Out("MagCellFlatten.AddSubCell: adding instance with id \"" & id & "\"");
    x := tbl.put(id,instance);
    <* ASSERT NOT x *>
  END AddSubCell;

PROCEDURE Flatten(root : MagCell.T) : TextCellInstanceTbl.T  =
  <* FATAL ANY *>
  VAR
    res := NEW(TextCellInstanceTbl.Default).init();
  BEGIN
    root.subCellMap(AddSubCell, res);
    RETURN res
  END Flatten;

BEGIN END MagCellFlatten.
