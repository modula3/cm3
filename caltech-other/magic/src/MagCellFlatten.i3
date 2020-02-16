(* $Id$ *)

INTERFACE MagCellFlatten;
IMPORT MagCell;
IMPORT TextCellInstanceTbl;

(* generate a table of instances from a root MagCell.T *)

PROCEDURE Flatten(root : MagCell.T) : TextCellInstanceTbl.T;

END MagCellFlatten.
