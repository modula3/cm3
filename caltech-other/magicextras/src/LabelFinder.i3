(* $Id$ *)

INTERFACE LabelFinder;
IMPORT MagCell, TextSet, MagLabelList AS LabelList;

PROCEDURE FindTextSetLabels(layout : MagCell.T;
                            aliasSet : TextSet.T;
                            ignoreEnclosingQuotes := TRUE) : LabelList.T;

END LabelFinder.
