(* $Id$ *)

INTERFACE MagLabelListExt;
IMPORT MagLabelList;
IMPORT Word;

CONST Brand = "MagLabelList";

TYPE T = MagLabelList.T;

(* lists are Equal if the elements are equal under MagLabel.Equal
   and in the same order. *)
PROCEDURE Equal(a, b : T) : BOOLEAN;
PROCEDURE Hash(a : T) : Word.T;

END MagLabelListExt.
