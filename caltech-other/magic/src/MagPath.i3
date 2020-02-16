(* $Id$ *)

INTERFACE MagPath;
IMPORT TextList;

(* parse space-/colon-separated path and set it *)
PROCEDURE Set(path : TEXT);

PROCEDURE SetFromList(path : TextList.T);

PROCEDURE Append(dirPath : TEXT);
(* add new dirPath as having highest priority *) 

PROCEDURE Get() : TextList.T;

END MagPath.
