(* $Id$ *)

INTERFACE TaggedStatus;
IMPORT RectSet;
IMPORT EndPointStatus;

TYPE 
  Status = EndPointStatus.T;

  T = RECORD status : Status; rectSet : RectSet.T END;

CONST Brand = "TaggedStatus";

END TaggedStatus.
