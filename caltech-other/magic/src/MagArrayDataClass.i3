(* $Id$ *)

INTERFACE MagArrayDataClass;
IMPORT MagArrayData;
IMPORT MagTransform, TransformList;
IMPORT MagArrayElemTransform;

TYPE U = MagArrayData.T;

(* used to compute location of each array element in an array of *)
(* subcells.  The array movement is done first, then the transform *)
PROCEDURE ToTransformList(READONLY transform : MagTransform.T;
                          READONLY arrayData : U) : TransformList.T;

PROCEDURE ToTransformIterator(READONLY transform : MagTransform.T;
                              READONLY a : U) : MagTransform.Iterator;


PROCEDURE FormatIndex(READONLY transform : MagArrayElemTransform.T) : TEXT;

END MagArrayDataClass.
