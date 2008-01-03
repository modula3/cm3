INTERFACE TextTextTblExtras;
IMPORT TextTextTbl;
IMPORT TextSet;

TYPE
  T = TextTextTbl.T;

PROCEDURE Scan(src: TEXT; valueLast := TRUE): T;
PROCEDURE ScanMore(src: TEXT; dest: T;
                   valueLast := TRUE;
                   captureAll: TextSet.T := NIL);

(* Input format:
| targ1 [[=] targ2 [[=] ...]] [=] value
separated by commas and/or newlines
*)


PROCEDURE Reverse(tbl: T): T;
PROCEDURE ReverseMore(tbl: T; dest: T);

(* map each value to the last-stored key referring to that value *)

END TextTextTblExtras.
