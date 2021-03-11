INTERFACE TextTextListTblExtras;
IMPORT TextTextListTbl;
TYPE
  T = TextTextListTbl.T;

PROCEDURE Scan(t: TEXT; 
               delims      := "\t, ";
               endDelims   := "\n;#%" ): T;

(* format: [key] [values], entries separated by newlines.
   use "endDelims" to end data before the end of a line. *)

END TextTextListTblExtras.
