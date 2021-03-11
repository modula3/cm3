INTERFACE FastNumParse;

(* 
   quickly do the following:

   given a CHAR buffer and an index into said buffer, start at such index
   and keep parsing as long as what is seen is a legal {INTEGER,LONGREAL}.

   return TRUE if anything seen, update z with numerical value of characters
   eaten.

   return FALSE and leave z and p as is if nothing seen.

   note: routines do NOT eat/skip whitespace 
*)

PROCEDURE Int     (READONLY buff : ARRAY OF CHAR; 
                   VAR      p    : CARDINAL; 
                   VAR      z    : INTEGER) : BOOLEAN;


PROCEDURE LongReal(READONLY buff : ARRAY OF CHAR; 
                   VAR      p    : CARDINAL; 
                   VAR      z    : LONGREAL) : BOOLEAN;

END FastNumParse.
