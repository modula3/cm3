(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright 95 Digital Equipment Corporation.
   Digital Internal Use Only
   Last modified on Mon Dec  4 13:22:51 PST 1995 by detlefs
*)

INTERFACE CGITrans;

IMPORT TextTextTbl;

EXCEPTION Error(TEXT);

CONST PostMeth = "POST";
      DefCT = "application/x-www-form-urlencoded";

PROCEDURE P(reqMeth, conType: TEXT := NIL;
            conLen := FIRST(INTEGER)): TextTextTbl.T RAISES { Error };
(* Parses standard input as the name value pairs of a "POST"-style html forms
   query.  If "reqMeth" or "conType" are non-NIL, they specify the request
   method and content type, respectively; if they are NIL, their
   values are fetched from the environment variables "REQUEST_METHOD"
   and "CONTENT_TYPE".  Similarly, if "conLen" is positive, of if is
   the default value and the environment variable "CONTENT_LENGTH"
   parses to a positive integer, it specifies the length of the
   content string.  Otherwise, standard input is read until EOF.
   
   BUGS: presently only handles POST queries and the content type
   specified by "DefCT" above.
*)

END CGITrans.
