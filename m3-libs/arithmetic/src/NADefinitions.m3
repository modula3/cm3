MODULE NADefinitions;
(** Arithmetic for Modula-3, see doc for details

Abstract: Utilities.

12/13/95  Harry George    Initial version
1/27/96   Harry George    Converted to m3na
*)
IMPORT IO,Stdio,Wr,Thread;

<*UNUSED*> CONST Module = "NADefinitions.";

(*---debug---*)
PROCEDURE debug(level:[0..3]; ftn,str:TEXT) (*RAISES {Thread.Alerted, Wr.Failure}*) =
<*FATAL Thread.Alerted, Wr.Failure*>
BEGIN
  IF verbosity >= level THEN
    Wr.PutText(Stdio.stdout,"arithmetic." & ftn & ":" & str & "\n");
    Wr.Flush(Stdio.stdout);
  END;
END debug;

(*---errors-----------*)
PROCEDURE err(ftn:TEXT; code:Err; errmsg :TEXT:=NIL) RAISES {Error}=
BEGIN
  IO.Put("arithmetic error:" & ftn & ":" & errmsg & "\n");
  RAISE Error(code);
END err;

(*==========================*)
BEGIN
  verbosity:=3;
END NADefinitions.
