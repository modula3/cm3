MODULE Debug;

IMPORT IO,Wr;

VAR
  deb : Wr.T;
  debug : BOOLEAN := TRUE;

PROCEDURE Write(txt : TEXT) =
  BEGIN
    IF debug THEN
      IO.Put(txt,deb);
      Wr.Flush(deb);
    END;
  END Write;

PROCEDURE Close() =
  BEGIN
    IF debug THEN
      Wr.Close(deb);
    END;
  END Close;

(* this is how to debug to stderr
Wr.PutText(Stdio.stderr,"testtest\n");
Wr.Flush(Stdio.stderr);
*)

BEGIN
  IF debug THEN
    deb := IO.OpenWrite("/tmp/debuglsp.txt");
  END;
END Debug.

