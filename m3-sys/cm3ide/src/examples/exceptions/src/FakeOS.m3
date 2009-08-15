
MODULE FakeOS;
IMPORT Rd, Wr, FileRd, FileWr;
IMPORT Thread, OSError;
	
(* Same as the last example, but this time, we catch exceptions that
   may occur while we are copying the contents, using the "TRY EXCEPT"
   clause. For each exception that is raised by the code we call, we
   propagate an "Error" exception to the caller. *)

PROCEDURE Copy(src, dest: TEXT) RAISES {Error} =
  VAR
    rd: Rd.T;
    wr: Wr.T;
    <* FATAL Thread.Alerted *>
  BEGIN
    TRY 
      rd := FileRd.Open (src);
      wr := FileWr.Open (dest);
      WITH contents = Rd.GetText (rd, LAST(INTEGER)) DO
        Wr.PutText (wr, contents);
      END;
      Rd.Close (rd);
      Wr.Close(wr);
    EXCEPT
    | Rd.Failure   => RAISE Error ("reading from " & src & "failed");
    | Wr.Failure   => RAISE Error ("writing to " & dest & "failed");
    | OSError.E    => RAISE Error ("some weird system problem occured");
    END
  END Copy;

(* Note that we have marked "Thread.Alerted" as fatal, because we
   don't think it will be raised at run-time. We could've easily added
   it to the set of raised exceptions, and mapped it into an "Error"
   exception.

   Also, what happens to the "Close" statements if there is an
   exception raised while the files are open? Yes, that's a problem,
   and you can use a "TRY FINALLY" statement to deal with it.

   These sorts of issues are usually not important in a short-lived,
   but they are important if you were to write long-lived,
   multi-threaded code. The nice part is that Modula-3 provides
   support mechanisms for dealing with long-lived applciations. *)

BEGIN
END FakeOS.
