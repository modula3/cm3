(* $Id$ *)

MODULE FileUtils;
IMPORT Wr, Rd, Thread, OSError, FileRd, FileWr, TextWr;
IMPORT Pathname;

PROCEDURE CopyRdToWr(rd : Rd.T; wr : Wr.T) 
  RAISES { Thread.Alerted, Rd.Failure, Wr.Failure } =
  CONST
    BufSize = 16*1024;
  VAR

    <*NOWARN*>buffer : ARRAY [0..BufSize-1] OF CHAR;
    (* pragma NOWARN because it otherwise complains about a large
       local variable.  The routine isn't recursive, so that shouldn't
       be a problem.
     *)

  BEGIN
    REPEAT
      WITH chunk = MIN(NUMBER(buffer), Rd.CharsReady(rd)),
           data = SUBARRAY(buffer,0,chunk) DO
        IF chunk = 0 THEN 
          Thread.Pause(0.001d0) (* stupid *)
        ELSE
          EVAL Rd.GetSub(rd, data); Wr.PutString(wr, data)
        END
      END
    UNTIL Rd.EOF(rd);
  END CopyRdToWr;
  
PROCEDURE Copy(from, to : Pathname.T) RAISES { OSError.E, Thread.Alerted, Rd.Failure, Wr.Failure } =
  VAR
    rd := FileRd.Open(from);
    wr := FileWr.Open(to);
  BEGIN
    TRY
      CopyRdToWr(rd,wr);
    FINALLY
      TRY 
        Wr.Close(wr) 
      FINALLY
        Rd.Close(rd)
      END
    END
  END Copy;

PROCEDURE Get(p : Pathname.T) : TEXT RAISES 
  { OSError.E, Thread.Alerted, Rd.Failure } =
  <* FATAL Wr.Failure *>
  VAR
    rd := FileRd.Open(p);
    iWr := NEW(TextWr.T).init();
  BEGIN
    TRY
      CopyRdToWr(rd,iWr);
      RETURN TextWr.ToText(iWr)
    FINALLY
      Rd.Close(rd)
    END
  END Get;

PROCEDURE GetToWr(wr : Wr.T; p : Pathname.T) RAISES 
  { OSError.E, Thread.Alerted, Rd.Failure, Wr.Failure } =
  VAR
    rd := FileRd.Open(p);
  BEGIN
    TRY
      CopyRdToWr(rd,wr);
    FINALLY
      Rd.Close(rd)
    END
  END GetToWr;

BEGIN END FileUtils.
