(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Aug 19 21:34:29 PDT 1996 by mhb                      *)

MODULE ImageUtils;

IMPORT FileRd, FileWr, Pipe, Process, OSError, Rd, RdCopy, Thread, Wr;

TYPE Format = {GIF, JPEG, XBM};

VAR
  Filters := ARRAY Format OF TEXT{
       (* GIF *) "giftoppm|ppmtopgm|pgmtopbm", 
       (* JPEG *) "djpeg -g|pgmtopbm", 
       (* XBM *)"xbmtopbm"};

PROCEDURE SetupGIF(filter: TEXT) =
  BEGIN
    Filters[Format.GIF] := filter;
  END SetupGIF;

PROCEDURE SetupJPEG(filter: TEXT) =
  BEGIN
    Filters[Format.JPEG] := filter;
  END SetupJPEG;

PROCEDURE SetupXBM(filter: TEXT) =
  BEGIN
    Filters[Format.XBM] := filter;
  END SetupXBM;

PROCEDURE giftopbm (source: Rd.T; dest: Wr.T)
  RAISES {Error, Thread.Alerted} =
  BEGIN 
   ToPBM(source, dest, Format.GIF);
  END giftopbm;

PROCEDURE jpegtopbm (source: Rd.T; dest: Wr.T)
  RAISES {Error, Thread.Alerted} =
  BEGIN
    ToPBM(source, dest, Format.JPEG)
  END jpegtopbm;

PROCEDURE xbmtopbm (source: Rd.T; dest: Wr.T)
  RAISES {Error, Thread.Alerted} =
  BEGIN
    ToPBM(source, dest, Format.XBM)
  END xbmtopbm;

PROCEDURE ToPBM (source: Rd.T; dest: Wr.T; format: Format)
  RAISES {Error, Thread.Alerted} =
  VAR hrChild, hwChild, hrSelf, hwSelf: Pipe.T;
  BEGIN
    TRY
      Pipe.Open(hr := hrChild, hw := hwSelf);
      Pipe.Open(hr := hrSelf, hw := hwChild);
      WITH p = Process.Create(
                 cmd := "sh",
                 params := ARRAY OF TEXT{"-c", Filters[format]},
                 stdin := hrChild, stdout := hwChild, stderr := NIL) DO
        TRY
          TRY hrChild.close() EXCEPT OSError.E => END;
          TRY hwChild.close() EXCEPT OSError.E => END;
          WITH wr = NEW(FileWr.T).init(hwSelf),
               rd = NEW(FileRd.T).init(hrSelf),
               fromSource = Thread.Fork(
                              NEW(Closure, rd := source, wr := wr)) DO
            TRY
              EVAL RdCopy.ToWriter(rd, dest);
            EXCEPT
            | Rd.Failure, Wr.Failure =>
                Thread.Alert(fromSource);
                RAISE Error
            | Thread.Alerted =>
                Thread.Alert(fromSource);
                RAISE Thread.Alerted
            END;
            EVAL Thread.AlertJoin(fromSource);
            CloseRd(rd);
          END
        FINALLY
          EVAL Process.Wait(p)
        END;
      END
    EXCEPT
      OSError.E => RAISE Error
    END
  END ToPBM;

TYPE
  Closure = Thread.Closure OBJECT
              rd: Rd.T;
              wr: Wr.T;
            OVERRIDES
              apply := PumpBits
            END;

PROCEDURE PumpBits (cl: Closure): REFANY =
  BEGIN
    TRY
      EVAL RdCopy.ToWriter(cl.rd, cl.wr);
      CloseWr(cl.wr);
    EXCEPT
      Rd.Failure, Wr.Failure, Thread.Alerted => 
    END;
    RETURN NIL;
  END PumpBits;

PROCEDURE CloseWr (wr: Wr.T) RAISES {Thread.Alerted} =
  BEGIN
    TRY Wr.Close(wr) EXCEPT Wr.Failure => END;
  END CloseWr;

PROCEDURE CloseRd (rd: Rd.T) RAISES {Thread.Alerted} =
  BEGIN
    TRY Rd.Close(rd) EXCEPT Rd.Failure => END;
  END CloseRd;

BEGIN
END ImageUtils.






