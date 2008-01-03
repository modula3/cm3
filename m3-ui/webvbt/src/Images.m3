(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Aug 27 13:22:06 PDT 1996 by najork                   *)
(*      modified on Wed Aug 16 15:59:32 PDT 1995 by mhb                      *)

MODULE Images;

IMPORT FileRd, FileWr, Image, ImageUtils, OSError, Pipe, Pixmap, Rd,
       TextRefTbl, TextRd, TextWr, Thread, Wr;

TYPE Format = {JPEG, GIF, XBM};

PROCEDURE FromJPEG (bits: TEXT): Pixmap.T
  RAISES {Error, Thread.Alerted} =
  BEGIN
    RETURN GetPixmap (bits, Format.JPEG)
  END FromJPEG;

PROCEDURE FromGIF (bits: TEXT): Pixmap.T
  RAISES {Error, Thread.Alerted} =
  BEGIN
    RETURN GetPixmap (bits, Format.GIF)
  END FromGIF;

PROCEDURE FromXBM (bits: TEXT): Pixmap.T
  RAISES {Error, Thread.Alerted} =
  BEGIN
    RETURN GetPixmap (bits, Format.XBM)
  END FromXBM;


(* This simple version of GetPixmap doesn't use pipes, which means that the
   bits get fondled an extra time as they go into a TEXT after being
   converted into pbm format. On the other hand, there isn't any thread 
   creation, and the version with pipes seems fragile (crashes with a
   "Thread.Alerted not handled") so perhaps this is the version to use. --mhb 7/22/95 *)
PROCEDURE GetPixmap (bits: TEXT; format: Format): Pixmap.T
  RAISES {Error, Thread.Alerted} =
  VAR
    rd : Rd.T;
    wr : Wr.T;
    raw: Image.Raw;
  BEGIN
    rd := TextRd.New(bits);
    wr := TextWr.New();
    TRY
      CASE format OF
      | Format.JPEG => ImageUtils.jpegtopbm(rd, wr);
      | Format.GIF => ImageUtils.giftopbm(rd, wr);
      | Format.XBM => ImageUtils.xbmtopbm(rd, wr);
      END;
      rd := TextRd.New(TextWr.ToText(wr));
      raw := Image.FromRd(rd);
      RETURN Image.Unscaled(raw)
    EXCEPT
      ImageUtils.Error, Image.Error, Rd.Failure => RAISE Error
    END
  END GetPixmap;


<*UNUSED*>
PROCEDURE GetPixmapWithPipes (bits: TEXT; format: Format): Pixmap.T
  RAISES {Error, Thread.Alerted} =
  VAR
    rd    : Rd.T;
    destWr: FileWr.T;
    destRd: FileRd.T;
    hr, hw: Pipe.T;
    pm    : Pixmap.T;
    raw   : Image.Raw;
  BEGIN
    rd := TextRd.New(bits);
    TRY Pipe.Open(hr, hw) EXCEPT OSError.E => RAISE Error END;
    TRY
      TRY
        destWr := NEW(FileWr.T).init(hw);
        destRd := NEW(FileRd.T).init(hr);
        WITH t = Thread.Fork(
                   NEW(Closure, rd := rd, wr := destWr, fmt := format)) DO
          TRY
            raw := Image.FromRd(destRd);
          EXCEPT
          | Rd.Failure, Image.Error => Thread.Alert(t); RAISE Error
          | Thread.Alerted => Thread.Alert(t); RAISE Thread.Alerted
          END;
          EVAL Thread.AlertJoin(t);
        END;
        pm := Image.Unscaled(raw);
        RETURN pm
      EXCEPT
        OSError.E => RAISE Error
      END
    FINALLY
      CloseRd(destRd)
    END;
  END GetPixmapWithPipes;

TYPE
  Closure = Thread.Closure OBJECT
              rd : Rd.T;
              wr : Wr.T;
              fmt: Format;
            OVERRIDES
              apply := PumpBits
            END;

PROCEDURE PumpBits (cl: Closure): REFANY =
  BEGIN
    TRY
      CASE cl.fmt OF
      | Format.JPEG => ImageUtils.jpegtopbm(cl.rd, cl.wr);
      | Format.GIF  => ImageUtils.giftopbm(cl.rd, cl.wr);
      | Format.XBM  => ImageUtils.xbmtopbm(cl.rd, cl.wr);
      END;
      CloseWr(cl.wr)
    EXCEPT
      ImageUtils.Error, Thread.Alerted =>
    END;
    RETURN NIL
  END PumpBits;

PROCEDURE CloseWr (wr: Wr.T) RAISES {Thread.Alerted} =
  BEGIN
    TRY Wr.Close(wr) EXCEPT Wr.Failure => END;
  END CloseWr;

PROCEDURE CloseRd (rd: Rd.T) RAISES {Thread.Alerted} =
  BEGIN
    TRY Rd.Close(rd) EXCEPT Rd.Failure => END;
  END CloseRd;



VAR 
  mu := NEW(MUTEX);
  cache := NEW(TextRefTbl.Default).init();

PROCEDURE FromCache (url: TEXT; VAR pm: Pixmap.T): BOOLEAN =
  VAR ref: REFANY; found: BOOLEAN;
  BEGIN
    LOCK mu DO
      found := cache.get(url, ref);
      IF found THEN pm := NARROW(ref, REF Pixmap.T)^ END
    END;
    RETURN found
  END FromCache;

PROCEDURE ToCache (url: TEXT; pm: Pixmap.T) =
  VAR ref := NEW(REF Pixmap.T);
  BEGIN
    ref^ := pm;
    LOCK mu DO EVAL cache.put(url, ref) END
  END ToCache;

BEGIN
END Images.
