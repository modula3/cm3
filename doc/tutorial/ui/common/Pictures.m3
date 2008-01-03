MODULE Pictures;

IMPORT Trestle, VBT, TextVBT, RigidVBT;
IMPORT Text, Image, PaintOp, Rd, PixmapVBT, UnixUtils, FileStream;

PROCEDURE GetPixmap (rd: Rd.T): VBT.T =
  VAR
    image: Image.Raw;
    op   : PaintOp.T;
  BEGIN
    TRY
      image := Image.FromRd(rd)
    EXCEPT
      Rd.Failure, Image.Error => RETURN NIL
    END;
    TYPECASE image OF
    | Image.RawBitmap => op := PaintOp.BgFg;
    | Image.RawPixmap => op := PaintOp.Copy;
    ELSE                         <* ASSERT FALSE *>
    END;
    RETURN PixmapVBT.New(Image.Unscaled(image), op)
  END GetPixmap;

PROCEDURE Get (file: Text.T): VBT.T =
  <* FATAL UnixUtils.Error *>
  VAR rd: Rd.T;
  BEGIN
    IF NOT UnixUtils.ProbeFile(file, FALSE) OR UnixUtils.IsDirectory(file) THEN
      RETURN NIL
    END;
    TRY
      rd := FileStream.OpenRead(file);
      TRY RETURN GetPixmap(rd) FINALLY Rd.Close(rd) END
    EXCEPT
      Rd.Failure =>              (* Do nothing about it. *)
    END;
    RETURN NIL
  END Get;

BEGIN
  stampVBT := Get("../common/stamp.pbm");
  wind_millVBT := Get("../common/wind_mill.pbm");
END Pictures.
