(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Tue Dec  1 17:36:35 PST 1992 by mhb    *)
(*      modified on Wed Oct 28 14:01:48 PST 1992 by steveg *)

MODULE VBTKitResources;

IMPORT Image, Pixmap, Rd, Rsrc, Thread, VBTKitBundle;

VAR 
  path := Rsrc.BuildPath("$VBTKITPATH", VBTKitBundle.Get());

PROCEDURE Get (name: TEXT): TEXT =
  <* FATAL Rsrc.NotFound, Thread.Alerted, Rd.Failure *>
  BEGIN
    WITH t = Rsrc.Get(name, path) DO
      IF t = NIL THEN <* ASSERT FALSE *> END;
      RETURN t
    END
  END Get;

PROCEDURE GetPixmap (name: TEXT): Pixmap.T =
  <* FATAL Rsrc.NotFound, Thread.Alerted, Image.Error, Rd.Failure *>
  VAR rd := Rsrc.Open(name, path);
  BEGIN
    TRY
      RETURN Image.Scaled(Image.FromRd(rd))
    FINALLY
      Rd.Close(rd)
    END;
  END GetPixmap;

BEGIN
END VBTKitResources.

