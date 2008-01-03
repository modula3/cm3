(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: FileReWr.m3,v 1.2 2001-09-19 15:03:34 wagner Exp $ *)

MODULE FileReWr;
IMPORT Rd, Wr, Pathname;
IMPORT TextWr;
IMPORT WrClass;
IMPORT FileRd, FileWr;
IMPORT OSError, Thread;
IMPORT Text;
REVEAL
  T = TextWr.T BRANDED OBJECT
    pathName: TEXT;
    closeMutex: MUTEX;
  OVERRIDES
    close := Close;
  END;

PROCEDURE Open(p: Pathname.T): T =
  BEGIN
    RETURN NEW(T,
               closeMutex := NEW(MUTEX),
               pathName := p).init();
  END Open;

PROCEDURE Close(self: T) RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    newText, oldText: TEXT;
    oldFile: FileRd.T;
    newFile: FileWr.T;
  BEGIN
    LOCK self.closeMutex DO
      WrClass.Unlock(self);
      newText := TextWr.ToText(self);
      WrClass.Lock(self);
      TextWr.T.close(self);
    END;
    TRY
      oldFile := FileRd.Open(self.pathName);
      oldText := Rd.GetText(oldFile, Text.Length(newText)+1);
      Rd.Close(oldFile);
    EXCEPT
    | OSError.E => oldText := NIL;
    | Rd.Failure(a) => RAISE Wr.Failure(a);
    END;
    IF oldText = NIL OR NOT Text.Equal(oldText, newText) THEN
      TRY
        newFile := FileWr.Open(self.pathName);
      EXCEPT
      | OSError.E => RAISE Wr.Failure(NIL);
      END;
      Wr.PutText(newFile, newText);
      Wr.Close(newFile);
    END;
  END Close;
BEGIN
END FileReWr.
