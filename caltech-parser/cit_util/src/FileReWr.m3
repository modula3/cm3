(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE FileReWr;
IMPORT Debug;
IMPORT Rd, Wr, Pathname;
IMPORT TextWr;
IMPORT WrClass;
IMPORT FileRd, FileWr;
IMPORT OSError, Thread;
IMPORT Text;
IMPORT TextSubs;
REVEAL
  T = TextWr.T BRANDED OBJECT
    pathName: TEXT;
    closeMutex: MUTEX;
    errorMsg: TEXT;
  OVERRIDES
    close := Close;
  END;

PROCEDURE Open(p: Pathname.T; errorMsg: TEXT := NIL): T =
  BEGIN
    RETURN NEW(T,
               closeMutex := NEW(MUTEX),
               pathName := p,
               errorMsg := errorMsg).init();
  END Open;

PROCEDURE Close(self: T) RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    newText, oldText: TEXT;
    oldFile: FileRd.T;
    newFile: FileWr.T;

  PROCEDURE Error() RAISES {Wr.Failure} = 
    BEGIN
      IF self.errorMsg = NIL THEN
        RAISE Wr.Failure(NIL);
      ELSE
        WITH ts = NEW(TextSubs.T).init() DO
          ts.add("%filename%", self.pathName);
          Debug.Error(ts.apply(self.errorMsg));
        END;
      END;
    END Error;

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
    | Rd.Failure => Error();
    END;
    IF oldText = NIL OR NOT Text.Equal(oldText, newText) THEN
      TRY
        newFile := FileWr.Open(self.pathName);
      EXCEPT
      | OSError.E => Error();
      END;
      Wr.PutText(newFile, newText);
      Wr.Close(newFile);
    END;
  END Close;
BEGIN
END FileReWr.
