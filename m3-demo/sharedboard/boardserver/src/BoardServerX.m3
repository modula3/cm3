(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

UNSAFE MODULE BoardServerX;
(* unsafe by virtue of "M3toC.StoT" *)

IMPORT SmallDB, FS, Atom, AtomList, Pickle, Scan, 
       OSError, Uerror, M3toC, Text,
       Board, BoardX, BoardServer, BoardTbl;

REVEAL T = Public BRANDED OBJECT
    mu: MUTEX;
    boards: BoardTbl.Default;
  OVERRIDES
    init := Init;
    create := Create;
    open := Open;
    save := Save;
    close := Close;
    remove := Remove;
  END;

CONST ExpectedBoards = 10;

PROCEDURE Init (bs: T): T =
  BEGIN
    bs.mu := NEW (MUTEX);
    bs.boards := NEW (BoardTbl.Default).init (ExpectedBoards);
    RETURN bs;
  END Init;

PROCEDURE Create (bs: T; boardName: TEXT): Board.T 
        RAISES {BoardServer.Failed} =
  VAR board: Board.T;
      pickle: SmallDB.T;
  BEGIN
    TRY
      LOCK bs.mu DO
        FS.CreateDirectory (boardName);
        pickle := SmallDB.New (boardName);
        board := NEW (BoardX.T).init (pickle, recover := FALSE);
        EVAL bs.boards.put (boardName, board);
        RETURN board;
      END;
    EXCEPT
    | OSError.E (code) =>
      RAISE BoardServer.Failed (OSError2Text (code));
    | SmallDB.CorruptedDB => 
      RAISE BoardServer.Failed ("SmallDB.CorruptedDB");
    | Pickle.Error =>
      RAISE BoardServer.Failed ("Pickle.Error");
    END;
  END Create;


PROCEDURE Open (bs: T; boardName: TEXT): Board.T 
        RAISES {BoardServer.Failed} =
  VAR board: Board.T;
      pickle: SmallDB.T;
  BEGIN
    TRY
      LOCK bs.mu DO
        IF bs.boards.get (boardName, board) THEN (* in-memory *)
          RETURN board;
        ELSE (* load pickle *)
          IF NOT FileExists (boardName & "/" & "Version_Number") THEN
            RAISE BoardServer.Failed (boardName & " not found");
          END;
          pickle := SmallDB.New (boardName);
          board := NEW (BoardX.T).init (pickle, recover := TRUE);
          EVAL bs.boards.put (boardName, board);
          RETURN board;
        END;
      END;
    EXCEPT
    | OSError.E (code) =>
      RAISE BoardServer.Failed (OSError2Text (code));
    | SmallDB.CorruptedDB => 
      RAISE BoardServer.Failed ("SmallDB.CorruptedDB");
    | Pickle.Error =>
      RAISE BoardServer.Failed ("Pickle.Error");
    END;
  END Open; 

PROCEDURE Save (bs: T; boardName: TEXT) 
        RAISES {BoardServer.Failed} =
  VAR board: Board.T;
  BEGIN
    TRY
      LOCK bs.mu DO
        IF bs.boards.get (boardName, board) THEN
          BoardX.Save (board);
        ELSE
          RAISE BoardServer.Failed ("Board not loaded");
        END;
      END;
    EXCEPT
    | OSError.E (code) =>
      RAISE BoardServer.Failed (OSError2Text (code));
    END;
  END Save;

PROCEDURE Close (bs: T; boardName: TEXT) 
        RAISES {BoardServer.Failed} =
  VAR board: Board.T;
  BEGIN
    TRY
      LOCK bs.mu DO
        IF bs.boards.get (boardName, board) THEN
          IF NOT BoardX.Busy (board) THEN
            EVAL bs.boards.delete (boardName, board);
            BoardX.Quit (board);
          END;
        ELSE
          RAISE BoardServer.Failed ("Board not loaded");
        END;
      END;
    EXCEPT
    | OSError.E (code) =>
      RAISE BoardServer.Failed (OSError2Text (code));
    END;
  END Close;

PROCEDURE Remove (bs: T; boardName: TEXT) 
    RAISES {BoardServer.Failed} =
  VAR board: Board.T;
  BEGIN
    TRY
      LOCK bs.mu DO
        IF bs.boards.get (boardName, board) THEN
          IF BoardX.Busy (board) THEN 
            RAISE BoardServer.Failed ("Board is busy")
          END;
          EVAL bs.boards.delete (boardName, board);
        END;
        (* delete directory and its contents *)
        VAR files := FS.Iterate (boardName);
            file: TEXT;
        BEGIN
          TRY
            WHILE files.next (file) DO
              FS.DeleteFile (boardName & "/" & file)
            END;
          FINALLY
            files.close ();
          END;
        END;
        (* do it again: for the ".nfs" file *)
        VAR files := FS.Iterate (boardName);
            file: TEXT;
        BEGIN
          TRY
            WHILE files.next (file) DO
              FS.DeleteFile (boardName & "/" & file)
            END;
          FINALLY
            files.close ();
          END;
        END;
        FS.DeleteDirectory (boardName);
      END;
    EXCEPT
    | OSError.E (code) =>
      RAISE BoardServer.Failed (OSError2Text (code));
    END;
  END Remove; 

PROCEDURE OSError2Text (al: AtomList.T): TEXT =
  VAR text := Atom.ToText (al.head);
  BEGIN 
    IF Text.Equal ("errno=", Text.Sub (text, 0, 6)) THEN
      TRY
        RETURN M3toC.StoT (Uerror.GetFrom_sys_errlist 
               (Scan.Int (Text.Sub (text, 6))))
      EXCEPT 
      | Scan.BadFormat =>
      END;
    END;
    RETURN text;
  END OSError2Text;

PROCEDURE FileExists (name:TEXT): BOOLEAN =
  BEGIN
    TRY
      EVAL FS.Status (name);
    EXCEPT
      OSError.E => RETURN (FALSE);
    END;
    RETURN TRUE;
  END FileExists;


BEGIN
END BoardServerX.
