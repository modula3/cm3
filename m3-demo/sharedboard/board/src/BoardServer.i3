(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

INTERFACE BoardServer;

IMPORT NetObj, Thread, 
       Board;

TYPE T = NetObj.T OBJECT 
  METHODS
    create (boardName: TEXT): Board.T 
        RAISES {Failed, NetObj.Error, Thread.Alerted};
    open (boardName: TEXT): Board.T 
        RAISES {Failed, NetObj.Error, Thread.Alerted};
    save (boardName: TEXT) 
        RAISES {Failed, NetObj.Error, Thread.Alerted};
    close (boardName: TEXT) 
        RAISES {Failed, NetObj.Error, Thread.Alerted};
    remove (boardName: TEXT) 
        RAISES {Failed, NetObj.Error, Thread.Alerted};
  END;

EXCEPTION Failed (TEXT);

(* The "create" method creates a new board and returns it. It takes a
   directory pathname as input, which is also the name of the board
   created. If the pathname is relative, it is interpreted relative to
   the directory from which the server was run.
   An exception is raised if the directory already exists or
   cannot be created.
   The state of the board is saved in the specified directory.

   The "open" method returns an already created board. (If the board is
   not already {\em loaded} in the memory of the server, it gets loaded from
   the saved state. Subsequent "open" calls use the in-memory copy.)

   The stable state of the board is updated in the background.
   The "save" method saves a snapshot of the board's state on disk,
   thus ensuring that the state is saved. (In this respect, it is like
   "flush" operation.) It also ensures that recovery will be faster
   because the log is truncated.
   "NotFound" is raised if the board has not been loaded into the
   server's memory. 

   The "close" method provides an opportunity for the server to unload
   the board. The server may unload a board only if it is not busy, that
   is, if the board has no clients.

   The "remove" method removes the board from existence: both the
   in-memory and the stable copies. 
*)

END BoardServer.
