(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* The "BoardServerX" interface allows the server to create a
   useful subtype of "BoardServer.T".
*)

INTERFACE BoardServerX;

IMPORT BoardServer;

TYPE T <: Public;
     Public = BoardServer.T OBJECT
     METHODS
       init (): T;
     END;

END BoardServerX. 
