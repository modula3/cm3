(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* The "ClientInfoX" interface allows the server to create a
   useful subtype of "ClientInfo.T".
*)

INTERFACE ClientInfoX;

IMPORT ClientInfo, Callback;

CONST Brand = "ClientInfoX 1.0";

TYPE T <: Public;
     Public = ClientInfo.T OBJECT
     METHODS
       init (cb: Callback.T): T;
     END;

END ClientInfoX. 
