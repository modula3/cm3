(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

INTERFACE AtomicClientList;

IMPORT ClientInfo, ClientInfoList;

TYPE T <: Public;
     Public = MUTEX OBJECT 
       list: ClientInfoList.T := NIL;
     METHODS
       add (ci: ClientInfo.T);
       remove (ci: ClientInfo.T);
     END;

END AtomicClientList.
