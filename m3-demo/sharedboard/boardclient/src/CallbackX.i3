(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* The "CallbackX" interface allows the client to create a
   useful subtype of "Callback.T".
*)

INTERFACE CallbackX;

IMPORT View, Callback;

TYPE T <: Public;
     Public = Callback.T OBJECT
     METHODS
       init (v: View.T): T;
     END;

(* The "init" method initializes a new callback object with the client
   window that owns it.
*)

END CallbackX.
