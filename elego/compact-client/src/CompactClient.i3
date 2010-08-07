(*---------------------------------------------------------------------------*)
INTERFACE CompactClient;

IMPORT Rd, TextTextTbl;
IMPORT MsgIF;

(*---------------------------------------------------------------------------*)
TYPE
  EB = {Ignore, Warn, Fail};
  (* defines the error behaviour: ignore all error, issue error messages
     or warnings, fail (and terminate the program if possible) *)

  T <: Default;

  Default = OBJECT
    connected  := FALSE; (* <=> an agent has been found and one or more
                                service objects have been imported *)
  METHODS
    init(host := "localhost"; msgif : MsgIF.T := NIL; eb := EB.Warn) : T;
    (* initialize the client (and connect) to the service, if possible *)

    rsrcText(name : TEXT; env : TextTextTbl.T := NIL) : TEXT;
    (* If the client is not connected, try to do so now. Return the
       named resource as text, or NIL if none is found. *)

    rsrcRd(name : TEXT; env : TextTextTbl.T := NIL) : Rd.T;
    (* If the client is not connected, try to do so now. Return a
       reader for the named resource, or NIL if none is found. *)

  END;

(*---------------------------------------------------------------------------*)
PROCEDURE RemoteRsrcText(VAR cl :  T; 
                         host   :  TEXT;
                         name   :  TEXT; 
                         env    :  TextTextTbl.T := NIL;
                         msgif  :  MsgIF.T := NIL; 
                         eb     := EB.Warn) : TEXT;
  (* This is a convenience procedure for standardized use in
     standalone programs. It allocates a new client object if it does
     not already exist, connects to the service if necessary, and
     the tries to fetch the resource as text. *)


(*---------------------------------------------------------------------------*)
PROCEDURE RemoteRsrcRd(VAR cl :  T; 
                       host   :  TEXT;
                       name   :  TEXT; 
                       env    :  TextTextTbl.T := NIL;
                       msgif  :  MsgIF.T := NIL; 
                       eb     := EB.Warn) : Rd.T;
  (* This is a convenience procedure for standardized use in
     standalone programs. It allocates a new client object if it does
     not already exist, connects to the service if necessary, and
     the tries to fetch the resource as reader. *)

END CompactClient.
