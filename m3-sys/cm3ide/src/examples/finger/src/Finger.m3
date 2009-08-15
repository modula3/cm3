
(* 
   "Finger" is an example of a simple TCP/IP client program;
   it can use the finger daemon on a remote machine to check
   for a person's "finger" information. 

   Given a username and host, finger will connect to finger
   port of the host, send the username and receive the finger
   information. This is done by hooking up a TCP/IP connection
   to a corresponding reader and writer.
*)
 
MODULE Finger EXPORTS Main;

IMPORT TCP, IP, ConnRW;
IMPORT IO, Params;
FROM Text IMPORT FindChar, Sub, Length;
IMPORT Thread; <* FATAL Thread.Alerted *>

(* Port 79 is the internet standard for the finger socket port. *)

CONST 
  FingerPort = 79;

(* Variables "user", "host" are used by code in this module. *)

VAR 
  user := "";
  host := "localhost";
  addr : IP.Address;

(*----------------------------------- command line parameters -----*)

(* Exception "Problem" is used to flag problems with 
   the parameters. *)

EXCEPTION 
  Problem;

PROCEDURE GetUserHost() RAISES {Problem} = 
  (* Parse the "user" and "host" names from the command-line. 
     Raise "Problem" in case of error. *)
  BEGIN
    IF Params.Count > 2 THEN
      IO.Put ("Syntax: finger user@host\n"); RAISE Problem;
    ELSE
      IF Params.Count = 2 THEN user := Params.Get(1) END;
      WITH at = FindChar(user, '@') DO
        IF at = -1 THEN
          host := "localhost";
        ELSE
          host := Sub (user, at+1, Length(user));
          user := Sub (user, 0, at);
        END;
      END;
    END;
  END GetUserHost;

(*-------------------------------------- main implementation of finger -------*)

BEGIN
  TRY

    (* Get the "user" and "host". *)
    GetUserHost();
    IO.Put ("(Checking for " & user & " finger information on host " & host & ")\n");

    (* Lookup "host" by name. *)
    IF NOT IP.GetHostByName (host, addr) THEN
      IO.Put ("Could not lookup hostname " & host & "\n"); 
      RAISE Problem;
    END;

    (* Connect to the endpoint at port 79 of "host". 
       Get a reader and a writer to that port. *)
    VAR
      endpoint := IP.Endpoint {addr, FingerPort};
      service := TCP.Connect(endpoint);
      rd := ConnRW.NewRd(service);
      wr := ConnRW.NewWr(service);
    BEGIN

     (* Send the user name to the writer; 
        read the whole response until EOF from the reader. *)

      IO.Put (user & "\n", wr);
      WHILE NOT IO.EOF (rd) DO 
        IO.Put (IO.GetLine(rd) & "\n") 
      END
    END
    (* Check for possible errors. *)
  EXCEPT
  | IO.Error, IP.Error => IO.Put ("Problem communicating with " & host & "...\n");
  | Problem => (* Error has already been printed, do nothing. *)
  END
END Finger.

(* The same "finger" program works on top of Unix socket or Windows winsock 
   libraries without requiring source changes. *)

