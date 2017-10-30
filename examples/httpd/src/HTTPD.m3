
MODULE HTTPD EXPORTS Main;
IMPORT TCP, IP, ConnRW;
IMPORT Rd, Wr, IO, Lex, FileRd;
IMPORT Thread, OSError, Text, Params, Process, Pathname;

(* Use "http://hostname:80/" to access this server. *)

CONST
  HTTP_Port = 80;   (* Each student must pick a different one. *)

PROCEDURE Error (wr: Wr.T; msg: TEXT) RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    Wr.PutText (wr, "400 " & msg );
    Wr.Flush (wr);
  END Error;

(* You should review the "TCP" and "IP" intefaces for more information
   regarding the TCP/IP calls. *)

(* First, create an endpoint on the "HTTP_Port". *)

VAR
  endpoint := IP.Endpoint {IP.GetHostAddr(), HTTP_Port};
  connector: TCP.Connector;
  server: TCP.T;
  rd: Rd.T; wr: Wr.T;
  path: TEXT;


BEGIN
  TRY

(* Get a connector *)
    connector := TCP.NewConnector(endpoint);

    LOOP

(* Use TCP.Accept to create a new server that can handle calls. *)
      server := TCP.Accept(connector);

      (* Create a reader and a writer to the server. *)
      rd := ConnRW.NewRd(server);
      wr := ConnRW.NewWr(server);

      TRY
        TRY

(* Look for a "GET pathname" request. Parse pathname and print it. *)
          Lex.Match (rd, "GET ");
          path := Lex.Scan (rd);
          IO.Put ("path=" & path & "\n");
          IO.Put (Rd.GetLine(rd) & "\n");

          (* send protocol reply *)
          Wr.PutText (wr, "HTTP/1.1 200\n\n");

(* If there is a request for root, return a welcome string, otherwise
   find the file residing in a subdirectory. *)

          IF Text.Equal (path, "/") THEN
            Wr.PutText (wr, "<H1>Welcome to our web server!</H1>" &
                            "Try <a href=welcome.html>this link</a>.\n");
          ELSE
            WITH file = FileRd.Open (Text.Sub (path, 1, Text.Length(path))) DO
              TRY
                WITH content = Rd.GetText (file, LAST(CARDINAL)) DO
                  Wr.PutText (wr, content);
                END
              FINALLY Rd.Close(file);
              END;
            END
          END;
	
(* Flush the writer so that the browser can see the results. *)

          Wr.Flush (wr);
        EXCEPT
          Lex.Error => Error (wr, "Only GET methods are supported\n");
        | OSError.E => Error (wr, "File not found or no permission.\n");
        | Rd.EndOfFile => Error (wr, "Request terminated prematurely.\n");
        END;
      FINALLY

(* Make sure to clean up on your way out. *)

        Rd.Close (rd);
        Wr.Close (wr);
        TCP.Close (server);
      END;

    END
  EXCEPT

(* Catch all the errors, of course... *)

  | Thread.Alerted => IO.Put ("Thread was alerted\n");
  | IP.Error => IO.Put ("IP error\n");
  | Rd.Failure, Wr.Failure => IO.Put ("Rd/Wr failure\n");
  END;

END HTTPD.

