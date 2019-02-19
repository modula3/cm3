(* $Id$ *)

INTERFACE Displayer;
IMPORT NetObj;
IMPORT ReadLineTable, ReadLineLogFormat, ReadLineError AS Error;
IMPORT Thread, Rd;

TYPE 
  T = NetObj.T OBJECT METHODS
    display(what : TEXT) RAISES { Error.E, NetObj.Error, Thread.Alerted   };
    (* display may raise Error.E either on a failure to communicate with
       interface, or on a failure to log, without distinction *)

    displayTbl(what : ReadLineTable.T;
               logFormat := ReadLineLogFormat.T.Normal) RAISES { Error.E, Rd.EndOfFile, NetObj.Error, Thread.Alerted   } ;
    (* may raise Rd.EndOfFile, e.g., if user quits interface during a dialog *)

    (* but this one isn't... *)
    asyncDisplay(what : TEXT) RAISES { Error.E, NetObj.Error, Thread.Alerted };

  END; 

END Displayer.
