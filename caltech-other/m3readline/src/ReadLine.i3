(* $Id$ *)

INTERFACE ReadLine;
IMPORT IP, Rd, OSError, Pathname;
IMPORT NetObj;
IMPORT Thread;
IMPORT Displayer;
IMPORT ReadLineError AS Error, ReadLineLogFormat, ReadLineTable;

(* 
   synchronous communication with a human via a terminal.

   Connects via the external "readlinefe" program to the terminal.
   readlinefe provides GNU readline editing.  See the sources of
   that program for more details.

   Not many of GNU readline's editing tricks are supported, only
   the basics.

   The system can be disabled in one of two ways:

   RTParam  @M3noreadline
   Env var  NOM3READLINE (can be over-overriden with @M3readline)

 *)

TYPE
  Default <: U;

  NonReadLine <: U;
  (* a T that doesn't actually use readline.  Simplify debugging, 
     etc. Will be returned by Default's init under above-mentioned
     conditions. *)

  U = Public OBJECT METHODS
    init(startGetter := TRUE) : T RAISES { IP.Error, NetObj.Error, Thread.Alerted  };
    (* setting startGetter to FALSE is for debugging, won't start
       the keyboard reader (you have to do it manually) *)
  END;

  T = Public;

  Public = Displayer.T OBJECT METHODS
    startProc(doDebug := FALSE) RAISES { Error.E, NetObj.Error, Thread.Alerted  }; 
    (* start Unix process front-end *)

    (* all the following are synchronized w.r.t. each other... *)
    readLine() : TEXT 
      RAISES { Rd.EndOfFile, Error.E, NetObj.Error, Thread.Alerted   };

    setPrompt(to : TEXT) RAISES { Error.E, NetObj.Error, Thread.Alerted   };

    quit() RAISES { Error.E, NetObj.Error, Thread.Alerted   };

    startLogging(pn : Pathname.T; 
                 logAsync := FALSE; 
                 tblMode := TblMode.PromptUser) RAISES { OSError.E, NetObj.Error, Thread.Alerted   };
    (* set logAsync to TRUE if you want to log async messages too;
       tblMode says where to log tables to *)

    stopLogging() RAISES { Error.E, NetObj.Error, Thread.Alerted   };

    (* query methods *)
    getPrompt() : TEXT RAISES { NetObj.Error, Thread.Alerted  };

    (* global interface variables can be stored here *)
    setVar(name : TEXT; val : REFANY) RAISES { NetObj.Error, Thread.Alerted  };
    getVar(name : TEXT) : REFANY RAISES { NetObj.Error, Thread.Alerted  }; (* returns NIL if no mapping *)

    source(rd : Rd.T) RAISES { NetObj.Error , Thread.Alerted };
    (* source reads in a file, in a separate thread, until it hits EOF 
       and will return each
       line separately via the readLine method.  The implementation
       is such that reading from a pipe will work, as long as source has
       returned before the next call to readLine. *)
  END;

  LogFormat = ReadLineLogFormat.T;
  (* format for logging tables *)

  TblMode = { SameStream, PromptUser };
  (* where to log tables to *)

  Table = ReadLineTable.T;

CONST Null = VAL(0, CHAR); 
  (* Null may not appear in the arg to display or setPrompt *)


(* example code, from trading client:
  TRY
    WITH intf = NEW(UI).init(),
         rl = NEW(Default).init() DO
      rl.startProc();
      rl.display("Hello.\n");
      intf.run(rl)
    END
  EXCEPT
    ReadLineError.E =>
    Process.Crash("Error: " & 
      ErrorReport("unspecified error from user interface",NIL)) 
  END

*)

CONST Brand = "ReadLine";

END ReadLine.
