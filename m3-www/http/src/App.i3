(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Tue Aug 27 16:02:17 PDT 1996 by steveg *)

INTERFACE App;

EXCEPTION
  Error(TEXT); (* Error is almost never raised directly.  It
                  is raised through a call on the log method
                  with an Error status *)

CONST
  LogStatusText = ARRAY LogStatus OF TEXT{"Verbose", "Debug", "Status", "Error"};

TYPE
  LogStatus = {Verbose, Debug, Status, Error};
  Log <: LogPublic;
  LogPublic = OBJECT
  METHODS
    log(msg: TEXT; status: LogStatus) RAISES {Error};
    (* "log" is used to report a message "msg" and a "status".  If
       status is Error, then the "log" method
       should raise "Error" with "msg" as its argument. *)
  END;

VAR
  defaultLog: Log;
  (* writes Verbose, Debug and Status messages to stdout.
     writes Error messages to stderr and RAISES Error.
   *)
  nullLog: Log;
  (* throws away all input *)

CONST
  AnyArgument = NIL;

TYPE
  ArgSource = {Switch, Env, Config, Default, None};
  (* The argument sources correspond to command line switches,
     environment variables, configuration file entries, user interface
     objects and default values respectively. *)

  ArgHandler <: ArgHandlerPublic;
  ArgHandlerPublic = OBJECT
    id: INTEGER := 0;
    (* a local identifier that makes it convenient for a single
       procedure to distinguish arguments *)
    hasParam: BOOLEAN := TRUE;
    paramName: TEXT := "";
    (* "hasParam" is TRUE if the option expects a parameter, otherwise
       it is treated as a boolean value. "paramName" is the string
       displayed for the parameter if there is an error to report*)
    default: TEXT := "";
    (* The default value for the argument *)
  METHODS
    init(switchName, envName, configName: TEXT := NIL;
         register := TRUE): ArgHandler;
    (* initialize an arg handler. The names give the name of the
       element that is the source of the argument in that 
       environment. If
       register is TRUE, then call RegisterArgHandler with
       the handler. *)

    set(src: ArgSource; value: TEXT; log: Log) RAISES{Error};
    (* the "set" method is called at startup time.  It is
       only called with a single "src", but might be called
       multiple times if there are multiple instances of an
       argument.  

       The different argument sources have different
       priority.  The command line switches have top priority, then 
       environment variables, then config file entries and finally
       default values. 

       If "switchName" is "AnyArgument" then "set" is called 
       once for each unnamed argument in the command line.

       "src" is the source of the initialization.  
       "value" is the value of the argument.  If "self.hasParam" is 
         FALSE, the "value" is "TRUE" if the option is mentioned.

    *)
  END;


(* There is a pre-defined argument giving the config file name:
     switch argument:   -config <configFile>
     env argument:      HTTP_CONFIG <configFile>
     default value:     .http_config
 *)
PROCEDURE RegisterArgHandler(handler: ArgHandler);

(* Initialize the program arguments from the command line
   switches, environment variables, config file and defaults.
   If log = NIL then log := HTTP.defaultLog
   If defaultConfigFile = NIL then defaultConfigFile := ".http_config"
   If logConfiguration then all of the argument values are written
      to the log 
*)
PROCEDURE InitializeArguments(log: Log := NIL; 
                              defaultConfigFile: TEXT := NIL;
                              logConfiguration: BOOLEAN := TRUE) 
  RAISES {Error};

PROCEDURE Debug(): BOOLEAN;
  (* debug = TRUE AND noDebug = FALSE *)
PROCEDURE Verbose(): BOOLEAN;
  (* verbose = TRUE AND noVerbose = FALSE *)

TYPE
  Value = {Debug, NoDebug, Verbose, NoVerbose};
PROCEDURE SetValue(value: Value; f: BOOLEAN);
(* These procedures check the value of the standard arguments for debug
   and verbose modes.   There are arguments for setting debug and verbose.

   switch     env            config     default
   -debug     APP_DEBUG      debug:     "FALSE"
   -noDebug   APP_NODEBUG    noDebug:   "FALSE"
   -verbose   APP_VERBOSE    verbose:   "FALSE"
   -noVerbose APP_NOVERBOSE  noVerbose: "FALSE"
*)

PROCEDURE GetHostName(ipAddr: BOOLEAN := FALSE): TEXT;
(* if "ipAddr" then the local IP address is returned 
   (i.e. 16.0.1.135).  Otherwise the host name is return
   (i.e. reddog.pa.dec.com). *)

END App.
