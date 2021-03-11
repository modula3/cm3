(* $Id$ *)

INTERFACE ReadLineUI;
IMPORT ReadLine, TextReader, ReadLineHelpNode;
IMPORT NetObj;
IMPORT Thread;
IMPORT ReadLineError;

EXCEPTION Error(TEXT);
EXCEPTION Quit;

(* using ReadLine.Public so it can be used with Network Objects *)

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(myName : TEXT; helpRoot : ReadLineHelpNode.T := NIL) : T;

    run(intf : ReadLine.Public) RAISES { ReadLineError.E, NetObj.Error, Thread.Alerted };
    
    getIntf() : ReadLine.Public;

    command(r : TextReader.T) : BOOLEAN RAISES { Error, 
                                                 Quit,
                                                 NetObj.Error,
                                                 Thread.Alerted };
    (* 
       this method to be overridden by implementors.  The return
       value is provided so that subtyping implementors can determine
       whether or not another implementation has executed the command. 
       
       The default version provides the following commands:
       
       quit            ->    RAISEs Quit
       log "filename"  ->    start ReadLine logging of output
       nolog           ->    stop ReadLine logging

       optionally, if help arg is defined on init

       help            ->    run help system
    *)

    chain(t : T); (* chain another interface at lower priority:
                     commands will go to this interface first, to the
                     chained one later. *)
  END;

CONST Brand = "ReadLineUI";

END ReadLineUI.
