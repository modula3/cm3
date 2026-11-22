GENERIC INTERFACE CommandLoop(Context);
IMPORT Fmt;
IMPORT TextList;
IMPORT Term;
IMPORT Pathname;
FROM CommandLoop IMPORT Error;

(* "CommandLoop"s for the Layman *)

TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    init(ctx: Context.T; prompt := "> "): T;


    (* define a new command; "NIL" means do nothing besides pre/post steps *)

    c(cmd: Command; names: TEXT;
      simpleHelp, extendedHelp: TEXT := NIL);


    (* Define variables of various types.
       Return values should be stored in "ctx" and used as needed.
    *)

    integer(name: TEXT;
            desc: TEXT    := NIL;
            default       := 0;
            lo            := FIRST(INTEGER);
            hi            := LAST(INTEGER);
            base          := 10;
            userCanChange := TRUE           ): REF INTEGER;

    boolean(name: TEXT;
            desc: TEXT := NIL;
            default    := FALSE;
            chg        := TRUE   ): REF BOOLEAN;

    longReal(name: TEXT;
             desc: TEXT := NIL;
             default    := 0.0D0;
             style      := Fmt.Style.Auto;
             digits     := 10;
             chg        := TRUE  ): REF LONGREAL;


    (* set steps that will run before/after all subsequently defined
       commands (until the next step is set); turn off with "NIL" *)

    setPreStep(cmd: Command := NIL);
    setPostStep(cmd: Command := NIL);


    (* run the commandloop *)

    run(sourcePath: Pathname.T := NIL);
  END;

  Command = PROCEDURE (ctx: Context.T;  
                       args: TextList.T;
                       term: Term.T)
  RAISES {Error};

END CommandLoop.
