INTERFACE CommandLoop;
IMPORT Term;
IMPORT Pathname;
IMPORT TextList;
TYPE
  T <: Public;
  Public = OBJECT METHODS
    init(prompt := "> ";
         help := "help ?";
         quit := "quit ^D";
         input := "input source";
         save := "save"): T;
    (* builtin features can be disabled by passing the empty TEXT. *)

    putCommand(names: TEXT; cmd: Command);
    (* "names" is a space-separated list of variants.
       "cmd" must not be "NIL".
    *)

    run(source: Pathname.T := NIL; term: Term.T := NIL);
    (* If "source#NIL" then read from file instead of from terminal.
       If "term=NIL" then use "Term.Default()". *)

    setPreStep(cmd: Command := NIL);
    setPostStep(cmd: Command := NIL);
    (* set steps that will run before/after all subsequently defined
       commands (until the next step is set); turn off with "NIL".
       These commands are anonymous and can act quietly if desired.
    *)
  END;


  (* User object.
     Default "execute" and "complete" gracefully do nothing.

     Typically, the user will override "Command" adding a context field,
     which the user initializes to some object
     that remembers the application state.
  *)
  Command <: CommandPublic;
  CommandPublic = OBJECT
    simpleHelp: TEXT:=NIL;(* one-line help: "<arg1> <arg2>.. -- explanation" *)
    hasExtendedHelp := FALSE;
    pre, post: Command := NIL; (* automatically set by "putCommand" *)

  METHODS
    execute(args: TextList.T; term: Term.T) RAISES {Error};
    (* "args.head" is the command name. *)

    extendedHelp(args: TextList.T): TEXT RAISES {Error};
    (* "args.head" is the command name. *)

    complete(VAR input: TEXT) RAISES {Error};
  END;

EXCEPTION
  Error(TEXT);

END CommandLoop.
