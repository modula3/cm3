
(* This example showcases pickles for saving objects into streams. *)

MODULE PickleExample EXPORTS Main;

IMPORT Pickle, Wr, FileWr, Rd, FileRd;
(* Import the "Pickle" interface to to take snapshots of 
   objects and turn snapshots back to live objects, "Wr"
   and "FileWr" to write snapshots to files, and "Rd",
   "FileRd" to read snapshots from files. *)

IMPORT Atom, AtomList;
(* Import the "Atom" interface, and list of "Atom"s. 
   An "Atom" is like a symbol, you can convert text to "Atom" and 
   then compare it with other "Atom"s efficiently, instead of using
   text operations. *)

IMPORT Action, AtomActionTbl;
(* Import the "Action" interface, defined in this package, and the
   "AtomActionTbl", a table mapping of atom->action. *)

IMPORT Process, IO; <* FATAL IO.Error *>
(* Utilities for the interaction with user. *)

(*----------------------------------------------- AtomList operations ------*)

(* "Contains", "Insert" and "Print" are utility functions which
   call "AtomList" operations. *)

PROCEDURE Contains(list: AtomList.T; atom: Atom.T): BOOLEAN = 
(* Check to see if a list contains a particular atom. *)
  BEGIN
    RETURN AtomList.Member (list, atom);
  END Contains;

PROCEDURE Insert (VAR list: AtomList.T; atom: Atom.T) =
(* Insert an element into the list. *)
  BEGIN
    IF NOT Contains(list, atom) THEN
      list := AtomList.Cons (atom, list);
    END
  END Insert;

PROCEDURE Print(x: AtomList.T) =
(* Print out all elements of the list by iterating over its members. *)
  BEGIN
    WHILE x # NIL DO
      IO.Put (Atom.ToText (x.head) & " ");
      x := x.tail;
    END;
  END Print;


(*----------------------------------------------- Command operations ------*)

(* Definition of what commands should do.  
   "Commands", "Names" and "Actions" define initial values for the action 
   table. *)

TYPE  
  Commands =  { Show,   Quit,   Reset,   Help,  Load,  Save };

CONST 
  Names = ARRAY Commands OF TEXT {"show","quit","reset","help","load","save"};
  Actions = ARRAY Commands OF Action.T {Show, Quit, Reset, Help, Load, Save};

(* Each procedure defines what each action should do. Note that "Actions"
   includes elements that happen to be procedures. Note also that
   "Action.T" is the same as "PROCEDURE ()" hence, we can assign any
   of "Quit", "Rest", "Help", or "Show" to Actions. *)

PROCEDURE Quit()  = BEGIN Process.Exit(0); END Quit;
PROCEDURE Reset() = BEGIN input_set := NIL; END Reset;
PROCEDURE Show()  = BEGIN Print(input_set); IO.Put ("\n"); END Show;
PROCEDURE Help()  = BEGIN IO.Put("Commands: show, reset, help, quit, " &
                                 "load, save.\n" &
                                 "Otherwise: insert into the list.\n"); 
                    END Help;

(* Procedures "Save" and "Load" use pickels to save and load
   the database. *)


(*----------------------------------------------- Pickle operations ------*)

CONST
  DB = "db";

PROCEDURE Save() =  BEGIN WITH wr = IO.OpenWrite(DB) DO
                            Pickle.Write (wr, input_set);
                            Wr.Close (wr);
                          END;
                    END Save;

PROCEDURE Load() =  BEGIN WITH rd = IO.OpenRead (DB) DO
                            input_set := Pickle.Read (rd);
                            Rd.Close (rd);
                          END;
                     END Load;



(*----------------------------------------------- Main program ------*)

(* "command_table" is an atom->action table. 
   "input_set" is an atom list, containing all the elements 
   that will be entered. *)

VAR
  command_table              := NEW(AtomActionTbl.Default).init();
  input_set     : AtomList.T := NIL;

BEGIN

(* Initialize Commands. *)

  FOR i := FIRST(Commands) TO LAST(Commands) DO
     EVAL command_table.put (Atom.FromText(Names[i]), Actions[i]);
  END;

  IO.Put ("Welcome to the atomic database.\n");
  IO.Put ("Try any of commands: show quit reset help.\n");
  IO.Put ("Any other string will be entered into the database.\n\n");

(* Loop, get the command line. If it's a command, do it. Otherwise insert
   the command line into the "input_set". *)

  LOOP
    IO.Put ("persistent atom-db > ");
    IF IO.EOF () THEN EXIT END;
    VAR
      cmd := IO.GetLine();
      atom := Atom.FromText(cmd);
      action: Action.T;
    BEGIN

    (* If "atom" is in the command_table then run the corresponding
       "action()" otherwise, "Insert" the "atom" into the "input_set". *)

      IF command_table.get(atom, action) 
         THEN action();
         ELSE Insert(input_set, atom);
      END;
    END;
  END;

END PickleExample.
