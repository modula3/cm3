INTERFACE Cooker;
IMPORT Term;
IMPORT TextList;

TYPE
  Completer = OBJECT METHODS do(VAR input: TEXT); END;

PROCEDURE Input(prompt:="> ";
                completer: Completer := NIL;
                previous: TextList.T := NIL;
                default:="";
                fatalControl:=TRUE;
                emptySelectsLast:=FALSE;
                term: Term.T := NIL): TEXT;


PROCEDURE Print(t:="");

END Cooker. 
