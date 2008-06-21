
MODULE Copy EXPORTS Main;
IMPORT FakeOS, Params, Process, IO;

(* The "Copy" program imports a number of interfaces. "FakeOS" is an
   interface in this package; "Params" provides access to command-line 
   parameters; "Process" provides system management calls, such as
   "Process.Exit" which will end our program. Finally, "IO" is the
   basic input/output interface. Note that system and user-defined 
   interfaces are not distinguished. See the <<a href=FakeOS>>"FakeOS" 
   interface<</a>> as an example of a user-defined interface.

   The program follows a simple logic:
|     Make sure that the user has specified arguments correctly. 
|     If parameters are wrong, return an error code. 
|     Otherwise, pass them along to "FakeOS.Copy". *)

BEGIN
  IF Params.Count # 3 THEN
    IO.Put ("Syntax: copy <source> <destination>\n");
    Process.Exit (2);
  END;

(* Use the "WITH" statement to bind new names to parameters. 
   Here "source" is bound to the first parameter and 
   "destination" to the second. By using the "WITH" statement
   you can avoid creating new variables while keeping your
   code easy to read. *)

  WITH source = Params.Get(1) DO
     WITH destination = Params.Get(2) DO
       FakeOS.Copy (source, destination);
     END;
  END;

END Copy.

