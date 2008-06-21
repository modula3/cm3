
MODULE PartiallyOpaque EXPORTS Main;
IMPORT Person, Employee;
IMPORT IO;

(* The main module of this program will be a client of the "Person"
   interface. *)

(* Procedure "Describe" uses the "fullname" method of "Person.T" to print
   out a textual description of a person. Note that "Person.T" does not
   reveal its internal structure, but it does reveal the "fullname" method,
   which is enough to allow us to produce the "Describe" procedure within
   this module, instead of making it an internal part of the implementation
   of "Person". *)

PROCEDURE Describe(p: Person.T; desc: TEXT) =
(* Write a textual description, "desc" of a "Person.T" object "person"
   to standard output. *)
BEGIN
  IO.Put (p.fullname() & " is " & desc & ".\n");
END Describe;


(* Assign new "Person.T" instances to three variables "jane", "june", "john", and "jack".
   Note that the "v := NEW(T).init(...)" idiom in declaring these instances. *)

VAR
  john := NEW(Person.T).init("John", "Doe", Person.Gender.Male);
  jane := NEW(Employee.T).init("Jane","Doe", Person.Gender.Female, "ACME Ltd");
  june := NEW(Employee.T).init("June","Doe", Person.Gender.Female, "Mass. State");
  jack := NEW(Employee.T).init("Jack","Smith", Person.Gender.Male, "ACME Ltd");

(* Assign new "Person.T" with a speical "fullname" method to the variable 
   "moda". *)

VAR
  moda := NEW(Person.T, fullname := Modanna);

PROCEDURE Modanna(<*UNUSED*>self: Person.T): TEXT = BEGIN RETURN "Modanna" END Modanna;

(* The main body will make a few calls to "Describe". Note the use of "john.fullname()"
   (to call a method defined on a "Person.T", and "Person.T.fullname(june)" (to call
   a "Person.T" method on an "Employee.T". In the case of calling supertype methods,
   the compiler will check to make sure that you are actually making calls to a supertype. *)

CONST
  address = "123 Main Street";

BEGIN
  Describe (moda, "a pop icon");
  Describe (john, "a resident at " & address);
  Describe (jane, "a resident at " & address);
  Describe (june, "a resident at " & address);
  Describe (jack, john.fullname() & "'s brother.");
  Describe (june, "called " & Person.T.fullname(june) & " outside work");
END PartiallyOpaque.
