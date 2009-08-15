
MODULE OpaqueExample EXPORTS Main;
IMPORT Person;

(* The main module of this program will be a client of the "Person"
   interface. *)

(* Assign new "Person.T" instances to three variables "jane", "june", and "john". 
   (Use combination of positional or keyword binding of parameters for fun.) *)

VAR
  jane: Person.T := Person.New(firstname := "June", lastname := "Doe", 
                    gender := Person.Gender.Female);
  june := Person.New("June","Doe", gender := Person.Gender.Female);
  john := Person.New("John","Doe", Person.Gender.Male);

(* Declare a constant for their address. *)
CONST
  address = "123 Main Street";

(* Call "Person.Describe" a few times. Note that "jane.firstname", 
   "jane.lastname", or even "june.fullname()" are not available, eventhough
   they are available within the implementation of "Person.T", since
   "Person.T" is a completely opaque type. *)

BEGIN

  Person.Describe (jane, "lives at " & address);
  Person.Describe (june, "lives at " & address);
  Person.Describe (john, "lives at " & address);

(* Indeed, even if we invent our own "Person" type compatible with 
   the implementation of "Person.T", the compiler would prevent us from passing
   it into "Person.Describe". In the way we have programmed the "Person" 
   interface, the only way to get valid "Person.T" objects is by 
   calling "Person.New". 

   By using opaque types, the "Person" interface achieves full encapsulation! *)

END OpaqueExample.
