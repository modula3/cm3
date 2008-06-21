

INTERFACE Person; IMPORT Wr;

(* The "Person" interface provides an opaque type "T" (also called an abstract
   data type). "Person.T" with two operations:

   <<UL>><<li>>
   "New" for creating new instances
   <<li>>
   "Describe" for printing a textual description of "Person.T" instances.
   <</UL>> 

   By definition, an opaque interface should describe precisely what the 
   implementation does without revealing anything about the structure 
   of the implementation or how it does the work. *)

TYPE
  T <: REFANY;
(* Declare "Person.T" as an opaque type.

   The statement "U <: V" means that "U" is a subtype of "V". When you see
   such a declaration, you can assume that an instance of "U" supports
   at least as many operations as an instance of "V". In this case, since 
   "V" is "REFANY", all you can assume about "Person.T" is that it is a 
   reference, so you can store it, or compare it with another reference 
   of the same type for equality. *)

  Gender = {Female, Male};
(* "Person.Gender" is an enumeration type with elements "Female" and "Male". *)

PROCEDURE New (firstname, lastname: TEXT;
               gender: Gender): T;
(* Create a new "Person.T" given a first name, a last name, and a "gender". *)

PROCEDURE Describe(person: T; desc: TEXT; wr: Wr.T := NIL);
(* Write a textual description, "desc" of a "Person.T" object "person" to 
   to the writer stream "wr". 
   If "wr" is not specified, write the description to the standard output. *)

END Person.

