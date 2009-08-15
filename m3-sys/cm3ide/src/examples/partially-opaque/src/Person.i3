
INTERFACE Person; 

(* The "Person" interface declares the type partially-opaque type "T",
   with two operations "init" and "fullname". 

   The idiom "T <: Public; Public = OBJECT ... END" states that type
   "Person.T" supports at least operations "init" and "fullname", without
   revealing the exact structure of "Person.T", or revealing what other
   methods "Person.T" may support. *)

TYPE
  T <: Public; 
  (* "T" is a subtype of type "Public" which will be defined next. *)

  Public = OBJECT 
  METHODS
    init(firstname, lastname: TEXT; gender: Gender): T;
    (* Initialize using "firstname", "lastname", and "gender".
       Return the initialized object. The "init" method is used
       by convention to included initialization for types
       that can be instantiated, such as "Person.T". *) 

    fullname(): TEXT;
    (* Return the full name of the "Person.T" object in question. *)
  END;

  Gender = {Female, Male};
  (* Male or female flag. *)

(* Unlike the case for fully opaque types, in this interface, 
   there is no need for providing a "New" procedure; clients of
   this interface can freely instantiate "Person.T" using the 
   built-in "NEW" operation. 

   "Person.T" also provides the "fullname()" method, which returns 
   a text string containing the name of a person; hence the 
   "Describe" method can now be implemented in the client code 
   as well.

   This also means that clients of this interface can create
   new subtypes of "Person.T". By calling "Person.T.init", such
   subtypes can assure that the "Person.T" portion of the object
   is initialized properly. 

   See the <<a href=./Employee.i3>>"Employee"<</a>> as an example. *)

END Person.

