

MODULE Objects EXPORTS Main; IMPORT IO;

(* This program introduces you to the basics of 
   object-oriented programming with CM3. 

   An object associates some 'state' with some 
   'behavior'. A Modula-3 object is a record--its state--paired 
   with a method suite--its behavior--or a collection 
   of procedures that operate on the object. *)

(* The type "Person" declares a new object type with 
   fields "firstname", "lastname", and "gender". (The type "Gender"
   is an enumeration field.) "Person" also defines a method "fullname()" which 
   is implemented by procedure "PrintName". *)

TYPE
  Person = OBJECT
    firstname, lastname: TEXT;
    gender: Gender;
 METHODS
   fullname(): TEXT := PrintName;
 END;

 Gender  = {Female, Male};

PROCEDURE PrintName (self: Person): TEXT =
  CONST Title = ARRAY Gender OF TEXT {"Ms.", "Mr."};
  BEGIN
    RETURN Title[self.gender] & " " & self.firstname & " " & self.lastname;
  END PrintName;


(* Any code that can see the declaration of the object type "Person" can 
   create new instances of that type. (You can use interfaces to control
   the visibility of object types, but for now let's ignore visibility issues.)
   So, anywhere in this module, you can create a new instance of the 
   type "Person". Here the new object is assigned to the variable "john".  *)

VAR
  john := NEW(Person, 
              gender := Gender.Male, 
              firstname := "John", lastname := "Smith");


(* You can pass objects as parameters to other procedures (and hence
   to other methods, since they are just fancy procedures. Here is one such 
   procedure, "Describe" which takes a "Person" object and a text description
   and prints a line to the standard output using the "fullname()" method. *)


PROCEDURE Describe (person: Person; description: TEXT) = 
BEGIN
  IO.Put (person.fullname() & " is " & description & ".\n");
END Describe;

(* "Describe" calls the "fullname" method of the person. Of course, different
   "Person" objects can have different "fullname" methods, so, ultimately you can pass
   different subtypes of "Person" into this procedure. Here we create one such subtype,
   named "Employee" which has some additional fields. Note that it shares the same 
   implementation as "Person" for the "fullname()" method, but it introduces a new
   method named "company_location()", which we do not specify here. *)

TYPE
  Employee = Person OBJECT 
    company: TEXT;
  METHODS
    company_location(): TEXT;
  END;

(* Here, we create a new instance of this type, named "jane".
   You can list the fields of an object in any order when you initialize it. *)

VAR
  jane := NEW(Employee, 
              firstname := "Jane",
	      lastname := "Doe",
	      company:= "ACME Ltd",
              gender := Gender.Female);


(* You can create new subtypes that override
   existing methods. In this case, the "fullname()" method of
   "Doctor" objects, skips the first name, and uses a professional
   title for refering to a "Doctor" person, implemented via
   "PrintDoctorName". (We use default assignment for "title".
    Note that "PrintDoctorName"'s self argument is of type
    "Doctor".) *)

TYPE
  Doctor = Person OBJECT
    title: TEXT := NIL;
  OVERRIDES
    fullname := PrintDoctorName;
  END;

PROCEDURE PrintDoctorName(self: Doctor): TEXT =
  VAR
    result: TEXT := "Dr. " & self.lastname;
  BEGIN
    IF self.title # NIL THEN
      result := result & ", " & self.title & ", ";
    END;
    RETURN result;
  END PrintDoctorName;

(* Let's create a couple of instances of Doctor. *)

VAR
  dr_who := NEW(Doctor, 
                lastname := "Who", 
                title := "Time Lord");

  dr_quinn := NEW(Doctor, 
                  lastname := "Quinn", 
                  title := "Medicine Woman");


(* There is also a short-hand for creating one-of-a-kind objects
   types as part of a "NEW" call. That's how "joe" gets created. *)

VAR
  joe      := NEW(Person, firstname := "Joe", lastname := "Schmo",
                   fullname := AnAverage);

PROCEDURE AnAverage(self: Person): TEXT =
BEGIN
  RETURN "An average " & self.firstname & " " & self.lastname;
END AnAverage;

(* 
   Finally, make a few calls to "Describe" just to show that
   it works. *)
BEGIN
  Describe (john, "a nice person");
  Describe (jane, "an employee of " & jane.company);
  Describe (dr_who, "a bit weird");
  Describe (dr_quinn, "not for real");
  Describe (joe, "probably good enough for working on this project");
END Objects.

(* Feel free to create your own subtypes of "Person"! *)

