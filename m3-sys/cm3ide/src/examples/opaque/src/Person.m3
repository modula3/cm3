
MODULE Person;
IMPORT IO, Wr;

(* The implementation of the "Person" interface implements
   the opaque type "Person.T". To do so, it ``reveals''
   the representation of "Person.T". Within the implementation 
   of interface "Person", we can 
   use the items declared in the interface without qualificaiton,
   hence, "T" in this module refers to "Person.T" and "Gender" 
   refers to "Person.Gender".

   Since the only information specified in the "Person" interface is that
   "Person.T" is a reference--more precisely, "Person.T <: REFANY"--its
   implementation is left to specify the full strcture of the object. 
   Indeed, the revelation of "Person.T" looks not unlike a regular object 
   type declaration. *)

REVEAL
  T = BRANDED OBJECT
    firstname, lastname: TEXT;
    gender: Gender;
  METHODS
    fullname(): TEXT := FullName;
  END;

(* The "BRANDED" keyword will ensure that instances of
   "Person.T" are distinct from all other types, even if they
   have the same structure. Essentially, the "BRANDED" keyword
   overrides Modula-3's structural equivalence for this type. See
   the <<a href=/reference>>language reference<</a>> for more 
   information. *)

CONST
  Title = ARRAY Gender OF TEXT {"Ms.", "Mr."};
(* Declare an array of title names. The outside world, of course
   does not know about the existance of this array since 
   it is not visible within the interface "Person". *)

(* Define the procedure "FullName", the implementation of method "fullname()"
   of "Person.T". Since "FullName" is not exported by the "Person" interface
   it will not be visible to any outside modules. *)
PROCEDURE FullName (p: T): TEXT =
BEGIN
  RETURN Title[p.gender] & " " & p.firstname & " " & p.lastname;
END FullName;

(* Define the procedure "Describe" which is exported via interface
   "Person", hence it is visible to all clients of the "Person"
   interface. Since "Describe" is defined within this
   module, the representation of "p.fullname()" is visible within
   its body. *)

PROCEDURE Describe(p: T; desc: TEXT; wr: Wr.T := NIL) =
BEGIN
  IO.Put (p.fullname() & " is " & desc & ".\n", wr);
END Describe;

BEGIN END Person.


