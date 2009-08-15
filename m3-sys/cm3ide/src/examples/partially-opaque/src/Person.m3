
MODULE Person;

(* The implementation of the "Person" interface implements
   the partially opaque type "Person.T". To do so, it ``reveals''
   the representation of "Person.T" fully.

   The "Person" interface already defines the signatures for procedures
   "init" and "fullname". It is the role of this module to implement
   these methods, and add the underlying implementation structure. *)

REVEAL
  T = Public BRANDED OBJECT
    firstname, lastname: TEXT;
    gender: Gender;
  OVERRIDES
    init := Init;
    fullname:= FullName;
  END;

(* The "BRANDED" keyword will ensure that instances of other
   types with identical structure cannot masquerade as "Person.T"
   objects. *)

CONST
  Title = ARRAY Gender OF TEXT {"Ms.", "Mr."};


(* Procedure "Init" initializes a "Person.T" object, and returns the
   "self" parameter just initialized. *)

PROCEDURE Init (self: T; firstname, lastname: TEXT; gender: Gender): T =
BEGIN
  self.firstname := firstname;
  self.lastname := lastname;
  self.gender := gender;
  RETURN self;
END Init;

(* Define the procedure "FullName", the implementation of method "fullname()"
   of "Person.T". Procedure "FullName" itself is not exported to the clients
   of "Person" interface, but the method "fullname()" of "Person.T" is visible
   to clients. *)

PROCEDURE FullName (p: T): TEXT =
BEGIN
  RETURN Title[p.gender] & " " & p.firstname & " " & p.lastname;
END FullName;

BEGIN END Person.


