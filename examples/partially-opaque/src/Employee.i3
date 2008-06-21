
INTERFACE Employee;
IMPORT Person;

TYPE
  T <: Public; Public = Person.T OBJECT
  METHODS
    init(first, last: TEXT; 
         gender: Person.Gender; 
         company: TEXT): T;
  END;

END Employee.

