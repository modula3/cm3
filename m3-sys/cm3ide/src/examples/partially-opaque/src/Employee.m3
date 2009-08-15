
MODULE Employee;
IMPORT Person;
IMPORT TextIntTbl, Fmt;

REVEAL
  T = Public BRANDED OBJECT
   company: TEXT;
   id: INTEGER;
  OVERRIDES
   init := Init;
   fullname := FullName;
  END;

VAR
  employee_count := NEW(TextIntTbl.Default).init();

PROCEDURE Init (self: T; first, last: TEXT; gender: Person.Gender;
                company: TEXT): T = 
VAR
  emp_id := 0;
BEGIN

  EVAL Person.T.init(self, first, last, gender);
  self.company := company;
  
  EVAL employee_count.get(company, emp_id);
  INC(emp_id); 
  self.id := emp_id;
  EVAL employee_count.put(company, emp_id);

  RETURN self;
END Init;

PROCEDURE FullName(emp: T): TEXT =
BEGIN
  RETURN  Person.T.fullname(emp) & ", " &
          "employee #" & Fmt.Int(emp.id) & " at " & emp.company & ",";
END FullName;

BEGIN END Employee.
        
