MODULE Test EXPORTS Main;

IMPORT
  DeepCopy, Atom;

TYPE
  Test = REF RECORD
    one: REFANY;
    two: Test;
  END;

VAR
  test, copy: Test;
  

BEGIN
  WITH test = NEW(Test) DO
    test.one := Atom.FromText("hello");
    
    test.two := NEW(Test);
    test.two.one := NEW(Test);
    test.two.two := test.two.one;
    copy := DeepCopy.Copy(test);
  END;
END Test.
