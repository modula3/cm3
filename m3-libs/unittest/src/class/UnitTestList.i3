INTERFACE UnitTestList;

IMPORT UnitTest;

TYPE
  T <: Public;
  Public = UnitTest.T OBJECT
           METHODS
             init (name: TEXT; READONLY children: ARRAY OF UnitTest.T; ):
                   UnitTest.T;
             addChild (child: UnitTest.T; );
           END;

END UnitTestList.
