INTERFACE UnitTestAtom;

IMPORT UnitTest;

TYPE
  T <: Public;
  Public =
    UnitTest.T BRANDED OBJECT
    METHODS
      init (name: TEXT; ): T;
      test ();                   (* abstract method, must be overridden
                                    with specific unit test code *)
      error (message: TEXT; );   (* this routine must be called by the unit
                                    test code when a test fails *)
      message (message: TEXT; ); (* this routine can be called by the unit
                                    test code for intermediate messages *)
    END;

END UnitTestAtom.
