INTERFACE UnitTestTerminal;

TYPE
  T = OBJECT
      METHODS
        put      (message: TEXT; );
        indent   ();
        deindent ();
      END;

END UnitTestTerminal.
