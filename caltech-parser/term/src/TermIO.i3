INTERFACE TermIO;
TYPE
  T = OBJECT METHODS
    getChar(): CHAR;
    getLine(prompt := ">"): TEXT;
    putLine(t: TEXT);
    putText(t: TEXT);
  END;
VAR
  stdio: T;
END TermIO.
