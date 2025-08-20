INTERFACE TermIO;

(* todo: make this object extend Term.T *)

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
