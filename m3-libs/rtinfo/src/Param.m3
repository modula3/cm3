(* (C) 1999, 2013 Dragiša Durić, dragisha@m3w.org
*)
MODULE Param;

IMPORT Params, Text;
  
PROCEDURE Switches(): CARDINAL =
  VAR
    i, j: CARDINAL := 0;
  BEGIN
    j := 1;
    WHILE j < Params.Count DO
      WITH p = Params.Get(j) DO
        IF Text.GetChar(p, 0) = '-' THEN
          IF Text.Equal(p, "--") THEN
            RETURN i;
          ELSIF Text.Length(p) > 1 THEN
            INC(i);
          END;
        END;
      END;
      
      INC(j);
    END;

    RETURN i;
  END Switches;  
  
PROCEDURE Count(): CARDINAL =
  VAR
    i: CARDINAL := 0;
    j: CARDINAL;
    noMore: BOOLEAN := FALSE;
  BEGIN
    j := 1;
    WHILE j < Params.Count DO
      WITH p = Params.Get(j) DO
        IF noMore THEN
          INC(i);
        ELSIF Text.GetChar(p, 0) = '-' THEN
          IF Text.Equal(p, "--") THEN
            IF noMore THEN
              INC(i);
            ELSE
              noMore := TRUE;
            END;
          ELSIF Text.Length(p) = 1 THEN
            INC(i);
          END;
        ELSE
          INC(i);
        END;
      END;
      
      INC(j);
    END;    
        
    RETURN i;
  END Count;
  
PROCEDURE Switch(opt: TEXT): BOOLEAN =
  BEGIN
    IF opt # NIL AND Text.GetChar(opt, 0) = '-' AND Text.Length(opt) > 1 AND NOT Text.Equal(opt, "--") THEN
      FOR j := 1 TO Params.Count - 1 DO
        IF Text.Equal(Params.Get(j), opt) THEN
          RETURN TRUE;
        END;
      END;
    END;
    
    RETURN FALSE;
  END Switch;  
  
PROCEDURE GetSwitch(opt: TEXT; VAR res: TEXT): BOOLEAN =
  VAR
    len: CARDINAL;
  BEGIN
    IF opt # NIL AND Text.GetChar(opt, 0) = '-' AND Text.Length(opt) > 1 AND NOT Text.Equal(opt, "--") THEN
      len := Text.Length(opt);
      IF Text.GetChar(opt, len-1) # ':' THEN
        RETURN FALSE;
      END;
      FOR i:=1 TO Params.Count - 1 DO
        WITH p = Params.Get(i) DO
          IF Text.Equal(Text.Sub(p, 0, len), opt) THEN
            res := Text.Sub(p, len);
            RETURN TRUE;
          END;
        END;
      END;
    END;
    
    RETURN FALSE;
  END GetSwitch;   
  
PROCEDURE Item(n: CARDINAL): TEXT =
  VAR
    i, j: CARDINAL := 0;
    noMore: BOOLEAN := FALSE;
  BEGIN
    IF n = 0 THEN
      RETURN Params.Get(0);
    END;

    j := 1;
    WHILE j < Params.Count DO
      WITH p = Params.Get(j) DO
        IF noMore THEN
          INC(i);
        ELSIF Text.GetChar(p, 0) = '-' THEN
          IF Text.Equal(p, "--") THEN
            IF noMore THEN
              INC(i);
            ELSE
              noMore := TRUE;
            END;
          ELSIF Text.Length(p) = 1 THEN
            INC(i);
          END;
        ELSE
          INC(i);
        END;
        IF i = n THEN
          RETURN p;
        END;        
      END;

      INC(j);
    END;    
        
    RETURN NIL;
  END Item;

(* Not used for above code. No need for it.
VAR
  args: REF ARRAY OF TEXT;
  switch, param: REF ARRAY OF BOOLEAN;
  noMore := FALSE;
BEGIN
  args := NEW(REF ARRAY OF TEXT, Params.Count);
  switch := NEW(REF ARRAY OF BOOLEAN, Params.Count);
  param := NEW(REF ARRAY OF BOOLEAN, Params.Count);
  FOR i := 0 TO Params.Count-1 DO
    WITH arg = Params.Get(i) DO
      args[i] := arg;
      IF i>0 AND Text.Equal(arg, "--") THEN
        noMore := TRUE;
      END;
      switch[i] := i>0 AND NOT noMore AND Text.GetChar(arg, 0) = '-' AND Text.Length(arg) > 1;
      param[i] := i>0 AND NOT switch[i] AND NOT Text.Equal(arg, "--");
    END;
  END;
*)

BEGIN
END Param.  
