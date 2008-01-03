(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: TYPECASE statements *)

MODULE Main;

FROM Test IMPORT checkI, done;

VAR
  i: INTEGER;

  r:  REFANY;
    
  l: REF INTEGER;
  c: REF CHAR;
  z: REF BOOLEAN;
  
BEGIN
  c := NEW (REF CHAR); c^ := 'a';
  l := NEW (REF INTEGER); l^ := 256;
  z := NEW (REF BOOLEAN); z^ := TRUE;

  i := 0;
  r := l;
  TYPECASE r OF
  | (REF INTEGER) =>  INC (i, 1);
  | (REF CHAR)    =>  INC (i, 2);
  | NULL          =>  INC (i, 4);
  | REFANY        =>  INC (i, 8);
  END;
  checkI (i, 1);

  i := 0;
  r := c;
  TYPECASE r OF
  | (REF INTEGER) =>  INC (i, 1);
  | (REF CHAR)    =>  INC (i, 2);
  | NULL          =>  INC (i, 4);
  | REFANY        =>  INC (i, 8);
  END;
  checkI (i, 2);

  i := 0;
  r := NIL;
  TYPECASE r OF
  | (REF INTEGER) =>  INC (i, 1);
  | (REF CHAR)    =>  INC (i, 2);
  | NULL          =>  INC (i, 4);
  | REFANY        =>  INC (i, 8);
  END;
  checkI (i, 1);

  i := 0;
  r := z;
  TYPECASE r OF
  | (REF INTEGER) =>  INC (i, 1);
  | (REF CHAR)    =>  INC (i, 2);
  | NULL          =>  INC (i, 4);
  | REFANY        =>  INC (i, 8);
  END;
  checkI (i, 8);

  i := 0;
  r := c;
  TYPECASE r OF
  | (REF INTEGER) =>  INC (i, 1);
  | (REF CHAR)    =>  INC (i, 2);
  | NULL          =>  INC (i, 4);
  ELSE                INC (i, 8);
  END;
  checkI (i, 2);

  i := 0;
  r := z;
  TYPECASE r OF
  | (REF INTEGER) =>  INC (i, 1);
  | (REF CHAR)    =>  INC (i, 2);
  | NULL          =>  INC (i, 4);
  ELSE                INC (i, 8);
  END;
  checkI (i, 8);


  i := 0;
  r := l;
  TYPECASE r OF
  | (REF INTEGER)(n) =>  DEC (i, n^);
  | (REF CHAR)       =>  INC (i, 2);
  | NULL             =>  INC (i, 4);
  ELSE                   INC (i, 8);
  END;
  checkI (i, -256);

  done ();

END Main.
