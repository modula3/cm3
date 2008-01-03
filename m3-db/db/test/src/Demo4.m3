
MODULE Demo4 EXPORTS Main;

IMPORT DB, IO, Fmt, Text;

VAR
  db: DB.T;
  stmt: DB.Stmt;

BEGIN

TRY

  IO.Put ("Connecting... \n");
  db := DB.Connect ("test", "", "");

  IO.Put ("Creating a new statment... on\n");
  stmt := db.new_stmt();

  IO.Put ("Turning autocommit on\n");
  db.auto_commit(on := TRUE);
  
  IO.Put ("Preparing a statement.\n");
  stmt.prepare ("select * from types");

  IO.Put ("Executing a statement.\n");
  stmt.execute ("select * from types");
  
  LOOP
    WITH res = stmt.fetch() DO
      IF res = NIL THEN EXIT END;
      FOR i := FIRST(res^) TO LAST(res^) DO
        TYPECASE res^[i] OF 
        | REF INTEGER(i) => IO.Put ("(int) " & Fmt.Int (i^) & "\n");
        | REF REAL(r)    => IO.Put ("(float) " & Fmt.Real (r^) & "\n");
	| DB.RefString(s) => IO.Put ("(string) " & Text.FromChars(s^^) & "\n");
	| DB.RefTime(t) => IO.Put ("(time)" & 
	                           Fmt.Int (t.hour) & ":" & 
	                           Fmt.Int (t.minute) & ":" & 
	                           Fmt.Int (t.second));
	| DB.RefDate(d) => IO.Put ("(date)" & 
	                           Fmt.Int (d.month) & "-" & 
	                           Fmt.Int (d.day) & "-" & 
	                           Fmt.Int (d.year));
        ELSE
          IO.Put ("Demo: don't know about the result type\n");
        END
      END
    END;
  END;
EXCEPT
   |   DB.Error(desc) => IO.Put ("db error: " & desc.description & "\n");
END;

END Demo4.


  
