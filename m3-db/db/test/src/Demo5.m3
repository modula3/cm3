
MODULE Demo5 EXPORTS Main;

IMPORT DB, IO, Fmt, Text;

VAR
  db: DB.T;
  stmt: DB.Stmt;
  query: TEXT;
  desc: DB.ResultDesc;

BEGIN

  TRY
    IO.Put ("Connecting... \n");
    db := DB.Connect ("test", "", "");
  EXCEPT
    DB.Error(e) => IO.Put ("Can't connect to database: " & e.description & "\n")
  END;

  LOOP
    TRY
      stmt := db.new_stmt();

      TRY 
        IO.Put ("m3sql> ");
        query := IO.GetLine();
        IF Text.Equal (query, "quit") OR
          Text.Equal (query, "exit") THEN
          EXIT;
        END;
        
        IO.Put ("Preparing a statement.\n");
        stmt.prepare (query);
        
        IO.Put ("Executing a statement.\n");
        stmt.execute (query);
        
        desc := stmt.describe_result();
        IF desc # NIL THEN 
          FOR i := FIRST(desc^) TO LAST(desc^) DO
            IO.Put (desc[i].name & ":" & Fmt.Int (ORD (desc[i].type)));
            IO.Put (" ");
          END;
          IO.Put ("\n");
        END;
        
        IO.Put ("Number of rows: " & Fmt.Int (stmt.num_rows()) & "\n");
        
        LOOP
          WITH res = stmt.fetch() DO
            IF res = NIL THEN EXIT END;
            FOR i := FIRST(res^) TO LAST(res^) DO
              TYPECASE res^[i] OF 
              | REF INTEGER(i) => IO.Put ("(int) " & Fmt.Int (i^) & "\n");
              | REF REAL(r)    => IO.Put ("(float) " & Fmt.Real (r^) & "\n");
              | DB.RefString(s) => 
                CASE desc[i].type OF 
                | DB.DataType.Char => IO.Put ("(char " & 
                  Fmt.Int(desc[i].precision) &
                  ") ");
                | DB.DataType.VarChar => IO.Put ("(string) ");
                ELSE IO.Put ("(misc string or char) ");
                END;
                IO.Put (Text.FromChars(s^^) & "\n");
              | DB.RefTime(t) => IO.Put ("(time) " & 
                Fmt.Int (t.hour) & ":" & 
                Fmt.Int (t.minute) & ":" & 
                Fmt.Int (t.second) & "\n");
              | DB.RefDate(d) => IO.Put ("(date) " & 
                Fmt.Int (d.month) & "-" & 
                Fmt.Int (d.day) & "-" & 
                Fmt.Int (d.year) & "\n");
              | DB.RefTimestamp(ts) => IO.Put ("(timestamp) " & 
                Fmt.Int (ts.month) & "-" & 
                Fmt.Int (ts.day) & "-" & 
                Fmt.Int (ts.year) & " " & 
                Fmt.Int (ts.hour) & ":" & 
                Fmt.Int (ts.minute) & ":" & 
                Fmt.Int (ts.second) & "\n");
              ELSE
                IO.Put ("Demo: don't know about the result type\n");
              END
            END
          END;
        END; (* Fetch loop *)
        stmt.done();
        stmt.close();

      FINALLY
        db.commit(); 
      END;
    EXCEPT
    |   DB.Error(desc) => IO.Put ("database error: " & desc.description & "\n");
    |   IO.Error => IO.Put ("exiting\n");
    END;
  END;
  
END Demo5.


  
