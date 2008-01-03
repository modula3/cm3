UNSAFE MODULE DemoDB EXPORTS Main;

IMPORT
	DB, IO, Fmt;

FROM M3toC IMPORT StoT;
FROM Ctypes IMPORT char_star;

VAR
	server, database, userid, password, interface: TEXT;


PROCEDURE Demo(query: TEXT) =
VAR
	db : DB.Interface := NIL;
	conn: DB.T;
	st: DB.Stmt;
	count: INTEGER;
	cols: INTEGER;
BEGIN
	TRY
		db := DB.FindInterface(interface);
		conn := db.connect(database, userid, password);
		conn.auto_commit(FALSE);
		st := conn.new_stmt();
		st.prepare(query);
		st.execute(NIL);
		count := st.num_rows();
		IO.Put("Found " & Fmt.Int(count) & " rows and ");
		WITH desc = st.describe_result() DO
			cols := NUMBER(desc^);
			IO.Put(Fmt.Int(cols) & " columns.\n\n");
			FOR i := 0 TO cols-1 DO
				IO.Put(desc[i].name & "\t");
			END;
			IO.Put("\n");
		END;
		LOOP
			WITH res = st.fetch() DO
				IF res = NIL THEN EXIT END;
				FOR i := 0 TO LAST(res^) DO
					TYPECASE res[i] OF
					|	NULL =>
						IO.Put("<NULL>");
					| DB.RefString(t) =>
						IO.Put(StoT(LOOPHOLE(t^, char_star)));
					| REF INTEGER (i) =>
						IO.Put(Fmt.Int(i^));
					| REF REAL (r) =>
						IO.Put(Fmt.Real(r^));
					| REF LONGREAL (r) =>
						IO.Put(Fmt.LongReal(r^));
					| REF BOOLEAN (b) =>
						IO.Put(Fmt.Bool(b^));
					ELSE
					END;
					IF (i+1) MOD cols = 0 THEN
						IO.Put("\n");
					ELSE
						IO.Put("\t");
					END;
				END;
			END;
		END;
		st.done();
		st.close();
		conn.disconnect();
	EXCEPT
		DB.Error(err) => IO.Put("Error: " & err.description & ".\n");
	END;
END Demo;

BEGIN

	IO.Put("\n\nPostgreSQL\n----------\n");
	server := "10.0.1.4";
	database := "test";
	userid := "postgres";
	password := "";
	interface := "PostgreSQL";
	Demo("select * from test");

	IO.Put("\n\nMySQL\n----------\n");
	server := "localhost";
	database := "test";
	userid := "test";
	password := "";
	interface := "MySQL";
	Demo("select * from test");

	IO.Put("\n\nODBC\n----------\n");
	server := "";
	database := "PostgreSQL"; (* actually, the data source name *)
	userid := "postgres";
	password := "";
	interface := "ODBC";
	Demo("SELECT * FROM TEST");


END DemoDB.

