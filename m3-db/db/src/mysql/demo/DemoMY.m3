
UNSAFE MODULE DemoMY EXPORTS Main;

FROM M3toC IMPORT 
	CopyTtoS, StoT;

FROM Ctypes IMPORT 
	char_star, int, unsigned_int;

FROM MySQL IMPORT
	ResRef, FieldRef, Row;

IMPORT 
	IO, Fmt, MySQL;

VAR
  Null := LOOPHOLE(0,ADDRESS);
	host, user, passwd, db, socket: char_star := Null;
  
PROCEDURE Demo(sql: TEXT) = 
VAR
	mysql : MySQL.T;
	result : ResRef;
	field : FieldRef;
	row :  Row;
	mresult : MySQL.T;
	iresult : int;
	port, client_flag: unsigned_int := 0;
	nfields, nrows := 0;
BEGIN
  IO.Put("Connecting to MySQL database version "& StoT(MySQL.GetClientInfo())&"\n");

	(* Init stuff *)
	port := 3306;
	mysql := MySQL.Init(Null);
	IF mysql = Null THEN
		IO.Put("mysql_init call failed.\n");
		RETURN;
	ELSE
		IO.Put("mysql_init call OK.\n");
	END;
	
	(* Connect to the database *)
	mresult := MySQL.RealConnect(mysql, host, user, passwd, db, port, socket, client_flag);
	IF mresult = Null THEN
		IO.Put("mysql_real_connect call failed.\n");
		RETURN
	ELSE
		IO.Put("mysql_real_connect call OK.\n");
	END;

	(* Execute the query *)
	iresult := MySQL.Query(mysql, CopyTtoS(sql));
	IF iresult # 0 THEN
		IO.Put("mysql_query call failed.\n");
		RETURN;
	ELSE
		IO.Put("mysql_query call OK.\n");
	END;
	
	(* Get the results *)
	result := MySQL.StoreResult(mysql);
	IF result = Null THEN
		IO.Put("mysql_store_result call failed.\n\n\n");
		RETURN
	ELSE
		IO.Put("mysql_store_result call OK.\n\n\n");
	END;
	
	LOOP
		field := MySQL.FetchField(result);
		IF field = Null THEN EXIT END;
		INC(nfields);
		IO.Put(StoT(field.name) & "\t");
	END;
	IO.Put("\n\n");

	LOOP
		row := MySQL.FetchRow(result);
		IF row = Null THEN EXIT END;
		INC(nrows); 
		FOR i := 0 TO nfields-1 DO
			IF row^[i] # Null THEN 
				IO.Put(StoT(row^[i]) & "\t");
				IO.Put("\t");
			ELSE
				IO.Put("[NULL]\t")
			END;
		END;
		IO.Put("\n"); 
	END;
	IO.Put("\n"); 

	IO.Put("Total colums, rows: " & Fmt.Int(nfields) & ", " & Fmt.Int(nrows) & "\n");

	MySQL.FreeResult(result);

	(* Close the connection *)
	MySQL.Close(mysql);
	IO.Put("mysql connection closed.\n")
	
END Demo;

BEGIN
	(* You'll have to change these for it work on your system! *)
	host := CopyTtoS("localhost");
	user := CopyTtoS("test");
	db := CopyTtoS("test");
	Demo("select * from test");
END DemoMY.
  

