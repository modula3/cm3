MODULE DB;

(*	Darko Volaric September 2002
		An adaption of the previous version to allow concurrent connections to
		different database interfaces. This module is used as to glue the database specific
		modules together and provide backward compatiblity. 
		Please email darko@peter.com.au with any problems.
*)


IMPORT
	PostgreSQLDB, ODBCDB, MySQLDB, Text;

VAR
	Default: Interface := NIL;
	Interfaces : InterfaceList := NIL;

REVEAL
	Interface = InterfacePublic BRANDED OBJECT
	OVERRIDES
		set_default := SetDefaultInterface;
	END;

PROCEDURE Init() =
BEGIN
	Interfaces := NEW(InterfaceList, 3);
	Interfaces^[0] := PostgreSQLDB.GetInterface();
	Interfaces^[1] := ODBCDB.GetInterface();
	Interfaces^[2] := MySQLDB.GetInterface();
	FOR i := 0 TO LAST(Interfaces^) DO
		IF Interfaces[i] # NIL THEN
			Default := Interfaces[i];
			EXIT;
		END;
	END;
	IF Default = NIL THEN
		(* FIXME: Riase a error, but should an execption be raised at module init time? *)
	END;
END Init;

PROCEDURE GetInterfaces(): InterfaceList =
BEGIN
	RETURN Interfaces;
END GetInterfaces;

PROCEDURE SetDefaultInterface(this: Interface) =
BEGIN
	Default := this;
END SetDefaultInterface;

PROCEDURE FindInterface(name: TEXT): Interface  RAISES {Error} =
BEGIN
	IF name = NIL THEN name := "" END;
	(* there's not going to be alot of interfaces... linear search is good! *)
	FOR i := 0 TO LAST(Interfaces^) DO
		IF Interfaces[i] # NIL AND Text.Equal(name, Interfaces[i].name) THEN
			RETURN Interfaces[i];
		END;
	END;
	RAISE Error(NEW(ErrorDesc, 
		state := ARRAY[0..5] OF CHAR{' ',..}, 
		description := "Interface '" & name & "' not found.",
		native_err := 0
	));
END FindInterface;

PROCEDURE GetDefaultInterface(): Interface =
BEGIN
	RETURN Default;
END GetDefaultInterface;

PROCEDURE Connect (
	database, user_id, password: TEXT; 
	server: TEXT := NIL;
	interface: Interface := NIL
): T RAISES {Error} =
BEGIN
	IF interface = NIL THEN
		RETURN Default.connect(database, user_id, password, server);
	ELSE
		RETURN interface.connect(database, user_id, password, server);
	END;
END Connect;


PROCEDURE GetDataSources (): DescList  = 
BEGIN
	RETURN Default.get_data_sources();
END GetDataSources;

PROCEDURE GetDrivers (): DescList  =
BEGIN
	RETURN Default.get_drivers();
END GetDrivers;

BEGIN
	Init();
END DB.
