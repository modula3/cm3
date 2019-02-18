(* $Id$  *)
(* vim: tabstop=2:expandtab:shiftwidth=2 
*)

MODULE Config;
IMPORT Thread;
IMPORT Params, Text, Wr, Stdio, Process, Scan, Lex, FloatMode;
IMPORT TextTextTbl, TextIntTbl, TextSetDef;
IMPORT FlagList;

<* FATAL Thread.Alerted, Wr.Failure *>

PROCEDURE DefineFlag(flag, id : TEXT; type : OptionType) =
BEGIN
  WITH flagRec = NEW(T, option := flag, id := id, type := type) DO
    flags := FlagList.Cons(flagRec, flags);
  END;
END DefineFlag;

PROCEDURE CheckInput(usageMsg : TEXT) =
BEGIN
  TRY
    ReadInFlags();
  EXCEPT
    UsageError(s) => Abort(s & "\n" & usageMsg);
  END;
END CheckInput;

PROCEDURE ReadInFlags() RAISES {UsageError} =
VAR
  i : CARDINAL := 1;
BEGIN
	WHILE i < Params.Count DO
		WITH flag = Params.Get(i) DO
		  IF Text.Equal(Text.Sub(flag, 0, 1), "-") THEN
        VAR
          flagList : FlagList.T := flags;
        BEGIN
          WHILE flagList # NIL DO
            IF Text.Equal(flag, flagList.head.option) THEN
              CASE flagList.head.type OF
              | OptionType.boolean =>
                EVAL boolSettings.insert(flagList.head.id);
              | OptionType.integer =>
                INC(i);
                IF i = Params.Count THEN
                   RAISE UsageError("ERROR: Integer parameter expected to follow " & flag);
                END;
                TRY
                  WITH param = Scan.Int(Params.Get(i)) DO
                    EVAL boolSettings.insert(flagList.head.id);
                    EVAL intSettings.put(flagList.head.id, param);
                  END;
                EXCEPT
                | Lex.Error, FloatMode.Trap =>
                    RAISE UsageError("ERROR: Integer parameter expected to follow " & flag);
                END;
              | OptionType.string =>
                INC(i);
                IF i = Params.Count THEN
                   RAISE UsageError("ERROR: String parameter expected to follow " & flag);
                END;
                WITH param = Params.Get(i) DO
                  EVAL boolSettings.insert(flagList.head.id);
                  EVAL strSettings.put(flagList.head.id, param);
                END;
              ELSE
              
              END;
               
              EXIT;
            END;
            flagList := flagList.tail;
          END;
          IF flagList = NIL THEN
            RAISE UsageError("");
          END;
        END;
        lastFlagID := i + 1;
      END;
		END;
    INC(i);
	END;
END ReadInFlags;


PROCEDURE LastIndex() : CARDINAL =
BEGIN
  RETURN lastFlagID;
END LastIndex;

PROCEDURE IsSet(key : TEXT) : BOOLEAN =
BEGIN
  RETURN boolSettings.member(key);
END IsSet;

PROCEDURE Set(key : TEXT) : BOOLEAN =
BEGIN
  RETURN boolSettings.insert(key);
END Set;


PROCEDURE IntVal(key : TEXT) : INTEGER  =
VAR
  val : INTEGER;
BEGIN
  EVAL intSettings.get(key, val);
  RETURN val;
END IntVal;

PROCEDURE StringVal(key : TEXT) : TEXT  =
VAR
  val : TEXT;
BEGIN
  EVAL strSettings.get(key, val);
  RETURN val;
END StringVal;

PROCEDURE Equal(k1, k2 : T) : BOOLEAN =
BEGIN
  RETURN Text.Equal(k1.option, k2.option);
END Equal;

PROCEDURE Abort(usageMsg : TEXT;) =
BEGIN
  Wr.PutText(Stdio.stderr, usageMsg);
  Process.Exit(1);                  
END Abort;


VAR
  flags : FlagList.T;
  boolSettings : TextSetDef.T;
  intSettings : TextIntTbl.T;
  strSettings : TextTextTbl.T;
  lastFlagID : CARDINAL := 1;
  
BEGIN
	strSettings := NEW(TextTextTbl.Default).init(256);
	boolSettings := NEW(TextSetDef.T).init(256);
	intSettings := NEW(TextIntTbl.Default).init(256);
END Config.
