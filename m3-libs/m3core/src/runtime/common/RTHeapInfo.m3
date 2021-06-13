(*| Copyright (C) 1990, Digital Equipment Corporation       *)
(*| All rights reserved.                                    *)
(*| See the file COPYRIGHT for a full description.          *)
(*|                                                         *)
(*| Last modified on Wed Oct 12 14:41:01 PDT 1994 by kalsow *)

UNSAFE MODULE RTHeapInfo;

IMPORT RT0, RTAllocCnts, RTOS, RTParams, RTPerfTool, RTType;
IMPORT Text, Thread, Cstring, Word;

TYPE Closure = Thread.Closure OBJECT OVERRIDES apply := Producer END;

VAR
  drain        : RTPerfTool.Handle;
  self         : Thread.T := NIL;
  cnt          : INTEGER := 0;
  buf          : ARRAY [0..255] OF INTEGER;
  update       : LONGREAL := 5.0d0;
  n_types_sent : CARDINAL := 0;

PROCEDURE Producer (<*UNUSED*> self: Thread.Closure): REFANY =
  VAR
    n: INTEGER;  nTypes: RT0.Typecode;
  BEGIN
    LOOP
      RTOS.LockHeap ();
        nTypes := MIN (RTAllocCnts.n_types, RTType.MaxTypecode()+1);

        SendTypes (nTypes);
  
        (* count the non-zero stats *)
        n := 0;
        FOR i := 0 TO nTypes-1 DO
          IF RTAllocCnts.n_objects[i] # 0 THEN INC (n) END;
        END;

        (* and send'm *)
        Send (SENDING_COUNTS);
        Send (n);
        FOR i := 0 TO nTypes-1 DO
          WITH n_obj = RTAllocCnts.n_objects[i] DO
            IF (n_obj # 0) THEN
              Send (i);  Send (n_obj);  n_obj := 0;
              WITH n_bytes = RTAllocCnts.n_bytes[i] DO
                IF (n_bytes # 0) THEN Send (n_bytes);  n_bytes := 0; END;
              END;
            END;
          END;
        END;
        Flush ();
      RTOS.UnlockHeap ();

      Thread.Pause (update);
    END;
  END Producer;

PROCEDURE SendTypes (nTypes: INTEGER) =
  (* LL = heap lock *)
  TYPE TK = RT0.TypeKind;
  VAR t: RT0.TypeDefn;  str: ADDRESS;  n: INTEGER;  buf: ARRAY [0..31] OF CHAR;
  BEGIN
    n := nTypes - n_types_sent;
    IF (n > 0) THEN
      (* describe the new types *)
      Send (SENDING_TYPES);
      Send (n);
      FOR i := n_types_sent TO n_types_sent + n - 1 DO
        t := RTType.Get (i);
        IF (t.kind = ORD (TK.Array))
          THEN Send (-1);          (* variable sized object *)
          ELSE Send (t.dataSize);  (* fixed size *)
        END;
        str := t.name;
        IF (str # NIL)
          THEN n := Cstring.strlen (str);
          ELSE n := BuildTypeName (t, buf);  str := ADR (buf[0]);
        END;
        Send (n);
        Flush ();
        EVAL RTPerfTool.Send (drain, str, n);
      END;
      n_types_sent := nTypes;
    END;
  END SendTypes;

PROCEDURE BuildTypeName (t: RT0.TypeDefn;  VAR buf: ARRAY OF CHAR): INTEGER =
  CONST Digits = ARRAY [0..15] OF CHAR {'0','1','2','3','4','5','6','7',
                                        '8','9','a','b','c','d','e','f'};
  VAR uid, n: INTEGER;
  BEGIN
    buf[0] := '<';
    buf[1] := '_';
    buf[2] := 't';
    uid := t.selfID;
    FOR i := 10 TO 3 BY -1 DO
      buf[i] := Digits[ Word.And (uid, 16_f) ];
      uid := Word.RightShift (uid, 4);
    END;
    buf[11] := ' ';
    buf[12] := 'T';
    buf[13] := 'C';
    buf[14] := '=';
    uid := t.typecode;
    n := 1;
    WHILE (uid > 10) DO INC (n);  uid := uid DIV 10 END;
    uid := t.typecode;
    FOR i := 14+n TO 15 BY -1 DO
      buf[i] := Digits[ uid MOD 10 ];
      uid := uid DIV 10;
    END;
    buf[15+n] := '>';
    buf[16+n] := '\000';
    RETURN 15+n;
  END BuildTypeName;

PROCEDURE Send (i: INTEGER) =
  BEGIN
    IF (cnt >= NUMBER (buf)) THEN Flush () END;
    buf[cnt] := i;
    INC (cnt);
  END Send;

PROCEDURE Flush () =
  BEGIN
    IF (cnt > 0) THEN
      EVAL RTPerfTool.Send (drain, ADR (buf), cnt * BYTESIZE (buf[0]));
      cnt := 0;
    END;
  END Flush;

PROCEDURE SetUpdate (txt: TEXT) =
  VAR n: INTEGER := 0;  ch: INTEGER;
  BEGIN
    IF (txt = NIL) OR Text.Length (txt) = 0 THEN RETURN END;
    FOR i := 0 TO Text.Length(txt)-1 DO
      ch := ORD (Text.GetChar (txt, i)) - ORD ('0');
      IF (ch < 0) OR (9 < ch) THEN RETURN END;
      n := 10 * n + ch;
    END;
    update := MAX (1.0d0, FLOAT (n, LONGREAL));
  END SetUpdate;

PROCEDURE Init () =
  BEGIN
    IF RTPerfTool.Start ("shownew", drain) THEN
      RTAllocCnts.countsOn := TRUE;
      SetUpdate (RTParams.Value ("update"));
      self := Thread.Fork (NEW (Closure));
    END;
  END Init;

BEGIN
END RTHeapInfo.

