
MODULE IPTypeinVBT;
IMPORT TypeinVBT, KeyFilter, TextPort, VBTClass, VBT;
IMPORT IP, Scan, TextRd, Text, Fmt, Lex, Rd, Thread, FloatMode;

REVEAL
  T = TypeinVBT.T BRANDED "IPTypeinVBT.T" OBJECT OVERRIDES
        key := Key;
      END;

PROCEDURE IsComplete(ip: TEXT): BOOLEAN = 
  TYPE   
    Ch = {Digit, Dot};
  CONST
    mustfind = ARRAY OF Ch {Ch.Digit, Ch.Dot, Ch.Digit, Ch.Dot, Ch.Digit, Ch.Dot, Ch.Digit };
  VAR
    where := FIRST(mustfind);
    a := ARRAY [1..15] OF CHAR { VAL(0,CHAR), ..};
  BEGIN
    Text.SetChars (a, ip); 
    FOR i := FIRST(a) TO LAST(a) DO
      CASE a[i] OF
      | '0'..'9' => 
        IF mustfind[where] = Ch.Dot THEN
          INC(where);
        END;
        IF mustfind[where] # Ch.Digit THEN
          RETURN FALSE;
        END;
      | '.' => 
        <* ASSERT mustfind[where] # Ch.Dot *>
        INC(where);
      ELSE
        EXIT;
      END;
    END;
    RETURN where = LAST(mustfind);
 END IsComplete;
          
PROCEDURE Key (t: T; READONLY cd: VBT.KeyRec) =
  <* FATAL FloatMode.Trap, Lex.Error *>
  BEGIN
    IF cd.wentDown AND cd.whatChanged # VBT.NoKey AND
        NOT KeyFilter.IsModifier (cd.whatChanged) THEN
          WITH txt = TextPort.GetText(t), 
               len = Text.Length (txt),
               lastthree = Text.Sub (txt, MAX(len-3,0), 3), 
               dot_in_lastthree = Text.FindChar (lastthree, '.') # -1 DO
            CASE cd.whatChanged OF 
            | ORD('0')..ORD('9') =>
               IF IsComplete(txt) THEN
                 IF NOT dot_in_lastthree THEN RETURN END;
               ELSIF len >= 3 AND NOT dot_in_lastthree THEN
                 VAR fake := cd; BEGIN
                   fake.whatChanged := ORD('.');
                   VBTClass.Key (t, fake);
                   TypeinVBT.T.key (t, cd);
                   RETURN;
                 END;
               END;
               WITH lastnum = 
                 Text.Sub (lastthree, MAX(0, Text.FindCharR(lastthree, '.')+1), 3) DO
                 IF NOT Text.Empty (lastnum) THEN
                   CASE Scan.Int(lastnum) OF 
                   | 0..24 => (* continue *)
                   | 25    => IF cd.whatChanged > ORD('5') THEN RETURN END;
                   ELSE     RETURN
                   END;
                 END;
               END;
               TypeinVBT.T.key (t, cd);
            | ORD('.') => 
              IF IsComplete (txt) THEN
                (* ignore incoming keys *)
              ELSIF len # 0 AND Text.GetChar(txt, len-1) # '.' THEN
                TypeinVBT.T.key (t, cd);
              END;
            | ORD(FIRST(CHAR))..ORD('.')-1,
              ORD('.')+1..ORD('0')-1,
              ORD('9')+1..ORD(LAST(CHAR)) =>
              (* ignore any printable characters *)
            ELSE (* other keys, like Return, tab, etc... *)
              TypeinVBT.T.key (t, cd);
            END
        END;
    END
  END Key;

PROCEDURE Get(v: T): IP.Address RAISES {InvalidAddress} = 
  VAR
    ip: IP.Address;
    rd := TextRd.New(TextPort.GetText(v));
  BEGIN
    TRY
      FOR i := FIRST(ip.a) TO LAST(ip.a) DO
        ip.a[i] := Lex.Int (rd);
        IF ip.a[i] < 0 OR ip.a[i] > 255 THEN RAISE InvalidAddress END;
        IF i # LAST(ip.a) THEN Lex.Match (rd, ".") END;
      END;
    EXCEPT
    | Lex.Error, Rd.Failure, Thread.Alerted, FloatMode.Trap => 
        RAISE InvalidAddress;
    END;
    RETURN ip;
  END Get;

PROCEDURE Put (v: T; addr: IP.Address) RAISES {InvalidAddress} = 
  PROCEDURE Conv (i: INTEGER): TEXT RAISES {InvalidAddress} = 
    BEGIN
      IF i >= 0 AND i <= 255 THEN
        RETURN Fmt.Int(i);
      END;
      RAISE InvalidAddress;
    END Conv;
  VAR
    txt := Conv(addr.a[0]);
  BEGIN
    FOR i := 1 TO 3 DO
      txt := txt & "." & Conv(addr.a[i]); 
    END;
    TextPort.SetText(v, txt);
  END Put;

BEGIN
END IPTypeinVBT.
