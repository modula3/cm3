MODULE PrefixDB;
IMPORT FileRd;
IMPORT Rd;
IMPORT RegEx;
IMPORT Text;
IMPORT TextList;
IMPORT TextUtils;
IMPORT Thread;
IMPORT OSError;

<* FATAL Thread.Alerted, OSError.E, Rd.Failure *>
<* FATAL RegEx.Error *>

TYPE
  Cell = REF RECORD
    pat: RegEx.Pattern;
    val: TEXT;
    tail: Cell;
  END;

REVEAL
  T = Public BRANDED OBJECT
    cells: Cell;
  OVERRIDES
    apply := Apply;
  END;

PROCEDURE Delimit(key: TEXT): TEXT =
  CONST
    DelimChar = '%';
  VAR
    Delimiter := Text.FromChar(DelimChar);
  BEGIN
    <* ASSERT Text.FindChar(key, DelimChar) = -1 *>
    RETURN Delimiter & key & Delimiter;
  END Delimit;

PROCEDURE Apply(self: T; to: TEXT): TEXT =
  VAR
    p := "";
    cur := self.cells;
    key := Delimit(to);
  BEGIN
    WHILE cur # NIL DO
      IF RegEx.Execute(cur.pat, key) # -1 THEN
        p := cur.val & p;
      END;
      cur := cur.tail;
    END;
    RETURN p & to;
  END Apply;

PROCEDURE FromFile(named: TEXT): T =
  VAR
    self := NEW(T, cells:=NIL);
    rd := FileRd.Open(named);
    l, cur: TextList.T;
  BEGIN
    TRY
      LOOP
        l := TextList.ReverseD(TextUtils.Shatter(Rd.GetLine(rd)));
        IF l # NIL THEN
          cur := l.tail;
          WHILE cur # NIL DO
            self.cells := NEW(Cell,
                              pat:=RegEx.Compile(Delimit(cur.head)),
                              val:=l.head,
                              tail:=self.cells);
            cur := cur.tail;
          END;
        END;
      END;
    EXCEPT Rd.EndOfFile =>
    END;
    RETURN self;
  END FromFile;

BEGIN
END PrefixDB.
