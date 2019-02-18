MODULE TextSubs;
IMPORT SortedTextTextTbl;
IMPORT Text;
IMPORT TextWr;
IMPORT Wr, Thread;
IMPORT Fmt;

(*
FROM Debug IMPORT S;
CONST
  DebugLevel = 0;
*)

<* FATAL Wr.Failure, Thread.Alerted *>
REVEAL
  T = Public BRANDED OBJECT
    starts: SET OF CHAR;
    tbl: SortedTextTextTbl.T;
  OVERRIDES
    init := Init;
    add := Add;
    int := Int;
    apply := Apply;
  END;

PROCEDURE Init(self: T): T =
  BEGIN
    self.tbl := NEW(SortedTextTextTbl.Default).init();
    self.starts := SET OF CHAR{};
    RETURN self;
  END Init;

PROCEDURE Add(self: T; original, replacement: TEXT) =
  BEGIN
    <* ASSERT Text.Length(original) > 0 *>
    self.starts := self.starts + SET OF CHAR{Text.GetChar(original, 0)};
    EVAL self.tbl.put(original, replacement);
  END Add;

PROCEDURE Int(self: T; original: TEXT; replacement: INTEGER) =
  BEGIN
    self.add(original, Fmt.Int(replacement));
  END Int;

PROCEDURE Apply(self: T; src: TEXT): TEXT =
  VAR
    wr := TextWr.New();
    c: CHAR;
    ind, pos, lastFlushed: INTEGER := 0;
    textLen := Text.Length(src);
    iter: SortedTextTextTbl.Iterator;
    original, replacement: TEXT;
    key, prefix: TEXT;
  PROCEDURE Flush() =
    BEGIN
      Wr.PutText(wr, Text.Sub(src, lastFlushed, pos - lastFlushed));
    END Flush;
  BEGIN
    WHILE pos < textLen DO
      c := Text.GetChar(src, pos);
      IF c IN self.starts THEN
        (* S("analysing: " & Text.Sub(src, pos), DebugLevel); *)
        iter := self.tbl.iterateOrdered();
        ind := pos;
        original := "";
        REPEAT
          INC(ind);
          key := Text.Sub(src, pos, ind-pos);
          (* S("key = " & key, DebugLevel); *)
          prefix := Text.Sub(original, 0, ind-pos);
          CASE Text.Compare(prefix, key) OF
          | -1 =>
            VAR
              lastOriginal := original;
              lastReplacement := replacement;
            BEGIN
              iter.seek(key);
              IF iter.next(original, replacement) THEN
                (* S("found: " & original, DebugLevel); *)
              ELSE
                original := Text.FromChar(LAST(CHAR));
              END;
              prefix := Text.Sub(original, 0, ind-pos);
              IF NOT Text.Equal(prefix, key) THEN
                (* S("exiting", DebugLevel); *)
                original := lastOriginal;
                replacement := lastReplacement;
                ind := textLen;
              END;
            END;
          | 1 =>
            ind := textLen;
            (* S("exiting", DebugLevel); *)
          | 0 =>
            (* S("holding", DebugLevel); *)
          END;
        UNTIL ind = textLen;
        IF Text.Length(original) > 0 AND
          Text.Equal(Text.Sub(src, pos, Text.Length(original)), original) THEN
          Flush();
          Wr.PutText(wr, replacement);
          INC(pos, Text.Length(original));
          lastFlushed := pos;
          DEC(pos);
        END;
      END;
      INC(pos);
    END;
    Flush();
    RETURN TextWr.ToText(wr);
  END Apply;

BEGIN
END TextSubs.
    
