MODULE %lex;
%gen
IMPORT %tok;
IMPORT TextRd;
IMPORT Rd, Thread;
IMPORT SeekRd;
FROM %tok IMPORT Token;
<* FATAL Rd.Failure, Thread.Alerted *>

REVEAL
  T = Public BRANDED "%lex" OBJECT
    textCache: TEXT;
    charCache: CHAR;
    posBeforeToken: INTEGER;
    rd: Rd.T;
%alloc\
  OVERRIDES
    setRd := SetRd;
    get := Get;
    unget := Unget;
    error := Error;
    rewind := Rewind;
    fromText := FromText;
    getRd := GetRd;
    getText := GetText;
    purge := Purge;
%ovr\
  END;

TYPE
  Byte = BITS 8 FOR [0..255];
  StateRef = BITS %lastStateRefBits FOR [0..%lastStateRefVal];
  TransRef = BITS %lastTransRefBits FOR [0..%lastTransRefVal];
  OutCode = BITS %lastOutBits FOR [0..%lastOutVal];

  S = RECORD
    keyBegin, keyEnd: Byte;
    target: StateRef;
    next: TransRef;
    output: OutCode;
  END;
  X = RECORD
    keyBegin, keyEnd: Byte;
    target: StateRef;
    next: TransRef;
  END;

CONST
  First = ARRAY CHAR OF [0..%lastStateRefVal] {
%First};

  States = ARRAY [1..%lastStateRefVal] OF S {
%States};

  Trans = ARRAY [1..%lastTransRefVal] OF X {
%Trans};

PROCEDURE SetRd(self: T; rd: Rd.T): %tok.RdLexer =
  BEGIN
    self.textCache := "";
    self.charCache := '\000';
    self.posBeforeToken := -1;
    self.rd := rd;
    RETURN self;
  END SetRd; 

PROCEDURE NextCode(self: T): OutCode RAISES {Rd.EndOfFile} =
  VAR
    rd := self.rd;
    lastAcceptingOutput: INTEGER := 0;
    lastAcceptingPos: INTEGER := Rd.Index(rd);
    firstChar := Rd.GetChar(rd);
    curState := First[firstChar];
    curTrans: INTEGER;
    c: Byte;
  BEGIN
    self.charCache := firstChar;
    self.posBeforeToken := lastAcceptingPos;
    TRY
      WHILE curState # 0 DO
        WITH state = States[curState] DO
          IF state.output # 0 THEN
            lastAcceptingOutput := state.output;
            lastAcceptingPos := Rd.Index(rd);
          END;
          IF state.keyBegin = 1 AND state.keyEnd = 255 THEN
            curState := state.target;
          ELSE
            c := ORD(Rd.GetChar(rd));
            IF c >= state.keyBegin AND c <= state.keyEnd THEN
              curState := state.target;
            ELSE
              curTrans := state.next;
              WHILE curTrans # 0 DO
                WITH trans = Trans[curTrans] DO
                  IF c >= trans.keyBegin AND c <= trans.keyEnd THEN
                    curState := trans.target;
                    curTrans := 0;
                  ELSE
                    curTrans := trans.next;
                  END;
                END;
              END;
            END;
          END;
        END;
      END;
    EXCEPT
    | Rd.EndOfFile =>
      IF lastAcceptingOutput = 0 THEN
        Rd.Seek(rd, lastAcceptingPos);
        RAISE Rd.EndOfFile;
      END;
    END;
    Rd.Seek(rd, lastAcceptingPos);
    RETURN lastAcceptingOutput;
  END NextCode;

PROCEDURE Get(self: T): Token RAISES {Rd.EndOfFile} =
  VAR
    result: Token;
  BEGIN
    SeekRd.DiscardPrevious(self.rd);
    REPEAT
      self.textCache := NIL;
      CASE NextCode(self) OF
      | 0 => <* ASSERT FALSE *> (* unmatched *)
%case\
      END;
    UNTIL result # NIL;
    RETURN result;
  END Get; 

PROCEDURE Unget(self: T) =
  BEGIN     
    <* ASSERT self.posBeforeToken # -1 *>
    Rd.Seek(self.rd, self.posBeforeToken);
    self.posBeforeToken := -1;
  END Unget;

PROCEDURE GetText(self: T): TEXT =
  VAR
    len: INTEGER;
  BEGIN
    IF self.textCache = NIL THEN
      <* ASSERT self.posBeforeToken # -1 *>
      len := Rd.Index(self.rd) - self.posBeforeToken;
      Rd.Seek(self.rd, self.posBeforeToken);
      self.textCache := Rd.GetText(self.rd, len);
    END;
    RETURN self.textCache;
  END GetText;

PROCEDURE Purge(self: T): INTEGER =
  BEGIN
    RETURN 0%purge;
  END Purge;

PROCEDURE GetRd(self: T): Rd.T =
  BEGIN RETURN self.rd; END GetRd;

PROCEDURE Rewind(self: T) =
  BEGIN Rd.Seek(self.rd, 0); EVAL self.setRd(self.rd); END Rewind;

PROCEDURE FromText(self: T; t: TEXT): %tok.RdLexer =
  BEGIN RETURN self.setRd(TextRd.New(t)); END FromText;

PROCEDURE Error(self: T; message: TEXT) =
  BEGIN SeekRd.E(self.rd, message); END Error;

(* default token methods *)
%default\
PROCEDURE char(self: T): Token =
  BEGIN RETURN %tok.NewConstToken(ORD(self.charCache)); END char;

BEGIN
END %lex.
