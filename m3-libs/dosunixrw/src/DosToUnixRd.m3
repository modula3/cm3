MODULE DosToUnixRd;

(* The implementation is rather inefficient, because it reads each
   character separately.  But the implementation is sufficiently simply in
   order to trust it. *)

IMPORT Rd, RdClass, Thread;

PROCEDURE New (rd: Rd.T): T =
  BEGIN
    RETURN NEW(T).init(rd);
  END New;

REVEAL
  T = Public BRANDED "DosToUnixRd.T" OBJECT
        source: Rd.T;
      OVERRIDES
        init := Init;
        seek := Seek;
        (*
        This method can not naturally be implemented, because they
        require reading of the source data.

        length := Length;
        *)
        close  := Close;
        getSub := GetSub;
      END;


PROCEDURE Init (rd: T; src: Rd.T): T =
  BEGIN
    rd.source := src;

    rd.buff := NEW(REF ARRAY OF CHAR, 1);
    rd.st := 0;
    rd.closed := FALSE;
    rd.seekable := FALSE;
    rd.intermittent := TRUE;
    rd.cur := 0;
    rd.lo := rd.cur;
    rd.hi := rd.cur;

    RETURN rd
  END Init;

PROCEDURE FetchNextChar (rd: T; ): BOOLEAN
  RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    REPEAT
      IF rd.source.getSub(rd.buff^) = 0 THEN RETURN FALSE; END;
    UNTIL rd.buff[0] # '\r';
    rd.lo := rd.hi;
    rd.hi := rd.lo + 1;
    RETURN TRUE;
  END FetchNextChar;

PROCEDURE Seek (rd: T; pos: CARDINAL; <* UNUSED *> dontBlock: BOOLEAN; ):
  RdClass.SeekResult RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    rd.cur := pos;
    IF pos = rd.hi THEN
      IF NOT FetchNextChar(rd) THEN RETURN RdClass.SeekResult.Eof; END;
    ELSE
      (* if pos=rd.lo, do nothing *)
      (* We cannot really seek.  We can only fetch the next character, when
         needed. *)
      <* ASSERT pos = rd.lo *>
    END;
    RETURN RdClass.SeekResult.Ready;
  END Seek;

(* untested *)
PROCEDURE GetSub (rd: T; VAR a: ARRAY OF CHAR): CARDINAL
  RAISES {Rd.Failure, Thread.Alerted} =
  VAR readSoFar: CARDINAL := 0;
  BEGIN
    WHILE readSoFar < NUMBER(a) AND (rd.cur = rd.lo OR FetchNextChar(rd)) DO
      a[readSoFar] := rd.buff[0];
      INC(readSoFar);
      INC(rd.cur);
    END;
    RETURN readSoFar;
  END GetSub;

<* UNUSED *>
PROCEDURE GetSubSimple (rd: T; VAR a: ARRAY OF CHAR): CARDINAL
  RAISES {Rd.Failure, Thread.Alerted} =
  VAR readSoFar: CARDINAL := 0;
  BEGIN
    (* We cannot implement the method this way, because before fetching
       data from our source reader, we have to ship the data in our
       buffer. *)
    WHILE readSoFar < NUMBER(a)
            AND rd.source.getSub(SUBARRAY(a, readSoFar, 1)) > 0 DO
      IF a[readSoFar] # '\r' THEN
        (* If there is a 'carriage return', then read again.  That is, we
           simply throw away CR characters. *)
        INC(readSoFar);
      END;
    END;
    INC(rd.lo, readSoFar);
    INC(rd.hi, readSoFar);
    INC(rd.cur, readSoFar);
    RETURN readSoFar;
  END GetSubSimple;

PROCEDURE Close (rd: T) RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    rd.source.close();
    rd.closed := TRUE;
  END Close;

BEGIN
END DosToUnixRd.
