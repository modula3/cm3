MODULE UnixToDosWr;

(* The implementation is rather inefficient, because it reads each
   character separately.  But the implementation is sufficiently simply in
   order to trust it. *)

IMPORT Wr, WrClass, Thread;

PROCEDURE New (wr: Wr.T): T =
  BEGIN
    RETURN NEW(T).init(wr);
  END New;

REVEAL
  T = Public BRANDED "UnixToDosWr.T" OBJECT
        target: Wr.T;
      OVERRIDES
        init := Init;
        (*
        These methods can not naturally be implemented, because they
        require reading of the source data.

        seek := Seek;
        length := Length;
        *)
        flush     := Flush;
        close     := Close;
        putString := PutString;
      END;


PROCEDURE Init (wr: T; target: Wr.T; ): T =
  BEGIN
    wr.target := target;
    wr.st := 0;
    wr.closed := FALSE;
    wr.buff := NIL;
    wr.seekable := FALSE;
    wr.cur := 0;
    wr.buffered := FALSE;
    wr.lo := wr.cur;
    wr.hi := wr.cur;
    RETURN wr
  END Init;


PROCEDURE Flush (wr: T) RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    wr.target.flush();
  END Flush;

PROCEDURE PutString (wr: T; READONLY buf: ARRAY OF CHAR; )
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    FOR i := FIRST(buf) TO LAST(buf) DO
      IF buf[i] = '\n' THEN
        wr.target.putString(ARRAY [0 .. 0] OF CHAR{'\r'});
      END;
      wr.target.putString(SUBARRAY(buf, i, 1));
    END;
  END PutString;

PROCEDURE Close (wr: T) RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    wr.target.close();
    wr.closed := TRUE;
  END Close;

BEGIN
END UnixToDosWr.
