(* Copyright 1992 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* Last modified on Wed Feb  8 16:30:53 PST 1995 by kalsow   *)
(*      modified on Tue Nov  3 13:15:26 PST 1992 by sclafani *)

UNSAFE MODULE Midic;

IMPORT M3toC, Midi, MidiPrivate, Word;
IMPORT Ctypes, Uerror, Unix, Uuio;
FROM Midi IMPORT Failure;

REVEAL
  T = Midi.T BRANDED OBJECT
        midic: INTEGER;
      OVERRIDES
        init  := Open;
        play  := Play;
        close := Close;
      END;

TYPE Byte = BITS 8 FOR [0 .. 255];

PROCEDURE Open (t: T; name: TEXT): Midi.T RAISES {Failure} =
  VAR
    status    : INTEGER;
    discipline: INTEGER;
    info: RECORD
            sg_ispeed: Ctypes.char;   (* input speed *)
            sg_ospeed: Ctypes.char;   (* output speed *)
            sg_erase : Ctypes.char;   (* erase character *)
            sg_kill  : Ctypes.char;   (* kill character *)
            sg_flags : Ctypes.short;  (* mode flags *)
          END;
  BEGIN
    (* Connect to our midic *)
    WITH string = M3toC.CopyTtoS (name) DO
      t.midic := Unix.open (string, Unix.O_RDWR,
                            Word.Or (Unix.MROWNER, Unix.MWOWNER));
      M3toC.FreeCopiedS (string);
    END;
    IF t.midic < 0 THEN UnixFail ("open"); END;

    (* Set up midic device as we want it *)
    status := Unix.ioctl (t.midic, Unix.TIOCGETD, ADR (discipline));
    IF status < 0 THEN UnixFail ("TIOCGETD"); END;
    status := Unix.ioctl (t.midic, Unix.TIOCGETP, ADR (info));
    IF status < 0 THEN UnixFail ("TIOCGETP"); END;
    discipline := Unix.NTTYDISC;
    info.sg_ispeed := 13;       (* B9600 *)
    info.sg_ospeed := 13;       (* B9600 *)
    info.sg_flags := 16_20;     (* RAW *)
    status := Unix.ioctl (t.midic, Unix.TIOCSETD, ADR (discipline));
    IF status < 0 THEN UnixFail ("TIOCSETD"); END;
    status := Unix.ioctl (t.midic, Unix.TIOCSETP, ADR (info));
    IF status < 0 THEN UnixFail ("TIOCSETP"); END;
    status := Unix.ioctl (t.midic, Unix.TIOCGETD, ADR (discipline));
    IF status < 0 THEN UnixFail ("TIOCGETD"); END;
    status := Unix.ioctl (t.midic, Unix.TIOCGETP, ADR (info));
    IF status < 0 THEN UnixFail ("TIOCGETP"); END;
    RETURN t;
  END Open;

PROCEDURE Close (t: T) RAISES {Failure} =
  BEGIN
    WITH status = Unix.close (t.midic) DO
      IF status < 0 THEN UnixFail ("close"); END;
    END;
  END Close;

PROCEDURE Play (         t     : T;
                READONLY buf   : ARRAY OF Byte;
                         start : CARDINAL        := 0;
                         length: CARDINAL        := LAST (CARDINAL))
  RAISES {Failure} =
  BEGIN
    length := MIN (length, NUMBER (buf) - start);
    length := Uuio.write (t.midic, ADR (buf [start]), length);
    IF length < 0 THEN UnixFail ("write"); END;
  END Play;

PROCEDURE UnixFail (msg: TEXT) RAISES {Failure} =
  BEGIN
    RAISE Failure (msg & ": unix error: "
                     & M3toC.StoT (Uerror.GetFrom_sys_errlist (
                                     Uerror.errno)));
  END UnixFail;

BEGIN
END Midic.
