(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE RdExtras;

IMPORT Thread, Rd, ASCII;

PROCEDURE Skip(
    s: Rd.T;
    READONLY skip := ASCII.Spaces;
    unget := TRUE)
    : CHAR
    RAISES {Rd.Failure, Rd.EndOfFile, Thread.Alerted};
(* Gets characters from 's' until the end of stream is reached or a character
which is not in the set 'skip' is read. If a character which is not in the set
'skip' is read it is returned as the result and ungot if 'unget' is TRUE. If
end of stream is reached the 'EndOfFile' exception is raised. *)

PROCEDURE GetUntil(
    s: Rd.T;
    VAR chars: ARRAY OF CHAR;
    READONLY terminate := ASCII.Spaces;
    unget := TRUE)
    : CARDINAL
    RAISES {Rd.Failure, Thread.Alerted};
(* Gets characters from 's' and puts them into 'chars' until the end of stream
is reached, a character which is in the set 'terminate' is read or 'chars' is
filled up:
a) If the end of stream is reached no exception is raised and the number of
characters put into 'chars' is returned.
b) If a terminating character is read it is not put into 'chars' and is ungot
unless 'unget' is FALSE (i.e. if 'unget' is TRUE 'GetUntil' will ensure that
an immediately following get/put operation will read/overwrite the terminator
first). The number of characters put into 'chars' is returned.
c) If 'chars' is filled up one more character is read. If end of stream is
reached or the character is a terminator a) or b) applies. Otherwise the
character is unread and NUMBER(chars) + 1 is returned to indicate overflow *)

PROCEDURE GetChars(
    s: Rd.T;
    VAR chars: ARRAY OF CHAR;
    READONLY skip := ASCII.Spaces;
    READONLY terminate := ASCII.Spaces;
    unget := TRUE)
    : CARDINAL
    RAISES {Rd.Failure, Rd.EndOfFile, Thread.Alerted};
(* Reads characters from 's' and puts them into 'chars'; returns the number of
characters inserted into 'chars' or 'NUMBER(chars) + 1' if 'chars' overflows.
  First uses 'Skip' to skip any characters in 'skip', then reads characters
and puts them into 'chars' using 'GetUntil'.
  Note that 'EndOfStream' is only raised if the end of stream is reached during
the initial skip. Once a significant character has been read the end of stream
is treated as a terminator.
  'GetChars' may return 0. This happens if it encounters a terminal character
before it finds a character not in the 'skip' set *)

PROCEDURE GetText(
    s: Rd.T;
    READONLY skip := ASCII.Set{};
    READONLY terminate := ASCII.Spaces;
    unget := TRUE)
    : TEXT
    RAISES {Rd.Failure, Rd.EndOfFile, Thread.Alerted};
(* Similar to 'GetChars' but builds a text instead of putting characters into
an array. There is no limit to the length of text which can be read (other
than running out memory!) *)

END RdExtras.
