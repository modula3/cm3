(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu Jan 30 09:45:28 PST 1992 by kalsow     *)

UNSAFE MODULE RT0;

IMPORT Word;(* THIS MODULE ALMOST MUST NOT IMPORT ANY OTHERS! *)

(* See RefHeaderBitFields
 * Actual bitfields are avoided so that C++ bootstrap
 * is endian neutral
 *)

(* Most fields are boolean so helpers for that. *)
PROCEDURE Set (header: UNTRACED REF RefHeader; value: BOOLEAN; offset: [0..63]) =
BEGIN
  header.word := Word.Insert (header.word, ORD (value), offset, 1);
END Set;

PROCEDURE Get (header: UNTRACED REF RefHeader; offset: [0..63]): BOOLEAN =
BEGIN
  RETURN Word.Extract (header.word, offset, 1) # 0;
END Get;

PROCEDURE Forwarded (header: UNTRACED REF RefHeader): BOOLEAN =
BEGIN
  RETURN Get (header, 0);
END Forwarded;

PROCEDURE GetTypecode (header: UNTRACED REF RefHeader): Typecode =
BEGIN
  RETURN Word.Extract (header.word, 1, 20);
END GetTypecode;

PROCEDURE Dirty (header: UNTRACED REF RefHeader): BOOLEAN =
BEGIN
  RETURN Get (header, 21);
END Dirty;

PROCEDURE Gray (header: UNTRACED REF RefHeader): BOOLEAN =
BEGIN
  RETURN Get (header, 22);
END Gray;

PROCEDURE Weak (header: UNTRACED REF RefHeader): BOOLEAN =
BEGIN
  RETURN Get (header, 23);
END Weak;

PROCEDURE MarkA (header: UNTRACED REF RefHeader): BOOLEAN =
BEGIN
  RETURN Get (header, 24);
END MarkA;

PROCEDURE MarkB (header: UNTRACED REF RefHeader): BOOLEAN =
BEGIN
  RETURN Get (header, 25);
END MarkB;

PROCEDURE Spare (header: UNTRACED REF RefHeader): [0..63] =
BEGIN
  RETURN Word.Extract (header.word, 26, 6);
END Spare;

PROCEDURE SetForwarded (header: UNTRACED REF RefHeader; value := TRUE) =
BEGIN
  Set (header, value, 0);
END SetForwarded;

PROCEDURE SetDirty (header: UNTRACED REF RefHeader; value: BOOLEAN) =
BEGIN
  Set (header, value, 21);
END SetDirty;

PROCEDURE SetGray (header: UNTRACED REF RefHeader; value: BOOLEAN) =
BEGIN
  Set (header, value, 22);
END SetGray;

PROCEDURE SetWeak (header: UNTRACED REF RefHeader; value: BOOLEAN) =
BEGIN
  Set (header, value, 23);
END SetWeak;

PROCEDURE SetMarkA (header: UNTRACED REF RefHeader; value: BOOLEAN) =
BEGIN
  Set (header, value, 24);
END SetMarkA;

PROCEDURE SetMarkB (header: UNTRACED REF RefHeader; value: BOOLEAN) =
BEGIN
  Set (header, value, 25);
END SetMarkB;

PROCEDURE SetSpare (header: UNTRACED REF RefHeader; value: [0..63]) =
BEGIN
  header.word := Word.Insert (header.word, value, 26, 6);
END SetSpare;

PROCEDURE Pack (typecode: Typecode; dirty: BOOLEAN := FALSE): RefHeader =
VAR header := RefHeader {0};
BEGIN
  header.word := Word.Insert (0, ORD(typecode), 1, 20);
  SetDirty (ADR (header), dirty);
  RETURN header;
END Pack;

BEGIN
END RT0.

