(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* PaintPrivate.def, by msm & cgn, Wed May  6 16:57:44 1987 *)
(* Last modified on Sat Apr  2 16:46:50 PST 1994 by heydon  *)
(*      modified on Thu May 20 14:42:00 PDT 1993 by msm     *)
(*      modified on Mon Feb 24 13:57:24 PST 1992 by muller  *)
(*      modified on Thu Dec 12  0:33:16 PST 1991 by gnelson *)
(*      modified on Mon Jun  4 11:30:30 PDT 1990 by steveg *)
<*PRAGMA LL*>

(* This interface defines the layout of entries
   in paint batches. *)

INTERFACE PaintPrivate;

IMPORT Rect, Point, Trapezoid, Word;

TYPE
  PaintOp = INTEGER;
  Pixmap = INTEGER;
  Font = INTEGER;

(* In a paint batch, "PaintOps", "Pixmaps", and "Fonts" are represented
   by integers in a screentype-dependent way.  During rescreening an
   old batch might find its way to a screen of the wrong type, causing
   garbage to be painted; but the garbage will be painted over with
   the correct pixels promptly.  *)

TYPE
  PaintCommand = {RepeatCom, TintCom, TextureCom,
    PixmapCom, ScrollCom, TrapCom, TextCom,
    ExtensionCom};
  PackedCommand = PaintCommand;
  FixedSzCommand =
    [PaintCommand.RepeatCom..PaintCommand.TrapCom];
  ByteOrder = {MSBFirst, LSBFirst};
  PackedByteOrder = ByteOrder;

VAR (*CONST*)
  HostByteOrder: ByteOrder;

(* There are eight types of entries; each of which begins with a word
   containing a "PaintCommand" that indicates which type of entry it is.

   Entries of type "TintCom", "TextureCom", "PixmapCom", "ScrollCom",
   "TrapCom", and "TextCom" are used to implement the "VBT" operations
   "PaintTint", "PaintTexture", "PaintPixmap", "Scroll", "PaintTrapezoid",
   and "PaintText/PaintSub".

   A "RepeatCom" entry in a batch indicates that the preceding entry
   is to be re-executed with its clipping rectangle changed to that
   of the "RepeatCom" entry.  For example, these are used for
   implementing "PolyTint", "PolyTexture", and "PaintRegion".  There
   are some restrictions on where "RepeatCom" entries can occur.

   "ExtensionCom" entries can be used to implement additional painting
   operations beyond those that are built into Trestle.

   Some of the entries are fixed size; that is, the size of the entry
   is determined by their type.  The following array gives the sizes
   of the fixed-size commands: *)

CONST
  WS = BYTESIZE(Word.T);
  ComSize =
    ARRAY FixedSzCommand OF INTEGER
    {(BYTESIZE(CommandRec) + WS-1) DIV WS,
     (BYTESIZE(TintRec) + WS-1) DIV WS,
     (BYTESIZE(PixmapRec) + WS-1) DIV WS,
     (BYTESIZE(PixmapRec) + WS-1) DIV WS,
     (BYTESIZE(ScrollRec) + WS-1) DIV WS,
     (BYTESIZE(TrapRec) + WS-1) DIV WS};

(* "ComSize[c]" equals the size in "Word.T"s of a paint batch entry for
   the command "c". *)

TYPE
  CommandRec =
    RECORD command: PackedCommand; clip: Rect.T END;
  CommandPtr = UNTRACED REF CommandRec;
  RepeatPtr = CommandPtr;

  (* We define a "Rec" and a "Ptr" type for each kind of batch entry.

     Every batch entry is a ``pseudo-subtype'' of a "Command", in the
     sense that its record type has "CommandRec" as a prefix.

     A repeat command has no other fields besides the command identifier
     itself and the clipping rectangle.  Hence a "RepeatPtr" is simply
     a pointer to a "CommandRec".

     All of the batch entries that are not repeat commands contain
     a "PaintOp".  They are all pseudo-subtypes of the following
     "Rec" and "Ptr" types: *)

  PaintRec = RECORD
    command: PackedCommand;
    clip: Rect.T;
    op: PaintOp
  END;
  PaintPtr = UNTRACED REF PaintRec;

(* The following four entry types correspond to "PaintTint", "PaintPixmap",
   "Scroll", and "PaintTrapezoid" operations.  *)

  TintRec = RECORD
    command: PackedCommand;
    clip: Rect.T;
    op: PaintOp
  END;
  TintPtr = UNTRACED REF TintRec;

  PixmapRec = RECORD
    command: PackedCommand;
    clip: Rect.T;
    op: PaintOp;
    delta: Point.T;
    pm: Pixmap
  END;
  PixmapPtr = UNTRACED REF PixmapRec;
  TexturePtr = PixmapPtr;

  ScrollRec = RECORD
    command: PackedCommand;
    clip: Rect.T;
    op: PaintOp;
    delta: Point.T;
  END;
  ScrollPtr = UNTRACED REF ScrollRec;

(* It is illegal for a "ScrollRec" to be directly followed in a batch
   by a "Repeat" command. *)

  TrapRec = RECORD
    command: PackedCommand;
    clip: Rect.T;
    op: PaintOp;
    delta: Point.T;
    pm: Pixmap;
    p1, p2: Point.T;
    m1, m2: Trapezoid.Rational;
  END;
  TrapPtr = UNTRACED REF TrapRec;

(* If "tr" is a "TrapRec", then "tr.p1" and "tr.p2" are points that
   are on the extensions of the west and east edges of the trapezoid,
   and "tr.m1" and "tr.m2" are the slopes of the west and east edges.
   The slopes are given as "(delta v) / (delta h)".  A zero denominator
   represents an infinite slope; i.e., a vertical edge.  A zero
   numerator is illegal.  *)

(* The entries that are not fixed-size are pseudo-subtypes of
   "VarSzRec", which contains a "size" field with the number of
   "Word.T"'s in the entire entry.  *)

  VarSzRec = RECORD
    command: PackedCommand;
    clip: Rect.T;
    op: PaintOp;
    szOfRec: INTEGER;
  END;
  VarSzPtr = UNTRACED REF VarSzRec;

(* "PaintText" and "PaintSub" operations result in the following entry
   type, in which "command" will equal "TextCom": *)

TYPE Prop = {Clipped, FontSub};
     Props = SET OF Prop;

  TextRec = RECORD
    command: PackedCommand;
    clip: Rect.T;
    op: PaintOp;
    szOfRec: INTEGER;
    byteOrder: PackedByteOrder;
    props: Props;
    refpt: Point.T;
    fnt: Font;
    xftFnt : ADDRESS;
    txtsz, dlsz: INTEGER;

(*
| (* dl: ARRAY [0..dlsz-1] OF VBT.Displacement *)
| (* chars: ARRAY [0..txtsz-1] OF CHAR *)
*)

  END;
  TextPtr = UNTRACED REF TextRec;

(* In a "TextRec", "Prop.Clipped" must be in "props" if the bounding box of
   "Text.FromChars(chars)" painted at "refpt" in the font "fnt" is not a
   subset of "clip". A "TextRec" can be directly followed in a batch by a
   "Repeat" only if "Prop.Clipped" is present. "Prop.FontSub" is in "props"
   implies that the bounding box that must be cleared before painting is not
   the natural bounding box for the font.  The "dl" and "chars" fields are
   declared in comments since Modula-3 does not allow a record to contain a
   variable-sized array; they must be accessed using address arithmetic. The
   "chars" field will be padded out so that the "TextRec" ends on a word
   boundary. *)

(* The "byteOrder" field defines the byteorder of the characters.  (Since
   paint batches can be transported across address spaces and merged,
   the byte order could be different for different records in a paint
   batch.) *)

  ExtensionRec = RECORD
    command: PackedCommand;
    clip: Rect.T;
    op: PaintOp;
    szOfRec: INTEGER;
    delta: Point.T;
    pm: Pixmap;
    fnt: Font;
    subCommand: INTEGER;

(*
| (* extensionData: ARRAY OF CHAR *)
*)

  END;
  ExtensionPtr = UNTRACED REF ExtensionRec;

(* An "ExtensionRec" can be used to implement painting operations that
   exploit rendering primitives that may be available on some particular
   implementation.  Extension commands get a "PaintOp", a "delta", a "pm",
   and a "fnt" ``for free''; they can also put whatever data they need
   into the rest of the extension data part of the record.  The field
   "szOfRec" is the number of "Word.Ts" in the extension record,
   including the extension data.  When an "ExtensionRec" is translated,
   it's "clip" and "delta" fields are translated automatically; its
   extension data is unaffected.  *)

PROCEDURE CommandLength(p: CommandPtr): INTEGER;
(* Return the length in words of the command entry "p". *)

END PaintPrivate.
