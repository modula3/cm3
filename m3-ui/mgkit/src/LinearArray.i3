(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Mon Jul 20 23:41:21 1992 by steveg   *)

<*PRAGMA LL*>

(* Lists, Queues, Stacks and Buffers.  Lumped together because they are all
   linear displays of things.
*)

INTERFACE LinearArray;

IMPORT
  Axis, MG, MGV, PaintOp;

TYPE
  T <: TPublic;
  TPublic = MG.Group OBJECT
              <* LL = v.mu *>
              graphic        : MG.T;
              next, prev            : T;
              linkToNext, linkToPrev: LinkerRec;
            METHODS
              <* LL < v.mu *>
              init (v: V; graphic: MG.T): T;
              (* displays self using "graphic", sets the linker to the
                 default linker (if it is NIL), centers the graphic around
                 the origin, sets Pos(self) to the origin, and sets
                 visibility to 0.0 (invisible). *)

              <* LL = v.mu *>
              setNextPrev(v: V; np: NP; t: T);
              setNextPrevLink(v: V; np: NP; READONLY link: LinkerRec);
            END;

TYPE
  NP = {Next, Prev, Label};
  HT = {Head, Tail};

TYPE
  V <: VPublic;
  VPublic =
    MGV.V OBJECT
      <* LL = self.mu *>
      dx, dy := 5.0;
      (* separation distance between elements *)

      axis := Axis.T.Hor;
      (* axis of elements *)

      alignment := Alignment.AboveLeft;
      (* side of elements to locate elements that are "align"ed. *)

      linker: Linker := NIL;
      (* self.linker.new(from, to) returns a graphical element that acts as
         a link connecting "from" and "t".  The link ends should be
         individually controllable so "from" and "to" can move separately.

         If linker = NIL, linkerDefault is used and returns a MG.Line with
         MG.LineEnds at "from" and "to".  The visibility of "to" controls
         the visibility of the default link. *)

      (* internal state fields *)
      head, tail          : T := NIL;
      headLabel, tailLabel: T := NIL;
      labelLinks             := FALSE;

      aligned: T := NIL;
      (* list of aligned elements, so they are displayed *)

      cntElems := 0;
      (* count of elements in the linear array, maintained by insert and
         delete *)

      width, height: REAL;
      (* size of elements *)

    METHODS
      <* LL < self.mu *>
      init (width, height: REAL): V;
      (* dimensions of a box or label in the linear array *)

      <* LL = self.mu *>
      setLabel (ht: HT; graphic: MG.T);
      (* show "graphic" at the head/tail of the "list", include a link
         "self.labelLinks" is TRUE *)

      clear ();
      (* reset to have no elements *)

      align (t, dest: T);
      (* Creates and animation moving "t" to be aligned with "dest". *)

      insert (t, prev: T);
      (* Creates and animation to insert "t" after "prev".  If "prev" = NIL
         then insert as new head.  Handles next, prev, and links.  Updates
         head and tail as necessary *)

      delete (t: T);
      (* Creates and animation to delete "t" from "self"'s list.  Handles
         next, prev, and links.  Updates head and tail as necessary *)
    END;
(* A vbt displaying a linear array of elements. *)

<* LL < v.mu *>
PROCEDURE Align (v: V; t, dest: T);
PROCEDURE Clear (v: V);
PROCEDURE Delete (v: V; t: T);
PROCEDURE Insert (v: V; t, prev: T);
PROCEDURE Link (v: V; from, to: T);
(* The three above procedures handle locking, new shapes, animation, and
   redisplay for calls of the align, insert and delete methods *)

TYPE
  Alignment = {None, AboveLeft, AboveRight, BelowLeft, BelowRight};
  (*| Position of "align"ed elements relative to the destination
      element.  Head/Tail pointers (if any) are aligned on the opposite
      side (or "BelowRight" is used if "None" was specified) *)

TYPE
  LinkerRec = RECORD from, to: MG.T; fromT, toT: T END;
  Linker =
    OBJECT METHODS new (v: V; from, to: T; type: NP): LinkerRec END;
(* v.linker.new returns a pair of MG.T elements controlling a graphical
   link between "from" and "to".   If either "from" or "to" is NIL
   then a "very short" link should be produced with both ends
   controlled by the non-NIL end.  "type" maybe be used to distinguish
   next, prev and label links.  *)

VAR linkerDefault: Linker;
(* If v.linker = NIL, linkerDefault is used and returns a MG.Line
   with MG.LineEnds at "from" and "to".  The visibility of "to"
   controls the visibility of the default link. *)

TYPE
  List <: V;

TYPE
  DoublyLinkedList <: DoublyLinkedListPublic;
  DoublyLinkedListPublic = List OBJECT
    nextLinkColor, prevLinkColor: PaintOp.ColorScheme;
  END;
  (* Like a List except that linkToPrev is maintained and displayed *)


TYPE
  QSB = V OBJECT
       METHODS
         <* LL = self.mu *>
         push (t: T);
         pop  (): T;
       END;

  Queue <: QSB;
  (* A FIFO - self.push pushes at the tail and self.pop pops from the head *)

  Stack <: QSB;
  (* A LIFO - self.push pushes at the head and self.pop pops from the head *)

TYPE
  Buffer <: BufferPublic;
  BufferPublic =
    QSB OBJECT
      <* LL = self.mu *>
      pushSide, popSide: HT;
      (* pushSide = HT.Tail, popSide = HT.Head => Queue behavior pushSide =
         HT.Tail, popSide = HT.Tail => Stack behavior *)

      slots               : REF ARRAY OF MG.T := NIL;
      headIndex, tailIndex: INTEGER           := 0;

      emptyColor: PaintOp.ColorScheme := NIL;
      (* emptyColor rectangle will fill empty slots in the buffer.  if no
         color is given, then the empty slots will be invisible *)
    METHODS
      <* LL < self.mu *>
      init (width, height: REAL; size: CARDINAL; pushSide, popSide: HT):
            Buffer;

      <* LL = self.mu *>
      grow (newSize: CARDINAL);
    END;

<* LL < v.mu *>
PROCEDURE Push (v: QSB; t: T);
PROCEDURE Pop (v: QSB);

PROCEDURE GrowBuffer(v: Buffer; newSize: INTEGER);

END LinearArray.
