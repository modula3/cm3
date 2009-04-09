(* Copyright 1996-2003 John D. Polstra.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgment:
 *      This product includes software developed by John D. Polstra.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id$ *)

MODULE EventSync;

IMPORT
  AnyEvent, Cursor, FormsVBT, ReactivityVBT, RefList, Split, TSplit,
  Text, Thread, VBT;

<* FATAL Split.NotAChild *>

TYPE
  Registration = FormsVBT.Closure OBJECT
      cond: Thread.Condition;
      interactors: InteractorList := NIL;
      trigger: Interactor := NIL;
      event: AnyEvent.T;
    OVERRIDES
      apply := HandleEvent;
    END;

  Interactor = REF RECORD
      name: TEXT;
      ord: INTEGER;
      filter: ReactivityVBT.T := NIL;
      state: ReactivityVBT.State;
      cursor: Cursor.T;
    END;

  InteractorList = RefList.T;

PROCEDURE DetailWait(fv: FormsVBT.T;
                     names: TEXT;
		     VAR event: AnyEvent.T): CARDINAL
  RAISES {Error, FormsVBT.Error, Thread.Alerted} =
  VAR
    reg := NEW(Registration,
	       cond := NEW(Thread.Condition),
	       interactors := ParseNames(names));
    iList := reg.interactors;
    inter: Interactor;
    child: VBT.T;
    parent: VBT.Split;
  BEGIN
    LOCK VBT.mu DO
      WHILE iList # NIL DO
	inter := iList.head;

	(* Switch all ancestor TSplits so that the interactor will be
	   displayed.  Also, find the nearest ancestor filter, if any. *)
	child := FormsVBT.GetVBT(fv, inter.name);
	parent := VBT.Parent(child);
	WHILE parent # NIL DO
	  TYPECASE parent OF
	  | TSplit.T(tSplit) =>
	      TSplit.SetCurrent(tSplit, child);
	  | ReactivityVBT.T(filter) =>
	      IF inter.filter = NIL THEN inter.filter := filter END;
	  ELSE (* Ignore *) END;
	  child := parent;
	  parent := VBT.Parent(child);
	END;

	(* If the caller is interested in events from this interactor,
	   then attach our event handler to it. *)
	IF inter.ord >= 0 THEN FormsVBT.Attach(fv, inter.name, reg) END;

	(* If we found a filter, then save its current state and make it
	   active. *)
	IF inter.filter # NIL THEN
	  inter.state := ReactivityVBT.Get(inter.filter);

	  (* FIXME - This function is slated for a future SRC release,
	     but most people don't have it yet.
	  inter.cursor := ReactivityVBT.GetCursor(inter.filter);
	  *)(* so we use this instead ... *)
	  inter.cursor := Cursor.DontCare;
	  (**)

	  ReactivityVBT.Set(inter.filter, ReactivityVBT.State.Active,
	    Cursor.DontCare);
	END;

	iList := iList.tail;
      END;

      WHILE reg.trigger = NIL DO
	TRY
	  Thread.AlertWait(VBT.mu, reg.cond);
	EXCEPT Thread.Alerted =>
	  (* Restore the original state, then reraise the exception. *)
	  iList := reg.interactors;
	  WHILE iList # NIL DO
	    inter := iList.head;
	    IF inter.filter # NIL THEN  (* Restore filter state. *)
	      ReactivityVBT.Set(inter.filter, inter.state, inter.cursor);
	    END;
	    IF inter.ord >= 0 THEN  (* Event-generating interactor. *)
	      FormsVBT.Attach(fv, inter.name, NIL);
	    END;
	    iList := iList.tail;
	  END;
	  RAISE Thread.Alerted;
	END;
      END;
    END;

    event := reg.event;
    RETURN reg.trigger.ord;
  END DetailWait;

PROCEDURE HandleEvent(reg: Registration;
                      fv: FormsVBT.T;
		      name: TEXT;
		      <*UNUSED*> when: VBT.TimeStamp) =
  <* FATAL FormsVBT.Error *>
  VAR
    iList := reg.interactors;
    inter: Interactor;
  BEGIN
    (* The calling thread holds VBT.mu. *)
    WHILE iList # NIL DO
      inter := iList.head;
      IF inter.filter # NIL THEN  (* Restore filter state. *)
	ReactivityVBT.Set(inter.filter, inter.state, inter.cursor);
      END;
      IF inter.ord >= 0 THEN  (* Event-generating interactor. *)
	FormsVBT.Attach(fv, inter.name, NIL);
	IF Text.Equal(name, inter.name) THEN
	  reg.trigger := inter;
	  reg.event := FormsVBT.GetTheEvent(fv);
	END;
      END;
      iList := iList.tail;
    END;
    <* ASSERT reg.trigger # NIL *>
    Thread.Signal(reg.cond);
  END HandleEvent;

PROCEDURE ParseNames(names: TEXT): InteractorList
  RAISES {Error} =
  CONST
    Blanks = SET OF CHAR{' ', '\t', '\n', '\r'};
    Stoppers = Blanks + SET OF CHAR{'='};
    Digits = SET OF CHAR{'0'..'9'};
  VAR
    len := Text.Length(names);
    pos: CARDINAL := 0;
    end: CARDINAL;
    head, tail: InteractorList := NIL;
    name: TEXT;
    inter: Interactor;
  BEGIN
    LOOP
      (* Skip white space. *)
      WHILE pos < len AND Text.GetChar(names, pos) IN Blanks DO
	INC(pos);
      END;

      IF pos >= len THEN EXIT END;

      (* Scan to the end of the name. *)
      end := pos;
      REPEAT
	INC(end);
      UNTIL end >= len OR Text.GetChar(names, end) IN Stoppers;

      name := Text.Sub(names, pos, end - pos);
      inter := NEW(Interactor, name := name);

      IF end < len AND Text.GetChar(names, end) = '=' THEN
	(* Caller specified ordinal. *)
	INC(end);
	pos := end;
	inter.ord := 0;
	WHILE end < len DO
	  WITH ch = Text.GetChar(names, end) DO
	    IF NOT ch IN Digits THEN EXIT END;
	    inter.ord := 10*inter.ord + ORD(ch) - ORD('0');
	  END;
	  INC(end);
	END;
	IF end = pos OR
	end < len AND NOT Text.GetChar(names, end) IN Blanks THEN
	  RAISE Error("Invalid number after \"" & name & "=\"");
	END;
      ELSE  (* No events desired from this interactor. *)
	inter.ord := -1;
      END;


      (* Append the new entry to the list. *)
      WITH l = NEW(InteractorList, head := inter, tail := NIL) DO
	IF tail = NIL THEN head := l ELSE tail.tail := l END;
	tail := l;
      END;

      pos := end;
    END;

    RETURN head;
  END ParseNames;

PROCEDURE Wait(fv: FormsVBT.T; names: TEXT): CARDINAL
  RAISES {Error, FormsVBT.Error, Thread.Alerted} =
  VAR
    event: AnyEvent.T;
  BEGIN
    RETURN DetailWait(fv, names, event);
  END Wait;

BEGIN
END EventSync.
