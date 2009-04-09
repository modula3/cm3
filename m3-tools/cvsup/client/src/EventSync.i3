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
<* PRAGMA LL *>

(* The "EventSync" interface makes it possible to program FormsVBT
   graphical user interfaces procedurally, without using callbacks.
   Instead of a web of event handlers and interacting state machines,
   the programmer creates one or more threads.  Each thread handles
   a more-or-less independent portion of the user interface, in a
   sequential, procedural style.  For many programmers, that kind
   of style is preferred, because it is more intuitive to them.

   The principal idea behind this approach is as follows.  Most
   user interfaces allow several interleaved interactions to take
   place simultaneously.  For example, in the midst of operating
   a file browser, the user may decide to raise the help window.
   Generally, such user interfaces can be decomposed into a number
   of relatively independent sub-GUIs, each of which follows a
   strictly sequential flow of control.  The programmer implements
   each such sub-GUI as a separate thread, which, for the most
   part, operates independently of the other sub-GUI threads.
   Within each thread, the programming is done in the familiar
   procedural style, to wit: wait for any of several legal user
   interactions to take place, then do something, based on the
   particular interaction the user chose.

   The "EventSync" interface supports the above style of programming.
   It provides the necessary synchronization between GUI-generated
   events and the programmer's sequential threads of control.  All
   events are handled behind the scenes; the programmer need not
   be aware of them at all.  In addition, "EventSync" conveniently
   manages FormsVBT "Filter" components, enabling and disabling
   them automatically at the appropriate times. *)

INTERFACE EventSync;

IMPORT AnyEvent, FormsVBT, Thread;

EXCEPTION Error(TEXT);

PROCEDURE DetailWait(fv: FormsVBT.T;
                     names: TEXT;
                     VAR event: AnyEvent.T): CARDINAL
  RAISES {Error, FormsVBT.Error, Thread.Alerted};
<* LL.sup < VBT.mu *>
(* Activate the components specified in "names", then wait for an event
   from one of them.  Pass the event back via "event", and return an
   identifying value corresponding to the component that triggered the
   event. *)

(* The "names" parameter specifies the set of FormsVBT interactors from
   which the user is allowed to choose.  The wait ends when the user
   activates one of them.  Here is an example of what "names" typically
   looks like:

| "quitButton=0 retryButton=1 resetButton=2"

   Here, "quitButton", "retryButton", and "resetButton" are the names of
   FormsVBT components.  The numbers after the equal signs serve to
   identify which component was activated to end the wait.  In this
   example, if the user pressed the reset button, "DetailWait" would
   return the value 2.  The identifying numbers are arbitrary
   non-negative integers.  They need not be sequential or unique.

   Continuing this example, the call to "DetailWait" might look like
   this:

| CASE DetailWait(fv, "quitButton=0 retryButton=1 resetButton=2", event) OF
| | 0 => (* Code for the "quit" action *)
| | 1 => (* Code for the "retry" action *)
| | 2 => (* Code for the "reset" action *)
| ELSE <* ASSERT FALSE *> END;

   As mentioned above, FormsVBT "Filter" components, if present, are
   managed automatically.  For example, suppose that the quit button is
   specified like this:

| (Filter Dormant
|   (Button %quitButton "Quit"))

   The quit button will begin in the dormant state, i.e., grayed-out
   in appearance, and unresponsive to user gestures.  However, when the
   "DetailWait" procedure is invoked, the "EventSync" package will
   notice that the quit button is a descendent of a "Filter", and will
   automatically change its state to "Active".  When "DetailWait"
   returns, it will restore all filters to their original states.  In
   this way, the user gets an automatic indication of what his options
   are at all times.

   This automatic feedback also can be applied to non-interactive
   components, such as "Text" components.  In the "names" argument, such
   components should appear without the equal sign or the identifying
   number.  For example:

| "quitButton=0 retryButton=1 resetButton=2 alertMessage"

   Here, any "Filter" above the "alertMessage" component will be
   activated as usual.  But "DetailWait" will not expect to receive any
   events from that component.  Obviously, at least one of the
   components mentioned in "names" needs to have an equal sign and
   identifying number; otherwise, the wait will never end.

   "DetailWait" is alertable.  It restores all filters to their original
   states before raising its "Thread.Alerted" exception.

   Often, detailed information about the triggering event is not needed.
   The simpler "Wait" procedure can be used, in that case. *)

PROCEDURE Wait(fv: FormsVBT.T; names: TEXT): CARDINAL
  RAISES {Error, FormsVBT.Error, Thread.Alerted};
<* LL.sup < VBT.mu *>
(* Like "DetailWait", except that the triggering event is not passed back
   to the caller. *)

END EventSync.
