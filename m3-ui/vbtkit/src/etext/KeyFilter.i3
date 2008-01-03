(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Jim Meehan and Mark Manasse                                            *)
(* Last modified on Sun Mar 21 21:27:49 PST 1993 by meehan                   *)
(*      modified on Thu Mar  4 21:15:32 PST 1993 by msm                      *)
(*                                                                           *)
(* NOTE: The table of character combinations is in the file:                 *)
(*       /proj/m3/pkg/vbtkitdoc/ISO-Latin.tex                                *)

INTERFACE KeyFilter;

IMPORT VBT;

TYPE
  T = OBJECT
        next: T
      METHODS
        apply (v: VBT.T; cd: VBT.KeyRec)
      END;
  Composer <: T OBJECT
              METHODS
                feedback (v: VBT.T; composing: BOOLEAN)
              END;
  ComposeChar <: Composer;
  Diacritical <: Composer;

PROCEDURE IsModifier (c: VBT.KeySym): BOOLEAN;
(* Test whether "c" is a ``modifier'' key, such as Shift,
   Control, or Meta.  Such keys are usually ignored by
   "Composer"s.  Equivalent to:
| KeyboardKey.Shift_L <= c AND c <= KeyboardKey.Hyper_R
   *)

END KeyFilter. 

(* A "KeyFilter"'s "apply" method takes a "VBT.T" and a "VBT.KeyRec"
   and may pass them on to the "KeyFilter" in its "next" field,
   possibly having altered the "KeyRec" in the process.  For example,
   a ``transparent'' filter would simply call "SELF.next.apply(v,cd)".
   An ``upper-case filter'' (transducer) would convert lower-case
   characters to upper-case before passing them on.

   A "KeyFilter" may also maintain an internal state, and it is not
   required to call "SELF.next.apply" on every call.  Various
   ``character composition'' schemes, for example, involve typing one
   character (e.g., a key labeled ``Compose Character'') followed by
   two others, which are all ``composed'' to produce a single
   character. That is, they effectively implement a ``look-ahead''
   reader.

   A "Composer" is a subtype that provides a "feedback" method; the
   intention is that the "apply" method calls "SELF.feedback(v, TRUE)"
   when it sees a key that begins a multi-character sequence, and
   "SELF.feedback(v, FALSE)" when it sees a key that ends a sequence.
   The default "feedback" method is a no-op, but a client may wish to
   override that in order to provide a visual cue to the user that
   key-composition is in effect (e.g., changing the cursor).
   Otherwise, the user might not understand why typed character are
   not being ``echoed.''

   Two types of "Composer"s are provided, "ComposeChar" and
   "Diacritical".  "ComposeChar" produces the ISO Latin-1 (8-bit,
   extended ASCII) characters, using the VT220 style of composition:
   when the filter sees a "Keyrec" whose "whatChanged" field is
   "KeyboardKey.MultiKey", it calls "SELF.feedback(v,TRUE)"; after two
   more "KeyRec"s have been passed to it, it looks for those two keys
   in an internal table.  If it finds a character, then it passes it
   to "SELF.next.apply". For example, on many keyboards, there is a
   key labeled "Compose" or "Compose Character", which produces the
   "MultiKey" code. When you type that key, followed by ``c'' and
   ``o'', the filter passes the character for the copyright symbol,
   \copyright, to the "next" filter. If there is no entry in the
   table, the filter does not pass anything to the "next" filter.  In
   any case, it always returns to its initial state.

   For some users, the ``Compose'' key is also the ``meta'' or
   ``option'' key. Holding this key down and typing ``a'', for
   example, produces a "KeyRec" with the "mod1" modifier (which
   Trestle represents as "VBT.Modifier.Option"). When the
   "ComposeChar" filter sees a "KeyRec" with this modifier, it
   assumes that the user is {\em not} composing an 8-bit character,
   so it calls "SELF.feedback(v,FALSE)" and "SELF.next.apply(v,cd)", and
   it returns to its initial state.

   A "Diacritical" filter also produces 8-bit characters. The filter
   looks at 2-character sequences; comma followed by ``c'', for
   example, produces an ``c'' with a cedilla, \c{c}. If the sequence
   is not defined, such as comma followed by space, then filter passes
   both characters to the "next" filter; i.e., when it receives the
   second "KeyRec", it makes {\em two} calls to "SELF.next.apply".
   (This is why the "KeyFilter" uses a "next" field instead of merely
   returning a "KeyRec".)

   Here is an example showing the intended use of this interface.
   Assume that "TextEditingVBT" is a subtype of "VBT" used for typing
   text, such as "TypeinVBT.T" or "TextPort.T".  A client would
   override the "key" method in order to filter the keys delivered to
   the supertype's "key" method.

| TYPE
|   MyTextEditor =
|     TextEditingVBT.T OBJECT
|         comp: KeyFilter.ComposeChar
|       OVERRIDES
|         key := Key 
|       END;
|   Parent = Keyfilter.T OBJECT
|                OVERRIDES
|                   apply := ApplyParent
|                END;
| 
| PROCEDURE Key (v: MyTextEditor; READONLY cd; VBT.KeyRec) =
|   BEGIN
|     IF cd.wentDown AND cd.whatChanged # VBT.NoKey THEN
|       v.comp.apply (v, cd)
|     END
|   END Key;
|
| PROCEDURE ApplyParent (self : MyParent; 
|                        v    : VBT.T; 
|                        cd   : VBT.KeyRec) =
|   BEGIN
|     TextEditingVBT.T.key (v, cd)
|   END ApplyMyParent;
|
| VAR editor := NEW (MyTextEditor,
|                    comp := NEW (KeyFilter.ComposeChar,
|                                 next := NEW (Parent)));

   A "ComposeChar" object is not case-sensitive where there is no
   ambiguity.  For example, "c" and "o" can be combined to produce the
   copyright symbol, \copyright; so can "C" and "O", "c" and "O", or
   "C" and "o".  By contrast, "e" and "`" can be combined to produce a
   lower-case "e" with a grave accent, \`{e}, but "E" and "`" produce
   an upper-case "E" with a grave accent, \`{E}.

   Unless both of the characters are alphanumeric, they can be
   combined in either order.  So "`" and "e" have the same effect as
   "e" and "`", but "o" and "c" do {\em not} combine to form the
   copyright symbol.

*)
