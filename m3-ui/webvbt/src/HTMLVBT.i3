(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Sep 28 16:07:39 PDT 1995 by mhb                      *)
<* PRAGMA LL *>

(* An "HTMLVBT" is an abstract VBT class for displaying HTML. Subclasses
   override the methods here (in particular, the "init" method) with
   procedures that do something interesting. *)

INTERFACE HTMLVBT;

IMPORT Filter, HTML, Point, VBT;

TYPE
  T <: Public;
  Public =
    Filter.T OBJECT
    METHODS
      <* LL <= VBT.mu *>
      init (html: HTML.T): T;

      <* LL = VBT.mu *>
      hotlink (url: TEXT; READONLY cd: VBT.MouseRec);
      ismap   (url: TEXT; READONLY pt: Point.T; READONLY cd: VBT.MouseRec);
      isindex (typein: TEXT);
      form    ();
    END;

(* The call "v.init(...)" initialize "v" to be an "HTMLVBT.T" displaying
   the HTML given by "html".  The default method initializes "v" as a
   "Filter.T".

   The method "v.hotlink(url,cd)" is called when the user has clicked on a
   link.  The default is a noop.

   The method "v.ismap(url,pt,cd)" is called when the user has clicked on
   an active map.  The default is a noop.

   The method "v.isindex(typein)" is called when the user has entered a
   carriage return in the typein field of some html that contains an
   ISINDEX tag.  The default is a noop. *)

END HTMLVBT.
