(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Last modified on Tue Jan 26 09:20:57 PST 1993 by mhb      *)
(*      modified on Thu Sep 24 11:51:56 PDT 1992 by steveg   *)
<* PRAGMA LL *> 

(* "AlbumVBT" manages a photo album of snapshots of VBTs.  The
   snapshots are taken by "Image.FromVBT". *)

INTERFACE AlbumVBT;

IMPORT Axis, VBT;

TYPE
  T <: TPublic;
  TPrivate <: VBT.Split;
  TPublic = TPrivate OBJECT
            METHODS
              <* LL <= VBT.mu *>
              init (layoutAxis   : Axis.T;
                    size         : CARDINAL;
                    width, height: REAL       := 30.0): T;

              <* LL = VBT.mu *>
              add   (w: VBT.T);
              clear ();
            END;

(* The call "v.init(layoutAxis, size, width, height)" initializes
   "v" to be an "AlbumVBT.T".  An "AlbumVBT.T" is a "VBT.Split"
   that manages the layout of a series of images produced by
   taking snapshots of VBTs using "Image.FromVBT".  The images in "v"
   are all shaped "v.width" by "v.height" millimeters and laid
   out in a grid.

   If "v.layoutAxis" is horizontal, then there are "v.size"
   images laid out horizontally in each row, with rows laid out
   top to bottom.  If "v.layoutAxis" is vertical, then there are
   "v.size" images per column and the columns are laid out left
   to right.

   The method call
| v.add(w)
   adds a new snapshot to the album.  It occupies the next slot
   either vertically or horizontally (depending on
   "v.layoutAxis").

   The method call
| v.clear()
   removes all snapshots from the album. *)

END AlbumVBT.

