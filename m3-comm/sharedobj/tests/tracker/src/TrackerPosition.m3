(*
* TrackerPosition.m3 -- a tracker position object
* Copyright (C) Blair MacIntyre 1995
* Author          : Blair MacIntyre
* Created On      : Thu Jan 19 17:38:38 1995
* Last Modified By: Blair MacIntyre
* Last Modified On: Sun Jul 30 23:58:20 1995
* Update Count    : 87
* Status          : Unknown, Use with caution!
*
* $Source: /opt/cvs/cm3/m3-comm/sharedobj/tests/tracker/src/TrackerPosition.m3,v $
* $Date: 2001-12-02 13:14:14 $
* $Author: wagner $
* $Revision: 1.1.1.1 $
*
* $Log: not supported by cvs2svn $
* Revision 1.1.1.1  1996/03/03 19:20:26  bm
* Imported Sources
*
*
* HISTORY
*)

MODULE TrackerPosition EXPORTS TrackerPosition, TrackerPositionF;

PROCEDURE Set (self: S; READONLY val: Data) =
  BEGIN
    self.data := val;
  END Set;

PROCEDURE Get (self: S; VAR val: Data) =
  BEGIN
    val := self.data;
  END Get;

BEGIN
END TrackerPosition.
