(* Copyright (C) 1996, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Thu Aug 15 10:23:58 PDT 1996 by steveg *)

INTERFACE AppBackup;

<* PRAGMA LL *>

IMPORT App, Rd, Time, Wr;

(* Provides a simple interface to implement a backup file.

   After "init" is called, the "read" and "write" methods are
   called. 

   "read" is called once soon after "init" has returned and
   anytime "SynchronousRead" is called.

   "write" is called after "self.changed" is signaled, but not more
   often than "wait" seconds after the last write (unless "SynchronousWrite"
   was called).
 *)

TYPE
  T <: TPublic;
  TPublic = MUTEX OBJECT
    name: TEXT;
    log: App.Log;
  METHODS
    init(fileName: TEXT; wait: Time.T; log: App.Log): T RAISES {App.Error};
    modified();
    (* called to indicate the related data structure has changed and the
       backup file should be written with "self.wait" seconds. *)
    read(rd: Rd.T; initial: BOOLEAN) RAISES {App.Error};
    <* LL = self *>
    write(wr: Wr.T) RAISES {App.Error};
    <* LL = self *>
  END;

PROCEDURE SynchronousRead(t: T; initial: BOOLEAN) RAISES {App.Error};
PROCEDURE SynchronousWrite(t: T) RAISES {App.Error};
  <* LL < t *>

END AppBackup.
