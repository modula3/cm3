(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 20 08:48:49 PST 1995 by msm      *)
(*      modified on Tue Mar 10 19:11:14 1992 by steveg   *)
(*      modified on Mon Feb 24 13:56:12 PST 1992 by muller   *)
(*      modified on Sat Nov  2 15:59:41 PST 1991 by gnelson  *)
<*PRAGMA LL*>

INTERFACE TrestleConf;

(* The export implementation of TrestleConf is a noop.  The SRC
   implementation imports the Argo conference control machinery, if
   available.  You must have set your ARGOSERVER environment variable for
   this to do anything.  If you set ARGOCONFERENCE the application will
   join the named conference, if it exists; otherwise, the application will
   join whatever conference is currently active for USER *)

TYPE
  User <: UserPublic;

  UserPublic = OBJECT
    name: TEXT;
    displayName: TEXT
  METHODS <* LL = VBT.mu *>
    register()
  END;

(* User instantiates the register method; all Users must be registered 
   before use. *)

TYPE <* LL = VBT.mu *>
  UserProc = PROCEDURE (user, displayName: TEXT): User;

(* return NIL if the user's display can't be opened; the created User's
   register method will be called before this procedure returns *)

<* LL arbitrary; call must return before any other calls on
   this interface are made. *>
PROCEDURE Init(createUser: UserProc);

TYPE
  App <: AppPublic;

  AppPublic = OBJECT METHODS
    <* LL = VBT.mu *>
    (* client-supplied methods *)
    add(user: User);
    delete(user: User);
    suspend(user: User);
    activate(user: User);
    (* system supplied methods *)
    init(user: User);
    destroy()
  END;

  (* App implements the init method; clients should create Apps, and init
     them.  The application is added to the current conference for the user. 
     The destroy method is invoked when the application wants to destroy
     itself.
     The remaining calls are to be implemented by the client of this interface;
     conference control will call add and delete to attach or delete the
     application from the given user's session, and activate and suspend to say
     whether the app should be visible or not.  The application should
     be destroyed when it has no active users; invoking destroy
     causes all users to be deleted from the app.  The initial state of
     an app added to a user is suspended. *)

END TrestleConf.
