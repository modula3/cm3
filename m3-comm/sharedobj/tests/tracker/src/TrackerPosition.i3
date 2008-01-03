(*
* TrackerPosition.i3 -- a tracker position object
* Copyright (C) Blair MacIntyre 1995
* Author          : Blair MacIntyre
* Created On      : Thu Jan 19 17:38:38 1995
* Last Modified By: Blair MacIntyre
* Last Modified On: Wed Jul 26 22:32:23 1995
* Update Count    : 115
* Status          : Unknown, Use with caution!
*
* $Source: /opt/cvs/cm3/m3-comm/sharedobj/tests/tracker/src/TrackerPosition.i3,v $
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

INTERFACE TrackerPosition;

IMPORT Logitech, SharedObj, Thread;

CONST Brand = "TrackerPosition";

TYPE
  Data = Logitech.TrackerPosition;

  (* This interface serves as an example of how a programmer would
  create a shared object using the SharedObj package.

  We will follow the Modula-3 standard library convention that the
  main type in an interface (the one a user would allocate) is named
  "T" (thus requiring it be referred to as "TrackerPosition.T" in this
  case).  Additionally, the package designer would create a second
  type "Public" and declare that "T is a subtype of Public" via the
  opaque type declaration

  | TYPE
  |   T <: Public;
  |   Public = OBJECT ... END;

  Public would then contain those parts of the object that the package
  designer wished the user of their package to see.  "T" would then
  typically be revealled in the implementation of the package as a
  direct subtype of "Public" via the revelation

  | REVEAL
  |   T = Public BRANDED "Some brand" OBJECT ... END;

  In order for the shared object package to function, however, all of
  the methods of "Public" must be overriden by the shared object
  stub generation program to implement the proper shared object
  semantics.  This requires that the stub generator can make the above
  revelation, overriding all the methods as required.

  The general idea is therefore that a programmer creates an
  additional opaque revelation that can be used by the stub generator.
  Further, the base type of the object must be the shared object type
  "SharedObj.T".  For example, in this interface we declare 

  | TYPE
  |   T <: S;
  |   S <: Public;
  |   Public = SharedObj.T OBJECT ... END;

  The proper use of the package requires that the type that a user
  perform some equivalent opaque revelations, does not reveal the
  details of the top revelation ("T <: S", in this case) {\it and} not
  BRAND any of the revelations they do make.  In particular, in this
  example it requires that it can make the revelation:

  | REVEAL
  |   TrackerPosition.T = TrackerPosition.S BRANDED 
  |                       TrackerPosition.Brand OBJECT ... END;

  The main type in the interface, "T", is therefore the shared object.
  Users can use the supertype, "S" in this case, if they desire a
  non-shared version (when a non-shared version is passed between
  machines, the resulting copy is a completely new object, just as any
  other OBJECT would be handled).

  An important restriction of the system is that subtypes of the
  shared objects are not, in general, valid shared objects.  If you
  wish to make a shared object that is a subtype of another shared
  object, you should have your new object inherit from the {\it
  non-shared} object and have the stub generator generate a new shared
  object for your type.  To reiterate, inheriting from a shared object
  will not produce correct results.

  When an object is defined, the "SHARED UPDATE METHODS" pragma should
  be used to inform the stub generator which methods are update
  methods.  Any method that might cause the object data to be changed
  is an update method.  The copies of a shared object are kept
  synchronized by having all invocations of updates methods be
  executed on all copies in the same order.  It is therefore crucial
  that this pragma be used correctly!

  Access to the object is controlled by a single writer, multiple
  reader paradigm.  Update methods are executed on an object within an
  exclusive lock and non-update methods are executed inside a shared
  lock.  This allows multiple non-update methods to execute in
  parallel, while ensuring that update methods have exclusive access
  to the object while they are updating its internal state.
  Therefore, multiple threads can access the object simultaneously
  without problem.  

  This safety results in an important restriction, however: no update
  method can call any other update method of the same object.
  Non-Update methods can call any other methods and update methods can
  call non-update methods.  

  The ability of non-update methods to call update methods allows the
  designer of a shared object great flexibility in deciding how much
  of the processing required for an update be done once (at the
  calling site) and how much be done at all sites (during the update).
  For example, if a significant amount of work needs to be done to
  process the arguments for an update it could be done inside a
  non-update method and then the results used to call an update
  methods which updates the object state.  More importantly, because
  of the nature of the distributed update mechanism, certain
  parameters are prohibited from being used in update methods.
  Examples include any kinds of types that cannot be stored without
  changing their semantics or are associated with a particular
  machine, such as data stream readers or writers, network objects,
  shared objects, threads, condition variables, mutexs, etc.

  *)

  T <: S;
  S <: Public;
  Public = SharedObj.T OBJECT
           METHODS
             set (READONLY val: Data) RAISES {SharedObj.Error};
             get (VAR val: Data) RAISES {SharedObj.Error, Thread.Alerted};
             <* SHARED UPDATE METHODS set *>
           END;

END TrackerPosition.
