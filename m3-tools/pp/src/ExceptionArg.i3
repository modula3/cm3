(*
   ExceptionArg.i3
   David Nichols, Xerox PARC.
   August, 1991

   $Id$
*)

(* Copyright (c) 1991, 1992 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. *)

INTERFACE ExceptionArg;

(* This interface defines a single type meant to be used as the argument to
   exceptions.  It should be used as follows.

   Within an "abstraction," a collection of related interfaces and modules,
   ExceptionArg.T is subtyped to create new error codes (don't forget to
   brand the subtypes).  The typecodes of these new objects serve as the
   enumeration that defines the error type, while the "info" field contains
   an explanation meant for human consumption.  An exception handler that
   receives an ExceptionArg.T as the parameter can do a TYPECASE to
   determine the cause at whatever level of detail it likes.

   For example, a file system might define subtypes called ResourceFailure
   and IOFailure, corresponding to running out of some resource or
   encountering a low-level I/O error, respectively.  ResourceFailure could
   have subtypes named DiskFullFailure and QuotaFailure to distinguish
   between the entire disk being full vs a user's quota being exceeded.  A
   program that caught the exception could then use TYPECASE on any of
   these types.

   Each subtype of ExceptionArg.T can add additional fields necessary to
   characterize the situtation.  QuotaFailure could include the user's
   quota, for example.

   When exceptions cross abstraction boundaries, the subArg field is used.
   Often, the calling abstraction will catch the exception and translate it
   to an exception of its own.  In this case, the old ExceptionArg.T can be
   placed in the subArg field, providing the top-level handler or debugger
   with more information about the history of this exception.

   For example, a mail system might catch a file system exception and
   raise an exception describing its inability to deliver a mail message.
   The resultant ExceptionArg.T would have explanations of both the
   delivery failure and the quota failure that caused it.

   A suggested naming convention is to choose a verb (e.g. "to err" or "to
   fail") and to name the exception in the past tense of the verb ("Erred"
   or "Failed").  The first-level subtype of ExceptionArg.T can be named
   with the corresponding noun ("Error" and "Failure").  This leads to noun
   phrases for the other subtypes ("CommunicationsFailure",
   "ParameterError", etc). *)

TYPE
  T = OBJECT
        info  : TEXT;
        subArg: T      := NIL;
      END;

END ExceptionArg.
