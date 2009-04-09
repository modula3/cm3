(* Copyright 1998-2003 John D. Polstra.
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

INTERFACE AccessRules;

IMPORT IP, Logger, Pathname, Rd, Thread, Time;

TYPE
  T <: ROOT;

  CheckResult = { OK, TooMany, AuthRequired, Denied };

PROCEDURE Check(rules: T;
                addr: IP.Address;
		READONLY clients: ARRAY OF IP.Address): CheckResult;
(* Checks whether it is OK to serve a client at the given address.
   "clients" is an array containing the IP addresses of the clients
   currently being served, including the current one.  Array entries
   containing "IP.NullAddress" are ignored. *)

PROCEDURE Get(path: Pathname.T;
              maxAge: Time.T;
	      logger: Logger.T := NIL): T
  RAISES {Rd.Failure, Thread.Alerted};
(* Returns the access rules, reading or refreshing them from the given
   file if necessary.  The rules are refreshed if the file has changed,
   or if it has not been read during the past "maxAge" seconds. *)

END AccessRules.
