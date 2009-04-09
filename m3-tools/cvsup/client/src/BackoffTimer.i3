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
 * $Id: BackoffTimer.i3,v 1.1.1.1 2009-04-09 17:01:33 jkrell Exp $ *)

INTERFACE BackoffTimer;

IMPORT Thread, Time;

TYPE
  T <: ROOT;

(* A "BackoffTimer.T", or backoff timer, supports timed retries through
   exponential backoff with random jitter. *)

PROCEDURE New(min, max: Time.T;
              backoff: REAL;
	      jitter: REAL): T;
(* Creates a new backoff timer with the given parameters. *)

(* Disregarding "jitter" for a moment, the first retry interval will be
   "min".  Subsequent retry intervals will increase by the factor
   "backoff" each time, until "max" is reached.

   "jitter" is applied after the above calculation.  It specifies the
   magnitude of the random component relative to the number calculated
   above.  For example, a "jitter" of 0.1 will cause the interval to be
   perturbed randomly each time by plus or minus 10 percent. *)

PROCEDURE Pause(bt: T);
(* Pause until it is time for the next retry. *)

PROCEDURE AlertPause(bt: T) RAISES {Thread.Alerted};
(* Like "Pause", but alertable. *)

PROCEDURE Get(bt: T): Time.T;
(* Returns the retry interval that will be used the next time "Pause" or
   "AlertPause" is called. *)

END BackoffTimer.
