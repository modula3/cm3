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
 * $Id: WatchDog.m3,v 1.1.1.1 2009-04-09 17:02:02 jkrell Exp $ *)

MODULE WatchDog;

IMPORT
  Thread, Time;

REVEAL
  T = Thread.Closure BRANDED OBJECT
    target: Thread.T;
    thread: Thread.T;
    mu: MUTEX;			(* Monitors the following data members. *)
    timeout: Time.T;
    reset := FALSE;
    canceled := FALSE;
    expired := FALSE;
  OVERRIDES
    apply := Apply;
  END;

PROCEDURE Apply(wd: T): REFANY =
  VAR
    timeout: Time.T;
  BEGIN
    LOCK wd.mu DO
      timeout := wd.timeout;
    END;
    LOOP
      TRY
	Thread.AlertPause(timeout);
      EXCEPT Thread.Alerted => (* Just break out of the AlertPause. *) END;
      LOCK wd.mu DO
	IF wd.canceled THEN
	  EXIT;
	ELSIF wd.reset THEN
	  timeout := wd.timeout;
	  wd.reset := FALSE;
	ELSE
	  Thread.Alert(wd.target);
	  wd.expired := TRUE;
	  EXIT;
	END;
      END;
    END;
    RETURN NIL;
  END Apply;

PROCEDURE Cancel(wd: T) =
  BEGIN
    LOCK wd.mu DO
      wd.canceled := TRUE;
      Thread.Alert(wd.thread);
    END;
  END Cancel;

PROCEDURE Expired(wd: T): BOOLEAN =
  BEGIN
    LOCK wd.mu DO
      RETURN wd.expired;
    END;
  END Expired;

PROCEDURE Reset(wd: T;
                timeout: Time.T := -1.0d0) =
  BEGIN
    LOCK wd.mu DO
      <* ASSERT NOT wd.canceled *>
      IF timeout >= 0.0d0 THEN
	wd.timeout := timeout;
      END;
      wd.reset := TRUE;
      Thread.Alert(wd.thread);
    END;
  END Reset;

PROCEDURE New(timeout: Time.T;
              target: Thread.T := NIL): T =
  VAR
    wd: T;
  BEGIN
    IF target = NIL THEN target := Thread.Self() END;
    wd := NEW(T,
      mu := NEW(MUTEX),
      timeout := timeout,
      target := target);
    LOCK wd.mu DO
      wd.thread := Thread.Fork(wd);
    END;
    RETURN wd;
  END New;

BEGIN
END WatchDog.
