/*
 * waitpid.c --
 *
 *	This procedure emulates the POSIX waitpid kernel call on
 *	BSD systems that don't have waitpid but do have wait3.
 *	This code is based on a prototype version written by
 *	Mark Diekhans and Karl Lehenbauer.
 *
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

#ifndef lint
static char rcsid[] = "$Header: /opt/cvs/cm3/m3-libs/m3core/src/Csupport/SUN386/waitpid.c,v 1.1.1.1 2001-01-24 12:24:22 wagner Exp $ SPRITE (Berkeley)";
#endif /* not lint */

#include <sys/wait.h>
#include <errno.h>
#include <stdio.h>
#define WAIT_STATUS_TYPE union wait

/*
 * A linked list of the following structures is used to keep track
 * of processes for which we received notification from the kernel,
 * but the application hasn't waited for them yet (this can happen
 * because wait may not return the process we really want).  We
 * save the information here until the application finally does
 * wait for the process.
 */

typedef struct WaitInfo {
    int pid;				/* Pid of process that exited. */
    WAIT_STATUS_TYPE status;		/* Status returned when child exited
					 * or suspended. */
    struct WaitInfo *nextPtr;		/* Next in list of exited processes. */
} WaitInfo;

static WaitInfo *deadList = NULL;	/* First in list of all dead
					 * processes. */

/*
 *----------------------------------------------------------------------
 *
 * waitpid --
 *
 *	This procedure emulates the functionality of the POSIX
 *	waitpid kernel call, using the BSD wait3 kernel call.
 *	Note:  it doesn't emulate absolutely all of the waitpid
 *	functionality, in that it doesn't support pid's of 0
 *	or < -1.
 *
 * Results:
 *	-1 is returned if there is an error in the wait kernel call.
 *	Otherwise the pid of an exited or suspended process is
 *	returned and *statusPtr is set to the status value of the
 *	process.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

#ifdef waitpid
#   undef waitpid
#endif

int
waitpid(pid, statusPtr, options)
    int pid;			/* The pid to wait on.  Must be -1 or
				 * greater than zero. */
    int *statusPtr;		/* Where to store wait status for the
				 * process. */
    int options;		/* OR'ed combination of WNOHANG and
				 * WUNTRACED. */
{
    register WaitInfo *waitPtr, *prevPtr;
    int result;
    WAIT_STATUS_TYPE status;

    if ((pid < -1) || (pid == 0)) {
	errno = EINVAL;
	return -1;
    }

    /*
     * See if there's a suitable process that has already stopped or
     * exited. If so, remove it from the list of exited processes and
     * return its information.
     */

    for (waitPtr = deadList, prevPtr = NULL; waitPtr != NULL;
	    prevPtr = waitPtr, waitPtr = waitPtr->nextPtr) {
	if ((pid != waitPtr->pid) && (pid != -1)) {
	    continue;
	}
	if (!(options & WUNTRACED) && (WIFSTOPPED(waitPtr->status))) {
	    continue;
	}
	result = waitPtr->pid;
	*statusPtr = *((int *) &waitPtr->status);
	if (prevPtr == NULL) {
	    deadList = waitPtr->nextPtr;
	} else {
	    prevPtr->nextPtr = waitPtr->nextPtr;
	}
	free((char *) waitPtr);
	return result;
    }

    /*
     * Wait for any process to stop or exit.  If it's an acceptable one
     * then return it to the caller;  otherwise store information about it
     * in the list of exited processes and try again.  On systems that
     * have only wait but not wait3, there are several situations we can't
     * handle, but we do the best we can (e.g. can still handle some
     * combinations of options by invoking wait instead of wait3).
     */

    while (1) {
#if NO_WAIT3
	if (options & WNOHANG) {
	    return 0;
	}
	if (options != 0) {
	    errno = EINVAL;
	    return -1;
	}
	result = wait(&status);
#else
	result = wait3(&status, options, 0);
#endif
	if ((result == -1) && (errno == EINTR)) {
	    continue;
	}
	if (result <= 0) {
	    return result;
	}

	if ((pid != result) && (pid != -1)) {
	    goto saveInfo;
	}
	if (!(options & WUNTRACED) && (WIFSTOPPED(status))) {
	    goto saveInfo;
	}
	*statusPtr = *((int *) &status);
	return result;

	/*
	 * Can't return this info to caller.  Save it in the list of
	 * stopped or exited processes.  Tricky point: first check for
	 * an existing entry for the process and overwrite it if it
	 * exists (e.g. a previously stopped process might now be dead).
	 */

	saveInfo:
	for (waitPtr = deadList; waitPtr != NULL; waitPtr = waitPtr->nextPtr) {
	    if (waitPtr->pid == result) {
		waitPtr->status = status;
		goto waitAgain;
	    }
	}
	waitPtr = (WaitInfo *) malloc(sizeof(WaitInfo));
	waitPtr->pid = result;
	waitPtr->status = status;
	waitPtr->nextPtr = deadList;
	deadList = waitPtr;

	waitAgain: continue;
    }
}


/*************************************************************************
Return-Path: thomas@mw.lpc.ethz.ch 
Delivery-Date: Fri, 24 Jun 94 00:27:15 -0700
Return-Path: thomas@mw.lpc.ethz.ch
Received: by src-mail.pa.dec.com; id AA26587; Fri, 24 Jun 94 00:27:14 -0700
Received: from bernina-ether.ethz.ch by inet-gw-1.pa.dec.com (5.65/27May94)
	id AA09518; Fri, 24 Jun 94 00:24:12 -0700
Received: from mars.ethz.ch (actually mars-gw.ethz.ch) by bernina.ethz.ch 
          with SMTP inbound; Fri, 24 Jun 1994 09:23:48 +0200
Received: from merlot.ethz.ch by mars.ethz.ch; Fri, 24 Jun 94 09:23:46 +0200
From: thomas@mw.lpc.ethz.ch (Thomas Brupbacher)
Message-Id: <9406240723.AA27049@mars.ethz.ch>
Subject: waitpid.c for M3-3.3 / SUN386
To: kalsow@src.dec.com
Date: Fri, 24 Jun 94 9:23:44 MET DST
In-Reply-To: <9406231516.AA19704@jellybean.pa.dec.com>; from "kalsow@src.dec.com" at Jun 23, 94 8:16 am
X-Mailer: ELM [version 2.3 PL11]

Attached you'll find waitpid.c that I'm using. The copyright notice in
the header says it all.

Thomas Brupbacher
--
Thomas Brupbacher, Lab. f. Physikalische Chemie, ETH Zuerich, ch-8092 Zuerich
thomas@mw.lpc.ethz.ch   Phone:  +41/1/632-4340   Fax: +41/1/632-1021
*************************************************************************/
