/* Copyright 1998-2003 John D. Polstra.
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
 */

/*
 * Modula-3's signal interfaces are so inconsistent across architectures
 * that it's easier to use C.
 */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*Handler)(int);

/*
 * Set a signal handler, ensuring that SIGVTALRM (and hence thread
 * scheduling) will be blocked when the handler is executing.
 */
Handler
__cdecl
UnixMiscSignal(int sig, Handler func)
{
    struct sigaction sa = { 0 };
    struct sigaction osa = { 0 };

    memset(&sa, 0, sizeof(sa));
    memset(&osa, 0, sizeof(osa));
    sa.sa_handler = func != NULL ? (Handler)func : SIG_DFL;
    sigemptyset(&sa.sa_mask);
#ifdef SIGVTALRM
    sigaddset(&sa.sa_mask, SIGVTALRM);
#endif
#ifdef SA_RESTART
    sa.sa_flags |= SA_RESTART;
#endif
    if (sigaction(sig, &sa, &osa) == -1)
	return (Handler)-1;
    return (Handler)osa.sa_handler;
}

/*
 * Find out whether a signal is currently ignored.
 */
int
__cdecl
UnixMiscSigIsIgnored(int sig)
{
    struct sigaction osa = { 0 };

    memset(&osa, 0, sizeof(osa));
    return sigaction(sig, NULL, &osa) == -1 || osa.sa_handler == SIG_IGN;
}

#ifdef __cplusplus
} /* extern C */
#endif
