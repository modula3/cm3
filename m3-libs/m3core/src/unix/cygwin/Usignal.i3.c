/* $Id$ */

#include <signal.h>
#include <stdio.h>

int main()
{
    unsigned i;
    const static struct
    {
        const char* Format;
        unsigned Value;
    } Data[] =
{
"(* Copyright (C) 1990, Digital Equipment Corporation.                 *)", 0,
"(* All rights reserved.                                               *)", 0,
"(* See the file COPYRIGHT for a full description.                     *)", 0,
"(*                                                                    *)", 0,
"(* Last modified on Mon Jan  5 11:11:07 GMT 1998 by rrw               *)", 0,
"(*      modified on Fri Feb 24 15:18:21 PST 1995 by kalsow            *)", 0,
"(*      modified on Tue Feb 14 20:58:12 GMT 1995 by rrw1000@cam.ac.uk *)", 0,
"(*      modified on Tue Mar  2 17:18:02 PST 1993 by muller            *)", 0,
"", 0,
"(* $Id" "$ *)", 0,
"", 0,
"(* This file was generated from " __FILE__ ". Do not edit it. *)", 0,
"", 0,
"INTERFACE Usignal;", 0,
"", 0,
"FROM Ctypes IMPORT int;", 0,
"", 0,
"(*** <signal.h> ***)", 0,
"", 0,
"CONST", 0,
"  SIGINT = 16_%08x; (* interrupt *)", SIGINT,
"  SIGKILL = 16_%08x; (* kill (cannot be caught or ignored) *)", SIGKILL,
"", 0,
"TYPE", 0,
"  SignalHandler = ADDRESS;", 0,
"  SignalActionHandler = ADDRESS;", 0,
"", 0,
"", 0,
"(*** kill(2) - send signal to a process ***)", 0,
"", 0,
"<*EXTERNAL*> PROCEDURE kill (pid, sig: int): int;", 0,
"", 0,
"", 0,
"END Usignal.", 0,
};
    for (i = 0 ; i != sizeof(Data)/sizeof(Data[0]) ; ++i)
    {
        printf(Data[i].Format, Data[i].Value);
        printf("\n");
    }
    return 0;
}
