/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#include "Uucontext.h"

void Uucontext_set_stack(ucontext_t* a, void* Start, size_t Size)
{
    a->uc_stack.ss_sp = Start;
    a->uc_stack.ss_size = Size;
}
