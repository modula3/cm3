/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#if _MSC_VER > 1000
#pragma once
#endif

#ifndef UUCONTEXT_INCLUDED
#define UUCONTEXT_INCLUDED

#include <stddef.h>

#if defined(__OpenBSD__)
#include "context.h"
#else
#include <ucontext.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

void Uucontext__set_stack(ucontext_t* a, void* Start, size_t Size);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
