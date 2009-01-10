/* Copyright (C) 1992, Digital Equipment Corporation          */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#if _MSC_VER > 1000
#pragma once
#endif

#ifndef RTERROR_INCLUDED
#define RTERROR_INCLUDED

#include <stddef.h>
#include "m3text.h"

#ifdef __cplusplus
extern "C" {
#endif

void RTError__MsgPC(size_t, TEXT);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
