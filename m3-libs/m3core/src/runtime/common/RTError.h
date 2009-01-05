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

void RTError__MsgPC(size_t, TEXT);

#endif
