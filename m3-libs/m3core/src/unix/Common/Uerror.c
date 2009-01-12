/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#include <errno.h>

/* enable this and preprocess file to get approximate *.i3 contents, fix them in up in editor */
#if 0
#define X(x) <* EXTERNAL Uerror_##x *> VAR #x: int;
#include "UerrorX.h"
#undef X
#endif

/* check that Max is enough; if you get an error here, raise it in Uerror.i3 and here */
typedef int CheckMax[128 - sizeof(union{
#define X(x) char a##x[x];
#include "UerrorX.h"
#undef X
})];

#define X(x) const int Uerror_##x = x;
#include "UerrorX.h"
#undef X
