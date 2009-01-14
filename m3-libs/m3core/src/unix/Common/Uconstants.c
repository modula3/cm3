/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <assert.h>
#include <stddef.h>
#include <errno.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/socket.h>

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

const int Uexec_WNOHANG = WNOHANG;

const int Usignal_SIGINT = SIGINT;
const int Usignal_SIGKILL = SIGKILL;

const int Unix_FIONREAD = FIONREAD;

const int Unix_O_RDONLY = O_RDONLY;
const int Unix_O_RDWR = O_RDWR;
const int Unix_O_CREAT = O_CREAT;
const int Unix_O_EXCL = O_EXCL;
const int Unix_O_TRUNC = O_TRUNC;
const int Unix_O_NONBLOCK = O_NONBLOCK;
const int Unix_O_NDELAY = O_NONBLOCK; /* compat */
const int Unix_M3_NONBLOCK = O_NONBLOCK; /* compat */

const int Unix_F_OK = F_OK;
const int Unix_X_OK = X_OK;
const int Unix_W_OK = W_OK;
const int Unix_R_OK = R_OK;
