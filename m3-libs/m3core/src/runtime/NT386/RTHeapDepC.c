/* Copyright 1996, Critical Mass, Inc.   All rights reserved. */

/* The stubs that get from Modula-3 code into external DLLs
   make an indirect call through "m3_arg_check" to validate
   any possible heap pointers.  The variable must be statically
   initialized to avoid problems during startup.  RTHeapDep
   sets the final value. */

static void no_check (int mask) { }

void (*m3_arg_check)(int) = no_check;

