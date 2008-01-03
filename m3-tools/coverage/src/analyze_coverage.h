/* Copyright (C) 1989, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

/* Last modified on Fri Nov  3 11:26:06 1989 by muller         */

COMMAND ('r', READ_DATABASE,       LIST)
COMMAND ('w', WRITE_DATABASE,      LIST)
COMMAND ('l', SHOW_LINES,          LIST)
COMMAND ('L', SHOW_LINES_ALL,      NONE)
COMMAND ('p', SHOW_PROCS,          LIST)
COMMAND ('P', SHOW_PROCS_ALL,      NONE)
COMMAND ('S', AUGMENT_SOURCE_PATH, LIST)
COMMAND ('o', SELECT_OUTPUT,       ONE)
COMMAND ('v', VERBOSE_ON,          NONE)
COMMAND ('q', QUIT,                NONE)
COMMAND ('e', EXEC_FILE,           ONE)
