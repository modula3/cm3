# Copyright (C) 1990, Digital Equipment Corporation
# All rights reserved.
# See the file COPYRIGHT for a full description.

# Last modified on Wed Jul 30 13:55:56 EST 1997 by hosking

# --- #define _ASM
# --- #include <sys/asm_linkage.h>

# ---	ENTRY(RTStack__Flush)
	.section ".text"
	.align 4
	.global RTStack__Flush
	.type RTStack__Flush, #function
RTStack__Flush:

# ---	ta ST_FLUSH_WINDOWS
        ta 0x03
        retl
        nop

# ---	SET_SIZE(RTStack__Flush)
	.size RTStack__Flush, (.-RTStack__Flush)
