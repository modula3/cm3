        .seg    "text"
        .globl  RTMachine__SaveRegsInStack
RTMachine__SaveRegsInStack:
#if defined(__arch64__) || defined(__sparcv9)
        save    %sp,-128,%sp
        flushw
        ret
          restore %sp,2047+128,%o0
#else /* 32 bit SPARC */
        ta      0x3   ! ST_FLUSH_WINDOWS
        mov     %sp,%o0
        retl
        nop
#endif /* 32 bit SPARC */
        .size RTMachine__SaveRegsInStack, (.-GC_save_regs_in_stack)
