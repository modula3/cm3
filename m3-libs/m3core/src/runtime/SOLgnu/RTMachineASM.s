        .seg    "text"
        .globl  RTMachine__SaveRegsInStack
RTMachine__SaveRegsInStack:
        ta      0x3   ! ST_FLUSH_WINDOWS
        mov     %sp,%o0
        retl
        nop
        .size RTMachine__SaveRegsInStack, (.-RTMachine__SaveRegsInStack)
