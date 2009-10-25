        .seg    "text"
        .globl  ThreadPThread__FlushWindows
ThreadPThread__FlushWindows:
        ta      0x3   ! ST_FLUSH_WINDOWS
        mov     %sp,%o0
        retl
        nop
        .size ThreadPThread__FlushWindows, (.-ThreadPThread__FlushWindows)
