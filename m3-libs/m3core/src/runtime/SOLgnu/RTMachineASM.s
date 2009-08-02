  .file "RTMachineASM.s"
  .section ".text",#alloc,#execinstr
  .align 8
  .global RTMachine__SaveRegsInStack
  .type RTMachine__SaveRegsInStack,#function
RTMachine__SaveRegsInStack:
        ta      0x3   ! ST_FLUSH_WINDOWS
        mov     %sp,%o0
        retl
        nop
        .size RTMachine__SaveRegsInStack, (.-RTMachine__SaveRegsInStack)
