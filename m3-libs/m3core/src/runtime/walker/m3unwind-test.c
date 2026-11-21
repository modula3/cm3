/* a test of generating a backtrace with libunwind.
 * Run it, and run it under a debugger, break on ntdll!RtlCaptureContext
 * and observe stack and compare to what it prints. They match.
 *
 * For example:
 *
 * 0:000> k
 *
 * Child-SP          RetAddr               Call Site
 * 000000e9`6f32ee58 00007ff6`a783731e     ntdll!RtlCaptureContext
 * 000000e9`6f32ee60 00007ff6`a7837419     m3unwind_test!show_backtrace+0x8e
 * 000000e9`6f32f8e0 00007ff6`a7837e78     m3unwind_test!main+0x9
 * (Inline Function) --------`--------     m3unwind_test!invoke_main+0x22
 * 000000e9`6f32f910 00007ffc`fb18dda7     m3unwind_test!__scrt_common_main_seh+0x10c
 * 000000e9`6f32f950 00007ffc`fbcfee4c     KERNEL32!BaseThreadInitThunk+0x17
 * 000000e9`6f32f980 00000000`00000000     ntdll!RtlUserThreadStart+0x2c
 *
 * 0:000> g
 * ip:7ff6a783731e sp:e96f32ee60 ofp:8E
 * ip:7ff6a7837419 sp:e96f32f8e0 ofp:9
 * ip:7ff6a7837e78 sp:e96f32f910 ofp:10C
 * ip:7ffcfb18dda7 sp:e96f32f950 ofp:17
 * ip:7ffcfbcfee4c sp:e96f32f980 ofp:2C
 */
#include <stdio.h>
#include "m3unwind.h"

void show_backtrace (void) {
  unw_cursor_t cursor={0};
  unw_context_t uc={0};
  unw_word_t ip=0;
  unw_word_t sp=0;
  int ret=0;
  char name[50]={0};
  size_t name_len = 50;
  unw_word_t ofp=0;

  unw_getcontext(&uc);
  unw_init_local(&cursor, &uc);
  do {
    unw_get_reg(&cursor, UNW_REG_IP, &ip);
    unw_get_reg(&cursor, UNW_REG_SP, &sp);
    printf ("ip:%llx sp:%llx", (unsigned long long)ip, (unsigned long long)sp);

    ret = unw_get_proc_name(&cursor, name, name_len, &ofp);
    if (name[0]) printf(" name:%s",name);
    printf(" ofp:%lX", (unsigned long)ofp);
    printf("\n");
  } while (unw_step(&cursor) > 0 && ip);
}

int main() {
    show_backtrace();
}
