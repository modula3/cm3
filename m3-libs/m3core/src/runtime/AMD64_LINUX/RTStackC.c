/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/* Last modified on Thu May  4 09:34:11 PDT 1995 by kalsow     */
/*      modified on Tue May 18 13:21:11 PDT 1993 by muller     */
/*      modified on Tue Jan 19 15:20:48 PST 1993 by burrows    */

/* This file implements the stack walking functions of
   the RTStack interface. */

#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <signal.h>

#include <libunwind.h>

//del this when tested
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif


/* TYPE Frame = RECORD pc, sp: ADDRESS;  
 *                     cursor : ADDRESS;
 *                     lock : INTEGER; END; */
typedef struct {
  unsigned long pc;
  unsigned long sp;
  unw_cursor_t *cursor;
  long lock;
} Frame;

#define FrameLock 0x1234567890

//just a test
void show_backtrace (void) {
  unw_cursor_t cursor; unw_context_t uc;
  unw_word_t ip, sp;

  int ret;
  char name[50];
  size_t name_len = 50;
  unw_word_t ofp;

  unw_getcontext(&uc);
  unw_init_local(&cursor, &uc);
  while (unw_step(&cursor) > 0) {
    unw_get_reg(&cursor, UNW_REG_IP, &ip);
    unw_get_reg(&cursor, UNW_REG_SP, &sp);
    printf ("ip = %lx, sp = %lx\n", (long) ip, (long) sp);

    ret = unw_get_proc_name(&cursor, name, name_len, &ofp);
    printf("name %s\n",name);
  }
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE ProcName (READONLY f: Frame): ADDRESS;
   Return the null-terminated constant string that names the procedure
   corresponding to the stack frame "f".  Returns NIL if no name is
   known. */

char* RTStack__ProcName (Frame *f)
{
  int ret;
  char *name;
  size_t name_len = 50;
  unw_word_t ofp;

  name = (char *) malloc(name_len);
//fix this - mem leak if alloced here. should do it in m3
  ret = unw_get_proc_name(f->cursor, name, name_len, &ofp);
  if (ret == 0) {
printf("ProcName %s\n",name);
    return name;
  } else {
    return 0;
  }
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE GetThreadFrame (VAR f: Frame;  start: ADDRESS;  len: INTEGER);
   Return in "f" the frame of the thread whose machine state is in bytes
   [start .. start+len).  Returns with f.pc=NIL on failure. */

void RTStack__GetThreadFrame (Frame *f, char *start, int len)
{

// not sure we need this. dont think its called and its
// intel specific regs ??
printf("Err GetThreadFrame\n");

}

/*---------------------------------------------------------------------------*/
/* PROCEDURE CurrentFrame (VAR(*OUT*) f: Frame)
 * returns the frame that corresponds to its caller. */

void RTStack__CurFrame (Frame *f)
{
  unw_context_t *uc;
  unw_cursor_t *cursor;
  unw_word_t ip, sp;

  //callback to m3 to do the alloc on the traced heap ??
  uc = (unw_context_t *) malloc(sizeof(unw_context_t));
  cursor = (unw_cursor_t *) malloc(sizeof(unw_cursor_t));

//  printf("CurFrame size context %d size cursor %d\n",sizeof(unw_context_t), sizeof(unw_cursor_t));

  f->lock = FrameLock;
  unw_getcontext(uc);
  unw_init_local(cursor, uc);
  unw_get_reg(cursor, UNW_REG_IP, &ip);
  unw_get_reg(cursor, UNW_REG_SP, &sp);
  //test using bp for stack pointer
  unw_get_reg(cursor, UNW_X86_64_RBP, &sp);
  f->pc = ip;
  f->sp = sp;
  f->cursor = cursor;

//printf ("ip = %lx, sp = %lx\n", (long) ip, (long) sp);

  if (f->lock != FrameLock) abort ();
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE PreviousFrame (READONLY callee: Frame;  VAR(*OUT*)caller: Frame)
   Return the stack frame that called "callee".  Returns with pc = NIL if
   "callee" is the first frame on the stack or its predecessor is ill-formed.
   */

void RTStack__PrevFrame (Frame* callee, Frame* caller)
{
  unw_word_t ip, sp;
  int ret;
  char *str;

//printf("PrevFrame \n");

if (!callee->cursor) printf("no cursor\n");

  if (callee->lock != FrameLock) abort ();
  *caller = *callee;

  ret = unw_step(caller->cursor);
  if (ret > 0) {
    unw_get_reg(caller->cursor, UNW_REG_IP, &ip);
    unw_get_reg(caller->cursor, UNW_REG_SP, &sp);
  //test using bp for stack pointer
  unw_get_reg(caller->cursor, UNW_X86_64_RBP, &sp);
    caller->pc = ip;
    caller->sp = sp;
//printf ("cip = %lx, csp = %lx\n", (long) ip, (long) sp);
  } else {
//printf("PrevFrame ret err\n");
    caller->pc = 0;
    caller->sp = 0;
  }
  if (caller->lock != FrameLock) abort ();
}


/*---------------------------------------------------------------------------*/
/* PROCEDURE Unwind (READONLY f: Frame);
   Restore the machine state back to the frame "f".  All callee-saved
   registers must be restored to the state they were in when frame "f"
   made its last call. */

void RTStack__Unwind (Frame *target)
{
  int ret;

//printf("Unwind \n");
if (!target->cursor) printf("no cursor\n");

  if (target->lock != FrameLock) abort ();
  ret = unw_resume(target->cursor);
  //success means unreachable from here
if (ret < 0) printf("unwind error\n");
}

/*
 * Get the procedure info for this frame including the personality function
 * and language specific data
 *
 * The unw_get_proc_info() routine returns auxiliary information about the procedure that created the stack frame identified by argument cp. The pip argument is a pointer to a structure of type unw_proc_info_t which is used to return the information. The unw_proc_info_t has the following members:

unw_word_t start_ip
The address of the first instruction of the procedure. If this address cannot be determined (e.g., due to lack of unwind information), the start_ip member is cleared to 0.
unw_word_t end_ip
The address of the first instruction beyond the end of the procedure. If this address cannot be determined (e.g., due to lack of unwind information), the end_ip member is cleared to 0.
unw_word_t lsda
The address of the language-specific data-area (LSDA). This area normally contains language-specific information needed during exception handling. If the procedure has no such area, this member is cleared to 0.
unw_word_t handler
The address of the exception handler routine. This is sometimes called the personality routine. If the procedure does not define a personality routine, the handler member is cleared to 0.
unw_word_t gp
The global-pointer of the procedure. On platforms that do not use a global pointer, this member may contain an undefined value. On all other platforms, it must be set either to the correct global-pointer value of the procedure or to 0 if the proper global-pointer cannot be obtained for some reason.
unw_word_t flags
A set of flags. There are currently no target-independent flags. For the IA-64 target, the flag UNW_PI_FLAG_IA64_RBS_SWITCH is set if the procedure may switch the register-backing store.
int format
The format of the unwind-info for this procedure. If the unwind-info consists of dynamic procedure info, format is equal to UNW_INFO_FORMAT_DYNAMIC. If the unwind-info consists of a (target-specific) unwind table, it is equal to to UNW_INFO_FORMAT_TABLE. All other values are reserved for future use by libunwind. This member exists for use by the find_proc_info() call-back (see unw_create_addr_space(3)). The unw_get_proc_info() routine may return an undefined value in this member.
int unwind_info_size
The size of the unwind-info in bytes. This member exists for use by the find_proc_info() call-back (see unw_create_addr_space(3)). The unw_get_proc_info() routine may return an undefined value in this member.
void *unwind_info
The pointer to the unwind-info. If no unwind info is available, this member must be set to NULL. This member exists for use by the find_proc_info() call-back (see unw_create_addr_space(3)). The unw_get_proc_info() routine may return an undefined value in this member.
 */

void RTStack__GetProcInfo(Frame *f) {

  int res;
  unw_proc_info_t info;

  res = unw_get_proc_info(f->cursor, &info);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
