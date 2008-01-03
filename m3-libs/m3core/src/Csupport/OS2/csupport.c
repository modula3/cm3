#include <unistd.h>
#include <setjmp.h>

extern char _osmode;

int sigsetmask ()
  {
  return 0;
  }

int readlink ()
  {
  return -1;
  }

int setitimer ()
  {
  return 0;
  }

int vfork ()
  {
  return fork ();
  }

int getdtablesize ()
  {
  return (128);
  }

typedef long int __ptr_t;
typedef struct
  {
    long int __bx, __si, __di;
    __ptr_t __bp;
    __ptr_t __sp;
    __ptr_t __pc;
  } __jmp_buf[1];

/* 0 = ebx, 1 = esi, 2 = edi, 3 = esp, 4 = ebp, 5 = eip */
int __setjmp (__jmp_buf linux_buf)
  {
   asm ("pushl   %edi");            /* push edi */
   asm ("movl    8(%ebp),%edi");    /* edi = &jmp_buf [0] */

   asm ("movl    %ebx,0(%edi)");    /* jmp_buf [0].ebx = ebx */
   asm ("movl    %esi,4(%edi)");    /* jmp_buf [0].esi = esi */

   asm ("movl    -4(%ebp),%eax");   /* eax = pushed edi */
   asm ("movl    %eax,8(%edi)");    /* jmp_buf [0].edi = eax (pushed edi) */

   asm ("movl    0(%ebp),%eax");    /* eax = pushed ebp */
   asm ("movl    %eax,12(%edi)");   /* jmp_buf [0].ebp = eax (pushed ebp) */

   asm ("movl    %esp,%eax");       /* eax = esp */
   asm ("addl    $12,%eax");        /* eax = eax + 12 (edi+ebp+ret addr) */
   asm ("movl    %eax,16(%edi)");   /* jmp_buf [0].esp = old esp */

   asm ("movl    4(%ebp),%eax");    /* eax = ret addr */
   asm ("movl    %eax,20(%edi)");   /* jmp_buf [0].eip = ret addr */

   asm ("popl    %edi");            /* pop edi */
   return 0;
  }

void __longjmp (__jmp_buf linux_buf, int res)
  {
  asm ("movl    8(%ebp),%edi");    /* edi = jmp_buf */
  asm ("movl    12(%ebp),%eax");   /* eax = retval */
  asm ("cmpl    $0, %eax");        /* eax == 0 ? */
  asm ("jne     ret_ok");
  asm ("movl    $1, %eax");        /* eax = 1 */
asm ("ret_ok:");
  asm ("movl    12(%edi),%ebp");   /* ebp = jmp_buf [0].ebp */
  asm ("movl    16(%edi),%esp");   /* esp = jmp_buf [0].esp */

  asm ("pushl   20(%edi)");        /* push jmp_buf [0].eip */

  asm ("movl    0(%edi),%ebx");    /* ebx = jmp_buf [0].ebx */
  asm ("movl    4(%edi),%esi");    /* esi = jmp_buf [0].esi */
  asm ("movl    8(%edi),%edi");    /* edi = jmp_buf [0].edi */

  asm ("ret");                     /* actually jump to new eip */
  }

void _longjmp (__jmp_buf linux_buf, int res)
  {
  __longjmp (linux_buf, res);
  }

