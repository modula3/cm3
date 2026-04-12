// This is a proof of concept for threads under DJGPP.
// g++ -fno-stack-clash-protection
// g++ -std=gnu++11
//
#include <assert.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <setjmp.h>
#include <signal.h>
#include <sys/time.h>
#include <string.h>
#include <atomic>
#include <sys/exceptn.h>
#include <dpmi.h>
using namespace std;

typedef void* (*P)(void*);

struct Thread
{
    // stack
    char* free;
    char* map;
    size_t map_size;
    size_t pagesize;
    char* mprotect[2];

    P p;
    void* arg;
    jmp_buf jb;
};

void* thread (void* arg)
{
    char buf[999];
    int len = sprintf(buf, "thread%ld\n", (long)arg);
    while (1)
    {
        write(1, buf, len);
        sleep(1);
    }
    return 0;
}

atomic<int> g_Critical;
#define MAX_THREAD 1024
Thread g_Threads[MAX_THREAD];
atomic<size_t> g_CurrentThread;
atomic<size_t> g_ThreadCount;

Thread* create_thread(P p, void* arg, size_t size = 2 << 20) 
{
    ++g_Critical;
    Thread* thread = &g_Threads[g_ThreadCount++];
    size_t pagesize = getpagesize();
    char* aligned_start = 0;
    char* aligned_end = 0;
    char* unaligned_start = 0;
    char* unaligned_end = 0;
    size_t pages = 0;
    size_t align = 0;

    thread->pagesize = pagesize;
    thread->p = p;
    thread->arg = arg;
    // TODO locking

    // round size up to pagesize and add 4 pages
    size = (size + pagesize - 1) / pagesize + 4 * pagesize;

    thread->free = unaligned_start = (char*)malloc (size);

    align = ((size_t)unaligned_start) % pagesize;
    if (align)
        aligned_start = unaligned_start + pagesize - align;
    else
        aligned_start = unaligned_start;

    unaligned_end = unaligned_start + size;
    aligned_end =  unaligned_end - align;

    // real code would mprotect each end

    if (setjmp(thread->jb) == 0)
    {
        thread->jb->__esp = (size_t)(aligned_end - pagesize);
        thread->jb->__eip = (size_t)p;
        *(size_t*)(thread->jb->__esp -= sizeof(size_t)) = (size_t)arg;
        *(size_t*)(thread->jb->__esp -= sizeof(size_t)) = 0; // return addres and alignment
        *(size_t*)(thread->jb->__esp -= sizeof(size_t)) = 0; // return addres and alignment
        *(size_t*)(thread->jb->__esp -= sizeof(size_t)) = 0; // return addres and alignment
    }
    --g_Critical;
    return thread;
}

void CopyContext (jmp_buf to, jmp_buf from)
{
    // This is based on setjmp/longjmp.
    assert(from->__eip);
    to->__eip = from->__eip;
    to->__ebx = from->__ebx;
    to->__ecx = from->__ecx;
    to->__edx = from->__edx;
    to->__ebp = from->__ebp;
    to->__esi = from->__esi;
    to->__edi = from->__edi;
    to->__esp = from->__esp;
    to->__eflags = from->__eflags;
}

void timer (int a)
{
    int state = __dpmi_get_and_disable_virtual_interrupt_state ();

    //todo synchronize better i.e. with all malloc, etc.
    if (g_Critical == 0)
    {
        ++g_Critical;

        size_t previousThread = g_CurrentThread;
        size_t nextThread = g_CurrentThread;

        auto exception_state_ptr = __djgpp_exception_state_ptr;
        assert (exception_state_ptr);

        // Skip ahead to a non-running thread.
        do
        {
            nextThread = (nextThread + 1) % MAX_THREAD;
        } while (g_Threads[nextThread].jb->__eip == 0);

        // Longjmp out of a djgpp exception handler is not allowed.
        // It results in the signal being blocked indefinitely even
        // if unblocked.
        //
        // Instead modify the state returned to on the stack via
        // semi-internal __djgpp_exception_state_ptr.
        //
        if (previousThread != nextThread)
        {
            CopyContext(g_Threads[previousThread].jb, *exception_state_ptr);
            CopyContext(*exception_state_ptr, g_Threads[nextThread].jb);
            g_Threads[nextThread].jb->__eip = 0; // mark as running
            g_CurrentThread = nextThread;
        }

        --g_Critical;
    }
    __dpmi_get_and_set_virtual_interrupt_state (state);
}

int main()
{
    struct timeval selected_interval;
    struct itimerval it;

    memset (&selected_interval, 0, sizeof (selected_interval));
    memset (&it, 0, sizeof (it));

    selected_interval.tv_sec = 0;
    selected_interval.tv_usec = 100 * 1000;
    it.it_interval = selected_interval;
    it.it_value    = selected_interval;

    for (int i = 0; i < MAX_THREAD; ++i)
        create_thread (thread, (void*)(intptr_t)i);

    signal(SIGALRM, timer);
    setitimer(ITIMER_REAL, &it, NULL);

    while (1)
        sleep(100);
}
