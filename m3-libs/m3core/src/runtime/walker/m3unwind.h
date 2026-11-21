/* A quick libunwind abstraction and Windows implementation to satisfy Modula-3.
 * TODO: CaptureStackBackTrace instead.
 */
#pragma once
#if !_WIN32
#include <libunwind.h>
#elif _WIN64
#include <stddef.h>
#include "windows.h"
#if __cplusplus
extern "C" {
#endif

typedef size_t unw_word_t;

typedef struct _unw_proc_info_t {
    size_t start_ip;
    size_t end_ip;
    size_t lsda;
    size_t handler;
} unw_proc_info_t;

typedef struct _unw_context_t {
    CONTEXT context;
} unw_context_t;
/* Cursor is conceptually just context, but it
 * stores stack bounds as an optimization
 * and FunctionEntry to fix a fence post problem,
 * where init_local produces the first result, not step.
 */
typedef struct _unw_cursor_t {
    CONTEXT context;
    PRUNTIME_FUNCTION FunctionEntry;
    SIZE_T ImageBase;
    SIZE_T StackLow;         /* to terminate/sanitize unwind at end of stack */
    SIZE_T StackHigh;        /* to terminate unwind at end of stack */
    SIZE_T EstablisherFrame; /* for hypothetical call to RtlUnwind */
    size_t Handler;
    PVOID  HandlerData;
    SIZE_T OffsetInFunction;
} unw_cursor_t;

/* unw_getcontext must be a tailcall OR it must unwind its context. 
 * Otherwise we will end up later unwinding through a returned stack.
 * Here we take effectively tailcall approach, by force inline, by macro.
 *void unw_getcontext(context);*/
#define unw_getcontext(c) RtlCaptureContext(&(c)->context)
void unw_init_local (unw_cursor_t*, unw_context_t*);
void unw_get_reg (unw_cursor_t*, unsigned short, size_t*);
#define UNW_REG_IP 1
#define UNW_REG_SP 2
int unw_step (unw_cursor_t*);
int unw_get_proc_name (unw_cursor_t*, char*, size_t, size_t*);
int unw_get_proc_info (unw_cursor_t* cursor, unw_proc_info_t* info);

#if __cplusplus
}
#endif
#endif
