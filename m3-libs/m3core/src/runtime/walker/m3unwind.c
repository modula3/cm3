#if _WIN64
#include "m3unwind.h"
#if defined(_AMD64_)
#define INSTRUCTION Rip
#define STACK       Rsp
#elif defined(_X86_)
#define INSTRUCTION Eip
#define STACK       Esp
#elif defined(_ARM64_)
#define INSTRUCTION Pc
#define STACK       Sp
#else
#error unknown target
#endif

/*void unw_getcontext(unw_context_t* context) { RtlCaptureContext(&context->context); }*/

/* History table is for two pass dispatch+unwind and flows as an opaque
* pointer RtlDispatchException => dispatcher context => frame handler => RtlUnwindEx.
* Pass NULL and nothing is lost. Backtrace is one pass.
* There is no history to revisit.
*/
const PUNWIND_HISTORY_TABLE UnwindHistoryTable = NULL;

void unw_init_local (unw_cursor_t* cursor, unw_context_t* unwind_context)
{
    PCONTEXT context = &unwind_context->context;
    SIZE_T instruction = context->INSTRUCTION;
    SIZE_T stack = context->STACK;
    ZeroMemory(cursor, sizeof(*cursor));
    cursor->FunctionEntry = RtlLookupFunctionEntry (instruction, &cursor->ImageBase, UnwindHistoryTable);
    cursor->context = *context;
    GetCurrentThreadStackLimits (&cursor->StackLow, &cursor->StackHigh);
    if (cursor->FunctionEntry)
        cursor->OffsetInFunction = (instruction - (cursor->ImageBase + cursor->FunctionEntry->BeginAddress));
}

void unw_get_reg (unw_cursor_t* cursor, unsigned short nreg, size_t* reg)
{
    PCONTEXT context = &cursor->context;
    *reg = 0;
    switch (nreg) {
    case UNW_REG_IP: *reg = context->INSTRUCTION; break;
    case UNW_REG_SP: *reg = context->STACK; break;
    }
}

int unw_step (unw_cursor_t* cursor)
{
    int ret = 1;
    PCONTEXT context = &cursor->context;
    SIZE_T instruction = context->INSTRUCTION;
    SIZE_T stack = context->STACK;
    if (stack < cursor->StackLow || stack >= cursor->StackHigh || instruction == 0 || instruction == stack)
        return -1;
    if (cursor->FunctionEntry) {
        /* Handler and HandlerData might be off by one frame from libunwind point of view,
         * but are not used. */
        cursor->Handler = (size_t)RtlVirtualUnwind (
            UNW_FLAG_UHANDLER | UNW_FLAG_EHANDLER,
            cursor->ImageBase, instruction, cursor->FunctionEntry, context, &cursor->HandlerData, &cursor->EstablisherFrame, 0);
        cursor->FunctionEntry = RtlLookupFunctionEntry (context->INSTRUCTION, &cursor->ImageBase, UnwindHistoryTable);
        cursor->OffsetInFunction =
          (cursor->FunctionEntry ? (context->INSTRUCTION - (cursor->ImageBase + cursor->FunctionEntry->BeginAddress)) : 0);
    } else {
        instruction = *(size_t*)stack;
        if (instruction == stack || instruction == 0)
            ret = -1;
        else
            stack += sizeof(stack);
        context->INSTRUCTION = instruction;
        context->STACK = stack;
        cursor->OffsetInFunction = 0;
    }
    return ret;
}

int unw_get_proc_name (unw_cursor_t* cursor, char* name, size_t len, size_t* OffsetInFunction)
/* Function names come from debug symbols or exports and are not generally available. */
{
    if (OffsetInFunction) *OffsetInFunction = cursor->OffsetInFunction;
    if (name && len) name[0]=0;
    return 0;
}

int unw_get_proc_info (unw_cursor_t* cursor, unw_proc_info_t* info)
{
    ZeroMemory (info, sizeof(*info));
    if (cursor->FunctionEntry) {
        info->start_ip = (cursor->ImageBase + cursor->FunctionEntry->BeginAddress);
        info->end_ip = (cursor->ImageBase + cursor->FunctionEntry->EndAddress);
        info->handler = cursor->Handler;
        info->lsda = (size_t)cursor->HandlerData;
    }
    return 0;
}
#endif
