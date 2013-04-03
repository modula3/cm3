#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif


#ifdef __APPLE__
#include <unistd.h>
#include <mach/mach_error.h>
#include <mach/mach_init.h>
#include <mach/mach_traps.h>
#include <mach/mach_vm.h>
#include <stdlib.h>
struct classify_adr_t; typedef struct classify_adr_t classify_adr_t;
struct classify_adr_t
{
    const char* name;
    void* init;
    void* base;
    size_t size;
};
static void get_adr_base_and_size(void* address, void** base, size_t* size);
static classify_adr_t adr_classes[] = {
    {"heap"},
    {"rdata", (void*)""},
    {"data", &adr_classes},
    {"text", (void*)get_adr_base_and_size }
    };
#define count_of(a) (sizeof(a)/sizeof((a)[0]))

static void get_adr_base_and_size(void* address, void** base, size_t* size)
{
    mach_port_t object_name = { 0 };
    task_t task = { 0 };
    vm_region_basic_info_data_t info = { 0 };
    mach_vm_size_t mach_vm_size = { 0 };
    mach_vm_address_t mach_vm_address = (size_t)address;
    mach_msg_type_number_t count = VM_REGION_BASIC_INFO_COUNT_64;
    kern_return_t status = task_for_pid(mach_task_self(), getpid(), &task);
    *base = 0;
    *size = 0;
    if (status)
        return;
    status = mach_vm_region(task, &mach_vm_address, &mach_vm_size, VM_REGION_BASIC_INFO,
			 (vm_region_info_t)&info, &count, &object_name);
    if (status)
        return;
    *base = (void*)(size_t)mach_vm_address;
    *size = (size_t)mach_vm_size;
}
static void classify_adr_init_1(classify_adr_t* c)
{
    if (c->base && c->size)
        return;
    get_adr_base_and_size(c->init, &c->base, &c->size);
}
static char* classify_adr_init(void)
{
    unsigned i;
    if (!adr_classes[0].init)
        adr_classes[0].init = malloc(1);
    for (i = 0; i < count_of(adr_classes); ++i)
        classify_adr_init_1(&adr_classes[i]);
}
static int classify_adr_1(void* a, classify_adr_t* c)
{
    char* b = (char*)a;
    char* base = (char*)c->base;
    /*printf("classify_adr_1 %p %p %p\n", b, base, base + c->size);*/
    return b >= base && b < (base + c->size);
}
static const char* classify_adr(void* a)
{
    unsigned i;
    classify_adr_init();
    for (i = 0; i < count_of(adr_classes); ++i)
        if (classify_adr_1(a, &adr_classes[i]))
            return adr_classes[i].name;
    return "";
}
#else
static char char* classify_adr(void* a)
{
    return "";
}
#endif

void
put_adr(const char* s, void* p)
{
    printf("%s %p %p %s %s\n", s, p, *(void**)p, classify_adr(p), classify_adr(*(void**)p));
}

#ifdef __cplusplus
} /* extern "C" { */
#endif
