#include <unistd.h>
#include <mach/mach_error.h>
#include <mach/mach_init.h>
#include <mach/mach_traps.h>
//#include <mach/vm_map.h>
#include <mach/mach_vm.h>
#include <stdio.h>
#include <stdlib.h>

int bss1;
int bss2;
void text1(){}
const int rdata1 = __LINE__;
const int rdata2 = __LINE__;
int data1 = __LINE__;
void text2(){}
int data2 = __LINE__;
const int rdata3 = __LINE__;
int data3 = __LINE__;
void text3(){}
int bss3;

void* rdata[2000] = { (void*)&rdata1, (void*)&rdata2, (void*)&rdata3, 0 };
void* data[] = { &data1, &data2, &data3, 0 };
void* bss[] = { &bss1, &bss2, &bss3, 0 };
void* text[] = { (void*)&text1, (void*)&text2, (void*)&text3, 0 };
void* strings[] = { (void*)"abc", (void*)"def", 0 };

void F1(const char* s, void** p)
{
    mach_vm_address_t address = (size_t)*p;

    printf("%s:", s);
    while (*p)
        printf("%p ", *p++);

    int nsubregions = 0;
    int num_printed = 0;
    vm_region_basic_info_data_t info = { 0 };
    mach_vm_size_t size = { 0 };
    mach_port_t object_name = { 0 };
    task_t task = { 0 };

    kern_return_t rc = task_for_pid(mach_task_self(), getpid(), &task);
    if (rc)
    {
        fprintf (stderr, "task_for_pid() failed with error %d - %s\n", rc, mach_error_string(rc));
        exit(1);
    }

    mach_msg_type_number_t count = VM_REGION_BASIC_INFO_COUNT_64;
    kern_return_t kret = mach_vm_region (task, &address, &size, VM_REGION_BASIC_INFO,
			 (vm_region_info_t) &info, &count, &object_name);
    if (kret != KERN_SUCCESS)
    {
        printf("mach_vm_region: Error %d - %s", kret, mach_error_string(kret));
        return;
    }
    printf("address:%p size:%p\n", (void*)address, (void*)size);
}

int main()
{
    F1("rdata", rdata);
    F1("data", data);
    F1("bss", bss);
    F1("text", text);
    F1("strings", strings);
    return 0;
}
