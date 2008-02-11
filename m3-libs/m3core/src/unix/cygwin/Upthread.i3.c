/* $Id$ */

#include <pthread.h>
#include <stdio.h>

#define SIZE(a) (((sizeof(a) + sizeof(int) - 1)) / sizeof(int))

int main()
{
    unsigned i;
    const static struct
    {
        const char* Format;
        unsigned Value;
    } Data[] = {
"(* $Id" "$ *)", 0,
"", 0,
"(* This file was generated from " __FILE__ ". Do not edit it. *)", 0,
"", 0,
"INTERFACE Upthread;", 0,
"", 0,
"END Upthread.", 0,
};
    for (i = 0 ; i != sizeof(Data)/sizeof(Data[0]) ; ++i)
    {
        printf(Data[i].Format, Data[i].Value);
        printf("\n");
    }
    return 0;
}
