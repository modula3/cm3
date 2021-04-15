/* Copyright (C) 1993, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/*                                                             */
/* Last modified on Mon Sep 20 11:46:17 PDT 1993 by kalsow     */
/*      modified on Thu Jul 15 16:23:08 PDT 1993 by swart      */

#pragma warning(disable:4820) /* padding inserted */
#pragma warning(disable:4255) /* () change to (void) */
#pragma warning(disable:4668) /* #if of undefined symbol */

#include <string.h>
#include <memory.h>
#include <stdlib.h>
#include <stdio.h>
#include <windows.h>
#include "iphlpapi.h"

#ifdef __cplusplus
extern "C" {
#endif

// Get the 48bit MAC adddress.
int/*BOOL*/
__cdecl
MachineIDC__CanGet(unsigned char* id)
{
    BOOL Success = { 0 };
    DWORD Error = { 0 };
    DWORD Size = { 0 };
    MIB_IFTABLE* Table = { 0 };
    DWORD i = { 0 };
    DWORD NumEntries = { 0 };
    UINT pass;

    ZeroMemory(id, 6);

    /* Call until it fits, typically twice. */
    Size = 0;
    while ((Error = GetIfTable(Table, &Size, TRUE)) == ERROR_INSUFFICIENT_BUFFER)
    {
        free(Table);
        /* Favor calloc over malloc for the "safety" of zero-initialization.
         * HeapAlloc also has a flag HEAP_ZERO_MEMORY.
         */
        Table = (MIB_IFTABLE*)calloc(1, Size);
        if (Table == NULL)
        {
            Error = ERROR_NOT_ENOUGH_MEMORY;
            goto Exit;
        }
    }
    if (Error)
        goto Exit;

    NumEntries = Table->dwNumEntries;
#if 0
    printf("Table->dwNumEntries %d\n", NumEntries);
    for (i = 0; (!Success) && (i < NumEntries); ++i)
    {
        MIB_IFROW* const Row = &Table->table[i];
        unsigned char * const phys = Row->bPhysAddr;
        printf("%X/%u/%02X%02X%02X%02X%02X%02X\n", Row->dwPhysAddrLen, Row->dwType,
            phys[0], phys[1], phys[2], phys[3], phys[4], phys[5]);
     }
#endif

    for (pass = 0; pass <= 2; ++pass)
    {
        for (i = 0; i < NumEntries; ++i)
        {
            MIB_IFROW* const Row = &Table->table[i];
            unsigned char * const phys = Row->bPhysAddr;
            size_t const len = Row->dwPhysAddrLen;
           DWORD const type = Row->dwType;

           if (len != 6)
               continue;

           /* Be pickier on earlier passes. */

            if (pass == 0 && type != IF_TYPE_ETHERNET_CSMACD)
                continue;

            if (pass != 0 && type != IF_TYPE_ETHERNET_CSMACD && type != IF_TYPE_IEEE80211)
                continue;

            if ((pass == 0 || pass == 1) && memcmp(&phys[3], "RAS", 3) == 0)
                continue;

            memcpy(id, phys, 6);
            Success = TRUE;
            goto Exit;
        }
    }
Exit:
    free(Table);
    return Success;
}

#ifdef __cplusplus
} /* extern "C" */
#endif

#if 0 /* test code */

int main()
{
    unsigned char id[6] = { 0 };
    BOOL i = { 0 };
    
    i = MachineIDC__CanGet(id);
    printf("%d %02X-%02X-%02X-%02X-%02X-%02X %c%c%c%c%c%c\n", i,
           id[0], id[1], id[2], id[3], id[4], id[5],
           id[0], id[1], id[2], id[3], id[4], id[5]);
    system("getmac");

    return 0;
}

#endif
