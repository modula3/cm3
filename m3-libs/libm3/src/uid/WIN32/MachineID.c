/* Copyright (C) 1993, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/*                                                             */
/* Last modified on Mon Sep 20 11:46:17 PDT 1993 by kalsow     */
/*      modified on Thu Jul 15 16:23:08 PDT 1993 by swart      */

#include <string.h>
#include <memory.h>
#include <winsock2.h>
#include <iphlpapi.h>
#include <stdlib.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

/* older cm3 config files do not link with iphlpapi.lib so use LoadLibrary/GetProcAddress */
typedef DWORD (__stdcall * GetIfTable_t)(PMIB_IFTABLE pIfTable, PULONG pdwSize, BOOL bOrder);

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
    static GetIfTable_t s_getIfTable;
    GetIfTable_t getIfTable = s_getIfTable;
    UINT pass;

    ZeroMemory(id, 6);
    
    if (getIfTable == NULL)
    {
        HMODULE hmodule = LoadLibraryW(L"iphlpapi.dll");
        if (hmodule == NULL)
            goto Exit;
        getIfTable = (GetIfTable_t)GetProcAddress(hmodule, "GetIfTable");
        if (getIfTable == NULL)
            goto Exit;
        s_getIfTable = getIfTable;
    }

    /* Call until it fits, typically twice. */
    Size = 0;
    while ((Error = getIfTable(Table, &Size, TRUE)) == ERROR_INSUFFICIENT_BUFFER)
    {
        free(Table);
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
        MIB_IFROW* Row = &Table->table[i];
        unsigned char * phys = Row->bPhysAddr;
        size_t len = Row->dwPhysAddrLen;
        if (len == 6)
        {
            printf("%02X%02X%02X%02X%02X%02X\n",
                   phys[0], phys[1], phys[2], phys[3], phys[4], phys[5]);
        }
     }
#endif
 
    for (pass = 0; pass <= 1; ++pass)
    {
        for (i = 0; (!Success) && (i < NumEntries); ++i)
        {
            MIB_IFROW* Row = &Table->table[i];
            unsigned char * phys = Row->bPhysAddr;
            size_t len = Row->dwPhysAddrLen;
            if ((Row->dwType != IF_TYPE_ETHERNET_CSMACD || len != 6)
                || (pass == 0 && len == 6 && memcmp(&phys[3], "RAS", 3) == 0))
            {
#if 0
                printf("skipping %X/%02X%02X%02X%02X%02X%02X\n", len,
                       phys[0], phys[1], phys[2], phys[3], phys[4], phys[5]);
#endif
                continue;
            }
            memcpy(id, phys, 6);
        }
        Success = TRUE;
    }
Exit:
    free(Table);
    return Success;
}

#ifdef __cplusplus
} /* extern "C" */
#endif

#if 1 /* test code */

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
